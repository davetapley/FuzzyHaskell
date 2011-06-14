%include lhs2TeX.fmt
%if False

>   module FuzzyPLF where
>   import Prelude hiding ( (&&), (||), not, and, or, any, all )
>   import qualified Prelude ( (&&), (||), not )
>   import List hiding ( and, or, any, all )
>   import Maybe 

>   infix 0 ==>
>   
>   class Logic a where
>       true, false :: a
>       (&&), (||)  :: a -> a -> a
>       not         :: a -> a
>       (==>)       :: Double -> a -> a
>   
>       and, or     :: [a] -> a
>       and         = foldr (&&) true
>       or          = foldr (||) false
>       any, all    :: Logic b => (a -> b) -> [a] -> b
>       any p       = or . map p
>       all p       = and . map p

>   instance Logic Bool where
>       true    = True
>       false   = False
>       p&&q    = p Prelude.&& q
>       p||q    = p Prelude.|| q
>       not     = Prelude.not
>       p==>q   = (p==1) Prelude.&& q

%endif

\subsection{The FuzzyPLF module}
\label{fuzzyPLF}

Here we see how the piecewise linear function approach to representing a membership function (as introduced by van den Broek) may be unified with the |Logic| class approach given by Meehan and Joy in Animated Fuzzy Logic \cite{meehan1998afl}. 
In the latter paper the module name |Fuzzy| was chosen, thus to indicate the difference this module shall be called |FuzzyPLF|. To demonstrate the use of the |Fuzzy| module an example program residing in the module |Shoe| was given and accordingly our version shall be called |ShoePLF|, its implementation may be found in Section \ref{shoePLF}.

>   type Point  = (Double, Double)
>   type PLF    = [Point]

>   instance Logic PLF where
>       true    = [(0,1), (1,1)]
>       false   = [(0,0), (1,0)]
>       (&&)    = junction min
>       (||)    = junction max
>       not a   = [(x,1-y) | (x,y) <- a]
>       a ==> c = [(x,y*a) | (x,y) <- c]

Let's review the implementations which define our new |PLF| type as an instance of the |Logic| class.
\begin{itemize}
    \item The definitions for |true| and |false| introduces a restriction not present in the |instance Logic Double|, namely a domain must be imposed at this stage such that start and ending domain values can be chosen. Here the domain is specified as the $[0,1]$ range. Whether this restriction should be considered a disadvantage is questionable when one considers that the |Double| implementation shall impose a domain range when discretisation takes place in the defuzzification step. 
    \item Conjunction and disjunction of piecewise linear fuzzy sets is performed by the abstracted |junction| function which is covered below. It can be seen here that it takes an operator which evaluates two co-domain values, thus the operator is expected to be a t-norm or t-conorm. In mimicking the behaviour of the original paper we can see Zadeh's minimum and maximum operators are used.
    \item To get a Zadeh negation (as used in the Meehan and Joy paper) of the function we take one minus each co-domain value. Because the function is linear in between these points the one minus will hold at any point.
\end{itemize}

Next we look at the |junction| function. It and its supporting functions give the functionality provided by the Miranda functions |dac|, |maxplf| and |minplf| in the van den Broek papers \cite{vandenbroek1997frc} covered in Chapter \ref{litReview}. The technique used by |junction| differs slightly in such a way as to provide more what the author regards more readable code, as with the original implementation it relies on the fact that in taking a junction of two fuzzy sets the given t-(co)norm function is applied only to the co-domain values. The critical difference is that van den Broek's implementation moves through both lists (representing the PLFs) at the same time, generating new domain values where necessary and computing the corresponding co-domain values at the same time. In contrast the Haskell implementation given here computes two compatible PLFs (|x1''| and |x2''|) and then combines them using a list comprehension.

>   junction :: (Double -> Double -> Double) -> PLF -> PLF -> PLF
>   junction f x1 x2 = [(x, f a b) | ((x,a),(_,b)) <- zip x1'' x2'']
>       where
>           x1' = discretise x1 x2
>           x2' = discretise x2 x1
>           x1'' = xovers x1' x2'
>           x2'' = xovers x2' x1'

The term compatible PLFs is used to denote two which have identical sets of domain values
\footnote{Haskell's list comprehension syntax requires the more correct expression |((x,a),(x,b))| to be written in the implementation as |((x,a),(_,b))| in the |junction| function.}
such that at no point in a linear interval between two domain values does the greater co-domain change from one PLF to the other. Formally we state that for a common indexed set of domain values $X$ and two PLFs with membership functions $f_{1}$ and $f_{2}$ we must satisfy equation \ref{eqn:compatibleDomains}
\begin{equation}
    \forall x_{n}, x_{n+1} \in X,  
    \forall x', x'' \in [x_{n}, x_{n+1}], i
    ¬(f_{1}(x') < f_{2}(x') \wedge f_{1}(x') > f_{2}(x'')
    \label{eqn:compatibleDomains}
\end{equation}

To implement this two functions are used to make two PLFs compatible, they are |discretise| and |xovers|. The former ensures the PLF given as the first argument contains all the domain values the PLF in the second argument has and the latter identifies cross over points: 

\begin{enumerate}
    \item Below we see a list comprehension is used to compute all the extra domain and co-domain pairs (those domain values which are present in the second PLF (|plf2|) but not the first (|plf1|). It can be read as:
    
    Return pairings of |x| and its co-domain value in the first PLF (|apply x1 x|), where the |x|s are the domain values of the second PLF (|map fst x2|) which are not domain values in the first PLF |not $ elem x (map fst x1)|. Because all extra domain and co-domain pairs are appended (|++|) to the end of the first PLF the list must be sorted by the |orderpoints| function before being returned. 

>   discretise :: PLF -> PLF -> PLF
>   discretise x1 x2 =  orderpoints $ x1 ++ [(x, apply x1 x) | x <- map fst x2, not $ elem x (map fst x1)]

    \item Checking all the cross over points requires that both input PLFs have identical sets of domain points as guaranteed by the |discretise| function. With this true one pairs up the points in each PLF to create two lists of segments, the corresponding segments in each PLF are then inspected with the |xover| function to see if they cross. 

>   xovers :: PLF -> PLF -> PLF
>   xovers x1 x2 =  orderpoints $ x1 ++ (catMaybes $ map xover $ zip (pairs x1) (pairs x2))

Below |xover| checks if the lines given by two segments cross over each other.
If not (such that all co-domain values in the second line are either above or are all below the first line) then |Nothing| is returned. In the case that they do |Just| the point at which they cross is returned. The |catMaybes| function in |xovers| can then eliminate those segments which do not contain a cross and append those which do.

>   xover :: ((Point,Point), (Point,Point)) -> Maybe Point
>   xover (((x0,a0), (x1,a1)), ((_,b0), (_,b1)))
>       | (a1-a0) == (b1-b0)    = Nothing
>       | (x0>=y) || (y>=x1)    = Nothing
>       | otherwise             = Just (y,c)
>       where
>           y = x0 + (x1-x0) * (a0-b0) / (a0-b0 + b1-a1)
>           c = a0 + (a1-a0) * (y-x0) / (x1-x0)

\end{enumerate}

%if False

>   orderpoints :: Ord a => [(a,b)] -> [(a,b)]
>   orderpoints = sortBy (\a b -> compare (fst a) (fst b))

>   pairs :: [a] -> [(a,a)]
>   pairs xs = zip xs (tail xs)

%endif

The |apply| function shown below is a direct Haskell implementation of the Miranda function of the same name and serving the same purpose in van den Broek's paper. We shall see how it is unified with the Meehan and Joy system in the module |ShoePLF|.

>   apply :: PLF -> Double -> Double
>   apply [ ] _ = 0
>   apply [_] _ = 0
>   apply ((x0,y0) : (x1,y1) : zs) x 
>       | x < x0            = 0
>       | (x0==x1)||(x>x1)  = apply ((x1,y1) : zs) x
>       | otherwise         = y0 + (x-x0)*(y1-y0) / (x1-x0)

Piecewise linear versions of the |up|, |down| and |tri| functions are implemented using the van den Broek representation. One notable deviation from the Meehan and Joy versions are that domain values outside the $[a,b]$ are undefined with these representations. To alleviate this problem the |apply| function can be seen to return a membership of grade of 0 for any domain value which is not covered by the representation.

>   up, down, tri :: Double -> Double -> PLF

>   up a b      = [(a,0), (b,1)]
>   down a b    = [(a,1), (b,0)]
>   tri a b     = [(a,0), (mid,1), (b,0)]
>       where mid   = a + ( (b-a) / 2)

The four defuzzification techniques shown in "Animated Fuzzy Logic" are now implemented for van den Broek representation. Here we see the advantage of the representation become apparent as the values are computed numerically rather than relying on discretisation; this topic was covered in Section \ref{fuzzyInferencing}.

Due to the nature of the algorithm used for the |centroid| function the piecewise function must be |bound| at both ends. This can be seen to find the |minimum| and |maximum| domain values (i.e. the start and end points of the function) and create new co-domain values of zero for these. The general recursion scheme provided by |foldr2| processes each segment of the piecewise function in turn (provided by the repeated application of tail implemented as |iterate tail|) until all segments have been computed (and only one domain / co-domain pair remains as identified by |(> 1) . length|).

>   centroid :: PLF -> Double
>   centroid plf = (1/(6*(0.5 * (sum $ area $ bound plf)))) * (sum $ cent $ bound plf)
>       where
>           bound xs = [(minimum $ map fst xs, 0)] ++ xs ++ [(maximum $ map fst xs, 0)]
>           cent = foldr2 (\((x0,y0):(x1,y1):_) -> (x0+x1) * ( (x0*y1) - (x1*y0) )) 
>           area = foldr2 (\((x0,y0):(x1,y1):_) -> ( (x0*y1) - (x1*y0) )) 
>           foldr2 f xs = map f . takeWhile ((> 1) . length) . iterate tail $ xs



The median of maximum defuzzification technique is trivially defined as a function of the other two.

>   minmax, medmax, maxmax :: PLF -> Double
>   medmax plf = (minmax plf + maxmax plf) / 2

We can see |minmax| and |maxmax| utilise the list comprehension covered in the review of Haskell syntax in Section \ref{funcParadigm} and described there.

>   minmax plf = minimum [x | (x,y) <- plf, y >= maxy]
>       where maxy = maximum (map snd plf)
>   maxmax plf = maximum [x | (x,y) <- plf, y >= maxy]
>       where maxy = maximum (map snd plf)

Notably the |rulebase| function is identical to the definition in ``Animated Fuzzy Logic'', this is due to ad-hoc polymorphism as covered in Section \ref{funcParadigm}. 

>   rulebase :: Logic a => (a -> a -> a) -> [a] -> a
>   rulebase = foldr1 

import List
import Graphing
import Windowing

--import qualified Fuzzy
--import qualified FuzzyPLF
--import qualified Shoe
--import qualified ShoePLF

main = do
    -- newWindow "DavePlot" (subPlot [map ((:[]) . meehan) allSizes])
    -- newWindow "DavePlot" (subPlot [take 3 both, (take 3 . drop 3) both, drop 6 both])
    newWindow "DavePlot" (subPlot [[[], replicate 5 plf2],[replicate 10 plf3, replicate 20 plf4]])

    
allSizes = [1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95]

h10  = [1.5, 1.55.. 1.95]
h50  = [1.5, 1.525.. 1.95]
h100 = [1.5, 1.51.. 1.95]


-- top = pad 4 $ take 3 $ allsizes 
-- mid = pad 4 $ take 4 $ drop 3 $ allsizes 
-- bot = pad 4 $ drop 7 $ allsizes 
-- 
-- pad n xs = xs ++ replicate (n - length xs) []
-- 
plf1 :: Graphing.PLF
plf1 = [(0,0), (2,1), (4,0.5), (6,1), (8,1)]
-- 
plf2 :: Graphing.PLF
plf2 = [(0,1), (1,1), (2,0), (3,0)]

plf3 :: Graphing.PLF
plf3 = [(0,0), (2,1), (3,0)]

plf4 :: Graphing.PLF
plf4 = [(0,0), (1,1), (3,0)]
-- 
-- limiter :: Float -> PLF
-- limiter n = [(0,n), (3,n)]
-- 
-- PLF2down = zipWith (junction min) (replicate 6 PLF2) [limiter n|n<-[1,0.9..]]
-- PLF3down = zipWith (junction min) (replicate 6 PLF3) [limiter n|n<-[1,0.9..]]

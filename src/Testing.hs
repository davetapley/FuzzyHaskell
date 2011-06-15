--import List
import Graphing
import Windowing

main = do
    newWindow "DavePlot" (plotGraphs [[[], replicate 5 plf2],[replicate 10 plf3, replicate 20 plf4]])

    
plf1 :: Graphing.PLF
plf1 = [(0,0), (2,1), (4,0.5), (6,1), (8,1)]
-- 
plf2 :: Graphing.PLF
plf2 = [(0,1), (1,1), (2,0), (3,0)]

plf3 :: Graphing.PLF
plf3 = [(0,0), (2,1), (3,0)]

plf4 :: Graphing.PLF
plf4 = [(0,0), (1,1), (3,0)]

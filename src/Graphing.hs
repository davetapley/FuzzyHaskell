    module Graphing (plotGraphs, Point, PLF) where
    import Graphics.UI.GLUT hiding (minmax)
    import Graphics.UI.GLUT.Fonts
    import Graphics.Rendering.OpenGL.GL.RasterPos
    
    import Control.Monad

    type Point = (GLfloat, GLfloat)
    type PLF = [Point]

    plotGraphs :: [[[PLF]]] -> IO ()
    plotGraphs plfs' = 
        let plfs = (map . map . map . map) (\(x,y)->(realToFrac x, realToFrac y)) plfs' in do
        clear [ColorBuffer]
        mapM_ plot (positionSubgraphs (-0.9, 0.9) (-0.9, 0.9) plfs)
        flush

    positionSubgraphs :: Point -> Point -> [[[PLF]]] -> [(Point, Point, GLfloat, PLF)]
    positionSubgraphs lr tb plfs = [(x,y,z,plf) | (x,rs ) <- zip (sections lr plfs) plfs,
                                                  (y,ls ) <- zip (sections tb rs) rs,
                                                  (z,plf) <- zip [0, 0.1..] ls]
    
    sections :: (GLfloat, GLfloat) -> [a] -> [(GLfloat, GLfloat)]
    sections (min, max) xs = 
        [(i*((max-min)/(n))+min+0.1, (i+1)*((max-min)/n)+min-0.1) | i <- [0..n-1]]
        where n = (fromIntegral . length) xs

    plot :: (Point, Point, GLfloat, PLF) -> IO ()
    plot (x, y, z, plf) = when (not $ null plf) $ do
        when (z == 0.0) $ plotAxis x y z (minmax $ map fst plf) (minmax $ map snd plf)
        plotLines (toGLPoints plf x y z)


    plotLines :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
    plotLines cs = mapM_ (uncurry plotLine) (pairs cs)
        where pairs xs = zip xs (tail xs)


    plotLine :: (GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> IO () 
    plotLine (x1,y1,z1) (x2,y2,z2) = 
        renderPrimitive Lines $ mapM_ vertex [Vertex3 x1 y1 z1, Vertex3 x2 y2 z2] 


    toGLPoints :: PLF -> Point -> Point -> Float -> [(GLfloat, GLfloat, GLfloat)]
    toGLPoints ps (x1,x2) (y1,y2) z = [(x1+(xf * (x-xmin)), y1+(yf*y), z) | (x,y) <- ps]
        where
            (xmin, xmax) = minmax (map fst ps)
            (ymin, ymax) = minmax (map snd ps)
            xf = (x2-x1) / (xmax - xmin)
            yf = (y2-y1) 

    minmax xs = (minimum xs, maximum xs)

    plotAxis :: Point -> Point -> Float -> (Float,Float) -> (Float,Float) -> IO ()
    plotAxis (x1,x2) (y1,y2) z (xmin,xmax) (ymin,ymax) = do
       plotLine  (x1,y1,z) (x2,y1,z)
       plotLine  (x1,y1,z) (x1,y2,z)
       plotString (x1, (y1-0.1)) z (show xmin)
       plotString (x2, (y1-0.1)) z (show xmax)
       plotString ((x1-0.1), y1) z (show ymin)
       plotString ((x1-0.1), y2) z (show ymax)

    plotString :: Point -> Float -> String -> IO ()
    plotString (x,y) z str = do
       currentRasterPosition $= Vertex4 x y z 1
       renderString Fixed8By13 str

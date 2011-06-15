    module Windowing where
    import System.Exit
    import Data.IORef
    import Control.Exception
    import Graphics.UI.GLUT

    data LookAtMatrix = LookAtMatrix {eye :: Vertex3 GLdouble, center :: Vertex3 GLdouble, up :: Vector3 GLdouble}

    rootLookAtMatrix :: LookAtMatrix 
    rootLookAtMatrix = LookAtMatrix (Vertex3 0 0 0) (Vertex3 0 0 (-100)) (Vector3 0 1 0)

    newWindow :: IO () -> IO ()
    newWindow renderWorldContent = do
        (progName, args) <- getArgsAndInitialize
        createWindow progName

        lookAtRef <- newIORef rootLookAtMatrix
        displayCallback $= display renderWorldContent lookAtRef
        keyboardMouseCallback $= Just (kbm renderWorldContent lookAtRef)
        actionOnWindowClose $= MainLoopReturns

        mainLoop

    display :: IO () -> IORef LookAtMatrix -> DisplayCallback
    display renderWorldContent lookAtRef = do
        clear [ColorBuffer]
        loadIdentity

        LookAtMatrix eye center up <- readIORef lookAtRef
        lookAt eye center up

        renderWorldContent
        flush

    kbm :: IO () -> IORef LookAtMatrix -> KeyboardMouseCallback
    kbm renderWorldContent lookAtRef key keystate modifiers position = 
         let update dx dy dz = updateLookAtRefCenter lookAtRef (Vertex3 dx dy dz) >> display renderWorldContent lookAtRef in do

             case key of
                 SpecialKey KeyLeft      -> update (-1) 0 0
                 SpecialKey KeyRight     -> update 1 0 0
                 SpecialKey KeyUp        -> update 0 (-1) 0
                 SpecialKey KeyDown      -> update 0 1 0
                 _                       -> return ()

    updateLookAtRefCenter :: IORef LookAtMatrix -> Vertex3 GLdouble -> IO ()
    updateLookAtRefCenter ref centerDelta = modifyIORef ref updateCenter
        where updateCenter lookAtMatrix = LookAtMatrix (eye lookAtMatrix) (center lookAtMatrix `add` centerDelta) (up lookAtMatrix)

    {-# INLINE add #-}
    add :: Num a => Vertex3 a -> Vertex3 a -> Vertex3 a
    add (Vertex3 v1x v1y v1z) (Vertex3 v2x v2y v2z) = Vertex3 (v1x + v2x) (v1y + v2y) (v1z + v2z)

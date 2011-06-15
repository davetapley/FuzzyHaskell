    module Windowing where
    import System.Exit
    import Data.IORef
    import Control.Exception
    import Graphics.UI.GLUT

    newWindow :: IO () -> IO ()
    newWindow renderWorldContent = do
        (progName, args) <- getArgsAndInitialize
        lookAtRef <- newIORef (Vertex3 0 0 (-100))
        createWindow progName
        displayCallback $= display renderWorldContent lookAtRef
        keyboardMouseCallback $= Just (kbm renderWorldContent lookAtRef)
        actionOnWindowClose $= MainLoopReturns
        mainLoop

    display :: IO () -> IORef (Vertex3 GLdouble) -> DisplayCallback
    display renderWorldContent lookAtRef = do
        clear [ColorBuffer]
        pov <- readIORef lookAtRef
        loadIdentity
        lookAt (Vertex3 0 0 0) pov (Vector3 0 1 0)
        renderWorldContent
        flush

    kbm :: IO () -> IORef (Vertex3 GLdouble) -> KeyboardMouseCallback
    kbm renderWorldContent lookAtRef key keystate modifiers position = 
         let update dx dy dz = updateLookAtRef lookAtRef (Vertex3 dx dy dz) >> display renderWorldContent lookAtRef in do

             case key of
                 SpecialKey KeyLeft      -> update (-1) 0 0
                 SpecialKey KeyRight     -> update 1 0 0
                 SpecialKey KeyUp        -> update 0 (-1) 0
                 SpecialKey KeyDown      -> update 0 1 0
                 _                       -> return ()

    updateLookAtRef :: IORef (Vertex3 GLdouble) -> Vertex3 GLdouble -> IO ()
    updateLookAtRef ref delta = modifyIORef ref $ add delta

    add :: Num a => Vertex3 a -> Vertex3 a -> Vertex3 a
    add (Vertex3 v1x v1y v1z) (Vertex3 v2x v2y v2z) = Vertex3 (v1x + v2x) (v1y + v2y) (v1z + v2z)

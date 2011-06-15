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
    kbm renderWorldContent lookAtRef key keystate modifiers position = do
         case key of
             SpecialKey KeyLeft      -> do {modifyIORef lookAtRef (\(Vertex3 x y z) -> (Vertex3 (x-1) y  z)); display renderWorldContent lookAtRef }
             SpecialKey KeyRight     -> do {modifyIORef lookAtRef (\(Vertex3 x y z) -> (Vertex3 (x+1) y  z)); display renderWorldContent lookAtRef }
             SpecialKey KeyUp        -> do {modifyIORef lookAtRef (\(Vertex3 x y z) -> (Vertex3 x (y-1)  z)); display renderWorldContent lookAtRef }
             SpecialKey KeyDown      -> do {modifyIORef lookAtRef (\(Vertex3 x y z) -> (Vertex3 x (y+1)  z)); display renderWorldContent lookAtRef }
             _                       -> return ()

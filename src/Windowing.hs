    module Windowing where
    import System.Exit
    import Data.IORef
    import Control.Exception
    import Graphics.UI.GLUT

    newWindow :: IO () -> IO ()
    newWindow renderWorldContent = do
        (progName, args) <- getArgsAndInitialize
        env <- newIORef (Vertex3 0 0 (-100))
        createWindow progName
        displayCallback $= display renderWorldContent env
        keyboardMouseCallback $= Just (kbm renderWorldContent env)
        actionOnWindowClose $= MainLoopReturns
        mainLoop

    display :: IO () -> IORef (Vertex3 GLdouble) -> DisplayCallback
    display renderWorldContent env = do
        clear [ColorBuffer]
        pov <- readIORef env
        loadIdentity
        lookAt (Vertex3 0 0 0) pov (Vector3 0 1 0)
        renderWorldContent
        flush

    kbm :: IO () -> IORef (Vertex3 GLdouble) -> KeyboardMouseCallback
    kbm renderWorldContent env key keystate modifiers position = do
         case key of
             SpecialKey KeyLeft      -> do {modifyIORef env (\(Vertex3 x y z) -> (Vertex3 (x-1) y  z)); display renderWorldContent env }
             SpecialKey KeyRight     -> do {modifyIORef env (\(Vertex3 x y z) -> (Vertex3 (x+1) y  z)); display renderWorldContent env }
             SpecialKey KeyUp        -> do {modifyIORef env (\(Vertex3 x y z) -> (Vertex3 x (y-1)  z)); display renderWorldContent env }
             SpecialKey KeyDown      -> do {modifyIORef env (\(Vertex3 x y z) -> (Vertex3 x (y+1)  z)); display renderWorldContent env }
             _                       -> return ()

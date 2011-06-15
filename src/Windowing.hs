    module Windowing where
    import System.Exit
    import Data.IORef
    import Control.Exception
    import Graphics.UI.GLUT

    newWindow renderWorldContent = do
        (progName, args) <- getArgsAndInitialize
        env <- newIORef (renderWorldContent, Vertex3 0 0 (-100))
        createWindow progName
        displayCallback $= display env
        keyboardMouseCallback $= Just (kbm env)
        actionOnWindowClose $= MainLoopReturns
        mainLoop

    display env = do
        clear [ColorBuffer]
        (renderWorldContent,pov) <- readIORef env
        loadIdentity
        lookAt (Vertex3 0 0 0) pov (Vector3 0 1 0)
        renderWorldContent
        flush

    kbm env key keystate modifiers position = do
         case key of
             SpecialKey KeyLeft      -> do {modifyIORef env (\(cont, Vertex3 x y z) -> (cont, Vertex3 (x-1) y  z)); display env }
             SpecialKey KeyRight     -> do {modifyIORef env (\(cont, Vertex3 x y z) -> (cont, Vertex3 (x+1) y  z)); display env }
             SpecialKey KeyUp        -> do {modifyIORef env (\(cont, Vertex3 x y z) -> (cont, Vertex3 x (y-1)  z)); display env }
             SpecialKey KeyDown      -> do {modifyIORef env (\(cont, Vertex3 x y z) -> (cont, Vertex3 x (y+1)  z)); display env }
             _                       -> return ()

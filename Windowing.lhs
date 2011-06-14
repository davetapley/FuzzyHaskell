%include lhs2TeX.fmt

\section{Windowing}
\subsection{Control the plotting window}

Draw 2D and 3D graphs using OpenGL

%if False

>   module Windowing where
>   import System.Exit
>   import Data.IORef
>   import Control.Exception
>   import Graphics.UI.GLUT

%endif

>   newWindow name cont = do
>       (progName, args) <- getArgsAndInitialize
>       env <- newIORef (cont, Vertex3 0 0 (-100))
>       createWindow name
>       displayCallback $= display env
>       keyboardMouseCallback $= Just (kbm env)
>       actionOnWindowClose $= MainLoopReturns
>       mainLoop

   addContent cont = do
       displayCallback $= display cont
       display cont

        modifyIORef env (\(_, pov) -> (cont, pov))
        display env

>   display env = do
>       clear [ColorBuffer]
>       (cont,pov) <- readIORef env
>       loadIdentity
>       lookAt (Vertex3 0 0 0) pov (Vector3 0 1 0)
>       cont
>       flush

>   kbm env key keystate modifiers position = do
>        case key of
>            SpecialKey KeyLeft      -> do {modifyIORef env (\(cont, Vertex3 x y z) -> (cont, Vertex3 (x-1) y  z)); display env }
>            SpecialKey KeyRight     -> do {modifyIORef env (\(cont, Vertex3 x y z) -> (cont, Vertex3 (x+1) y  z)); display env }
>            SpecialKey KeyUp        -> do {modifyIORef env (\(cont, Vertex3 x y z) -> (cont, Vertex3 x (y-1)  z)); display env }
>            SpecialKey KeyDown      -> do {modifyIORef env (\(cont, Vertex3 x y z) -> (cont, Vertex3 x (y+1)  z)); display env }
>            _                       -> return ()

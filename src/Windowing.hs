    module Windowing where
    import System.Exit
    import Data.IORef
    import Control.Exception
    import Graphics.UI.GLUT

    type LookAtMatrix = ((Vertex3 GLdouble), (Vertex3 GLdouble), (Vector3 GLdouble))

    rootLookAtMatrix :: LookAtMatrix 
    rootLookAtMatrix = ((Vertex3 0 0 0), (Vertex3 0 0 (-100)), (Vector3 0 1 0))

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
        readIORef lookAtRef >>= uncurry3 lookAt
        renderWorldContent
        flush

    kbm :: IO () -> IORef LookAtMatrix -> KeyboardMouseCallback
    kbm renderWorldContent lookAtRef key keystate modifiers position = 
         let update dx dy dz = updateLookAtRef lookAtRef (Vertex3 dx dy dz) >> display renderWorldContent lookAtRef in do

             case key of
                 SpecialKey KeyLeft      -> update (-1) 0 0
                 SpecialKey KeyRight     -> update 1 0 0
                 SpecialKey KeyUp        -> update 0 (-1) 0
                 SpecialKey KeyDown      -> update 0 1 0
                 _                       -> return ()

    updateLookAtRef :: IORef LookAtMatrix -> Vertex3 GLdouble -> IO ()
    updateLookAtRef ref delta = modifyIORef ref $ \(a,b,c) -> (a, add b delta, c)

    {-# INLINE add #-}
    add :: Num a => Vertex3 a -> Vertex3 a -> Vertex3 a
    add (Vertex3 v1x v1y v1z) (Vertex3 v2x v2y v2z) = Vertex3 (v1x + v2x) (v1y + v2y) (v1z + v2z)

    {-# INLINE uncurry3 #-}
    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f ~(a,b,c) = f a b c


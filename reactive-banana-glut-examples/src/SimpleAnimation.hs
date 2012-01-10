import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Reactive.Banana
import Reactive.Banana.GLUT
import Reactive.Banana.GLUT.Input


-- Callback for reshapeCallback
resizeGLScene :: Size -> IO ()
resizeGLScene (Size width 0) = do resizeGLScene (Size width 1)
resizeGLScene size@(Size width height) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 (fromIntegral width / fromIntegral height) 0.1 100.0
    matrixMode $= Modelview 0
    loadIdentity

-- Callback for displayCallback
drawGLScene :: Int -> IO ()
drawGLScene time = do

    let
        rtri = (fromIntegral (time `mod` 5000)) * 360.0 / 5000.0
        rquad = (fromIntegral (time `mod` 3000)) * 360.0 / 3000.0

    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    translate $ Vector3 (-1.5) 0.0 ((-6.0) :: GLfloat)
    rotate rtri (Vector3 0.0 1.0 (0.0 :: GLfloat))
    renderPrimitive Triangles $ do
        -- front face
        color $ Color3 1.0 0.0 (0.0 :: GLfloat)
        vertex $ Vertex3 0.0 1.0 (0.0 :: GLfloat)
        color $ Color3 0.0 1.0 (0.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) (1.0 :: GLfloat)
        color $ Color3 0.0 0.0 (1.0 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) (1.0 :: GLfloat)
        -- right face
        color $ Color3 1.0 0.0 (0.0 :: GLfloat)
        vertex $ Vertex3 0.0 1.0 (0.0 :: GLfloat)
        color $ Color3 0.0 0.0 (1.0 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) (1.0 :: GLfloat)
        color $ Color3 0.0 1.0 (0.0 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) ((-1.0) :: GLfloat)
        -- back face
        color $ Color3 1.0 0.0 (0.0 :: GLfloat)
        vertex $ Vertex3 0.0 1.0 (0.0 :: GLfloat)
        color $ Color3 0.0 1.0 (0.0 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) ((-1.0) :: GLfloat)
        color $ Color3 0.0 0.0 (1.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) ((-1.0) :: GLfloat)
        -- left face
        color $ Color3 1.0 0.0 (0.0 :: GLfloat)
        vertex $ Vertex3 0.0 1.0 (0.0 :: GLfloat)
        color $ Color3 0.0 0.0 (1.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) ((-1.0) :: GLfloat)
        color $ Color3 0.0 1.0 (0.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) (1.0 :: GLfloat)
    loadIdentity
    translate $ Vector3 1.5 (0.0) ((-7.0) :: GLfloat)
    rotate rquad (Vector3 1.0 1.0 (1.0 :: GLfloat))
    renderPrimitive Quads $ do
        color $ Color3 0.0 1.0 (0.0 :: GLfloat)
        vertex $ Vertex3 1.0 1.0 ((-1.0) :: GLfloat)
        vertex $ Vertex3 (-1.0) 1.0 ((-1.0) :: GLfloat)
        vertex $ Vertex3 (-1.0) 1.0 (1.0 :: GLfloat)
        vertex $ Vertex3 1.0 1.0 (1.0 :: GLfloat)
        color $ Color3 1.0 0.5 (0.0 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) (1.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) (1.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) ((-1.0) :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) ((-1.0) :: GLfloat)
        color $ Color3 1.0 0.0 (0.0 :: GLfloat)
        vertex $ Vertex3 1.0 1.0 (1.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) 1.0 (1.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) (1.0 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) (1.0 :: GLfloat)
        color $ Color3 1.0 1.0 (0.0 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) ((-1.0) :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) ((-1.0) :: GLfloat)
        vertex $ Vertex3 (-1.0) 1.0 ((-1.0) :: GLfloat)
        vertex $ Vertex3 1.0 1.0 ((-1.0) :: GLfloat)
        color $ Color3 0.0 0.0 (1.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) 1.0 (1.0 :: GLfloat)
        vertex $ Vertex3 (-1.0) 1.0 ((-1.0) :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) ((-1.0) :: GLfloat)
        vertex $ Vertex3 (-1.0) (-1.0) (1.0 :: GLfloat)
        color $ Color3 1.0 0.0 (1.0 :: GLfloat)
        vertex $ Vertex3 1.0 1.0 ((-1.0) :: GLfloat)
        vertex $ Vertex3 1.0 1.0 (1.0	 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) (1.0 :: GLfloat)
        vertex $ Vertex3 1.0 (-1.0) ((-1.0) :: GLfloat)
    swapBuffers

createGLWindow :: String -> GLsizei -> GLsizei -> Bool -> IO ()
createGLWindow windowTitle width height fullscreen = do
    createWindow windowTitle
    windowSize $= Size width height
    actionOnWindowClose $= Exit
    if fullscreen then fullScreen else return ()

initGL = do
    shadeModel $= Smooth
    clearColor $= (Color4 0.0 0.0 0.0 0.0)
    clearDepth $= 1.0
    depthFunc $= Just Lequal
    hint PerspectiveCorrection $= Nicest

main = do

    getArgsAndInitialize
    createGLWindow "Test" 640 480 False
    initGL
    network <- compile $ do
        btime <- time
        ekeymouse <- keyMouseEvent
        let
            eesc = noModifier $ keyPressed (Char '\ESC') ekeymouse
            ef1 = noModifier $ keyPressed (SpecialKey KeyF1) ekeymouse
        ereshape <- reshapeEvent
        edisplay <- displayEvent
        eidle <- idleEvent
        let
            eidletick = apply (const <$> btime) eidle
            einctick = filterInc eidletick 15
            edisplaytick = apply (const <$> btime) edisplay
            erepaint = edisplaytick `union` einctick
        reactimate $ fullScreenToggle <$ ef1
        reactimate $ leaveMainLoop <$ eesc
        reactimate $ resizeGLScene <$> ereshape
        reactimate $ drawGLScene <$> erepaint
    actuate network
    mainLoop

eventPrint :: String -> Int -> IO()
eventPrint s i = do
    print s
    print i


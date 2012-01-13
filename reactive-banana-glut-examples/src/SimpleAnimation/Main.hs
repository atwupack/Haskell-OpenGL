import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Reactive.Banana
import Reactive.Banana.GLUT
import Reactive.Banana.GLUT.Input
import Reactive.Banana.GLUT.Util
import SimpleAnimation.State

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
drawGLScene :: AnimationState -> IO ()
drawGLScene state = do

    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    translate $ Vector3 (-1.5) 0.0 ((-6.0) :: GLfloat)
    rotate (rtri state) (Vector3 0.0 1.0 (0.0 :: GLfloat))
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
    rotate (rcube state) (Vector3 1.0 1.0 (1.0 :: GLfloat))
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
        ereshape <- reshapeEvent
        edisplay <- displayEvent
        eidle <- idleEvent
        let
            -- key events
            eesc = noModifier $ keyPressed (Char '\ESC') ekeymouse
            ef1 = noModifier $ keyPressed (SpecialKey KeyF1) ekeymouse
            ef2 = noModifier $ keyPressed (SpecialKey KeyF2) ekeymouse
            ef3 = noModifier $ keyPressed (SpecialKey KeyF3) ekeymouse
            ef4 = noModifier $ keyPressed (SpecialKey KeyF4) ekeymouse
            ef5 = noModifier $ keyPressed (SpecialKey KeyF5) ekeymouse

            -- idle event every 15ms containing current time
            eidletick = filterInc (apply (const <$> btime) eidle) 15
            -- display event containing current time
            edisplaytick = apply (const <$> btime) edisplay
            -- combine for repaint
            erepaint = edisplaytick `union` eidletick

            -- all events that change the state
            einctri = injectB (incTriangle <$> bstate) ef2
            edectri = injectB (decTriangle <$> bstate) ef3
            einccube = injectB (incCube <$> bstate) ef4
            edeccube = injectB (decCube <$> bstate) ef5
            eanim = apply (calcRotation <$> bstate) erepaint

            -- combine all state changing events
            estatechanged = eanim
                `union` einctri `union` edectri
                `union` einccube `union` edeccube
            -- behavior with current state
            bstate = stepper state estatechanged

        reactimate $ fullScreenToggle <$ ef1
        reactimate $ leaveMainLoop <$ eesc
        reactimate $ resizeGLScene <$> ereshape
        -- redraw the scene because of changed state
        reactimate $ drawGLScene <$> estatechanged
    actuate network
    mainLoop
    where
        state = AnimationState {
            rtri = 0.0,
            stri = 5,
            rcube = 0.0,
            scube = 3,
            lastTime = 0
        }



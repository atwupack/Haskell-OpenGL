import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GLUT
import Reactive.Banana.GLUT.Input
import Reactive.Banana.GLUT.Util
import Reactive.Banana.GLUT.Window
import SimpleAnimation.State
import SimpleAnimation.Objects
import Control.Monad

-- Callback for reshapeCallback
resizeGLScene :: Size -> IO ()
resizeGLScene (Size width 0) = resizeGLScene (Size width 1)
resizeGLScene size@(Size width height) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 (fromIntegral width / fromIntegral height) 0.1 100.0
    matrixMode $= Modelview 0
    loadIdentity

-- Callback for displayCallback
drawGLScene :: AnimObjects -> AnimationState -> IO ()
drawGLScene objects state = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    translate $ Vector3 (-1.5) 0.0 ((-6.0) :: GLfloat)
    rotate (rtri state) (Vector3 0.0 1.0 (0.0 :: GLfloat))
    callList $ pyramid objects
    loadIdentity
    translate $ Vector3 1.5 0.0 ((-7.0) :: GLfloat)
    rotate (rcube state) (Vector3 1.0 1.0 (1.0 :: GLfloat))
    callList $ cube objects
    swapBuffers

createGLWindow :: String -> GLsizei -> GLsizei -> Bool -> IO ()
createGLWindow windowTitle width height fullscreen = do
    initialDisplayMode $= [DoubleBuffered, RGBAMode,
        WithDepthBuffer,WithStencilBuffer, WithAlphaComponent]
    createWindow windowTitle
    perWindowKeyRepeat $= PerWindowKeyRepeatOff
    windowSize $= Size width height
    actionOnWindowClose $= Exit
    when fullscreen fullScreen

initGL = do
    shadeModel $= Smooth
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clearDepth $= 1.0
    depthFunc $= Just Lequal
    hint PerspectiveCorrection $= Nicest

stateKeyEvents :: Event t KeyMouseInfo -> Behavior t AnimationState -> Event t AnimationState
stateKeyEvents ekeymouse bstate = einctri `union` edectri
                `union` einccube `union` edeccube
                `union` epause
    where
        -- key events that change the state
        ef2 = noModifier $ keyPressed (SpecialKey KeyF2) ekeymouse
        ef3 = noModifier $ keyPressed (SpecialKey KeyF3) ekeymouse
        ef4 = noModifier $ keyPressed (SpecialKey KeyF4) ekeymouse
        ef5 = noModifier $ keyPressed (SpecialKey KeyF5) ekeymouse
        ekeyp = noModifier $ keyPressed (Char 'p') ekeymouse
        einctri = injectB (incTriangle <$> bstate) ef2
        edectri = injectB (decTriangle <$> bstate) ef3
        einccube = injectB (incCube <$> bstate) ef4
        edeccube = injectB (decCube <$> bstate) ef5
        epause = injectB (switchPause <$> bstate) ekeyp

main = do
    getArgsAndInitialize
    createGLWindow "Test" 640 480 False
    initGL
    objects <- createObjects
    network <- compile $ do
        btime <- time
        ekeymouse <- keyMouseEvent
        ereshape <- reshapeEvent
        edisplay <- displayEvent
        eidle <- idleEvent
        let
            -- key events for windows
            ef1 = noModifier $ keyPressed (SpecialKey KeyF1) ekeymouse
            eesc = noModifier $ keyPressed (Char '\ESC') ekeymouse

            -- idle event every 15ms containing current time
            eidletick = filterInc (apply (const <$> btime) eidle) 15
            -- display event containing current time
            edisplaytick = apply (const <$> btime) edisplay
            -- combine for repaint
            erepaint = edisplaytick `union` eidletick

            -- all events that change the state
            eanim = apply (calcRotation <$> bstate) erepaint

            -- combine all state changing events
            estatechanged = eanim
                `union` stateKeyEvents ekeymouse bstate
            -- behavior with current state
            bstate = stepper state estatechanged

        reactimate $ fullScreenToggle <$ ef1
        reactimate $ leaveMainLoop <$ eesc
        reactimate $ resizeGLScene <$> ereshape
        -- redraw the scene because of changed state
        reactimate $ drawGLScene objects <$> estatechanged
    actuate network
    mainLoop
    where
        state = AnimationState {
            rtri = 0.0,
            stri = 5,
            rcube = 0.0,
            scube = 3,
            lastTime = 0,
            paused = False
        }



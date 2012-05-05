-----------------------------------------------------------------------------
--
-- Module      :  Reactive.Banana.GLUT
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Reactive.Banana.GLUT (
    displayEvent,
    idleEvent,
    event0,event1,event4,
    time
) where

import Reactive.Banana
import Graphics.UI.GLUT
import Data.StateVar

addHandler4 :: SettableStateVar (Maybe (a -> b -> c -> d -> IO())) -> IO (AddHandler (a,b,c,d))
addHandler4 callbackVar = return addHandler
    where
        addHandler callback = do
            callbackVar $= Just iCallback
            return (callbackVar $= Nothing)
            where
                iCallback p1 p2 p3 p4 = callback (p1,p2,p3,p4)

event4 :: SettableStateVar (Maybe (a -> b -> c -> d -> IO())) -> NetworkDescription t (Event t (a,b,c,d))
event4 callbackVar = do
    addHandler <- liftIO $ addHandler4 callbackVar
    fromAddHandler addHandler

addHandler1 :: SettableStateVar (Maybe (a -> IO())) -> IO (AddHandler a)
addHandler1 callbackVar = return addHandler
    where
        addHandler callback = do
            callbackVar $= Just callback
            return (callbackVar $= Nothing)

event1 :: SettableStateVar (Maybe (a -> IO())) -> NetworkDescription t (Event t a)
event1 callbackVar = do
    addHandler <- liftIO $ addHandler1 callbackVar
    fromAddHandler addHandler

addHandler0 :: SettableStateVar (Maybe (IO())) -> IO (AddHandler ())
addHandler0 callbackVar = return addHandler
    where
        addHandler callback = do
            callbackVar $= Just iCallback
            return (callbackVar $= Nothing)
            where
                iCallback = callback ()

event0 :: SettableStateVar (Maybe (IO())) -> NetworkDescription t (Event t ())
event0 callbackVar = do
    addHandler <- liftIO $ addHandler0 callbackVar
    fromAddHandler addHandler


setDisplayHandler :: (() -> IO()) -> IO()
setDisplayHandler callback = displayCallback $= iCallback
    where
        iCallback :: IO()
        iCallback = callback ()

displayEvent :: NetworkDescription t (Event t ())
displayEvent = do
    (addHandler, runHandlers) <- liftIO newAddHandler
    liftIO $ setDisplayHandler runHandlers
    fromAddHandler addHandler


idleEvent :: NetworkDescription t (Event t ())
idleEvent = event0 idleCallback

time :: NetworkDescription t (Behavior t Int)
time = fromPoll $ get elapsedTime





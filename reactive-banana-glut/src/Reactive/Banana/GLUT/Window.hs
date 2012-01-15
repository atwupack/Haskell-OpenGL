-----------------------------------------------------------------------------
--
-- Module      :  Reactive.Banana.GLUT.Window
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Window-related events and behaviors
--
-----------------------------------------------------------------------------

module Reactive.Banana.GLUT.Window (
    -- * Window reshape
    reshapeEvent,
    -- * Window state
    stateEvent, closeEvent
) where

import Graphics.UI.GLUT
import Reactive.Banana
import Reactive.Banana.GLUT

-- | Event generated if the window has been reshaped.
-- This registers a new callback with GLUT on each call.
reshapeEvent :: NetworkDescription (Event Size)
reshapeEvent = event1 reshapeCallback

-- | Event generated if the current window's state canges.
-- This registers a new callback with GLUT on each call.
stateEvent :: NetworkDescription (Event WindowState)
stateEvent = event1 windowStateCallback

-- | Event genrated if the current window is closed.
-- This registers a new callback with GLUT on each call.
closeEvent :: NetworkDescription (Event ())
closeEvent = event0 closeCallback

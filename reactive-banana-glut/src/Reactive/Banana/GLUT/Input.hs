-----------------------------------------------------------------------------
--
-- Module      :  Reactive.Banana.GLUT.Input
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

module Reactive.Banana.GLUT.Input (
    keyMouseEvent, KeyMouseInfo, keyPressed, noModifier,withModifiers
) where

import Reactive.Banana
import Reactive.Banana.GLUT
import Graphics.UI.GLUT

type KeyMouseInfo = (Key, KeyState, Modifiers, Position)

keyMouseEvent :: NetworkDescription t (Event t KeyMouseInfo)
keyMouseEvent = event4 keyboardMouseCallback

keyPressed :: Key -> Event t KeyMouseInfo -> Event t KeyMouseInfo
keyPressed key event = filterE iFilter event
    where
        iFilter :: KeyMouseInfo -> Bool
        iFilter (k,Down,_,_) = k==key
        iFilter (_,_,_,_) = False

noModifier :: Event t KeyMouseInfo -> Event t KeyMouseInfo
noModifier event = filterE iFilter event
    where
        iFilter :: KeyMouseInfo -> Bool
        iFilter (_,_,m,_) = (shift m == Up) && (ctrl m == Up) && (alt m == Up)

withModifiers :: Modifiers -> Event t KeyMouseInfo -> Event t KeyMouseInfo
withModifiers mods event = filterE iFilter event
    where
        iFilter :: KeyMouseInfo -> Bool
        iFilter (_,_,m,_) = m==mods


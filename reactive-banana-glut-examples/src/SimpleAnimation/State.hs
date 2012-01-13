-----------------------------------------------------------------------------
--
-- Module      :  SimpleAnimation.State
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

module SimpleAnimation.State (
    AnimationState(..),
    calcRotation,
    incTriangle,decTriangle,
    incCube,decCube
) where

import Graphics.Rendering.OpenGL

data AnimationState = AnimationState {
    -- rotation of triangle
    rtri :: GLfloat,
    -- seconds for a triangle rotation
    stri :: Int,
    -- rotation of cube
    rcube :: GLfloat,
    -- seconds for a cube rotation
    scube :: Int,
    -- last draw time
    lastTime :: Int
    }

calcRotation :: AnimationState -> Int -> AnimationState
calcRotation as time = AnimationState {
    rtri = rtri as + fromIntegral (time - lastTime as) * 360.0 / fromIntegral mstri,
    stri = stri as,
    rcube = rcube as + fromIntegral (time - lastTime as) * 360.0 / fromIntegral mscube,
    scube = scube as,
    lastTime = time
    }
    where
        mstri = 1000 * stri as
        mscube = 1000 * scube as

decTriangle :: AnimationState -> AnimationState
decTriangle as@(AnimationState _ 1 _ _ _) = as
decTriangle (AnimationState rt st rc sc t) = AnimationState rt (st-1) rc sc t

incTriangle :: AnimationState -> AnimationState
incTriangle (AnimationState rt st rc sc t) = AnimationState rt (st+1) rc sc t

decCube :: AnimationState -> AnimationState
decCube as@(AnimationState _ _ _ 1 _) = as
decCube (AnimationState rt st rc sc t) = AnimationState rt st rc (sc-1) t

incCube :: AnimationState -> AnimationState
incCube (AnimationState rt st rc sc t) = AnimationState rt st rc (sc+1) t

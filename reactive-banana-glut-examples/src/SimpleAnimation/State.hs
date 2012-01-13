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
calcRotation as time = as {
    rtri = rtri as + fromIntegral (time - lastTime as) * 360.0 / fromIntegral mstri,
    rcube = rcube as + fromIntegral (time - lastTime as) * 360.0 / fromIntegral mscube,
    lastTime = time
    }
    where
        mstri = 1000 * stri as
        mscube = 1000 * scube as

decTriangle :: AnimationState -> AnimationState
decTriangle as@(AnimationState _ 1 _ _ _) = as
decTriangle as = as { stri = stri as - 1 }

incTriangle :: AnimationState -> AnimationState
incTriangle as = as { stri = stri as + 1 }

decCube :: AnimationState -> AnimationState
decCube as@(AnimationState _ _ _ 1 _) = as
decCube as = as { scube = scube as - 1 }

incCube :: AnimationState -> AnimationState
incCube as = as { scube = scube as + 1 }

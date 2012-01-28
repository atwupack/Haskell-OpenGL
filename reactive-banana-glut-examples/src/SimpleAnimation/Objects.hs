-----------------------------------------------------------------------------
--
-- Module      :  SimpleAnimation.Objects
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

module SimpleAnimation.Objects where

import Graphics.Rendering.OpenGL

data AnimObjects = AnimObjects { pyramid :: DisplayList, cube :: DisplayList }

createObjects :: IO AnimObjects
createObjects = do
    pyrlist <- defineNewList Compile $
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

    cubelist <- defineNewList Compile $
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

    return AnimObjects {pyramid = pyrlist, cube = cubelist }

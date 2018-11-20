{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Diagrams.Backend.SDL
import Diagrams.ThreeD.Attributes
import Diagrams.Prelude hiding (translation)
import Geometry.ThreeD.Shapes
import Geometry.ThreeD.Types
import Linear.V3
import Linear.V4
import Linear.Affine
import Linear.Projection
import Linear.Quaternion

import Geometry.Path
import Diagrams.Types
import Diagrams.Backend

import Linear.Matrix

instance FromTrail (Diagram V3) where
  fromLocTrail locT = mkQD (Prim (toPath locT)) (getEnvelope locT) mempty mempty

mydiagram :: Diagram V3
mydiagram =
     cube # sc orange # translate (V3 0 0 0)
  <> cube # sc red # translate (V3 (-2) 0 0)
  <> cube # sc green # translate (V3 2 0 0)
  <> fromVertices [mkP3 (-3) 0 0, mkP3 (-3) 2 0]
  <> fromVertices [mkP3 3 0 0, mkP3 3 2 0]

main :: IO ()
main = mainWith SDL mydiagram

-- mycam :: Camera
-- mycam = def & cameraPitch +~ 0.2
--             & cameraYaw   +~ 0.7

-- -1.0    0.0    0.0   -0.0
--  0.0    0.86  -0.5   -1.0
-- -0.0   -0.5   -0.86  -1.73
--  0.0    0.0    0.0    1.0

-- caml :: Camera -> M44 Float
-- caml cam = lookAt (cam^.cameraPos._Point)
--                   (view _Point $ cam^.cameraPos .+^ cam^.cameraOffset)
--                   (V3 0 1 0)

-- -1.0    0.0    0.0   -0.0
--  0.0    0.86  -0.5   -0.0
-- -0.0   -0.5   -0.86   0.0
--  0.0    0.0    0.0    1.0

-- caml' :: Camera -> M44 Float
-- caml' cam = lookAt zero (cam^.cameraOffset) (V3 0 1 0)


-- qcam :: Camera -> Quaternion Float
-- qcam cam =
--       axisAngle (V3 0 0 1) (cam^.cameraRoll)
--     * axisAngle (V3 0 1 0) (cam^.cameraYaw)
--     * axisAngle (V3 1 0 0) (cam^.cameraPitch)


-- camq :: Camera -> M44 Float
-- camq cam =
--   fix (mkTransformation (1/qcam cam) zero)
--   !*!
--   (identity & translation .~ (-cam^.cameraPos._Point))

-- fix :: M44 Float -> M44 Float
-- fix = (column _x %~ negated) . (column _z %~ negated)

ppm44 :: M44 Float -> IO ()
ppm44 = mapM_ ppv4

ppv4 :: V4 Float -> IO ()
ppv4 (V4 a b c d) =
 putStrLn  $ pp a ++ pp b ++ pp c ++ pp d
  where
  pp f | abs f < 0.01 = " 0.0   "
       | f < 0  = take 5 (show f ++ repeat ' ') ++ "  "
       | otherwise = take 5 (' ' : show f ++ repeat ' ') ++ "  "


  -- let viewMat = mkTransformation (recip $ cam^.camQuart) zero -- (cam^.cameraPos._Point)
  -- let viewMat  = viewMat' -- !*! modelMat
  -- let viewMat  = lookAt (cam^.cameraPos._Point)
  --                       -- zero
  --                       (view _Point $ cam^.cameraPos .+^ cam^.cameraOffset)
  --                       (V3 0 1 0)
  -- let projectionMatrix = perspective (3*pi/8) (4/3) 0.1 500 :: M44 Float
  -- let projectionMatrix = ortho (-5) 5 (-5) 5 0.1 100 :: M4

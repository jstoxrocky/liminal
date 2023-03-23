module Liminal.Intersection.Services.Projection where

import Prelude

import Data.Either (Either)
import Data.Number (tan)
import TransformationMatrix.Services.Division (divide)
import TransformationMatrix.Data.DivisionError (DivisionError)
import Liminal.Class.HasFieldOfView (class HasFieldOfView, getFov)
import Liminal.Class.HasInverse (class HasInverse, getInverse)
import Liminal.Class.HasProjection (class HasProjection, setProjection, getProjection)
import Liminal.Class.HasInverseProjection (class HasInverseProjection, setInverseProjection, getInverseProjection)
import TransformationMatrix.Data.Radians (Radians(..), degreeToRadians)
import TransformationMatrix.Data.Matrix4 (Matrix4(..), applyMatrix4, invert)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import TransformationMatrix.Data.Vector2 (Vector2(..))
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix)

near :: Number
near = 0.1

far :: Number
far = 2000.0

project
  :: forall a
   . HasInverse a
  => HasProjection a
  => a
  -> Vector3 Number
  -> Either DivisionError (Vector2 Number)
project projector v = do
  v' <- applyMatrix4 (getInverse projector) v
  Vector3 x y _ <- applyMatrix4 (getProjection projector) v'
  pure $ Vector2 x y

-- https://github.com/mrdoob/three.js/blob/9b274fef1d2b9ba0fc47410d06593e1f34f4df0e/src/math/Vector3.js#L280
unproject
  :: forall a
   . HasMatrix a
  => HasProjection a
  => HasInverseProjection a
  => a
  -> Vector3 Number
  -> Either DivisionError (Vector3 Number)
unproject projector v = do
  v' <- applyMatrix4 (getInverseProjection projector) v
  applyMatrix4 (getMatrix projector) v'

-- https://github.com/mrdoob/three.js/blob/4503ef10b81a00f5c6c64fe9a856881ee31fe6a3/src/math/Matrix4.js#L755
makePerspective
  :: Number
  -> Number
  -> Number
  -> Number
  -> Either DivisionError Matrix4
makePerspective left right top bottom = do
  x <- divide (2.0 * near) (right - left)
  y <- divide (2.0 * near) (top - bottom)
  a <- divide (right + left) (right - left)
  b <- divide (top + bottom) (top - bottom)
  c <- divide (-(far + near)) (far - near)
  d <- divide ((-2.0) * far * near) (far - near)
  -- This final value (x44) is set to 1.0 but but its zero in threejs
  -- We need this set to 1.0 or else we cannot project the origin to NDC...
  -- Not sure how it works in threejs with the zero. 

  -- Update: Set (x44) back to zero since raycasting doesn't work otherwise...
  -- Not sure what is right but going to keep it at zero for now...
  pure $ Matrix4
    x
    0.0
    a
    0.0
    0.0
    y
    b
    0.0
    0.0
    0.0
    c
    d
    0.0
    0.0
    (-1.0)
    0.0

calculateProjectionMatrix
  :: Number
  -> Number
  -> Either DivisionError Matrix4
calculateProjectionMatrix fov aspect = makePerspective left right top bottom
  where
  Radians theta = degreeToRadians $ 0.5 * fov
  top = near * (tan theta)
  height = 2.0 * top
  width = aspect * height
  left = (-0.5) * width
  right = left + width
  bottom = top - height

updateObjectProjection
  :: forall a
   . HasFieldOfView a
  => HasProjection a
  => HasInverseProjection a
  => Number
  -> a
  -> Either DivisionError a
updateObjectProjection aspect object = do
  projection <- calculateProjectionMatrix (getFov object) aspect
  inverseProjection <- invert projection
  pure $ ((setInverseProjection inverseProjection) <<< (setProjection projection)) object

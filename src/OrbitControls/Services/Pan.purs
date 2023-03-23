module Liminal.OrbitControls.Services.Pan where

import Prelude hiding (add)
import TransformationMatrix.Data.Vector2 (Vector2(..))
import Data.Number (tan)
import TransformationMatrix.Data.Radians (Radians(..), degreeToRadians)
import TransformationMatrix.Data.Vector3 (Vector3, multiplyByScalar, length, subtract, add)
import Liminal.Class.HasMatrix (class HasMatrix, getPosition, getMatrix)
import TransformationMatrix.Data.Matrix4 (Matrix4, getXColumn, getYColumn)
import Liminal.Transformation.Services.Translate (translateObject)
import Liminal.Class.HasFieldOfView (class HasFieldOfView, getFov)
import Liminal.Class.HasOrbitTarget (class HasOrbitTarget, setOrbitTarget)
import Liminal.Class.HasInverse (class HasInverse)

panX
  :: Number
  -> Matrix4
  -> Vector3 Number
panX distance matrix = multiplyByScalar (-distance) $ getXColumn matrix

panY
  :: Number
  -> Matrix4
  -> Vector3 Number
panY distance matrix = multiplyByScalar distance $ getYColumn matrix

-- Ripped from threejs.
-- Not quite sure what we're doing with angles...
-- Couldn't we just increment x and y?
calculatePanOffset
  :: forall a
   . HasFieldOfView a
  => HasMatrix a
  => Vector2 Number
  -> Vector3 Number
  -> a
  -> Vector3 Number
calculatePanOffset (Vector2 deltaX deltaY) target object = add xPan yPan
  where
  Radians angleFromCenterToTopOfView = degreeToRadians $ (getFov object) / 2.0
  offset = subtract (getPosition object) target
  targetDistance = length offset * tan angleFromCenterToTopOfView
  xPan = panX (2.0 * deltaX * targetDistance) (getMatrix object)
  yPan = panY (2.0 * deltaY * targetDistance) (getMatrix object)

panObject
  :: forall a
   . HasFieldOfView a
  => HasMatrix a
  => HasOrbitTarget a
  => HasInverse a
  => Vector2 Number
  -> Vector3 Number
  -> a
  -> a
panObject delta target object = translatedObject
  where
  panOffset = calculatePanOffset delta target object
  updatedObject = setOrbitTarget (add target panOffset) object
  translatedObject = translateObject panOffset updatedObject

module Liminal.OrbitControls.Services.Dolly where

import Prelude hiding (add)
import TransformationMatrix.Data.Vector3 (Vector3, subtract, add)
import Liminal.Class.HasMatrix (class HasMatrix, getPosition)
import Data.Either (Either)
import TransformationMatrix.Data.DivisionError (DivisionError)
import TransformationMatrix.Data.Spherical (Spherical(..), vector3ToSpherical, sphericalAnglesToVector3)
import Liminal.Transformation.Services.Translate (moveObjectTo)
import Liminal.Class.HasInverse (class HasInverse)

dollyMultiplier :: Number
dollyMultiplier = 0.95

dollyScale
  :: Number
  -> Number
dollyScale deltaY =
  if deltaY < 0.0 then dollyMultiplier
  else if deltaY > 0.0 then 1.0 / dollyMultiplier
  else 1.0

-- Ripped from threejs.
calculateDollyOffset
  :: forall a
   . HasMatrix a
  => Number
  -> Vector3 Number
  -> a
  -> Either DivisionError (Vector3 Number)
calculateDollyOffset deltaY target object = do
  let
    offset = subtract (getPosition object) target
  Spherical radius angles <- vector3ToSpherical offset
  let
    scaledRadius = radius * (dollyScale deltaY)
    dollyedOffset = sphericalAnglesToVector3 $ Spherical scaledRadius angles
  pure $ add target dollyedOffset

dollyObject
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Number
  -> Vector3 Number
  -> a
  -> Either DivisionError a
dollyObject deltaY target object = do
  offset <- calculateDollyOffset deltaY target object
  pure $ moveObjectTo offset object

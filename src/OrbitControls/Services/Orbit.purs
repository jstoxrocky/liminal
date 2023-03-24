module Liminal.OrbitControls.Services.Orbit where

import Prelude hiding (add)

import Data.Either (Either)
import Data.Number (pi)
import TransformationMatrix.Data.DivisionError (DivisionError)
import Liminal.Class.HasInverse (class HasInverse)
import Liminal.Class.HasMatrix (class HasMatrix, getPosition)
import TransformationMatrix.Data.Spherical (Spherical(..), SphericalAngles(..), vector3ToSpherical, sphericalAnglesToVector3)
import Liminal.Transformation.Services.LookAt (moveObjectToAndLookAt)
import TransformationMatrix.Data.Vector2 (Vector2(..))
import TransformationMatrix.Data.Vector3 (Vector3, subtract, add)
import Liminal.Class.HasOrbitTarget (class HasOrbitTarget)

-- https://github.com/mrdoob/three.js/blob/4503ef10b81a00f5c6c64fe9a856881ee31fe6a3/src/math/Spherical.js#L43
makeSafe
  :: Number
  -> Number
makeSafe phi = phi'
  where
  eps = 0.000001
  phi' = clamp eps (pi - eps) phi

rotateTheta
  :: Number
  -> Number
  -> Number
rotateTheta theta pointerTravelPctX = theta - 2.0 * pi * pointerTravelPctX

-- Does something weird at polar angles if makeSafe not included
rotatePhi
  :: Number
  -> Number
  -> Number
rotatePhi phi pointerTravelPctY = makeSafe $ phi - 2.0 * pi * pointerTravelPctY

rotateSphericalAngles
  :: Vector2 Number
  -> SphericalAngles
  -> SphericalAngles
rotateSphericalAngles (Vector2 deltaX deltaY) (SphericalAngles theta phi) = SphericalAngles
  (rotateTheta theta deltaX)
  (rotatePhi phi deltaY)

orbitOffsetAboutSelf
  :: Vector2 Number
  -> Vector3 Number
  -> Either DivisionError (Vector3 Number)
orbitOffsetAboutSelf delta offset = do
  Spherical radius angles <- vector3ToSpherical offset
  let
    rotatedAngles = rotateSphericalAngles delta angles
  pure $ sphericalAnglesToVector3 $ Spherical radius rotatedAngles

calculateOrbitOffset
  :: forall a
   . HasMatrix a
  => Vector2 Number
  -> Vector3 Number
  -> a
  -> Either DivisionError (Vector3 Number)
calculateOrbitOffset delta target object = do
  let
    offset = subtract (getPosition object) target
  rotatedOffset <- orbitOffsetAboutSelf delta offset
  pure (add target rotatedOffset)

orbitObject
  :: forall a
   . HasMatrix a
  => HasOrbitTarget a
  => HasInverse a
  => Vector2 Number
  -> Vector3 Number
  -> a
  -> Either DivisionError a
orbitObject delta target object = do
  position <- calculateOrbitOffset delta target object
  moveObjectToAndLookAt position target object

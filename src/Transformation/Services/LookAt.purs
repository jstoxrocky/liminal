module Liminal.Transformation.Services.LookAt where

import Prelude hiding (add)

import Data.Either (Either)
import TransformationMatrix.Data.DivisionError (DivisionError)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix, setMatrix, getPosition)
import Liminal.Class.HasOrbitTarget (class HasOrbitTarget, setOrbitTarget)
import TransformationMatrix.Data.RotationMatrix (lookAtRotation)
import TransformationMatrix.Data.Matrix4 (applyRotationMatrix)
import TransformationMatrix.Data.Vector3 (Vector3)
import Liminal.Transformation.Services.Invert (updateInverseMatrix)
import Liminal.Class.HasInverse (class HasInverse)
import Liminal.Transformation.Services.Translate (moveObjectToNoInverseUpdate)

lookAtNoInverseUpdate
  :: forall a
   . HasMatrix a
  => HasOrbitTarget a
  => Vector3 Number
  -> a
  -> Either DivisionError a
lookAtNoInverseUpdate target object = do
  let
    matrix = getMatrix object
    position = getPosition object
  rotationMatrix <- lookAtRotation position target
  let
    rotatedObject = setMatrix (applyRotationMatrix matrix rotationMatrix) object
    targetedObject = setOrbitTarget target rotatedObject
  pure targetedObject

moveObjectToAndLookAt
  :: forall a
   . HasMatrix a
  => HasInverse a
  => HasOrbitTarget a
  => Vector3 Number
  -> Vector3 Number
  -> a
  -> Either DivisionError a
moveObjectToAndLookAt position target object = do
  let
    movedObject = moveObjectToNoInverseUpdate position object
  lookedAtObject <- lookAtNoInverseUpdate target movedObject
  updateInverseMatrix lookedAtObject

module Liminal.Transformation.Services.Scale where

import Prelude hiding (add)

import Data.Either (Either)
import TransformationMatrix.Data.DivisionError (DivisionError)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix, setMatrix)
import TransformationMatrix.Data.Matrix4 (scale)
import Liminal.Transformation.Services.Invert (updateInverseMatrix)
import Liminal.Class.HasInverse (class HasInverse)

scaleObjectNoInverseUpdate
  :: forall a
   . HasMatrix a
  => Number
  -> a
  -> a
scaleObjectNoInverseUpdate multiplier object = setMatrix matrix object
  where
  matrix = scale multiplier (getMatrix object)

scaleObject
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Number
  -> a
  -> Either DivisionError a
scaleObject multiplier object = do
  let
    scaledObject = scaleObjectNoInverseUpdate multiplier object
  updateInverseMatrix scaledObject

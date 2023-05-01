module Liminal.Class.HasMatrix where

import TransformationMatrix.Data.Matrix4 (Matrix4)
import TransformationMatrix.Data.Vector3 (Vector3, subtract)
import Data.Either (Either)
import TransformationMatrix.Data.DivisionError (DivisionError)

class HasMatrix m where
  getMatrix :: m -> Matrix4
  setMatrix :: Matrix4 -> m -> m
  getPosition :: m -> Vector3 Number
  setPosition :: Vector3 Number -> m -> m
  getWorldPosition :: m -> Either DivisionError (Vector3 Number)

subtractPositions
  :: forall a b
   . HasMatrix a
  => HasMatrix b
  => b
  -> a
  -> Vector3 Number
subtractPositions b a = subtract (getPosition b) (getPosition a)

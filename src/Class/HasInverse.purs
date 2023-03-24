module Liminal.Class.HasInverse where

import TransformationMatrix.Data.Matrix4 (Matrix4)

class HasInverse m where
  getInverse :: m -> Matrix4
  setInverse :: Matrix4 -> m -> m
module Liminal.Class.HasInverseProjection where

import TransformationMatrix.Data.Matrix4 (Matrix4)

class HasInverseProjection m where
  getInverseProjection :: m -> Matrix4
  setInverseProjection :: Matrix4 -> m -> m

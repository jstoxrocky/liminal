module Liminal.Class.HasProjection where

import TransformationMatrix.Data.Matrix4 (Matrix4)

class HasProjection m where
  getProjection :: m -> Matrix4
  setProjection :: Matrix4 -> m -> m

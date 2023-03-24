module Liminal.Class.HasOrbitTarget where

import TransformationMatrix.Data.Vector3 (Vector3)

class HasOrbitTarget m where
  getOrbitTarget :: m -> Vector3 Number
  setOrbitTarget :: Vector3 Number -> m -> m

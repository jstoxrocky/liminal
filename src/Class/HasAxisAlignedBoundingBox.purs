module Liminal.Class.HasAxisAlignedBoundingBox where

import Liminal.Data.AxisAlignedBoundingBox (AxisAlignedBoundingBox)

class HasAxisAlignedBoundingBox m where
  getAxisAlignedBoundingBox :: m -> AxisAlignedBoundingBox

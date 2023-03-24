module Liminal.Class.HasBoundingBox where

import Liminal.Data.BoundingBox (BoundingBox)

class HasBoundingBox m where
  getBoundingBox :: m -> BoundingBox

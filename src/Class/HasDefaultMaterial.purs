module Liminal.Class.HasDefaultMaterial where

import Liminal.Data.Material (Material)

class HasDefaultMaterial m where
  getDefaultMaterial :: m -> Material

module Liminal.Updates.Class.SerializeGeometry where

import Liminal.Updates.Data.Geometry (Geometry)

class SerializeGeometry a where
  serializeGeometry :: a -> Geometry

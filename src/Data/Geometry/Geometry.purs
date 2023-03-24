module Liminal.Data.Geometry where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Data.BoxGeometry (BoxGeometry)
import Liminal.Data.LineGeometry (LineGeometry)
import Liminal.Data.PlaneGeometry (PlaneGeometry)
import Liminal.Data.SphereGeometry (SphereGeometry)

data Geometry
  = GeometryBox BoxGeometry
  | GeometrySphere SphereGeometry
  | GeometryPlane PlaneGeometry
  | GeometryLine LineGeometry

derive instance genericGeometry :: Generic Geometry _

instance showGeometry :: Show Geometry where
  show = genericShow

derive instance eqGeometry :: Eq Geometry

derive instance ordGeometry :: Ord Geometry
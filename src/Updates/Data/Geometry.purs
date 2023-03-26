module Liminal.Updates.Data.Geometry where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type BoxGeometryAttrs =
  { xspan :: Number
  , yspan :: Number
  , zspan :: Number }

type SphereGeometryAttrs =
  { radius :: Number }

type PlaneGeometryAttrs =
  { xspan :: Number
  , yspan :: Number }

type LineGeometryAttrs =
  { yspan :: Number }

-- type LineGeometryAttrs =
--   { points :: Array (Vector3 Number) }

data Geometry
  = BoxGeometry BoxGeometryAttrs
  | SphereGeometry SphereGeometryAttrs
  | PlaneGeometry PlaneGeometryAttrs
  | LineGeometry LineGeometryAttrs

derive instance genericGeometry :: Generic Geometry _

instance showGeometry :: Show Geometry where
  show = genericShow

derive instance eqGeometry :: Eq Geometry

derive instance orGeometry :: Ord Geometry

module Liminal.Data.SphereGeometry where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Data.Vector1 (Vector1(..))
import Liminal.Data.AxisAlignedBoundingBox (AxisAlignedBoundingBox(..))
import Liminal.Class.HasAxisAlignedVertices (class HasAxisAlignedVertices)
import Liminal.Class.HasAxisAlignedBoundingBox (class HasAxisAlignedBoundingBox)
import Liminal.Updates.Data.Geometry as Geometry
import Liminal.Updates.Class.SerializeGeometry (class SerializeGeometry)

newtype SphereGeometry = SphereGeometry { radius :: Number }

derive instance genericSphereGeometry :: Generic SphereGeometry _

instance showSphereGeometry :: Show SphereGeometry where
  show = genericShow

derive instance eqSphereGeometry :: Eq SphereGeometry

derive instance ordSphereGeometry :: Ord SphereGeometry

instance hasAxisAlignedVerticesSphereGeometry :: HasAxisAlignedVertices SphereGeometry Vector1 where
  getAxisAlignedVertices _ = Vector1 (Vector3 0.0 0.0 0.0)

instance hasAxisAlignedBoundingBoxSphereGeometry :: HasAxisAlignedBoundingBox SphereGeometry where
  getAxisAlignedBoundingBox geometry = calculateAxisAlignedBoundingBox geometry

instance serializeGeometrySphereGeometry :: SerializeGeometry SphereGeometry where
  serializeGeometry (SphereGeometry attrs) = Geometry.SphereGeometry attrs

calculateAxisAlignedBoundingBox
  :: SphereGeometry
  -> AxisAlignedBoundingBox
calculateAxisAlignedBoundingBox (SphereGeometry { radius }) = AxisAlignedBoundingBox min max
  where
  x = 0.0
  y = 0.0
  z = 0.0
  min = Vector3 (x - radius) (y - radius) (z - radius)
  max = Vector3 (x + radius) (y + radius) (z + radius)

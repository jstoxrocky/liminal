module Liminal.Data.LineGeometry where

import Prelude

import Data.Array.NonEmpty (singleton, cons)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Class.HasAxisAlignedBoundingBox (class HasAxisAlignedBoundingBox)
import Liminal.Class.HasAxisAlignedVertices (class HasAxisAlignedVertices)
import Liminal.Data.AxisAlignedBoundingBox (AxisAlignedBoundingBox, boundingBoxFromPoints)
import Liminal.Updates.Class.SerializeGeometry (class SerializeGeometry)
import Liminal.Updates.Data.Geometry as Geometry
import TransformationMatrix.Data.Vector2 (Vector2(..))
import TransformationMatrix.Data.Vector3 (Vector3, toXYZ)

newtype LineGeometry = LineGeometry { point1 :: Vector3 Number, point2 :: Vector3 Number }

derive instance genericLineGeometry :: Generic LineGeometry _

instance showLineGeometry :: Show LineGeometry where
  show = genericShow

derive instance eqLineGeometry :: Eq LineGeometry

derive instance ordLineGeometry :: Ord LineGeometry

instance hasAxisAlignedVerticesBoxGeometry :: HasAxisAlignedVertices LineGeometry Vector2 where
  getAxisAlignedVertices (LineGeometry { point1, point2 }) = Vector2 point1 point2

instance hasAxisAlignedBoundingBoxLineGeometry :: HasAxisAlignedBoundingBox LineGeometry where
  getAxisAlignedBoundingBox geometry = calculateAxisAlignedBoundingBox geometry

instance serializeGeometryLineGeometry :: SerializeGeometry LineGeometry where
  serializeGeometry (LineGeometry { point1, point2 }) = Geometry.LineGeometry { point1: toXYZ point1, point2: toXYZ point2 }

calculateAxisAlignedBoundingBox
  :: LineGeometry
  -> AxisAlignedBoundingBox
calculateAxisAlignedBoundingBox (LineGeometry { point1, point2 })
  = boundingBoxFromPoints $ cons point1 $ singleton point2

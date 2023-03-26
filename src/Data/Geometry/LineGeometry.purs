module Liminal.Data.LineGeometry where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import TransformationMatrix.Data.Vector2 (Vector2(..))
import Liminal.Data.AxisAlignedBoundingBox (AxisAlignedBoundingBox(..))
import Liminal.Class.HasAxisAlignedVertices (class HasAxisAlignedVertices)
import Liminal.Class.HasAxisAlignedBoundingBox (class HasAxisAlignedBoundingBox)
import Liminal.Updates.Data.Geometry as Geometry
import Liminal.Updates.Class.SerializeGeometry (class SerializeGeometry)

newtype LineGeometry = LineGeometry { yspan :: Number }

derive instance genericLineGeometry :: Generic LineGeometry _

instance showLineGeometry :: Show LineGeometry where
  show = genericShow

derive instance eqLineGeometry :: Eq LineGeometry

derive instance ordLineGeometry :: Ord LineGeometry

-- This implementation is dependent on how threejs creates a LineGeometry(y-span)
-- There is no "lateral depth" or "forward back depth" in the x and z-dimensions on creation. We create the positions of the
-- vertices as if the Line was in its newly created state, then we apply the Line's
-- transformation matrix to each vertex.
instance hasAxisAlignedVerticesBoxGeometry :: HasAxisAlignedVertices LineGeometry Vector2 where
  getAxisAlignedVertices (LineGeometry { yspan }) = vertices
    where
    x = 0.0
    y = 0.0
    z = 0.0
    halfYspan = yspan / 2.0
    vertices = Vector2
      (Vector3 x (y + halfYspan) z)
      (Vector3 x (y - halfYspan) z)

instance hasAxisAlignedBoundingBoxLineGeometry :: HasAxisAlignedBoundingBox LineGeometry where
  getAxisAlignedBoundingBox geometry = calculateAxisAlignedBoundingBox geometry (Vector3 0.0 0.0 0.0)

instance serializeGeometryLineGeometry :: SerializeGeometry LineGeometry where
  serializeGeometry (LineGeometry attrs) = Geometry.LineGeometry attrs

calculateAxisAlignedBoundingBox
  :: LineGeometry
  -> Vector3 Number
  -> AxisAlignedBoundingBox
calculateAxisAlignedBoundingBox (LineGeometry { yspan }) (Vector3 x y z) = AxisAlignedBoundingBox min max
  where
  halfYspan = yspan / 2.0
  min = Vector3 x (y - halfYspan) z
  max = Vector3 x (y + halfYspan) z

module Liminal.Data.PlaneGeometry where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Data.Vector4 (Vector4(..))
import Liminal.Data.AxisAlignedBoundingBox (AxisAlignedBoundingBox(..))
import Liminal.Class.HasAxisAlignedVertices (class HasAxisAlignedVertices)
import Liminal.Class.HasAxisAlignedBoundingBox (class HasAxisAlignedBoundingBox)
import Liminal.Updates.Data.Geometry as Geometry
import Liminal.Updates.Class.SerializeGeometry (class SerializeGeometry)

newtype PlaneGeometry = PlaneGeometry { xspan :: Number, yspan :: Number }

derive instance genericPlaneGeometry :: Generic PlaneGeometry _

instance showPlaneGeometry :: Show PlaneGeometry where
  show = genericShow

derive instance eqPlaneGeometry :: Eq PlaneGeometry

derive instance ordPlaneGeometry :: Ord PlaneGeometry

-- This implementation is dependent on how threejs creates a PlaneGeometry(x-span, y-span)
-- There is no "depth" in the z-dimension on creation. We create the positions of the
-- vertices as if the Plane was in its newly created state, then we apply the Plane's
-- transformation matrix to each vertex. 
instance hasAxisAlignedVerticesPlaneGeometry :: HasAxisAlignedVertices PlaneGeometry Vector4 where
  getAxisAlignedVertices (PlaneGeometry { xspan, yspan }) = vertices
    where
    x = 0.0
    y = 0.0
    z = 0.0
    halfXspan = xspan / 2.0
    halfYspan = yspan / 2.0
    vertices = Vector4
      (Vector3 (x - halfXspan) (y + halfYspan) z)
      (Vector3 (x + halfXspan) (y + halfYspan) z)
      (Vector3 (x - halfXspan) (y - halfYspan) z)
      (Vector3 (x + halfXspan) (y - halfYspan) z)

instance hasAxisAlignedBoundingBoxPlaneGeometry :: HasAxisAlignedBoundingBox PlaneGeometry where
  getAxisAlignedBoundingBox geometry = calculateAxisAlignedBoundingBox geometry

instance serializeGeometryPlaneGeometry :: SerializeGeometry PlaneGeometry where
  serializeGeometry (PlaneGeometry attrs) = Geometry.PlaneGeometry attrs

calculateAxisAlignedBoundingBox
  :: PlaneGeometry
  -> AxisAlignedBoundingBox
calculateAxisAlignedBoundingBox (PlaneGeometry { xspan, yspan }) = AxisAlignedBoundingBox min max
  where
  x = 0.0
  y = 0.0
  z = 0.0
  halfXspan = xspan / 2.0
  halfYspan = yspan / 2.0
  min = Vector3 (x - halfXspan) (y - halfYspan) z
  max = Vector3 (x + halfXspan) (y + halfYspan) z

module Liminal.Data.PlaneGeometry where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Data.Vector4 (Vector4(..))
import Liminal.Data.BoundingBox (BoundingBox(..))
import Liminal.Class.HasVertices (class HasVertices)
import Liminal.Class.HasBoundingBox (class HasBoundingBox)
import Liminal.Updates.Data.Geometry as Geometry
import Liminal.Updates.Class.SerializeGeometry (class SerializeGeometry)

newtype PlaneGeometry = PlaneGeometry { xspan :: Number, yspan :: Number }

derive instance genericPlaneGeometry :: Generic PlaneGeometry _

instance showPlaneGeometry :: Show PlaneGeometry where
  show = genericShow

derive instance eqPlaneGeometry :: Eq PlaneGeometry

derive instance ordPlaneGeometry :: Ord PlaneGeometry

instance hasVerticesBoxGeometry :: HasVertices PlaneGeometry Vector4 where
  getVertices geometry = calculateVertices geometry (Vector3 0.0 0.0 0.0)

instance hasBoundingBoxBoxGeometry :: HasBoundingBox PlaneGeometry where
  getBoundingBox geometry = calculateBoundingBox geometry (Vector3 0.0 0.0 0.0)

instance serializeGeometryPlaneGeometry :: SerializeGeometry PlaneGeometry where
  serializeGeometry (PlaneGeometry attrs) = Geometry.PlaneGeometry attrs

-- This implementation is dependent on how threejs creates a PlaneGeometry(x-span, y-span)
-- There is no "depth" in the z-dimension on creation. We create the positions of the
-- vertices as if the Plane was in its newly created state, then we apply the Plane's
-- transformation matrix to each vertex. 
calculateVertices
  :: PlaneGeometry
  -> Vector3 Number
  -> Vector4 (Vector3 Number)
calculateVertices (PlaneGeometry { xspan, yspan }) (Vector3 x y z) = vertices
  where
  halfXspan = xspan / 2.0
  halfYspan = yspan / 2.0
  vertices = Vector4
    (Vector3 (x - halfXspan) (y + halfYspan) z)
    (Vector3 (x + halfXspan) (y + halfYspan) z)
    (Vector3 (x - halfXspan) (y - halfYspan) z)
    (Vector3 (x + halfXspan) (y - halfYspan) z)

calculateBoundingBox
  :: PlaneGeometry
  -> Vector3 Number
  -> BoundingBox
calculateBoundingBox (PlaneGeometry { xspan, yspan }) (Vector3 x y z) = BoundingBox min max
  where
  halfXspan = xspan / 2.0
  halfYspan = yspan / 2.0
  min = Vector3 (x - halfXspan) (y - halfYspan) z
  max = Vector3 (x + halfXspan) (y + halfYspan) z

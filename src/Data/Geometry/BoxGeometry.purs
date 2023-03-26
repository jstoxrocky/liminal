module Liminal.Data.BoxGeometry where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Data.Vector8 (Vector8(..))
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Data.AxisAlignedBoundingBox (AxisAlignedBoundingBox(..))
import Liminal.Class.HasAxisAlignedVertices (class HasAxisAlignedVertices)
import Liminal.Class.HasAxisAlignedBoundingBox (class HasAxisAlignedBoundingBox)
import Liminal.Updates.Data.Geometry as Geometry
import Liminal.Updates.Class.SerializeGeometry (class SerializeGeometry)

newtype BoxGeometry = BoxGeometry { xspan :: Number, yspan :: Number, zspan :: Number }

derive instance genericBoxGeometry :: Generic BoxGeometry _

instance showBoxGeometry :: Show BoxGeometry where
  show = genericShow

derive instance eqBoxGeometry :: Eq BoxGeometry

derive instance ordBoxGeometry :: Ord BoxGeometry

instance hasAxisAlignedVerticesBoxGeometry :: HasAxisAlignedVertices BoxGeometry Vector8 where
  getAxisAlignedVertices (BoxGeometry { xspan, yspan, zspan }) = vertices
    where
    x = 0.0
    y = 0.0
    z = 0.0
    halfXspan = xspan / 2.0
    halfYspan = yspan / 2.0
    halfZspan = zspan / 2.0
    vertices = Vector8
      (Vector3 (x - halfXspan) (y + halfYspan) (z + halfZspan))
      (Vector3 (x + halfXspan) (y + halfYspan) (z + halfZspan))
      (Vector3 (x - halfXspan) (y - halfYspan) (z + halfZspan))
      (Vector3 (x + halfXspan) (y - halfYspan) (z + halfZspan))
      (Vector3 (x - halfXspan) (y + halfYspan) (z - halfZspan))
      (Vector3 (x + halfXspan) (y + halfYspan) (z - halfZspan))
      (Vector3 (x - halfXspan) (y - halfYspan) (z - halfZspan))
      (Vector3 (x + halfXspan) (y - halfYspan) (z - halfZspan))

instance hasAxisAlignedBoundingBoxBoxGeometry :: HasAxisAlignedBoundingBox BoxGeometry where
  getAxisAlignedBoundingBox geometry = calculateAxisAlignedBoundingBox geometry 

instance serializeGeometryBoxGeometry :: SerializeGeometry BoxGeometry where
  serializeGeometry (BoxGeometry attrs) = Geometry.BoxGeometry attrs

calculateAxisAlignedBoundingBox
  :: BoxGeometry
  -> AxisAlignedBoundingBox
calculateAxisAlignedBoundingBox (BoxGeometry { xspan, yspan, zspan }) = AxisAlignedBoundingBox min max
  where
  x = 0.0
  y = 0.0
  z = 0.0
  halfXspan = xspan / 2.0
  halfYspan = yspan / 2.0
  halfZspan = zspan / 2.0
  min = Vector3 (x - halfXspan) (y - halfYspan) (z - halfZspan)
  max = Vector3 (x + halfXspan) (y + halfYspan) (z + halfZspan)

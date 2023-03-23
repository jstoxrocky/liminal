module Liminal.Data.BoxGeometry where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Data.Vector8 (Vector8(..))
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Data.BoundingBox (BoundingBox(..))
import Liminal.Class.HasVertices (class HasVertices)
import Liminal.Class.HasBoundingBox (class HasBoundingBox)

newtype BoxGeometry = BoxGeometry { xspan :: Number, yspan :: Number, zspan :: Number }

derive instance genericBoxGeometry :: Generic BoxGeometry _

instance showBoxGeometry :: Show BoxGeometry where
  show = genericShow

derive instance eqBoxGeometry :: Eq BoxGeometry

derive instance ordBoxGeometry :: Ord BoxGeometry

instance hasVerticesBoxGeometry :: HasVertices BoxGeometry Vector8 where
  getVertices geometry = calculateVertices geometry (Vector3 0.0 0.0 0.0)

instance hasBoundingBoxBoxGeometry :: HasBoundingBox BoxGeometry where
  getBoundingBox geometry = calculateBoundingBox geometry (Vector3 0.0 0.0 0.0)

calculateVertices
  :: BoxGeometry
  -> Vector3 Number
  -> Vector8 (Vector3 Number)
calculateVertices (BoxGeometry { xspan, yspan, zspan }) (Vector3 x y z) = vertices
  where
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

calculateBoundingBox
  :: BoxGeometry
  -> Vector3 Number
  -> BoundingBox
calculateBoundingBox (BoxGeometry { xspan, yspan, zspan }) (Vector3 x y z) = BoundingBox min max
  where
  halfXspan = xspan / 2.0
  halfYspan = yspan / 2.0
  halfZspan = zspan / 2.0
  min = Vector3 (x - halfXspan) (y - halfYspan) (z - halfZspan)
  max = Vector3 (x + halfXspan) (y + halfYspan) (z + halfZspan)

module Liminal.Data.SphereGeometry where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Data.Vector1 (Vector1(..))
import Liminal.Data.BoundingBox (BoundingBox(..))
import Liminal.Class.HasVertices (class HasVertices)
import Liminal.Class.HasBoundingBox (class HasBoundingBox)

newtype SphereGeometry = SphereGeometry { radius :: Number }

derive instance genericSphereGeometry :: Generic SphereGeometry _

instance showSphereGeometry :: Show SphereGeometry where
  show = genericShow

derive instance eqSphereGeometry :: Eq SphereGeometry

derive instance ordSphereGeometry :: Ord SphereGeometry

instance hasVerticesBoxGeometry :: HasVertices SphereGeometry Vector1 where
  getVertices geometry = calculateVertices geometry (Vector3 0.0 0.0 0.0)

instance hasBoundingBoxBoxGeometry :: HasBoundingBox SphereGeometry where
  getBoundingBox geometry = calculateBoundingBox geometry (Vector3 0.0 0.0 0.0)

calculateVertices
  :: SphereGeometry
  -> Vector3 Number
  -> Vector1 (Vector3 Number)
calculateVertices _ position = Vector1 position

calculateBoundingBox
  :: SphereGeometry
  -> Vector3 Number
  -> BoundingBox
calculateBoundingBox (SphereGeometry { radius }) (Vector3 x y z) = BoundingBox min max
  where
  min = Vector3 (x - radius) (y - radius) (z - radius)
  max = Vector3 (x + radius) (y + radius) (z + radius)

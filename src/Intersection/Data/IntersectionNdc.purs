module Liminal.Intersection.Data.IntersectionNdc where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data IntersectionNdc a = IntersectionNdc Number Number a

instance functorIntersectionNdc :: Functor IntersectionNdc where
  map f (IntersectionNdc distanceNdcSquared distanceFromCameraSquared object) = IntersectionNdc distanceNdcSquared distanceFromCameraSquared $ f object

derive instance eqIntersectionNdc :: Eq a => Eq (IntersectionNdc a)

instance ordIntersectionNdc :: Ord a => Ord (IntersectionNdc a) where
  compare
    (IntersectionNdc distanceNdcSquared1 distanceFromCameraSquared1 object1)
    (IntersectionNdc distanceNdcSquared2 distanceFromCameraSquared2 object2) =
    if distanceNdcSquared1 < distanceNdcSquared2 then LT
    else if distanceNdcSquared1 == distanceNdcSquared2 then
      if distanceFromCameraSquared1 < distanceFromCameraSquared2 then LT
      else if distanceFromCameraSquared1 == distanceFromCameraSquared2 then compare object1 object2
      else GT
    else GT

derive instance genericIntersectionNdc :: Generic (IntersectionNdc a) _

instance showIntersectionNdc :: Show a => Show (IntersectionNdc a) where
  show = genericShow

getNdcHoveredObject
  :: forall a
   . IntersectionNdc a
  -> a
getNdcHoveredObject (IntersectionNdc _ _ object) = object

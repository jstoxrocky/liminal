module Liminal.Intersection.Data.IntersectionNdc where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data IntersectionNdc a = IntersectionNdc 
  { distanceNdcSquared :: Number
  , distanceSquared :: Number
  , object :: a }

instance functorIntersectionNdc :: Functor IntersectionNdc where
  map f (IntersectionNdc { distanceNdcSquared, distanceSquared, object }) = IntersectionNdc { distanceNdcSquared, distanceSquared, object: f $ object }

derive instance eqIntersectionNdc :: Eq a => Eq (IntersectionNdc a)

instance ordIntersectionNdc :: Ord a => Ord (IntersectionNdc a) where
  compare 
    (IntersectionNdc 
      { distanceNdcSquared: distanceNdcSquared1
      , distanceSquared: distanceSquared1
      , object: object1 }
    )
    (IntersectionNdc 
      { distanceNdcSquared: distanceNdcSquared2
      , distanceSquared: distanceSquared2
      , object: object2 }
    ) =
    if distanceNdcSquared1 < distanceNdcSquared2 then LT
    else if distanceNdcSquared1 == distanceNdcSquared2 then
      if distanceSquared1 < distanceSquared2 then LT
      else if distanceSquared1 == distanceSquared2 then compare object1 object2
      else GT
    else GT

derive instance genericIntersectionNdc :: Generic (IntersectionNdc a) _

instance showIntersectionNdc :: Show a => Show (IntersectionNdc a) where
  show = genericShow

getNdcHoveredObject
  :: forall a
   . IntersectionNdc a
  -> a
getNdcHoveredObject (IntersectionNdc { object }) = object


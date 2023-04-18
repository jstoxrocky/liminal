module Liminal.Intersection.Data.IntersectionNdc where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Class.HasUuid (class HasUuid, getUuid, setUuid)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix, setMatrix, getPosition, setPosition)
import Liminal.Class.HasInverse (class HasInverse, getInverse, setInverse)
import Liminal.Class.HasAxisAlignedBoundingBox (class HasAxisAlignedBoundingBox, getAxisAlignedBoundingBox)

data IntersectionNdc a = IntersectionNdc 
  { distanceNdcSquared :: Number
  , distanceSquared :: Number
  , object :: a }

instance functorIntersectionNdc :: Functor IntersectionNdc where
  map f (IntersectionNdc { distanceNdcSquared, distanceSquared, object }) = IntersectionNdc { distanceNdcSquared, distanceSquared, object: f $ object }

derive instance eqIntersectionNdc :: Eq a => Eq (IntersectionNdc a)

instance ordIntersectionNdc :: Ord a => Ord (IntersectionNdc a) where
  compare = compareIntersectionNdc

compareIntersectionNdc
  :: forall a b
  . IntersectionNdc a
  -> IntersectionNdc b
  -> Ordering
compareIntersectionNdc
  (IntersectionNdc 
    { distanceNdcSquared: distanceNdcSquared1
    , distanceSquared: distanceSquared1 }
  )
  (IntersectionNdc 
    { distanceNdcSquared: distanceNdcSquared2
    , distanceSquared: distanceSquared2 }
  ) =
  if distanceNdcSquared1 < distanceNdcSquared2 then LT
  else if distanceNdcSquared1 == distanceNdcSquared2 then
    if distanceSquared1 < distanceSquared2 then LT
    else if distanceSquared1 == distanceSquared2 then EQ
    else GT
  else GT

derive instance genericIntersectionNdc :: Generic (IntersectionNdc a) _

instance showIntersectionNdc :: Show a => Show (IntersectionNdc a) where
  show = genericShow

instance hasUuidIntersectionNdc :: HasUuid a => HasUuid (IntersectionNdc a) where
  getUuid (IntersectionNdc { object }) = getUuid object
  setUuid uuid (IntersectionNdc attrs@{ object }) = IntersectionNdc attrs { object = setUuid uuid object }

instance hasMatrixIntersectionNdc :: HasMatrix a => HasMatrix (IntersectionNdc a) where
  getMatrix (IntersectionNdc { object }) = getMatrix object
  setMatrix matrix (IntersectionNdc attrs@{ object }) = IntersectionNdc attrs { object = setMatrix matrix object }
  getPosition (IntersectionNdc { object }) = getPosition object
  setPosition v3 (IntersectionNdc attrs@{ object }) = IntersectionNdc attrs { object = setPosition v3 object }

instance hasInverseIntersectionNdc :: HasInverse a => HasInverse (IntersectionNdc a) where
  getInverse (IntersectionNdc { object }) = getInverse object
  setInverse inverseMatrix (IntersectionNdc attrs@{ object }) = IntersectionNdc attrs { object = setInverse inverseMatrix object }

instance hasAxisAlignedBoundingBoxIntersectionNdc :: HasAxisAlignedBoundingBox a => HasAxisAlignedBoundingBox (IntersectionNdc a) where
  getAxisAlignedBoundingBox (IntersectionNdc { object }) = getAxisAlignedBoundingBox object

getNdcHoveredObject
  :: forall a
   . IntersectionNdc a
  -> a
getNdcHoveredObject (IntersectionNdc { object }) = object


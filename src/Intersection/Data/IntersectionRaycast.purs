module Liminal.Intersection.Data.IntersectionRaycast where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TransformationMatrix.Data.Vector3 (Vector3)
import Liminal.Class.HasUuid (class HasUuid, getUuid, setUuid)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix, setMatrix, getPosition, setPosition)
import Liminal.Class.HasInverse (class HasInverse, getInverse, setInverse)
import Liminal.Class.HasAxisAlignedBoundingBox (class HasAxisAlignedBoundingBox, getAxisAlignedBoundingBox)

data IntersectionRaycast a = IntersectionRaycast 
  { distance :: Number
  , worldPosition :: (Vector3 Number) 
  , object :: a } 

instance functorIntersectionRaycast :: Functor IntersectionRaycast where
  map f (IntersectionRaycast { distance, worldPosition, object }) = IntersectionRaycast 
    { distance, worldPosition, object: f object }

derive instance genericIntersectionRaycast :: Generic (IntersectionRaycast a) _

instance showIntersectionRaycast :: Show a => Show (IntersectionRaycast a) where
  show = genericShow

derive instance eqIntersectionRaycast :: Eq a => Eq (IntersectionRaycast a)

instance ordIntersectionRaycast :: Ord a => Ord (IntersectionRaycast a) where
  compare
    (IntersectionRaycast { distance: distance1 })
    (IntersectionRaycast { distance: distance2 }) =
    if distance1 < distance2 then LT
    else if distance1 == distance2 then EQ
    else GT

instance hasUuidIntersectionRaycast :: HasUuid a => HasUuid (IntersectionRaycast a) where
  getUuid (IntersectionRaycast { object }) = getUuid object
  setUuid uuid (IntersectionRaycast attrs@{ object }) = IntersectionRaycast attrs { object = setUuid uuid object }

instance hasMatrixIntersectionRaycast :: HasMatrix a => HasMatrix (IntersectionRaycast a) where
  getMatrix (IntersectionRaycast { object }) = getMatrix object
  setMatrix matrix (IntersectionRaycast attrs@{ object }) = IntersectionRaycast attrs { object = setMatrix matrix object }
  getPosition (IntersectionRaycast { object }) = getPosition object
  setPosition v3 (IntersectionRaycast attrs@{ object }) = IntersectionRaycast attrs { object = setPosition v3 object }

instance hasInverseIntersectionRaycast :: HasInverse a => HasInverse (IntersectionRaycast a) where
  getInverse (IntersectionRaycast { object }) = getInverse object
  setInverse inverseMatrix (IntersectionRaycast attrs@{ object }) = IntersectionRaycast attrs { object = setInverse inverseMatrix object }

instance hasAxisAlignedBoundingBoxIntersectionRaycast :: HasAxisAlignedBoundingBox a => HasAxisAlignedBoundingBox (IntersectionRaycast a) where
  getAxisAlignedBoundingBox (IntersectionRaycast { object }) = getAxisAlignedBoundingBox object

getRaycastHoveredObject
  :: forall a
   . IntersectionRaycast a
  -> a
getRaycastHoveredObject (IntersectionRaycast { object }) = object

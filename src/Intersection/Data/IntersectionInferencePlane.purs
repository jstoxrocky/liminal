module Intersection.Data.IntersectionInferencePlane where

import Prelude
import TransformationMatrix.Data.Vector3 (Vector3)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype IntersectionInferencePlane = IntersectionInferencePlane 
  { worldPosition :: (Vector3 Number) }

derive instance genericIntersectionInferencePlane :: Generic IntersectionInferencePlane _

instance showIntersectionInferencePlane :: Show IntersectionInferencePlane where
  show = genericShow

derive instance eqIntersectionInferencePlane :: Eq IntersectionInferencePlane

derive instance ordIntersectionInferencePlane :: Ord IntersectionInferencePlane
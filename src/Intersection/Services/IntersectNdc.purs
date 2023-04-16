module Liminal.Intersection.Services.IntersectNdc where

import Prelude

import Liminal.Intersection.Data.IntersectionNdc (IntersectionNdc(..))
import Liminal.Intersection.Services.Projection (project)
import Data.Array ((:))
import Data.Either (Either)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, class Traversable)
import TransformationMatrix.Data.DivisionError (DivisionError)
import Liminal.Class.HasInverse (class HasInverse)
import Liminal.Class.HasMatrix (class HasMatrix, getPosition)
import Liminal.Class.HasProjection (class HasProjection)
import TransformationMatrix.Data.Vector2 (Vector2, vector2DistanceBetweenSquared)
import TransformationMatrix.Data.Vector3 (distanceBetweenSquared)

snappingDistance :: Number
snappingDistance = 0.03

snappingDistanceSquared :: Number
snappingDistanceSquared = snappingDistance * snappingDistance

accumulate
  :: forall a
   . Array a
  -> Maybe a
  -> Array a
accumulate acc Nothing = acc
accumulate acc (Just intersection) = intersection : acc

calculateNdcIntersection
  :: forall a b
   . HasMatrix a
  => HasMatrix b
  => HasInverse b
  => HasProjection b
  => Vector2 Number
  -> b
  -> a
  -> Either DivisionError (Maybe (IntersectionNdc a))
calculateNdcIntersection pointer projector object = do
  let
    objectPosition = getPosition object
    projectorPosition = getPosition projector
    distanceSquared = distanceBetweenSquared objectPosition projectorPosition
  positionNdc <- project projector objectPosition
  let
    distanceNdcSquared = vector2DistanceBetweenSquared (pointer) positionNdc
    isHovered = distanceNdcSquared < snappingDistanceSquared
    intersection =
      if isHovered then Just $ IntersectionNdc { distanceNdcSquared, distanceSquared, object }
      else Nothing
  pure intersection

intersectsNdc
  :: forall a p f
   . HasMatrix a
  => HasMatrix p
  => HasInverse p
  => HasProjection p
  => Foldable f
  => Traversable f
  => Vector2 Number
  -> p
  -> f a
  -> Either DivisionError (Array (IntersectionNdc a))
intersectsNdc pointer projector objects = do
  intersections <- traverse (calculateNdcIntersection pointer projector) objects
  pure $ foldl accumulate [] intersections


module Liminal.Intersection.Services.IntersectRaycast where

import Prelude hiding (add)

import Liminal.Intersection.Data.IntersectionRaycast (IntersectionRaycast(..))
import Liminal.Intersection.Services.Projection (unproject)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array ((:))
import Data.Either (Either)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, class Traversable)
import Data.Tuple (Tuple(..))
import TransformationMatrix.Services.Division (divide)
import TransformationMatrix.Data.DivisionError (DivisionError)
import Liminal.Data.AxisAlignedBoundingBox (AxisAlignedBoundingBox(..))
import Liminal.Data.Ray (Ray(..), applyMatrix4ToRay)
import Liminal.Class.HasAxisAlignedBoundingBox (class HasAxisAlignedBoundingBox, getAxisAlignedBoundingBox)
import Liminal.Class.HasInverseProjection (class HasInverseProjection)
import Liminal.Class.HasMatrix (class HasMatrix, getPosition)
import Liminal.Class.HasInverse (class HasInverse, getInverse)
import Liminal.Class.HasProjection (class HasProjection)
import TransformationMatrix.Data.Vector2 (Vector2(..))
import TransformationMatrix.Data.Vector3 (Vector3(..), add, subtract, normalize, multiplyByScalar)

accumulate
  :: forall a
   . Array a
  -> Maybe a
  -> Array a
accumulate acc Nothing = acc
accumulate acc (Just intersection) = intersection : acc

-- https://github.com/mrdoob/three.js/blob/9b274fef1d2b9ba0fc47410d06593e1f34f4df0e/src/core/Raycaster.js#L34
pointerToRay
  :: forall p
   . HasMatrix p
  => HasProjection p
  => HasInverseProjection p
  => p
  -> Vector2 Number
  -> Either DivisionError Ray
pointerToRay projector (Vector2 x y) = do
  let
    origin = getPosition projector
    z = 0.5
  unprojected <- unproject projector (Vector3 x y z)
  direction <- normalize (subtract unprojected origin)
  pure $ Ray origin direction

-- T represents the distance along the ray vector before a collision with a bounding plane.
solveForT
  :: Number
  -> Number
  -> Number
  -> Number
  -> Either DivisionError (Tuple Number Number)
solveForT direction boundingBoxMin boundingBoxMax origin = do
  inverseDirection <- divide 1.0 direction
  let
    Tuple min max =
      if inverseDirection >= 0.0 then Tuple boundingBoxMin boundingBoxMax
      else Tuple boundingBoxMax boundingBoxMin
    tmin = (min - origin) * inverseDirection
    tmax = (max - origin) * inverseDirection
  pure $ Tuple tmin tmax

-- https://github.com/mrdoob/three.js/blob/47b28bc564b438bf2b80d6e5baf90235292fcbd7/src/math/Ray.js#L323
-- So this WILL fail (DivisionError) if Camera is on an axis AND were looking straight down
-- the axis AND we put the pointer RIGHT on the origin. We can get this to happen in a test.
-- However, in practice, I wasn't even able to recreate this scenario organically by moving the pointer 
-- to try to get it to be on the origin (even after programatically placing the camera on axis and looking at origin). 
-- So, I don't think this function throwing too many DivisionErrors is anything to worry about.
calculateRaycastIntersection
  :: forall a
  . HasInverse a
  => HasAxisAlignedBoundingBox a
  => Ray
  -> a
  -> MaybeT (Either DivisionError) (IntersectionRaycast a)
calculateRaycastIntersection ray object = do
  axisAlignedRay <- MaybeT $ Just <$> applyMatrix4ToRay (getInverse object) ray
  let
    Ray origin direction = axisAlignedRay
    Vector3 originX originY originZ = origin
    Vector3 directionX directionY directionZ = direction
    AxisAlignedBoundingBox
      (Vector3 boundingBoxMinX boundingBoxMinY boundingBoxMinZ)
      (Vector3 boundingBoxMaxX boundingBoxMaxY boundingBoxMaxZ) = getAxisAlignedBoundingBox object

  -- #1
  Tuple txmin txmax <- MaybeT $ Just <$> solveForT directionX boundingBoxMinX boundingBoxMaxX originX
  Tuple tymin tymax <- MaybeT $ Just <$> solveForT directionY boundingBoxMinY boundingBoxMaxY originY
  -- Short Circuit
  (MaybeT <<< pure) $
    if txmin > tymax || tymin > txmax then Nothing
    else (Just unit)

  -- #2
  Tuple tzmin tzmax <- MaybeT $ Just <$> solveForT directionZ boundingBoxMinZ boundingBoxMaxZ originZ
  let
    tmin = max txmin tymin
    tmax = min txmax tymax
  -- Short Circuit
  (MaybeT <<< pure) $
    if tmin > tzmax || tzmin > tmax then Nothing
    else (Just unit)

  -- #3
  let
    tmin' = max tmin tzmin
    tmax' = min tmax tzmax
  -- Short Circuit
  -- if negative it means we have solved the equation but the solutions
  -- are "behind" the camera (so we don't care about them)
  (MaybeT <<< pure) $
    if tmax' < 0.0 then Nothing
    else Just unit

  let
    distance =
      if tmin' >= 0.0 then tmin'
      else tmax'

    localPosition = add origin (multiplyByScalar distance direction)
  pure $ IntersectionRaycast { distance, localPosition, object }

intersectRaycast
  :: forall a
  . HasInverse a
  => HasAxisAlignedBoundingBox a
  => Ray
  -> a
  -> Either DivisionError (Maybe (IntersectionRaycast a))
intersectRaycast ray object = runMaybeT $ calculateRaycastIntersection ray object

intersectsRaycast
  :: forall a f p
  . HasInverse a
  => HasAxisAlignedBoundingBox a
  => Foldable f
  => Traversable f
  => HasMatrix p
  => HasProjection p
  => HasInverseProjection p
  => Vector2 Number
  -> p
  -> f a
  -> Either DivisionError (Array (IntersectionRaycast a))
intersectsRaycast pointer projector objects = do
  ray <- pointerToRay projector pointer
  intersections <- traverse (intersectRaycast ray) objects
  pure $ foldl accumulate [] intersections
module Liminal.Transformation.Services.Translate where

import Prelude hiding (add)

import Liminal.Class.HasMatrix (class HasMatrix, setPosition, getPosition)
import TransformationMatrix.Data.Vector3 (Vector3, add, subtract)
import Liminal.Transformation.Services.Invert (updateInverseMatrixAfterTranslation)
import Liminal.Class.HasInverse (class HasInverse)
import Data.Traversable (class Traversable)

moveObjectToNoInverseUpdate
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Vector3 Number
  -> a
  -> a
moveObjectToNoInverseUpdate position object = setPosition position object

moveObjectTo
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Vector3 Number
  -> a
  -> a
moveObjectTo position object = inversedObject
  where
  translation = subtract position (getPosition object)
  positionedObject = moveObjectToNoInverseUpdate position object
  inversedObject = updateInverseMatrixAfterTranslation translation positionedObject

translateObject
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Vector3 Number
  -> a
  -> a
translateObject translation object = inversedObject
  where
  position = add (getPosition object) translation
  positionedObject = moveObjectToNoInverseUpdate position object
  inversedObject = updateInverseMatrixAfterTranslation translation positionedObject

moveObjectsTo
  :: forall a f
   . HasMatrix a
  => HasInverse a
  => Functor f
  => Traversable f
  => Vector3 Number
  -> f a
  -> f a
moveObjectsTo position objects = moveObjectTo position <$> objects

translateObjects
  :: forall a f
   . HasMatrix a
  => HasInverse a
  => Functor f
  => Traversable f
  => Vector3 Number
  -> f a
  -> f a
translateObjects translation objects = translateObject translation <$> objects

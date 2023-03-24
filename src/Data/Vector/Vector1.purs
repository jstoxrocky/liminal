module Liminal.Data.Vector1 where

import Prelude
import Data.Traversable (class Traversable)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Array.NonEmpty (NonEmptyArray, singleton)

data Vector1 a = Vector1 a

derive instance genericVector1 :: Generic (Vector1 a) _

instance showVector1 :: Show a => Show (Vector1 a) where
  show = genericShow

derive instance eqVector1 :: Eq a => Eq (Vector1 a)

instance functorVector1 :: Functor Vector1 where
  map f (Vector1 x1) = Vector1 (f x1)

instance foldableVector1 :: Foldable Vector1 where
  foldr f e (Vector1 x1) = foldr f e [ x1 ]
  foldl f e (Vector1 x1) = foldl f e [ x1 ]
  foldMap f (Vector1 x1) = foldMap f [ x1 ]

instance traversableVector1 :: Traversable Vector1 where
  traverse f (Vector1 x1) = Vector1 <$> (f x1)
  sequence (Vector1 m1) = Vector1 <$> m1

toNonEmptyArray :: forall a. Vector1 a -> NonEmptyArray a
toNonEmptyArray (Vector1 x1) = singleton x1

toArray :: forall a. Vector1 a -> Array a
toArray (Vector1 x1) = [ x1 ]

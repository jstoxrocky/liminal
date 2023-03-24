module Liminal.Data.Vector8 where

import Prelude
import Data.Traversable (class Traversable)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Maybe (Maybe(..))

data Vector8 a = Vector8 a a a a a a a a

derive instance genericVector8 :: Generic (Vector8 a) _

instance showVector8 :: Show a => Show (Vector8 a) where
  show = genericShow

derive instance eqVector8 :: Eq a => Eq (Vector8 a)

instance functorVector8 :: Functor Vector8 where
  map f (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) =
    Vector8
      (f x1)
      (f x2)
      (f x3)
      (f x4)
      (f x5)
      (f x6)
      (f x7)
      (f x8)

instance foldableVector8 :: Foldable Vector8 where
  foldr f e (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = foldr f e [ x1, x2, x3, x4, x5, x6, x7, x8 ]
  foldl f e (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = foldl f e [ x1, x2, x3, x4, x5, x6, x7, x8 ]
  foldMap f (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = foldMap f [ x1, x2, x3, x4, x5, x6, x7, x8 ]

instance traversableVector8 :: Traversable Vector8 where
  traverse f (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = Vector8
    <$> (f x1)
    <*> (f x2)
    <*> (f x3)
    <*> (f x4)
    <*> (f x5)
    <*> (f x6)
    <*> (f x7)
    <*> (f x8)
  sequence (Vector8 m1 m2 m3 m4 m5 m6 m7 m8) = Vector8
    <$> m1
    <*> m2
    <*> m3
    <*> m4
    <*> m5
    <*> m6
    <*> m7
    <*> m8

toNonEmptyArray :: forall a. Vector8 a -> NonEmptyArray a
toNonEmptyArray (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = cons' x1 [ x2, x3, x4, x5, x6, x7, x8 ]

toArray :: forall a. Vector8 a -> Array a
toArray (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = [ x1, x2, x3, x4, x5, x6, x7, x8 ]

head :: forall a. Vector8 a -> a
head (Vector8 x1 _ _ _ _ _ _ _) = x1

find
  :: forall a
  . Vector8 a
  -> (a -> Boolean)
  -> Maybe { found :: a, others :: NonEmptyArray a }
find (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) pred = result
  where
  result =
    if pred x1 then pure $ { found: x1, others: cons' x2 [ x3, x4, x5, x6, x7, x8 ] }
    else if pred x2 then pure $ { found: x2, others: cons' x1 [ x3, x4, x5, x6, x7, x8 ] }
    else if pred x3 then pure $ { found: x3, others: cons' x1 [ x2, x4, x5, x6, x7, x8 ] }
    else if pred x4 then pure $ { found: x4, others: cons' x1 [ x2, x3, x5, x6, x7, x8 ] }
    else if pred x5 then pure $ { found: x5, others: cons' x1 [ x2, x3, x4, x6, x7, x8 ] }
    else if pred x6 then pure $ { found: x6, others: cons' x1 [ x2, x3, x4, x5, x7, x8 ] }
    else if pred x7 then pure $ { found: x7, others: cons' x1 [ x2, x3, x4, x5, x6, x8 ] }
    else if pred x8 then pure $ { found: x8, others: cons' x1 [ x2, x3, x4, x5, x6, x7 ] }
    else Nothing

replace
  :: forall a
  . Vector8 a
  -> (a -> Boolean)
  -> a
  -> Vector8 a
replace v8@(Vector8 x1 x2 x3 x4 x5 x6 x7 x8) pred x = result
  where
  result =
    if pred x1 then Vector8 x x2 x3 x4 x5 x6 x7 x8
    else if pred x2 then Vector8 x1 x x3 x4 x5 x6 x7 x8
    else if pred x3 then Vector8 x1 x2 x x4 x5 x6 x7 x8
    else if pred x4 then Vector8 x1 x2 x3 x x5 x6 x7 x8
    else if pred x5 then Vector8 x1 x2 x3 x4 x x6 x7 x8
    else if pred x6 then Vector8 x1 x2 x3 x4 x5 x x7 x8
    else if pred x7 then Vector8 x1 x2 x3 x4 x5 x6 x x8
    else if pred x8 then Vector8 x1 x2 x3 x4 x5 x6 x7 x
    else v8

uncons
  :: forall a
   . Vector8 a
  -> { head :: a, tail :: NonEmptyArray a }
uncons (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = { head: x1, tail: cons' x2 [ x3, x4, x5, x6, x7, x8 ] }


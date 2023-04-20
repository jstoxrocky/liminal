module Liminal.Class.HasArrayForm where

import TransformationMatrix.Data.Vector2 (Vector2(..))

class HasArrayForm v where
  asArray :: forall a. v a -> Array a

instance hasArrayFormVector2 :: HasArrayForm Vector2 where
  asArray (Vector2 x1 x2) = [x1, x2]
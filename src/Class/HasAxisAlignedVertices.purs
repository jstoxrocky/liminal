module Liminal.Class.HasAxisAlignedVertices where

import Prelude
import TransformationMatrix.Data.Vector3 (Vector3)
import TransformationMatrix.Data.Matrix4 (Matrix4, setPosition)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix)

class HasAxisAlignedVertices m t | m -> t where
  getAxisAlignedVertices :: m -> t (Vector3 Number)

getMatricesAtAxisAlignedVertices
  :: forall a f
   . HasMatrix a
  => HasAxisAlignedVertices a f
  => Functor f
  => a
  -> f Matrix4
getMatricesAtAxisAlignedVertices object = (flip setPosition matrix) <$> vertices
  where
  matrix = getMatrix object
  vertices = getAxisAlignedVertices object

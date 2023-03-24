module Liminal.Transformation.Services.Invert where

import Prelude

import TransformationMatrix.Data.DivisionError (DivisionError)
import Liminal.Class.HasInverse (class HasInverse, setInverse, getInverse)
import TransformationMatrix.Data.Matrix4 (invert, inverseAfterTranslation)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix)
import Data.Either (Either)
import TransformationMatrix.Data.Vector3 (Vector3)

updateInverseMatrix
  :: forall a
   . HasMatrix a
  => HasInverse a
  => a
  -> Either DivisionError a
updateInverseMatrix object = do
  inverseMatrix <- invert (getMatrix object)
  let
    inverseObject = setInverse inverseMatrix object
  pure inverseObject

-- If we have only translated an object we can update its
-- inverse matrix using this trick to bypass the Either return type
updateInverseMatrixAfterTranslation
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Vector3 Number
  -> a
  -> a
updateInverseMatrixAfterTranslation translation object = inverseObject
  where
  inverseMatrix = inverseAfterTranslation translation (getInverse object)
  inverseObject = setInverse inverseMatrix object
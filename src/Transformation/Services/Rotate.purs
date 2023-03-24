module Liminal.Transformation.Services.Rotate where

import Prelude hiding (add)

import Data.Either (Either)
import TransformationMatrix.Data.DivisionError (DivisionError)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix, setMatrix)
import TransformationMatrix.Data.Matrix4 (rotateAboutAxisAtPoint)
import Liminal.Transformation.Services.Invert (updateInverseMatrix)
import Liminal.Class.HasInverse (class HasInverse)
import TransformationMatrix.Data.Axis (Axis(..), X(..), Y(..), Z(..))

rotateNoInverseUpdate
  :: forall a
   . HasMatrix a
  => a
  -> Number
  -> Axis
  -> a
rotateNoInverseUpdate object theta axis = setMatrix matrix object
  where
  matrix = rotateAboutAxisAtPoint (getMatrix object) theta axis

rotateObject
  :: forall a
   . HasMatrix a
  => HasInverse a
  => a
  -> Number
  -> Axis
  -> Either DivisionError a
rotateObject object theta axis = do
  let
    rotatedObject = rotateNoInverseUpdate object theta axis
  updateInverseMatrix rotatedObject

rotateAboutOriginXAxis
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Number
  -> a
  -> Either DivisionError a
rotateAboutOriginXAxis theta object = do
  let
    axis = Xaxis (Y 0.0) (Z 0.0)
    rotatedObject = rotateNoInverseUpdate object theta axis
  updateInverseMatrix rotatedObject

rotateAboutOriginYAxis
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Number
  -> a
  -> Either DivisionError a
rotateAboutOriginYAxis theta object = do
  let
    axis = Yaxis (X 0.0) (Z 0.0)
    rotatedObject = rotateNoInverseUpdate object theta axis
  updateInverseMatrix rotatedObject

rotateAboutOriginZAxis
  :: forall a
   . HasMatrix a
  => HasInverse a
  => Number
  -> a
  -> Either DivisionError a
rotateAboutOriginZAxis theta object = do
  let
    axis = Zaxis (X 0.0) (Y 0.0)
    rotatedObject = rotateNoInverseUpdate object theta axis
  updateInverseMatrix rotatedObject

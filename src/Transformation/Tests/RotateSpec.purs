module Liminal.Transformation.Tests.RotateSpec where

import Prelude hiding (add)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Data.Either (Either(..))
import Liminal.Transformation.Services.Invert (updateInverseMatrix)
import Data.Tuple (Tuple(..))
import TransformationMatrix.Data.Axis (Axis(..), X(..), Z(..))
import Liminal.Transformation.Services.Rotate (rotateObject, rotateNoInverseUpdate)
import TransformationMatrix.Data.Matrix4 (identity4)
import Liminal.Data.Mesh (Mesh(..))
import Liminal.Data.BoxGeometry (BoxGeometry(..))
import Data.Maybe (Maybe(..))

rotateSpec :: Spec Unit
rotateSpec = do
  describe "RotateSpec" do
    it "should rotate an Object" do
      let
        -- Setup
        uuid = 1
        matrix = identity4
        inverseMatrix = identity4
        geometry = BoxGeometry { xspan: 1.0, yspan: 1.0, zspan: 1.0 }
        material =
          { transparent: Nothing
          , opacity: Nothing
          , color: Nothing
          , emissiveColor: Nothing
          , emissiveIntensity: Nothing
          , needsUpdate: Nothing
          , visible: Nothing
          }
        mesh = Mesh { geometry, material, uuid, matrix, inverseMatrix }
        theta = 45.0
        axis = Yaxis (X 0.0) (Z 0.0)

        -- Expectations
        maybeLumber = do
          let
            rotatedObject = rotateNoInverseUpdate mesh theta axis
          updateInverseMatrix rotatedObject
        maybeExpectedResult = maybeLumber

        -- Test
        maybeResult = rotateObject mesh theta axis
        maybeResults = do
          expectedResult <- maybeExpectedResult
          result <- maybeResult
          pure $ Tuple result expectedResult
      case maybeResults of
        Left left -> fail $ "threw an error: " <> show left
        Right (Tuple result expectedResult) -> result `shouldEqual` expectedResult

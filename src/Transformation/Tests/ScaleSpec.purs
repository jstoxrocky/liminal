module Liminal.Transformation.Tests.ScaleSpec where

import Prelude hiding (add)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Data.Either (Either(..))
import Liminal.Transformation.Services.Invert (updateInverseMatrix)
import Data.Tuple (Tuple(..))
import TransformationMatrix.Data.Matrix4 (identity4)
import Liminal.Data.Mesh (Mesh(..))
import Liminal.Data.BoxGeometry (BoxGeometry(..))
import Liminal.Transformation.Services.Scale (scaleObject, scaleObjectNoInverseUpdate)

scaleSpec :: Spec Unit
scaleSpec = do
  describe "ScaleSpec" do
    it "should scale an object" do
      let
        -- Setup
        uuid = 1
        matrix = identity4
        inverseMatrix = identity4
        geometry = BoxGeometry { xspan: 1.0, yspan: 1.0, zspan: 1.0 }
        mesh = Mesh { geometry, uuid, matrix, inverseMatrix }
        multiplier = 1.5

        -- Expectations
        maybeLumber = do
          let
            scaledObject = scaleObjectNoInverseUpdate multiplier mesh
          updateInverseMatrix scaledObject
        maybeExpectedResult = maybeLumber

        -- Test
        maybeResult = scaleObject multiplier mesh
        maybeResults = do
          expectedResult <- maybeExpectedResult
          result <- maybeResult
          pure $ Tuple result expectedResult
      case maybeResults of
        Left left -> fail $ "threw an error: " <> show left
        Right (Tuple result expectedResult) -> result `shouldEqual` expectedResult

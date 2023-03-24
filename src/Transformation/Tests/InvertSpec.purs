module Liminal.Transformation.Tests.InvertSpec where

import Prelude
import Test.Spec (Spec, describe, it)
import Liminal.Transformation.Services.Invert (updateInverseMatrix, updateInverseMatrixAfterTranslation)
import Data.Either (Either(..))
import Test.Spec.Assertions (fail, shouldEqual)
import Data.Tuple (Tuple(..))
import Liminal.Class.HasInverse (getInverse, setInverse)
import Liminal.Class.HasMatrix (getMatrix)
import TransformationMatrix.Data.Matrix4 (invert, identity4, setPosition, inverseAfterTranslation)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Data.PerspectiveCamera (PerspectiveCamera(..))

invertSpec :: Spec Unit
invertSpec = do
  describe "InvertSpec" do
    it "should update the PerspectiveCameras inverse matrix" do
      let
        -- Setup
        position = Vector3 5.0 5.0 5.0
        matrix = setPosition position identity4
        camera = PerspectiveCamera 
          { uuid: 1
          , matrix
          , inverseMatrix: identity4
          , projection: identity4
          , inverseProjection: identity4
          , fov: 0.35
          , orbitTarget: Vector3 0.0 0.0 0.0
          }

        -- Expectations
        maybeInverseMatrix = invert (getMatrix camera)
        maybeInversePerspectiveCamera = (\inverseMatrix -> setInverse inverseMatrix camera) <$> maybeInverseMatrix
        maybeExpectedResult = maybeInversePerspectiveCamera

        -- Test
        maybeResult = updateInverseMatrix camera
        maybeResults = do
          result <- maybeResult
          expectedResult <- maybeExpectedResult
          pure $ Tuple result expectedResult
      case maybeResults of
        Left left -> fail $ "threw an error: " <> show left
        Right (Tuple result expectedResult) ->
          result `shouldEqual` expectedResult

    it "should update the PerspectiveCameras inverse matrix after a translation" do
      let
        -- Setup
        translation = Vector3 5.0 5.0 5.0
        position = translation
        matrix = setPosition position identity4
        camera = PerspectiveCamera 
          { uuid: 1
          , matrix
          , inverseMatrix: identity4
          , projection: identity4
          , inverseProjection: identity4
          , fov: 0.35
          , orbitTarget: Vector3 0.0 0.0 0.0
          }

        -- Expectations
        expecteInverseMatrix = inverseAfterTranslation translation (getInverse camera)
        expectedResult = setInverse expecteInverseMatrix camera

        -- Test
        result = updateInverseMatrixAfterTranslation translation camera
      result `shouldEqual` expectedResult
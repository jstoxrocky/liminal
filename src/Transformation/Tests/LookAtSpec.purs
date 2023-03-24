module Liminal.Transformation.Tests.LookAtSpec where

import Prelude hiding (add)

import Test.Spec (Spec, describe, it)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Test.Spec.Assertions (shouldEqual, fail)
import Data.Either (Either(..))
import Liminal.Transformation.Services.Invert (updateInverseMatrix)
import Data.Tuple (Tuple(..))
import Liminal.Data.PerspectiveCamera (PerspectiveCamera(..))
import TransformationMatrix.Data.Matrix4 (identity4)
import Liminal.Transformation.Services.Translate (moveObjectToNoInverseUpdate)
import Liminal.Transformation.Services.LookAt (lookAtNoInverseUpdate, moveObjectToAndLookAt)

lookAtSpec :: Spec Unit
lookAtSpec = do
  describe "LookAtSpec" do
    it "should move an object and look at a target" do
      let
        -- Setup
        camera = PerspectiveCamera 
          { uuid: 1
          , matrix: identity4
          , inverseMatrix: identity4
          , projection: identity4
          , inverseProjection: identity4
          , fov: 0.35
          , orbitTarget: Vector3 0.0 0.0 0.0
          }
        target = Vector3 1.0 2.0 3.0
        position = Vector3 4.0 5.0 6.0

        -- Expectations
        maybePerspectiveCamera = do
          let
            movedObject = moveObjectToNoInverseUpdate position camera
          lookedAtObject <- lookAtNoInverseUpdate target movedObject
          updateInverseMatrix lookedAtObject

        -- Test
        maybeResult = moveObjectToAndLookAt position target camera
        maybeResults = do
          expectedResult <- maybePerspectiveCamera
          result <- maybeResult
          pure $ Tuple result expectedResult

      case maybeResults of
        Left left -> fail $ "threw an error: " <> show left
        Right (Tuple result expectedResult) -> result `shouldEqual` expectedResult



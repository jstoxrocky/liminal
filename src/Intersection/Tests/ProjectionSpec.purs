module Liminal.Intersection.Tests.ProjectionSpec where

import Prelude
import Test.Spec (Spec, describe, it)
import Liminal.Intersection.Services.Projection (updateObjectProjection, calculateProjectionMatrix)
import Data.Either (Either(..))
import Test.Spec.Assertions (fail, shouldEqual)
import Data.Tuple (Tuple(..))
import Liminal.Class.HasFieldOfView (getFov)
import Liminal.Class.HasProjection (setProjection)
import Liminal.Class.HasInverseProjection (setInverseProjection)
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Data.PerspectiveCamera (PerspectiveCamera(..))
import TransformationMatrix.Data.Matrix4 (Matrix4(..), invert)

projectionSpec :: Spec Unit
projectionSpec = do
  describe "ProjectionSpec" do
    it "should update the Cameras projection matrix" do
      let
        -- Setup
        -- Force place camera on z axis facing origin
        -- position = Vector3 0.0 0.0 (-10.0)
        camera = PerspectiveCamera 
          { fov: 35.0
          , matrix: (Matrix4 (-1.0) 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 (-1.0) (-10.0) 0.0 0.0 0.0 1.0)
          , inverseMatrix: (Matrix4 (-1.0) 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 (-1.0) (-10.0) 0.0 0.0 0.0 1.0)
          , orbitTarget: (Vector3 0.0 0.0 0.0)
          , projection: (Matrix4 3.171594802363213 0.0 0.0 0.0 0.0 3.171594802363213 0.0 0.0 0.0 0.0 (-1.00010000500025) (-0.200010000500025) 0.0 0.0 (-1.0) 1.0)
          , inverseProjection: (Matrix4 0.3152987888789835 0.0 0.0 0.0 0.0 0.3152987888789835 0.0 0.0 0.0 0.0 (-0.8332569476271824) (-0.16665972251156205) 0.0 0.0 (-0.8332569476271824) 0.833340277488438)
          , uuid: 1 }
        aspect = 0.22

        -- Expectations
        maybeExpectedResult = do
          projection <- calculateProjectionMatrix (getFov camera) aspect
          inverseProjection <- invert projection
          pure $ ((setInverseProjection inverseProjection) <<< (setProjection projection)) camera

        -- Test
        maybeResult = updateObjectProjection aspect camera
        maybeResults = do
          result <- maybeResult
          expectedResult <- maybeExpectedResult
          pure $ Tuple result expectedResult

      case maybeResults of
        Left left -> fail $ "threw an error: " <> show left
        Right (Tuple result expectedResult) ->
          result `shouldEqual` expectedResult


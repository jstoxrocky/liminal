module Liminal.OrbitControls.Tests.OrbitSpec where

import Prelude
import Test.Spec (Spec, describe, it)
import Liminal.OrbitControls.Services.Orbit (orbitObject, calculateOrbitOffset)
import TransformationMatrix.Data.Vector2 (Vector2(..))
import Data.Either (Either(..))
import Test.Spec.Assertions (fail, shouldEqual)
import Liminal.Transformation.Services.LookAt (moveObjectToAndLookAt)
import Liminal.Data.PerspectiveCamera (PerspectiveCamera(..))
import Data.Tuple (Tuple(..))
import TransformationMatrix.Data.Vector3 (Vector3(..))
import TransformationMatrix.Data.Matrix4 (Matrix4(..))

orbitSpec :: Spec Unit
orbitSpec = do
  describe "OrbitSpec" do
    it "should orbit the Camera" do
      let
        -- Setup
        orbitTarget = Vector3 0.0 0.0 0.0
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
        delta = Vector2 0.1 0.7

        -- Expectations
        maybeOrbitCamera = do
          position <- calculateOrbitOffset delta orbitTarget camera
          moveObjectToAndLookAt position orbitTarget camera
        maybeExpectedResult = maybeOrbitCamera

        -- Test
        maybeResult = orbitObject delta orbitTarget camera
        maybeResults = do
          result <- maybeResult
          expectedResult <- maybeExpectedResult
          pure $ Tuple result expectedResult

      case maybeResults of
        Left left -> fail $ "threw an error: " <> show left
        Right (Tuple result expectedResult) ->
          result `shouldEqual` expectedResult

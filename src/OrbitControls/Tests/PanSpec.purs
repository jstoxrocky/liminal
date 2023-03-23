module Liminal.OrbitControls.Tests.PanSpec where

import Prelude hiding (add)
import Test.Spec (Spec, describe, it)
import Liminal.Transformation.Services.Translate (translateObject)
import TransformationMatrix.Data.Vector3 (Vector3(..), add)
import Liminal.OrbitControls.Services.Pan (calculatePanOffset, panObject)
import TransformationMatrix.Data.Vector2 (Vector2(..))
import Liminal.Data.PerspectiveCamera (PerspectiveCamera(..))
import Liminal.Class.HasOrbitTarget (setOrbitTarget)
import Test.Spec.Assertions (shouldEqual)
import TransformationMatrix.Data.Matrix4 (identity4)

panSpec :: Spec Unit
panSpec = do
  describe "PanSpec" do
    it "should pan the Camera" do
      let
        -- Setup
        orbitTarget = Vector3 0.0 0.0 0.0
        camera = PerspectiveCamera 
          { uuid: 1
          , matrix: identity4
          , inverseMatrix: identity4
          , projection: identity4
          , inverseProjection: identity4
          , fov: 0.35
          , orbitTarget: orbitTarget
          }
        delta = Vector2 0.1 0.7

        -- Expectations
        panOffset = calculatePanOffset delta orbitTarget camera
        updatedObject = setOrbitTarget (add orbitTarget panOffset) camera
        expectedResult = translateObject panOffset updatedObject

        -- Test
        result = panObject delta orbitTarget camera
      result `shouldEqual` expectedResult

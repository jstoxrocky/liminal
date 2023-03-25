module Liminal.Transformation.Tests.TranslateSpec where

import Prelude hiding (add)

import Liminal.Class.HasMatrix (getPosition, setPosition)
import Test.Spec (Spec, describe, it)
import TransformationMatrix.Data.Vector3 (Vector3(..), add, subtract)
import Test.Spec.Assertions (shouldEqual)
import Liminal.Transformation.Services.Invert (updateInverseMatrixAfterTranslation)
import TransformationMatrix.Data.Matrix4 (identity4)
import Liminal.Data.Mesh (Mesh(..))
import Liminal.Data.BoxGeometry (BoxGeometry(..))
import Liminal.Transformation.Services.Translate (moveObjectTo, translateObject)

translateSpec :: Spec Unit
translateSpec = do
  describe "TranslateSpec" do
    it "should move an object to a position" do
      let
        -- Setup
        uuid = 1
        matrix = identity4
        inverseMatrix = identity4
        geometry = BoxGeometry { xspan: 1.0, yspan: 1.0, zspan: 1.0 }
        mesh = Mesh { geometry, uuid, matrix, inverseMatrix }
        position = Vector3 1.0 2.0 3.0

        -- Expectations
        translation = subtract position (getPosition mesh)
        positionedLumber = setPosition position mesh
        expectedResult = updateInverseMatrixAfterTranslation translation positionedLumber

        -- Test
        result = moveObjectTo position mesh
      result `shouldEqual` expectedResult

    it "should translate an object" do
      let
        -- Setup
        uuid = 1
        matrix = identity4
        inverseMatrix = identity4
        geometry = BoxGeometry { xspan: 1.0, yspan: 1.0, zspan: 1.0 }
        mesh = Mesh { geometry, uuid, matrix, inverseMatrix }
        translation = Vector3 1.0 2.0 3.0

        -- Expectations
        position = add (getPosition mesh) translation
        positionedLumber = setPosition position mesh
        expectedResult = updateInverseMatrixAfterTranslation translation positionedLumber

        -- Test
        result = translateObject translation mesh
      result `shouldEqual` expectedResult

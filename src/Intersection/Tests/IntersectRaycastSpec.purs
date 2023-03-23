module Liminal.Intersection.Tests.IntersectRaycastSpec where

import Prelude
import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import TransformationMatrix.Data.Vector2 (Vector2(..))
import TransformationMatrix.Data.Vector3 (Vector3(..))
import Liminal.Intersection.Services.IntersectRaycast (calculateRaycastIntersection, intersectsRaycast, pointerToRay)
import Data.Array (sort, fromFoldable)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Tuple (Tuple(..))
import Liminal.Data.Mesh (Mesh(..))
import Liminal.Data.BoxGeometry (BoxGeometry(..))
import Liminal.Data.Material (emptyMaterial)
import TransformationMatrix.Data.Matrix4 (Matrix4(..), identity4)
import Liminal.Data.PerspectiveCamera (PerspectiveCamera(..))
import Liminal.Transformation.Services.Translate (moveObjectTo)
import Liminal.Class.HasUuid (setUuid)

dummyMesh :: Mesh BoxGeometry
dummyMesh = Mesh { geometry, material, uuid, matrix, inverseMatrix } where
  uuid = 1
  geometry = BoxGeometry { yspan: 8.0, xspan: 3.5, zspan: 1.5 }
  material = emptyMaterial
  matrix = identity4
  inverseMatrix = identity4

dummyCamera :: PerspectiveCamera
dummyCamera = PerspectiveCamera 
  { fov: 35.0
  , matrix: (Matrix4 (-1.0) 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 (-1.0) (-10.0) 0.0 0.0 0.0 1.0)
  , inverseMatrix: (Matrix4 (-1.0) 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 (-1.0) (-10.0) 0.0 0.0 0.0 1.0)
  , orbitTarget: (Vector3 0.0 0.0 0.0)
  , projection: (Matrix4 3.171594802363213 0.0 0.0 0.0 0.0 3.171594802363213 0.0 0.0 0.0 0.0 (-1.00010000500025) (-0.200010000500025) 0.0 0.0 (-1.0) 1.0)
  , inverseProjection: (Matrix4 0.3152987888789835 0.0 0.0 0.0 0.0 0.3152987888789835 0.0 0.0 0.0 0.0 (-0.8332569476271824) (-0.16665972251156205) 0.0 0.0 (-0.8332569476271824) 0.833340277488438)
  , uuid: 1 }

intersectRaycastSpec :: Spec Unit
intersectRaycastSpec = do
  describe "IntersectRaycastSpec" do
    it "should intersect with a lumber that is hovered over" do
      let
        -- Setup
        distanceX = 0.01
        distanceY = 0.01
        pointer = Vector2 distanceX distanceY

        -- Expectations
        maybeExpectedIntersections = do
          ray <- pointerToRay dummyCamera pointer
          intersection <- runMaybeT $ calculateRaycastIntersection ray dummyMesh
          pure $ fromFoldable intersection

        -- Test
        maybeIntersections = intersectsRaycast pointer dummyCamera [ dummyMesh ]
        maybeResults = do
          expectedIntersections <- maybeExpectedIntersections
          intersections <- maybeIntersections
          pure $ Tuple intersections expectedIntersections

      case maybeResults of
        Left left -> fail $ "threw an error: " <> show left
        Right (Tuple intersections expectedIntersections) ->
          sort intersections `shouldEqual` expectedIntersections

    it "should not intersect with a lumber that is not hovered over" do
      let
        -- Setup
        farAway = Vector3 99.0 99.0 99.0
        mesh = moveObjectTo farAway dummyMesh
        distanceX = 0.01
        distanceY = 0.01
        pointer = Vector2 distanceX distanceY

        -- Expectations
        expectedIntersections = []

        -- Test
        maybeIntersections = intersectsRaycast pointer dummyCamera [ mesh ]

      case maybeIntersections of
        Left left -> fail $ "threw an error: " <> show left
        Right intersections ->
          sort intersections `shouldEqual` expectedIntersections

    it "should intersect with two lumbers and sorting should place the closest one first" do
      let
        -- Setup
        mesh1 = moveObjectTo (Vector3 0.0 0.0 10.0) dummyMesh
        mesh2 = ((moveObjectTo (Vector3 0.0 0.0 0.0)) <<< (setUuid 2)) dummyMesh
        distanceX = 0.01
        distanceY = 0.01
        pointer = Vector2 distanceX distanceY

        -- Expectations
        maybeExpectedIntersections = do
          ray <- pointerToRay dummyCamera pointer
          intersection1 <- runMaybeT $ calculateRaycastIntersection ray mesh1
          intersection2 <- runMaybeT $ calculateRaycastIntersection ray mesh2
          pure $ fromFoldable intersection2 <> fromFoldable intersection1

        -- Test
        maybeIntersections = intersectsRaycast pointer dummyCamera [ mesh1, mesh2 ]
        maybeResults = do
          expectedIntersections <- maybeExpectedIntersections
          intersections <- maybeIntersections
          pure $ Tuple intersections expectedIntersections

      case maybeResults of
        Left left -> fail $ "threw an error: " <> show left
        Right (Tuple intersections expectedIntersections) ->
          sort intersections `shouldEqual` expectedIntersections

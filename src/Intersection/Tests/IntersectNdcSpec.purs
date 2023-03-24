module Liminal.Intersection.Tests.IntersectNdcSpec where

import Prelude
import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import TransformationMatrix.Data.Matrix4 (Matrix4(..), identity4)
import Liminal.Class.HasMatrix (getPosition)
import TransformationMatrix.Data.Vector2 (Vector2(..), vector2DistanceBetweenSquared)
import TransformationMatrix.Data.Vector3 (Vector3(..), distanceBetweenSquared)
import Liminal.Intersection.Services.IntersectNdc (intersectsNdc, snappingDistance, calculateNdcIntersection)
import Liminal.Intersection.Data.IntersectionNdc (IntersectionNdc(..))
import Data.Maybe (Maybe(..))
import Data.Array (sort)
import Liminal.Data.Mesh (Mesh(..))
import Liminal.Data.SphereGeometry (SphereGeometry(..))
import Liminal.Data.Material (emptyMaterial)
import Liminal.Data.PerspectiveCamera (PerspectiveCamera(..))
import Liminal.Transformation.Services.Translate (moveObjectTo)
import Liminal.Class.HasUuid (setUuid)

dummyMesh :: Mesh SphereGeometry
dummyMesh = Mesh { geometry, material, uuid, matrix, inverseMatrix } where
  uuid = 1
  geometry = SphereGeometry { radius: 0.1 }
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

intersectNdcSpec :: Spec Unit
intersectNdcSpec = do
  describe "IntersectNdcSpec" do
    it "should intersect an Orb in NDC when pointer is within snapping distance" do
      let
        -- Setup
        -- Place pointer inside snappingDistance
        distanceX = 0.5 * snappingDistance
        distanceY = 0.0
        pointer = Vector2 distanceX distanceY
        -- Math (Feb 19th 2023 values):
        -- snappingDistance = 0.03
        -- distanceX = snappingDistance / 2 == 0.015
        -- distanceX * distanceX == 0.00025
        -- Which is the returned distanceSquared

        -- Expectations
        -- MeasurementOrb is at origin and we have pointed the camera at origin
        -- Therefore, we can use Vector2 0.0 0.0 as the NDC coordianates
        measurementOrbPosition = getPosition dummyMesh
        cameraPosition = getPosition dummyCamera
        measurementOrbNdcPosition = Vector2 0.0 0.0
        distanceNdcSquared = vector2DistanceBetweenSquared pointer measurementOrbNdcPosition
        distanceFromProjectorSquared = distanceBetweenSquared measurementOrbPosition cameraPosition
        expectedIntersection = pure $ IntersectionNdc distanceNdcSquared distanceFromProjectorSquared dummyMesh

        -- Test
        maybeIntersection = calculateNdcIntersection pointer dummyCamera dummyMesh

      case maybeIntersection of
        Left left -> fail $ "threw an error: " <> show left
        Right intersection ->
          intersection `shouldEqual` expectedIntersection

    it "should not intersect an Orb in NDC when pointer is outside snapping distance" do
      let
        -- Setup
        -- Place pointer outside snappingDistance
        distanceX = 1.5 * snappingDistance
        distanceY = 0.0
        pointer = Vector2 distanceX distanceY

        -- Expectations
        expectedIntersection = Nothing

        -- Test
        maybeIntersection = calculateNdcIntersection pointer dummyCamera dummyMesh

      case maybeIntersection of
        Left left -> fail $ "threw an error: " <> show left
        Right intersection ->
          intersection `shouldEqual` expectedIntersection

    it "should intersect with two Orbs in NDC and sorting should place the closest one first" do
      let
        -- Setup
        -- Place two measurementOrbs along the z-axis
        -- geometry = measurementOrbGeometry
        -- material = measurementOrbDefaultMaterial
        -- inverseMatrix = identity4

        mesh1 = moveObjectTo (Vector3 0.0 0.0 10.0) dummyMesh
        mesh2 = ((moveObjectTo (Vector3 0.0 0.0 0.0)) <<< (setUuid 2)) dummyMesh
        mesh1Position = getPosition mesh1
        mesh2Position = getPosition mesh2
        cameraPosition = getPosition dummyCamera
        -- Place pointer inside snappingDistance
        distanceX = 0.5 * snappingDistance
        distanceY = 0.0
        pointer = Vector2 distanceX distanceY

        -- Expectations
        -- Both orbs will have the same NDC
        measurementOrbsNdcPosition = Vector2 0.0 0.0
        distanceNdcSquared = vector2DistanceBetweenSquared pointer measurementOrbsNdcPosition
        distanceFromProjectorSquared1 = distanceBetweenSquared mesh1Position cameraPosition
        distanceFromProjectorSquared2 = distanceBetweenSquared mesh2Position cameraPosition
        -- Sorted by distance from Camera
        intersectionNdc1 = IntersectionNdc distanceNdcSquared distanceFromProjectorSquared1 mesh1
        intersectionNdc2 = IntersectionNdc distanceNdcSquared distanceFromProjectorSquared2 mesh2
        expectedIntersections =
          [ intersectionNdc2
          , intersectionNdc1
          ]

        -- Test
        maybeIntersections = intersectsNdc pointer dummyCamera [ mesh1, mesh2 ]

      case maybeIntersections of
        Left left -> fail $ "threw an error: " <> show left
        Right intersection ->
          sort intersection `shouldEqual` expectedIntersections
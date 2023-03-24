module Liminal.Data.Mesh where

import Prelude hiding (add)
import Liminal.Data.Material (Material)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Class.HasUuid (class HasUuid, compareUuid)
import Liminal.Class.HasMatrix (class HasMatrix)
import TransformationMatrix.Data.Matrix4 (Matrix4, getPosition, setPosition)
import Liminal.Class.HasInverse (class HasInverse)
import Liminal.Class.HasGeometry (class HasGeometry)
import Liminal.Class.HasVertices (class HasVertices, getVertices)
import TransformationMatrix.Data.Vector3 (add)
import Liminal.Class.HasBoundingBox (class HasBoundingBox, getBoundingBox)
import Liminal.Data.BoundingBox (BoundingBox(..))

data Mesh a = Mesh 
  { geometry :: a
  , material :: Material
  , uuid :: Int
  , matrix :: Matrix4
  , inverseMatrix :: Matrix4 }

derive instance genericMesh :: Generic (Mesh a) _

instance showMesh :: Show a => Show (Mesh a) where
  show = genericShow

derive instance eqMesh :: Eq a => Eq (Mesh a)

instance uuidableMesh :: HasUuid (Mesh a) where
  getUuid (Mesh { uuid }) = uuid
  setUuid uuid' (Mesh attrs) = Mesh attrs { uuid = uuid' }

instance ordMesh :: Ord a => Ord (Mesh a) where
  compare = compareUuid

instance hasMatrixMesh :: HasMatrix (Mesh a) where
  getMatrix (Mesh { matrix }) = matrix
  setMatrix matrix (Mesh attrs) = Mesh attrs { matrix = matrix }
  getPosition (Mesh { matrix }) = getPosition matrix
  setPosition v3 (Mesh attrs@{ matrix }) = Mesh attrs { matrix = setPosition v3 matrix }

instance hasInverseMesh :: HasInverse (Mesh a) where
  getInverse (Mesh { inverseMatrix }) = inverseMatrix
  setInverse inverseMatrix (Mesh attrs) = Mesh attrs { inverseMatrix = inverseMatrix }

instance hasGeometryMesh :: HasGeometry (Mesh a) a where
  getGeometry (Mesh { geometry }) = geometry

instance hasVerticesMesh :: (Functor b, HasVertices a b) => HasVertices (Mesh a) b where
  getVertices (Mesh { matrix, geometry }) = (add (getPosition matrix)) <$> getVertices geometry

instance hasBoundingBoxMesh :: HasBoundingBox a => HasBoundingBox (Mesh a) where
  getBoundingBox (Mesh { geometry, matrix }) = case getBoundingBox geometry of
    BoundingBox v1 v2 -> BoundingBox (add v1 (getPosition matrix)) (add v2 (getPosition matrix))

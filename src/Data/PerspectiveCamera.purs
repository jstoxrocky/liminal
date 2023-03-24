module Liminal.Data.PerspectiveCamera where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Class.HasFieldOfView (class HasFieldOfView)
import Liminal.Class.HasMatrix (class HasMatrix)
import Liminal.Class.HasOrbitTarget (class HasOrbitTarget)
import Liminal.Class.HasProjection (class HasProjection)
import TransformationMatrix.Data.Matrix4 (Matrix4, getPosition, setPosition)
import Liminal.Class.HasUuid (class HasUuid, compareUuid)
import TransformationMatrix.Data.Vector3 (Vector3)
import Liminal.Class.HasInverse (class HasInverse)
import Liminal.Class.HasInverseProjection (class HasInverseProjection)

newtype PerspectiveCamera = PerspectiveCamera
  { uuid :: Int
  , matrix :: Matrix4
  , inverseMatrix :: Matrix4
  , projection :: Matrix4
  , inverseProjection :: Matrix4
  , fov :: Number
  , orbitTarget :: Vector3 Number
  }

derive instance genericPerspectiveCamera :: Generic PerspectiveCamera _

instance showPerspectiveCamera :: Show PerspectiveCamera where
  show = genericShow

derive instance eqPerspectiveCamera :: Eq PerspectiveCamera

instance ordPerspectiveCamera :: Ord PerspectiveCamera where
  compare = compareUuid

instance hasUuidPerspectiveCamera :: HasUuid PerspectiveCamera where
  getUuid (PerspectiveCamera { uuid }) = uuid
  setUuid uuid (PerspectiveCamera attrs) = PerspectiveCamera attrs { uuid = uuid }

instance hasMatrixPerspectiveCamera :: HasMatrix PerspectiveCamera where
  getMatrix (PerspectiveCamera { matrix }) = matrix
  setMatrix matrix (PerspectiveCamera attrs) = PerspectiveCamera attrs { matrix = matrix }
  getPosition (PerspectiveCamera { matrix }) = getPosition matrix
  setPosition v3 (PerspectiveCamera attrs@{ matrix }) = PerspectiveCamera attrs { matrix = setPosition v3 matrix }

instance hasProjectionPerspectiveCamera :: HasProjection PerspectiveCamera where
  getProjection (PerspectiveCamera { projection }) = projection
  setProjection projection (PerspectiveCamera attrs) = PerspectiveCamera attrs { projection = projection }

instance hasInverseProjectionPerspectiveCamera :: HasInverseProjection PerspectiveCamera where
  getInverseProjection (PerspectiveCamera { inverseProjection }) = inverseProjection
  setInverseProjection inverseProjection (PerspectiveCamera attrs) = PerspectiveCamera attrs { inverseProjection = inverseProjection }

instance hasOrbitTargetPerspectiveCamera :: HasOrbitTarget PerspectiveCamera where
  getOrbitTarget (PerspectiveCamera { orbitTarget }) = orbitTarget
  setOrbitTarget orbitTarget (PerspectiveCamera attrs) = PerspectiveCamera attrs { orbitTarget = orbitTarget }

instance hasFieldOfViewPerspectiveCamera :: HasFieldOfView PerspectiveCamera where
  getFov (PerspectiveCamera { fov }) = fov

instance hasInversePerspectiveCamera :: HasInverse PerspectiveCamera where
  getInverse (PerspectiveCamera { inverseMatrix }) = inverseMatrix
  setInverse inverseMatrix (PerspectiveCamera attrs) = PerspectiveCamera attrs { inverseMatrix = inverseMatrix }

module Liminal.Data.Ray where

import Prelude
import TransformationMatrix.Data.Vector3 (Vector3)
import TransformationMatrix.Data.Matrix4 (Matrix4, applyMatrix4, transformDirection)
import TransformationMatrix.Data.DivisionError (DivisionError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Either (Either)

data Ray = Ray (Vector3 Number) (Vector3 Number)

derive instance genericRay :: Generic Ray _

instance showRay :: Show Ray where
  show = genericShow

applyMatrix4ToRay
  :: Matrix4
  -> Ray
  -> Either DivisionError Ray
applyMatrix4ToRay matrix (Ray origin direction) = do
  Ray <$> (applyMatrix4 matrix origin) <*> (transformDirection matrix direction)

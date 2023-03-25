module Liminal.Updates.Data.TransformAction where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Updates.Class.SerializeAction (class SerializeAction, serializeAction)
import Liminal.Updates.Class.HasUpdates (class HasUpdates)
import Liminal.Class.HasUuid (class HasUuid, getUuid)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix)
import Liminal.Updates.Data.Action as Action
import TransformationMatrix.Data.Matrix4 (toArray)
import Liminal.Updates.Data.Updates (Updates(..), emptyUpdateAttrs)
import Liminal.Data.PerspectiveCamera (PerspectiveCamera)

newtype TransformAction a = TransformAction a

derive instance genericTransformAction :: Generic (TransformAction a) _

instance showTransformAction :: Show a => Show (TransformAction a) where
  show = genericShow

derive instance eqTransformAction :: Eq a => Eq (TransformAction a)

derive instance ordTransformAction :: Ord a => Ord (TransformAction a)

instance functorTransformAction :: Functor TransformAction where
  map f (TransformAction a) = TransformAction $ f a

instance serializeActionTransformCameraAction :: SerializeAction (TransformAction PerspectiveCamera) where
  serializeAction (TransformAction camera) = Action.TransformCameraAction { uuid: getUuid camera, matrix: (toArray <<< getMatrix) camera }
else instance cerealTransformAction :: (HasUuid a, HasMatrix a) => SerializeAction (TransformAction a) where
  serializeAction (TransformAction object) = Action.TransformAction { uuid: getUuid object, matrix: (toArray <<< getMatrix) object }

instance updateableTransformAction :: (HasUuid a, HasMatrix a, SerializeAction (TransformAction a)) => HasUpdates (TransformAction a) where
  update action = Updates emptyUpdateAttrs { actions = [ serializeAction action ] }

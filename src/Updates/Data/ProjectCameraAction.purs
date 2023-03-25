module Liminal.Updates.Data.ProjectCameraAction where

import Prelude

import Liminal.Updates.Class.SerializeAction (class SerializeAction, serializeAction)
import Liminal.Updates.Data.Action as Action
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Class.HasProjection (class HasProjection, getProjection)
import TransformationMatrix.Data.Matrix4 (toArray)
import Liminal.Updates.Class.HasUpdates (class HasUpdates)
import Liminal.Updates.Data.Updates (Updates(..), emptyUpdateAttrs)
import Liminal.Class.HasUuid (getUuid, class HasUuid)

newtype ProjectCameraAction a = ProjectCameraAction a

derive instance genericProjectCameraAction :: Generic (ProjectCameraAction a) _

instance showProjectCameraAction :: Show a => Show (ProjectCameraAction a) where
  show = genericShow

derive instance eqProjectCameraAction :: Eq a => Eq (ProjectCameraAction a)

derive instance ordProjectCameraAction :: Ord a => Ord (ProjectCameraAction a)

instance functorProjectCameraAction :: Functor ProjectCameraAction where
  map f (ProjectCameraAction a) = ProjectCameraAction $ f a

instance serializeActionProjectCameraAction :: (HasUuid a, HasProjection a) => SerializeAction (ProjectCameraAction a) where
  serializeAction (ProjectCameraAction object) 
    = Action.ProjectCameraAction 
      { uuid: getUuid object
      , projection: (toArray <<< getProjection) object }

instance hasUpdatesProjectCameraAction :: (SerializeAction (ProjectCameraAction a)) => HasUpdates (ProjectCameraAction a) where
  update action = Updates emptyUpdateAttrs { actions = [ serializeAction action ] }


module Liminal.Updates.Data.RemoveAction where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Updates.Class.HasUpdates (class HasUpdates)
import Liminal.Updates.Class.SerializeAction (class SerializeAction, serializeAction)
import Liminal.Class.HasUuid (class HasUuid, getUuid)
import Liminal.Updates.Data.Action as Action
import Liminal.Updates.Data.Updates (Updates(..), emptyUpdateAttrs)

newtype RemoveAction a = RemoveAction a

derive instance genericRemoveAction :: Generic (RemoveAction a) _

instance showRemoveAction :: Show a => Show (RemoveAction a) where
  show = genericShow

derive instance eqRemoveAction :: Eq a => Eq (RemoveAction a)

derive instance ordRemoveAction :: Ord a => Ord (RemoveAction a)

instance functorRemoveAction :: Functor RemoveAction where
  map f (RemoveAction a) = RemoveAction $ f a

instance serializeActionRemoveAction :: (HasUuid a) => SerializeAction (RemoveAction a) where
  serializeAction (RemoveAction object) = Action.RemoveAction { uuid: getUuid object }

instance hasUpdatesRemoveAction :: (HasUuid a) => HasUpdates (RemoveAction a) where
  update action = Updates emptyUpdateAttrs { actions = [ serializeAction action ] }

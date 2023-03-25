module Liminal.Updates.Data.MaterialAction where

import Prelude

import Liminal.Updates.Class.SerializeAction (class SerializeAction, serializeAction)
import Liminal.Updates.Data.Action as Action
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Class.HasUuid (class HasUuid, getUuid)
import Liminal.Data.MaterialUpdater (MaterialUpdater)
import Liminal.Updates.Class.HasUpdates (class HasUpdates)
import Liminal.Updates.Data.Updates (Updates(..), emptyUpdateAttrs)

data MaterialAction a = MaterialAction MaterialUpdater a

derive instance genericMaterialAction :: Generic (MaterialAction a) _

instance showMaterialAction :: Show a => Show (MaterialAction a) where
  show = genericShow

derive instance eqMaterialAction :: Eq a => Eq (MaterialAction a)

derive instance ordMaterialAction :: Ord a => Ord (MaterialAction a)

instance functorMaterialAction :: Functor MaterialAction where
  map f (MaterialAction skin a) = MaterialAction skin $ f a

instance serializeActionMaterialAction :: (HasUuid a) => SerializeAction (MaterialAction a) where
  serializeAction (MaterialAction materialUpdater object) 
    = Action.MaterialAction 
      { uuid: getUuid object
      , materialUpdater }

instance hasUpdatesAddAction :: (HasUuid a) => HasUpdates (MaterialAction a) where
  update action = Updates emptyUpdateAttrs { actions = [ serializeAction action ] }

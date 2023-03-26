module Liminal.Updates.Data.AddAction where

import Prelude

import Liminal.Updates.Class.SerializeAction (class SerializeAction, serializeAction)
import Liminal.Updates.Data.Action as Action
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Class.HasGeometry (class HasGeometry, getGeometry)
import Liminal.Class.HasMatrix (class HasMatrix, getMatrix)
import Liminal.Class.HasUuid (class HasUuid, getUuid)
import Liminal.Data.MaterialUpdater (MaterialUpdater)
import TransformationMatrix.Data.Matrix4 (toArray)
import Liminal.Updates.Class.HasUpdates (class HasUpdates)
import Liminal.Updates.Data.Updates (Updates(..), emptyUpdateAttrs)
import Liminal.Updates.Class.SerializeGeometry (class SerializeGeometry, serializeGeometry)

data AddAction a = AddAction MaterialUpdater a

derive instance genericAddAction :: Generic (AddAction a) _

instance showAddAction :: Show a => Show (AddAction a) where
  show = genericShow

derive instance eqAddAction :: Eq a => Eq (AddAction a)

derive instance ordAddAction :: Ord a => Ord (AddAction a)

instance functorAddAction :: Functor AddAction where
  map f (AddAction skin a) = AddAction skin $ f a

instance serializeActionAddAction :: (HasUuid a, HasMatrix a, HasGeometry a g, SerializeGeometry g) => SerializeAction (AddAction a) where
  serializeAction (AddAction materialUpdater object)
    = Action.AddAction 
      { uuid: getUuid object
      , matrix: (toArray <<< getMatrix) object
      , geometry: (serializeGeometry <<< getGeometry) object
      , materialUpdater }

instance hasUpdatesAddAction :: (HasUuid a, HasMatrix a, HasGeometry a g, SerializeGeometry g) => HasUpdates (AddAction a) where
  update action = Updates emptyUpdateAttrs { actions = [ serializeAction action ] }

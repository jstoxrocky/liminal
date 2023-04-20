module Liminal.Updates.Data.SetFromPointsAction where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Class.HasArrayForm (class HasArrayForm, asArray)
import Liminal.Class.HasAxisAlignedVertices (class HasAxisAlignedVertices, getAxisAlignedVertices)
import Liminal.Class.HasMatrix (class HasMatrix)
import Liminal.Class.HasUuid (class HasUuid, getUuid)
import Liminal.Updates.Class.HasUpdates (class HasUpdates)
import Liminal.Updates.Class.SerializeAction (class SerializeAction, serializeAction)
import Liminal.Updates.Data.Action as Action
import Liminal.Updates.Data.Updates (Updates(..), emptyUpdateAttrs)
import TransformationMatrix.Data.Vector3 (toXYZ)

newtype SetFromPointsAction a = SetFromPointsAction a

derive instance genericSetFromPointsAction :: Generic (SetFromPointsAction a) _

instance showSetFromPointsAction :: Show a => Show (SetFromPointsAction a) where
  show = genericShow

derive instance eqSetFromPointsAction :: Eq a => Eq (SetFromPointsAction a)

derive instance ordSetFromPointsAction :: Ord a => Ord (SetFromPointsAction a)

instance functorSetFromPointsAction :: Functor SetFromPointsAction where
  map f (SetFromPointsAction a) = SetFromPointsAction $ f a

instance serializeActionSetFromPointsAction :: (HasUuid a, HasAxisAlignedVertices a v, HasArrayForm v) => SerializeAction (SetFromPointsAction a) where
  serializeAction (SetFromPointsAction object) = Action.SetFromPointsAction 
    { uuid: getUuid object
    , points: toXYZ <$> (asArray $ getAxisAlignedVertices object) }
    
instance hasUpdatesSetFromPointsAction :: (HasUuid a, HasMatrix a, SerializeAction (SetFromPointsAction a)) => HasUpdates (SetFromPointsAction a) where
  update action = Updates emptyUpdateAttrs { actions = [ serializeAction action ] }

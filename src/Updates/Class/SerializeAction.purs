module Liminal.Updates.Class.SerializeAction where

import Liminal.Updates.Data.Action (Action)

class SerializeAction a where
  serializeAction :: a -> Action

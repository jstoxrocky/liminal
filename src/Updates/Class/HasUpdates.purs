module Liminal.Updates.Class.HasUpdates where

import Liminal.Updates.Data.Updates (Updates)

class HasUpdates m where
  update :: m -> Updates

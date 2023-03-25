module Liminal.Data.MaterialUpdater where

import Data.Maybe (Maybe(..))

type MaterialUpdater =
  { transparent :: Maybe Boolean
  , opacity :: Maybe Number
  , color :: Maybe Int
  , emissiveColor :: Maybe Int
  , emissiveIntensity :: Maybe Number
  , needsUpdate :: Maybe Boolean
  , visible :: Maybe Boolean
  }

emptyMaterialUpdater :: MaterialUpdater
emptyMaterialUpdater = 
  { transparent: Nothing
  , opacity: Nothing
  , color: Nothing
  , emissiveColor: Nothing
  , emissiveIntensity: Nothing
  , needsUpdate: Nothing
  , visible: Nothing
  }

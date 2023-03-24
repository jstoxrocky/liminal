module Liminal.Data.Material where

import Data.Maybe (Maybe(..))

type Material =
  { transparent :: Maybe Boolean
  , opacity :: Maybe Number
  , color :: Maybe Int
  , emissiveColor :: Maybe Int
  , emissiveIntensity :: Maybe Number
  , needsUpdate :: Maybe Boolean
  , visible :: Maybe Boolean
  }

emptyMaterial :: Material
emptyMaterial = 
  { transparent: Nothing
  , opacity: Nothing
  , color: Nothing
  , emissiveColor: Nothing
  , emissiveIntensity: Nothing
  , needsUpdate: Nothing
  , visible: Nothing
  }

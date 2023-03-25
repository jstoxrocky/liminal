module Liminal.Updates.Data.Updates where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Liminal.Updates.Data.Action (Action)

type UpdateAttrs =
  { isError :: Boolean
  , errorMessage :: String
  , actions :: Array Action
  }

newtype Updates = Updates UpdateAttrs

derive instance genericUpdates :: Generic Updates _

instance showUpdates :: Show Updates where
  show = genericShow

derive instance eqUpdates :: Eq Updates

derive instance orUpdates :: Ord Updates

emptyUpdateAttrs :: UpdateAttrs
emptyUpdateAttrs =
  { isError: false
  , errorMessage: ""
  , actions: []
  }

instance semigroupUpdates :: Semigroup Updates where
  append
    ( Updates
        { isError: isError1
        , errorMessage: errorMessage1
        , actions: actions1
        }
    )
    ( Updates
        { isError: isError2
        , errorMessage: errorMessage2
        , actions: actions2
        }
    ) = Updates
    { isError: isError1 && isError2
    , errorMessage: errorMessage1 <> errorMessage2
    , actions: actions1 <> actions2
    }

instance monoidUpdates :: Monoid Updates where
  mempty = Updates emptyUpdateAttrs


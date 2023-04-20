module Liminal.Updates.Data.Action where

import Prelude
import Liminal.Updates.Data.Geometry (Geometry)
import Liminal.Data.MaterialUpdater (MaterialUpdater)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type ProjectCameraAttrs =
  { uuid :: Int
  , projection :: Array Number
  }

type AddAttrs =
  { uuid :: Int
  , matrix :: Array Number
  , geometry :: Geometry
  , materialUpdater :: MaterialUpdater
  }

type RemoveAttrs =
  { uuid :: Int }

type TransformAttrs =
  { uuid :: Int
  , matrix :: Array Number
  }

type UpdateMaterialAttrs =
  { uuid :: Int
  , materialUpdater :: MaterialUpdater
  }

type SetFromPointsAttrs =
  { uuid :: Int
  , points :: Array { x :: Number, y :: Number, z :: Number } }

data Action
  = TransformCameraAction TransformAttrs
  | ProjectCameraAction ProjectCameraAttrs
  | AddAction AddAttrs
  | RemoveAction RemoveAttrs
  | TransformAction TransformAttrs
  | MaterialAction UpdateMaterialAttrs
  | SetFromPointsAction SetFromPointsAttrs

derive instance genericAction :: Generic Action _

instance showAction :: Show Action where
  show = genericShow

derive instance eqAction :: Eq Action

derive instance orAction :: Ord Action

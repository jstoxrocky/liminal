module Liminal.Pointer.Services.Pointer where

import Prelude
import TransformationMatrix.Data.Vector2 (Vector2(..), subtractVector2s, divideByScalar)
import Data.Either (Either)
import TransformationMatrix.Data.DivisionError (DivisionError)
import TransformationMatrix.Services.Division (divide)

pixelToNdc
  :: Number
  -> Number
  -> Vector2 Number
  -> Either DivisionError (Vector2 Number)
pixelToNdc width height (Vector2 pixelX pixelY) = do
  pctX <- divide pixelX width
  pctY <- divide pixelY height
  let
    ndcX = (pctX * 2.0) - 1.0
    ndcY = (pctY * (-2.0)) + 1.0
  pure $ Vector2 ndcX ndcY

travelPercentage
  :: Number
  -> Vector2 Number
  -> Vector2 Number
  -> Either DivisionError (Vector2 Number)
travelPercentage height startCoordinates endCoordinates = do
  let
    travel = subtractVector2s endCoordinates startCoordinates
  divideByScalar height travel

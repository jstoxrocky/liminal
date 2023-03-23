module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Liminal.Transformation.Tests.InvertSpec (invertSpec)
import Liminal.Transformation.Tests.ScaleSpec (scaleSpec)
import Liminal.Transformation.Tests.RotateSpec (rotateSpec)
import Liminal.Transformation.Tests.TranslateSpec (translateSpec)
import Liminal.Transformation.Tests.LookAtSpec (lookAtSpec)
import Liminal.OrbitControls.Tests.DollySpec (dollySpec)
import Liminal.OrbitControls.Tests.PanSpec (panSpec)
import Liminal.OrbitControls.Tests.OrbitSpec (orbitSpec)
import Liminal.Intersection.Tests.ProjectionSpec (projectionSpec)
import Liminal.Intersection.Tests.IntersectNdcSpec (intersectNdcSpec)
import Liminal.Intersection.Tests.IntersectRaycastSpec (intersectRaycastSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  invertSpec
  scaleSpec
  rotateSpec
  translateSpec
  lookAtSpec
  dollySpec
  panSpec
  orbitSpec
  projectionSpec
  intersectNdcSpec
  intersectRaycastSpec

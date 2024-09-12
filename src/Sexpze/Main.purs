module Sexpze.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Sexpze.Component.App as App

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI App.component {} =<< HA.awaitBody)

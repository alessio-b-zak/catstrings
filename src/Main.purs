module Main where

import Prelude
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (ALERT)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import CatStrings as CatStrings

main :: Eff (HA.HalogenEffects (alert :: ALERT)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI CatStrings.csApp unit body

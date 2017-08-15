module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import CatStrings as CatStrings

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI CatStrings.csApp unit body

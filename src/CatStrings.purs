module CatStrings where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Control.Monad.Aff (Aff)

import DOM (DOM)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Structures
import Utilities

type State = Project

data Query a = NewZero a
data Message

csApp :: forall eff. H.Component HH.HTML Query Unit Message (Aff (dom :: DOM | eff))
csApp =
  H.component
    { initialState: const initial
    , render
    , eval
    , receiver: const Nothing
    }
  where
  
  initial :: State
  initial =
    { diagram: Nothing
    , signature: Signature 
      { cells: Cells []
      , sigma: Nothing
      , k: 0
      , n: 0
      }
    , cacheSourceTarget: Nothing
    , initialized: false
    , viewControls: { project: 0, slices: [] }
    , selectedCell: Nothing
    }
  
  render :: State -> H.ComponentHTML Query
  render project =
    HH.div [classes ["app"]]
      [ renderSignature project.signature
      ]
  
  renderSignature :: Signature -> H.ComponentHTML Query
  renderSignature signature =
    HH.div [classes ["signature"]] $
      [ HH.p_ [ HH.text "New zerocell" ]
      ] <> renderSigma signature
  
  renderSigma :: Signature -> Array (H.ComponentHTML Query)
  renderSigma signature =
    maybe [] renderSigma (signatureSigma signature) <> 
    [ HH.div [classes ["sigma"]]
      [ HH.h2_ [ HH.text (show (signatureN signature) <> "-cells") ]
      , HH.div [classes ["cells"]] []
      ]
    ]
  
  eval :: Query ~> H.ComponentDSL State Query Message (Aff (dom :: DOM | eff))
  eval = case _ of
    NewZero reply ->
      pure reply
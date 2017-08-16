module CatStrings where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Data.String as Str
import Control.Monad.Aff (Aff)

import DOM (DOM)
import DOM.HTML.Indexed.InputType (InputType(InputColor))
import DOM.Event.Types (Event)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Color (Color, toHexString)

import Structures
import Utilities
import Algorithms

type State = Project

data Query a
  = NewZero a
  | UpdateCellColour Int Int Event a
  | UpdateCellName Int Int Event a

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
      [ HH.h1_ [ HH.text "CatStrings.purs" ]
      ] <>
      renderSigma signature
  
  renderSigma :: Signature -> Array (H.ComponentHTML Query)
  renderSigma signature =
    maybe [] renderSigma sigma <> 
    [ HH.div [classes ["sigmas"]]
      [ HH.div [classes ["sigma"]] $
        [ HH.h2_ [ HH.text (show dimension <> "-cells") ]
        , HH.div [classes ["cells"]] $
          mapWithIndex (renderSigmaCell signature dimension) cellArray
        ] <>
        [ HH.div
            [ classes ["newcell"]
            , HE.onClick (HE.input_ NewZero)
            ]
          [ HH.text "New zerocell" ]
        ] `if_` (signatureN signature == 0)
      ]
    ]
    where
      cellArray = signatureCellArray signature
      dimension = signatureN signature
      sigma = signatureSigma signature
  
  renderSigmaCell :: Signature -> Int -> Int -> Cell -> H.ComponentHTML Query
  renderSigmaCell signature dimension i cell =
    HH.div [classes ["sigma-cell"]]
      [ HH.div [classes ["sigma-cell-preview"]] []
      , HH.div [classes ["sigma-cell-body"]]
        [ HH.p_
          [ HH.input
              [ HE.onInput (HE.input $ UpdateCellName dimension i)
              , HP.value cell.name
              ]
          ]
        , HH.p_
          [ HH.input
              [ HE.onInput (HE.input $ UpdateCellColour dimension i)
              , HP.type_ InputColor
              , HP.value (toHexString cell.display.colour)
              ]
          ]
        ]
      ]
  
  eval :: Query ~> H.ComponentDSL State Query Message (Aff (dom :: DOM | eff))
  eval = case _ of
    NewZero reply -> do
      H.put =<< newZeroCell <$> H.get
      pure reply
    UpdateCellColour dimension i ev reply -> do
      mColour <- H.liftEff $ colourValue ev
      for_ mColour $ \colour -> do
        project <- H.get
        let f cell = cell {display = cell.display {colour = colour}}
        let mSignature' = updateSignature f dimension i project.signature
        for_ mSignature' $ \signature' -> H.put (project {signature = signature'})
      pure reply
    UpdateCellName dimension i ev reply -> do
      name <- H.liftEff $ inputValue ev
      unless (Str.null name) do
        project <- H.get
        let f cell = cell {name = name}
        let mSignature' = updateSignature f dimension i project.signature
        for_ mSignature' $ \signature' -> H.put (project {signature = signature'})
      pure reply

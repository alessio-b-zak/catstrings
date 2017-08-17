module CatStrings where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Array (singleton, mapWithIndex)
import Control.Monad.Aff (Aff)

import DOM (DOM)
import DOM.HTML.Indexed.InputType (InputType(InputColor))
import DOM.Event.Types (Event)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS (style)
import Halogen.HTML.SVG as SVG
import CSS.SVG as SC
import Color (toHexString)

import Structures
import Utilities
import Algorithms

type State = Project

data Query a
  = NewZero a
  | UpdateCellColour Int Int Event a
  | UpdateCellName Int Int Event a
  | AttachCell Cell a
  | SetCache Boundary a
  | Identity a
  | ClearCache a
  | ClearDiagram a

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
    newZeroCell $
    { diagram: Nothing
    , signature: Signature 
      { cells: Cells []
      , sigma: Nothing
      , dimension: 0
      , id: 1
      }
    , cacheSourceTarget: Nothing
    , initialized: false
    , viewControls: { project: 0, slices: [] }
    , selectedCell: Nothing
    }
  
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
    AttachCell cell reply -> do
      project <- H.get
      case project.diagram of
        Nothing -> H.put $ project {diagram = Just $ liftCell cell}
        Just _ -> pure unit
      pure reply
    SetCache boundary reply -> do
      project <- H.get
      case project.cacheSourceTarget of
        Just (Tuple cachedBoundary cachedDiagram)
          | cachedBoundary /= boundary ->
            for_ project.diagram $ \diagram ->
              let Tuple source target =
                    if boundary == Source
                    then Tuple diagram cachedDiagram
                    else Tuple cachedDiagram diagram in
              for_ (newCell source target project) $ \project' ->
                H.put $ project'
                  { diagram = Nothing
                  , cacheSourceTarget = Nothing
                  }
        _ -> for_ project.diagram $ \diagram ->
          H.put $ project { cacheSourceTarget = Just (Tuple boundary diagram), diagram = Nothing }
      pure reply
    Identity reply -> do
      project <- H.get
      let diagram' = do
            Diagram diagram <- project.diagram
            pure $ Diagram
              { source: Just $ Diagram diagram
              , cells: []
              , dimension: diagram.dimension + 1
              }
      H.put $ project { diagram = diagram' }
      pure reply
    ClearCache reply -> do
      project <- H.get
      H.put $ project { cacheSourceTarget = Nothing }
      pure reply
    ClearDiagram reply -> do
      project <- H.get
      H.put $ project { diagram = Nothing }
      pure reply

render :: State -> H.ComponentHTML Query
render project =
  HH.div [classes ["app"]] $
    [ renderSignature project.signature
    , HH.div [classes ["diagram"]] $
      maybe [] (singleton <<< renderDiagram project.signature) project.diagram
    , HH.div [classes ["buttons"]] $
      [ HH.button [ HE.onClick (HE.input_ (SetCache Source)) ]
        [ HH.u_ [ HH.text "S" ],  HH.text "ource" ]
      , HH.button [ HE.onClick (HE.input_ (SetCache Target)) ]
        [ HH.u_ [ HH.text "T" ],  HH.text "arget" ]
      , HH.button [ HE.onClick (HE.input_ Identity) ]
        [ HH.u_ [ HH.text "I" ],  HH.text "dentity" ]
      , HH.button [ HE.onClick (HE.input_ ClearDiagram) ]
        [ HH.u_ [ HH.text "C" ],  HH.text "lear" ]
      ] <> renderCache project
    ]

renderCache :: Project -> Array (H.ComponentHTML Query)
renderCache project = maybeToArray do
  Tuple boundary diagram <- project.cacheSourceTarget
  pure $ HH.div [classes ["cache"]]
    [ HH.h6_ [ HH.text (show boundary) ]
    , HH.div [ HE.onClick (HE.input_ ClearCache), classes ["clear-cache"] ]
      [ HH.text "×" ]
    , renderDiagram project.signature diagram
    ]

renderSignature :: Signature -> H.ComponentHTML Query
renderSignature signature =
  HH.div [classes ["signature"]] $
    [ HH.h1_ [ HH.text "CatStrings.purs" ]
    , HH.div [classes ["sigmas"]] $ renderSigma signature
    ]

renderSigma :: Signature -> Array (H.ComponentHTML Query)
renderSigma signature =
  maybe [] renderSigma sigma <> 
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
    ] `if_` (signatureDimension signature == 0)
  ]
  where
    cellArray = signatureCellArray signature
    dimension = signatureDimension signature
    sigma = signatureSigma signature

renderSigmaCell :: Signature -> Int -> Int -> Cell -> H.ComponentHTML Query
renderSigmaCell signature dimension i cell =
  HH.div [classes ["sigma-cell"]] $
    case cell.source, cell.target of
      Just source, Just target | not cell.singleThumbnail ->
        [ HH.div [ classes ["sigma-cell-preview"], HE.onClick (HE.input_ (AttachCell cell)) ]
          [ renderDiagram signature source ]
        , HH.div [ classes ["sigma-cell-preview"], HE.onClick (HE.input_ (AttachCell cell)) ]
          [ renderDiagram signature target ]
        ]
      _, _ -> 
        [ HH.div
            [ classes ["sigma-cell-preview"]
            , HE.onClick (HE.input_ (AttachCell cell))
            ]
          [ renderCell signature cell ]
        ]
    <> [ HH.div [classes ["sigma-cell-body"]]
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

renderCell :: Signature -> Cell -> H.ComponentHTML Query
renderCell signature cell =
  renderDiagram signature $ liftCell cell

renderDiagram :: Signature -> Diagram -> H.ComponentHTML Query
renderDiagram signature (Diagram {source:Nothing,cells:[cell], dimension}) =
  SVG.svg [SVG.viewBox 0 0 100 100] $
    case getCell signature cell.id of
      Just cell ->
        [ SVG.circle
          [ SVG.cx 50
          , SVG.cy 50
          , SVG.r 15
          , style $ SC.fill cell.display.colour
          ]
        ]
      Nothing -> []
renderDiagram signature (Diagram {source:Just source,cells,dimension}) =
  blankDiagram
renderDiagram signature _ = blankDiagram

blankDiagram :: H.ComponentHTML Query
blankDiagram = SVG.svg [SVG.viewBox 0 0 0 0] []

module CatStrings where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (for_, sequence)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Array (singleton, mapWithIndex, null, head, mapMaybe)
import Data.Unfoldable as Unfoldable
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.HTML.Indexed.InputType (InputType(InputColor))
import DOM.Event.Types (Event)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Query.EventSource as ES
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS (style)
import Halogen.HTML.SVG as SVG
import CSS.SVG as SC
import CSS.Size (px)
import Color (Color, toHexString, rgba)

import Structures
import Utilities
import Algorithms

type State = Project

data Query a
  = Init a
  | NewZero a
  | UpdateCellColour Int Int Event a
  | UpdateCellName Int Int Event a
  | AttachCell Cell a
  | SetCache Boundary a
  | Identity a
  | ClearCache a
  | HandleKey Event (ES.SubscribeStatus -> a)
  | ClearDiagram a

data Message

csApp :: forall eff. H.Component HH.HTML Query Unit Message (Aff (avar :: AVAR, dom :: DOM | eff))
csApp =
  H.lifecycleComponent
    { initialState: const initial
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
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
  
  eval :: Query ~> H.ComponentDSL State Query Message (Aff (avar :: AVAR, dom :: DOM | eff))
  eval = case _ of
    Init reply -> do
      document <- H.liftEff $ DOM.document =<< DOM.window
      H.subscribe $
        ES.eventSource' (onKeyPress document) (Just <<< H.request <<< HandleKey)
      pure reply
    NewZero reply -> do
      H.modify newZeroCell
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
    HandleKey event reply -> do
      H.liftEff (eventKey event) >>= case _ of
        Just char
          | char == 's' -> eval (SetCache Source unit)
          | char == 't' -> eval (SetCache Target unit)
          | char == 'i' -> eval (Identity unit)
          | char == 'c' -> eval (ClearDiagram unit)
        _ -> pure unit
      pure $ reply H.Listening

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
renderDiagram signature (Diagram {source:Nothing,cells:[dCell], dimension}) =
  SVG.svg [SVG.viewBox 0 0 100 100] $
    case getCell signature dCell.id of
      Just cell -> [ dot 50 50 cell.display.colour ]
      Nothing   -> []

renderDiagram signature d@(Diagram {source:Just source,cells,dimension})
  | dimension == 1 = renderLineDiagram signature d
renderDiagram signature _ = blankDiagram

renderLineDiagram signature (Diagram {source:Just source,cells:[],dimension}) =
  SVG.svg [SVG.viewBox 0 0 100 100]
    [ straightLine 0 50 100 50 colour ]
  where colour = maybeColour $ head $ getColours signature source
renderLineDiagram signature (Diagram {source:Just source,cells: dCells,dimension}) =
  SVG.svg [SVG.viewBox 0 0 100 100] $
    cells >>= \(Tuple i cell) ->
      [ straightLine (i*100) 50 (i*100+50) 50 (sourceColour cell)
      , straightLine (i*100+50) 50 (i*100+100) 50  (targetColour cell)
      , dot 50 50 (cellColour cell)
      ]
  where
    cells :: Array (Tuple Int Cell)
    cells = mapMaybe sequence $ map (getCell signature <<< dCellID) <$> zipIndex dCells
    sourceColour cell = maybeColour $ head <<< getColours signature =<< cell.source
    targetColour cell = maybeColour $ head <<< getColours signature =<< cell.target
renderLineDiagram signature _ = blankDiagram

getColours :: Signature -> Diagram -> Array Color
getColours signature diagram =
  maybeColour <<< map cellColour <<< getCell signature <<< dCellID <$> diagramCells diagram

straightLine :: Int -> Int -> Int -> Int -> Color -> H.ComponentHTML Query
straightLine x0 y0 x1 y1 =
  line ("M" <> show x0 <> "," <> show y0 <> "L" <> show x1 <> "," <> show y1)

line :: String -> Color -> H.ComponentHTML Query
line d colour =
  SVG.path
    [ SVG.d d
    , style do
        SC.stroke colour
        SC.strokeWidth (px 10.0)
    ]

dot :: Int -> Int -> Color -> H.ComponentHTML Query
dot x y colour =
  SVG.circle
    [ SVG.cx x
    , SVG.cy y
    , SVG.r 10
    , style $ SC.fill colour
    ]

blankDiagram :: H.ComponentHTML Query
blankDiagram = SVG.svg [SVG.viewBox 0 0 0 0] []

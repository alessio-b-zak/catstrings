module CatStrings where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (for_, sequence)
import Data.String as Str
import Data.Tuple (Tuple(..), uncurry)
import Data.Array (concat, head, init, last, length, mapWithIndex, singleton, snoc, unzip, zip)
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.State (State, get, put, evalState)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (alert, document) as DOM
import DOM.HTML.Indexed.InputType (InputType(InputColor))
import DOM.Event.Types (Event)
import Halogen as H
import Halogen.Query.EventSource as ES
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS (style)
import Halogen.HTML.SVG as SVG
import CSS.SVG as SC
import CSS.Size (px)
import Color (Color, toHexString)

import Structures
import Utilities
import Algorithms

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

csApp :: forall eff. H.Component HH.HTML Query Unit Message (Aff (avar :: AVAR, dom :: DOM, alert :: ALERT | eff))
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
  
  initial :: Project
  initial =
    newZeroCell $
    { diagram: Nothing
    , signature: Signature 
      { cells: []
      , sigma: Nothing
      , dimension: 0
      , id: 1
      }
    , cacheSourceTarget: Nothing
    , initialized: false
    , viewControls: { project: 0, slices: [] }
    , selectedCell: Nothing
    }
  
  eval :: Query ~> H.ComponentDSL Project Query Message (Aff (avar :: AVAR, dom :: DOM, alert :: ALERT | eff))
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
        let signature' = updateSignature f dimension i project.signature
        H.put (project {signature = signature'})
      pure reply
    UpdateCellName dimension i ev reply -> do
      name <- H.liftEff $ inputValue ev
      unless (Str.null name) do
        project <- H.get
        let f cell = cell {name = name}
        let signature' = updateSignature f dimension i project.signature
        H.put (project {signature = signature'})
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
              forEither (newCell source target project) (H.liftEff <<< alertError) $ \project' ->
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

alertError :: forall eff. Error -> Eff (dom :: DOM, alert :: ALERT | eff) Unit
alertError error = DOM.alert (errorToString error) =<< DOM.window
  where
    errorToString DifferentSources = "The sources of the two chosen diagrams do not match"
    errorToString DifferentTargets = "The targets of the two chosen diagrams do not match"
    errorToString error = show error

render :: Project -> H.ComponentHTML Query
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
renderSigma signature@(Signature s) =
  maybe [] renderSigma s.sigma <> 
  [ HH.div [classes ["sigma"]] $
    [ HH.h2_ [ HH.text (show s.dimension <> "-cells") ]
    , HH.div [classes ["cells"]] $
      mapWithIndex (renderSigmaCell signature s.dimension) s.cells
    ] <>
    [ HH.div
        [ classes ["newcell"]
        , HE.onClick (HE.input_ NewZero)
        ]
      [ HH.text "New zerocell" ]
    ] `if_` (signatureDimension signature == 0)
  ]

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
  fromMaybe blankDiagram $ fromError $ getCell signature dCell.id <#>
    \cell -> SVG.svg [SVG.viewBox 0 0 10 10] [ dot 5 5 cell.display.colour ]

renderDiagram signature d@(Diagram {source:Just source,cells,dimension})
  | dimension == 1 = fromMaybe blankDiagram $ fromError $ renderLineDiagram signature d
  | otherwise      = fromMaybe blankDiagram $ fromError $ render2DDiagram signature d
renderDiagram signature _ = blankDiagram

render2DDiagram :: Signature -> Diagram -> OrError (H.ComponentHTML Query)
render2DDiagram signature diagram = do
  g@(GraphicalSlices source slices) <- drawDiagram signature diagram 
  let width = graphicalSlicesWidth g
  let height = length slices
  svgDiagram <- map concat $ sequence $ zipIndex (slicePairs g) <#> \(Tuple h (Tuple preSlice postSlice)) -> do
    let
      lines :: forall e. GraphicalSource e -> Boolean -> OrError (Array (H.ComponentHTML Query))
      lines gSlice post = do
        regions <- orError NoSource $ init gSlice.regions
        rightRegion <- orError NoSource $ last gSlice.regions
        let
          offset = if post then 5 else 0
          preOffset = if not post && h == 0 then -1000 else 0
          postOffset = if post && h + 1 == height then 1000 else 0
          y0 = h * 10 + offset + preOffset
          y1 = h * 10 + offset + postOffset + 5
          
          central :: State String (Array (H.ComponentHTML Query))
          central = map (uncurry (<>) <<< unzip) $ sequence $ zipIndex (zip regions (zip gSlice.cells gSlice.cellPositions)) <#>
            \(Tuple i (Tuple region (Tuple cell pos))) -> do
              prevPath <- get
              let
                x = pos * 10
                x' = if postSlice.rewriteKey <= i && i < postSlice.rewriteKey + postSlice.rewriteInputs
                      then postSlice.rewriteCoord * 10
                      else pos * 10
                Tuple x0 x1 = if post then Tuple x' x else Tuple x x'
                herePath = path post x0 y0 x1 y1
                nextPath = path (not post) x1 y1 x y0
              put nextPath
              pure $ Tuple
                (svgRegion ("M" <> herePath <> "L" <> prevPath <> "L" <> show x0 <> "," <> show y0) region.display.colour)
                (line' herePath cell.display.colour)
          
          right :: State String (Array (H.ComponentHTML Query))
          right = do
            prevPath <- get
            let x = width * 10 + 1000
            let herePath = path post x y0 x y1
            pure [svgRegion ("M" <> herePath <> "L" <> prevPath <> "L" <> show x <> "," <> show y0) rightRegion.display.colour]
          
        pure $ evalState (lift2 (flip (<>)) central right) (path post (-1000) y1 (-1000) y0)
        
    before <- lines preSlice false
    after <- lines postSlice true
    pure $ before <> after `snoc` dot (postSlice.rewriteCoord * 10) (h * 10 + 5) (postSlice.rewriteCell.display.colour)
  pure $ SVG.svg [SVG.viewBox 0 0 (10*width) (10*(max 1 height))] svgDiagram

renderLineDiagram :: Signature -> Diagram -> OrError (H.ComponentHTML Query)
renderLineDiagram signature (Diagram {source:Just source,cells:[],dimension}) = do
  colour <- orError NoSource <<< head =<< getColours signature source
  pure $ SVG.svg [SVG.viewBox 0 0 10 10]
          [ line true 0 5 10 5 colour ]
renderLineDiagram signature (Diagram {source:Just source,cells: dCells,dimension}) = do
  -- cells :: OrError [(Int, Cell)]
  cells <- sequence $ sequence <$> map (getCell signature <<< _.id) <$> zipIndex dCells
  lines <- map concat <$> sequence $ cells <#> \(Tuple i cell) -> do
    sourceColour <- onlyColour =<< orError NoSource cell.source
    targetColour <- onlyColour =<< orError NoTarget cell.target
    pure
      [ line true (i*10) 5 (i*10+5) 5 sourceColour
      , line true (i*10+5) 5 (i*10+10) 5 targetColour
      , dot 5 5 (cellColour cell)
      ]
  
  pure $ SVG.svg [SVG.viewBox 0 0 10 10] lines
  where
    onlyColour diagram = orError NoSource <<< head =<< getColours signature diagram
renderLineDiagram signature _ = pure blankDiagram

getColours :: Signature -> Diagram -> OrError (Array Color)
getColours signature diagram =
  sequence $ map cellColour <$> getCell signature <$> _.id <$> diagramCells diagram

line :: Boolean -> Int -> Int -> Int -> Int -> Color -> H.ComponentHTML Query
line post x0 y0 x1 y1 = svgLine ("M" <> path post x0 y0 x1 y1)

line' :: String -> Color -> H.ComponentHTML Query
line' str = svgLine ("M" <> str)

path :: Boolean -> Int -> Int -> Int -> Int -> String
path post x0 y0 x1 y1
  | x0 == x1  = show x0 <> "," <> show y0 <> "L" <> show x1 <> "," <> show y1
  | otherwise = show x0 <> "," <> show y0 <> "L" <> show x1 <> "," <> show y1

svgLine :: String -> Color -> H.ComponentHTML Query
svgLine d colour =
  SVG.path
    [ SVG.d d
    , style do
        SC.stroke colour
        SC.strokeWidth (px 1.0)
    ]

svgRegion :: String -> Color -> H.ComponentHTML Query
svgRegion d colour =
  SVG.path
    [ SVG.d d
    , SVG.shapeRendering "crispEdges"
    , style do
        SC.fill colour
    ]

dot :: Int -> Int -> Color -> H.ComponentHTML Query
dot x y colour =
  SVG.circle
    [ SVG.cx x
    , SVG.cy y
    , SVG.r 1
    , style $ SC.fill colour
    ]

blankDiagram :: H.ComponentHTML Query
blankDiagram = SVG.svg [SVG.viewBox 0 0 0 0] []

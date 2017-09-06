module CatStrings where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (for_, sequence)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Array (concat, cons, elem, foldr, head, init, last, length
                  , mapWithIndex, null, replicate, singleton, snoc, unzip, zip
                  , (!!))
import Data.Ord (abs)
import Data.BooleanEq ((⊕))
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.State (State, get, put, evalState)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (ALERT)
import DOM.HTML.Window (alert, document) as DOM
import DOM.HTML.Indexed.InputType (InputType(InputColor))
import DOM.Event.Types (Event, MouseEvent, mouseEventToEvent)
import DOM.Event.Event (stopPropagation)
import Halogen as H
import Halogen.Query.EventSource as ES
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS (style)
import Halogen.HTML.SVG as SVG
import CSS.SVG as SC
import CSS.Size (px)
import Color (Color, toHexString, hsla, toHSLA)
import Color.Scheme.X11 (darkgoldenrod)

import Structures
import Utilities
import Algorithms
import Rendering

data Query a
  = Init a
  | NewZero a
  | UpdateCellColour Int Int Event a
  | UpdateCellName Int Int Event a
  | AttachCell Cell MouseEvent a
  | PerformAttach (Tuple (Maybe Boundary) (Array Int)) a
  | SetCache Boundary a
  | Identity a
  | ClearCache a
  | HandleKey Event (ES.SubscribeStatus -> a)
  | ClearDiagram a
  | CancelAttach a

data Message

csApp :: forall eff. H.Component HH.HTML Query Unit Message
                      (Aff (avar :: AVAR, dom :: DOM, alert :: ALERT | eff))
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
    , matches: Nothing
    }
  
  eval :: Query ~> H.ComponentDSL Project Query Message
                    (Aff (avar :: AVAR, dom :: DOM, alert :: ALERT | eff))
  eval = case _ of
    Init reply -> do
      doc <- H.liftEff $ DOM.document =<< DOM.window
      H.subscribe $
        ES.eventSource' (onKeyPress doc) (Just <<< H.request <<< HandleKey)
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
        H.put $ project {signature = signature'}
      pure reply
    AttachCell cell ev reply -> do
      H.liftEff $ stopPropagation (mouseEventToEvent ev)
      project <- H.get
      let cellDiagram = liftCell cell
      case project.diagram of
        Nothing -> H.put $ project {diagram = Just cellDiagram}
        Just diagram
          | diagramDimension cellDiagram == 0 ->
              H.liftEff $ alert "Cannot attach a 0-cell"
          | diagramDimension diagram + 1 == diagramDimension cellDiagram -> do
              H.put $ project
                { matches = (match project.signature diagram <$> cell.source) <#> 
                      { id: cell.id
                      , source: []
                      , target: []
                      , rewrite: _
                      }
                }
          | diagramDimension diagram >= diagramDimension cellDiagram -> do
              let
                dimensionDelta = diagramDimension diagram - diagramDimension cellDiagram
                sig = project.signature
              H.put $ project
                { matches = do
                    sourceDiagram <-
                      foldr (<=<) pure (replicate dimensionDelta diagramSource) diagram
                    sourceDiagramSource <- head <=< fromError $ getSlices sig sourceDiagram
                    sourceDiagramTarget <- last <=< fromError $ getSlices sig sourceDiagram
                    cellDiagramSource <- head <=< fromError $ getSlices sig cellDiagram
                    cellDiagramTarget <- last <=< fromError $ getSlices sig cellDiagram
                    let sourceMatches = match sig sourceDiagramSource cellDiagramTarget
                    let targetMatches = match sig sourceDiagramTarget cellDiagramSource
                    pure
                      { id: cell.id
                      , source: sourceMatches
                      , target: targetMatches
                      , rewrite: []
                      }
                }
          | otherwise ->
              H.liftEff $ alert $ "Cannot attach/rewrite a "
                                  <> show (diagramDimension cellDiagram) <> "-cell to a "
                                  <> show (diagramDimension cellDiagram) <> "-cell"
            -- case cell.source, cell.target of
            --   Just source, Just target ->
            --     case match project.signature diagram (liftCell cell) of
            --       [] -> H.put $ project { matches = Nothing }
            --       matches ->
            --         H.put $ project { matches = Just { id: cell.id, locations: matches } }
      pure reply
    PerformAttach (Tuple Nothing coords) reply -> do
      project <- H.get
      let
        oeDiagram' = do
          diagram <- orError NoDiagram project.diagram
          cellID <- orError NoMatch $ project.matches <#> _.id
          cell <- getCell project.signature cellID
          src <- orError NoSource cell.source
          tgt <- orError NoTarget cell.target
          rewrite coords diagram src tgt
      forEither oeDiagram'
        (H.liftEff <<< alertError) $
        \diagram' -> H.put (project {diagram = Just diagram'})
      eval $ CancelAttach reply
    PerformAttach (Tuple (Just boundary) coords) reply -> do
      project <- H.get
      let
        oeDiagram' = do
          diagram <- orError NoDiagram project.diagram
          cellID <- orError NoMatch $ project.matches <#> _.id
          attachDiagram <- liftCell <$> getCell project.signature cellID
          attach project.signature boundary coords attachDiagram diagram
      forEither oeDiagram'
        (H.liftEff <<< alertError) $
        \diagram' -> H.put (project {diagram = Just diagram'})
      eval $ CancelAttach reply
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
              forEither (newCell source target project)
                (H.liftEff <<< alertError) $
                \project' ->
                  H.put $ project'
                    { diagram = Nothing
                    , cacheSourceTarget = Nothing
                    }
        _ -> for_ project.diagram $ \diagram ->
          H.put $ project
            { cacheSourceTarget = Just (Tuple boundary diagram)
            , diagram = Nothing
            }
      eval $ CancelAttach reply
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
      eval $ CancelAttach reply
    ClearCache reply -> do
      project <- H.get
      H.put $ project { cacheSourceTarget = Nothing }
      eval $ CancelAttach reply
    ClearDiagram reply -> do
      project <- H.get
      H.put $ project { diagram = Nothing }
      eval $ CancelAttach reply
    HandleKey event reply -> do
      H.liftEff (eventKey event) >>= case _ of
        Just char
          | char == 's' -> eval (SetCache Source unit)
          | char == 't' -> eval (SetCache Target unit)
          | char == 'i' -> eval (Identity unit)
          | char == 'c' -> eval (ClearDiagram unit)
          | char == 'n' -> eval (NewZero unit)
        _ -> pure unit
      pure $ reply H.Listening
    CancelAttach reply -> do
      H.modify (_ { matches = Nothing })
      pure reply

alertError :: forall eff. Error -> Eff (dom :: DOM, alert :: ALERT | eff) Unit
alertError error = alert (errorToString error)
  where
    errorToString DifferentSources =
      "The sources of the two chosen diagrams do not match"
    errorToString DifferentTargets =
      "The targets of the two chosen diagrams do not match"
    errorToString error = show error

alert :: forall eff. String -> Eff (dom :: DOM, alert :: ALERT | eff) Unit
alert error = DOM.alert error =<< DOM.window

render :: Project -> H.ComponentHTML Query
render project =
  HH.div [classes ["app"]] $
    [ renderSignature project.signature
    , HH.div [classes ["diagram"]] $
      maybe [] (singleton <<< renderDiagram project.matches project.signature) project.diagram
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
    , renderDiagram Nothing project.signature diagram
    ]

renderSignature :: Signature -> H.ComponentHTML Query
renderSignature sig =
  HH.div [classes ["signature"], HE.onClick (HE.input_ CancelAttach)] $
    [ HH.h1_ [ HH.text "CatStrings.purs" ]
    , HH.div [classes ["sigmas"]] $ renderSigma sig
    ]

renderSigma :: Signature -> Array (H.ComponentHTML Query)
renderSigma sig@(Signature s) =
  maybe [] renderSigma s.sigma <> 
  [ HH.div [classes ["sigma"]] $
    [ HH.h2_ [ HH.text (show s.dimension <> "-cells") ]
    , HH.div [classes ["cells"]] $
      mapWithIndex (renderSigmaCell sig s.dimension) s.cells
    ] <>
    [ HH.div
        [ classes ["newcell"]
        , HE.onClick (HE.input_ NewZero)
        ]
      [ HH.u_ [ HH.text "N" ],  HH.text "ew 0-cell" ]
    ] ?? (s.dimension == 0)
  ]

renderSigmaCell :: Signature -> Int -> Int -> Cell -> H.ComponentHTML Query
renderSigmaCell sig dimension i cell =
  HH.div [classes ["sigma-cell"]] $
    case cell.source, cell.target of
      Just source, Just target | not cell.singleThumbnail ->
        [ HH.div
            [ classes ["sigma-cell-preview"]
            , HE.onClick (HE.input (AttachCell cell))
            ]
          [ renderDiagram Nothing sig source ]
        , HH.div
            [ classes ["sigma-cell-preview"]
            , HE.onClick (HE.input (AttachCell cell))
            ]
          [ renderDiagram Nothing sig target ]
        ]
      _, _ -> 
        [ HH.div
            [ classes ["sigma-cell-preview"]
            , HE.onClick (HE.input (AttachCell cell))
            ]
          [ renderDiagram Nothing sig $ liftCell cell ]
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

renderDiagram :: Maybe Matches -> Signature -> Diagram -> H.ComponentHTML Query
renderDiagram matches sig (Diagram {source:Nothing,cells:[dCell], dimension}) =
  fromMaybe blankDiagram $ fromError $ do
    cell <- getCell sig dCell.id
    let validMatch = [] `elem` fromMaybe [] (matches <#> _.rewrite)
    let overlay = [ matchOverlay Nothing [] "M2,2v6h6v-6Z" ] ?? validMatch
    pure $ SVG.svg [SVG.viewBox 0 0 10 10] $
      [ dot 5 5 cell.display.colour ] <> overlay

renderDiagram matches sig d@(Diagram {source:Just source,cells,dimension})
  | dimension == 1 = catch blankDiagram $ renderLineDiagram matches sig d
  | otherwise      = catch blankDiagram $ render2DDiagram matches sig d
renderDiagram _ _ _  = blankDiagram

render2DDiagram :: Maybe Matches -> Signature -> Diagram
                -> OrError (H.ComponentHTML Query)
render2DDiagram mMatches sig diagram = do
  g@(GraphicalSlices source slices) <- drawDiagram sig diagram 
  let width = graphicalSlicesWidth g
  let height = length slices
  svgDiagram <- map concat $ sequence $ zipIndex (slicePairs g) <#>
    \(Tuple h (Tuple preSlice postSlice)) -> do
      let
        lines :: forall e. GraphicalSource e -> Boolean
              -> OrError (Tuple (Array (H.ComponentHTML Query))
                                (Array (H.ComponentHTML Query)))
        lines gSlice post = do
          regions <- orError NoSource $ init gSlice.regions
          rightRegion <- orError NoSource $ last gSlice.regions
          let
            offset = if post then 5 else 0
            preOffset = if not post && h == 0 then -1000 else 0
            postOffset = if post && h + 1 == height then 1000 else 0
            y0 = h * 10 + offset + preOffset
            y1 = h * 10 + offset + postOffset + 5
            irccps = zipIndex (zip regions (zip gSlice.cells gSlice.cellPositions))
            
            central :: State String (Tuple (Array (H.ComponentHTML Query))
                                           (Array (H.ComponentHTML Query)))
            central = map unzip $ sequence $ irccps <#>
              \(Tuple i (Tuple region (Tuple cell pos))) -> do
                prevPath <- get
                let
                  key = postSlice.rewriteKey
                  key' = key + if post
                                then postSlice.rewriteOutputs
                                else postSlice.rewriteInputs
                  x = pos * 5
                  x' = if key <= i && i < key'
                        then postSlice.rewriteCoord * 5
                        else pos * 5
                  Tuple x0 x1 = if post then Tuple x' x else Tuple x x'
                  herePath = path post x0 y0 x1 y1
                  nextPath = path (not post) x1 y1 x0 y0
                  regionPath = "M" <> herePath <> "L" <> prevPath <> "Z"
                put nextPath
                pure $ Tuple
                  (svgRegion regionPath region.display.colour)
                  (line' herePath cell.display.colour)
            
            right :: State String (H.ComponentHTML Query)
            right = do
              prevPath <- get
              let
                x = width * 5 + 1000
                herePath = path post x y0 x y1
                regionPath = "M" <> herePath <> "L" <> prevPath <> "Z"
              pure $ svgRegion regionPath rightRegion.display.colour
            
          pure $ evalState (do
              Tuple regions lines <- central
              region' <- right
              pure $ Tuple (regions `snoc` region') lines
          ) (path post (-1000) y1 (-1000) y0)
        
        dotSVG = dot (postSlice.rewriteCoord * 5) (h * 10 + 5)
                     (postSlice.rewriteCell.display.colour)
      
      Tuple beforeRegions beforeLines <- lines preSlice false
      Tuple afterRegions afterLines <- lines postSlice true
      pure $ beforeRegions <> afterRegions <> beforeLines <> afterLines `snoc` dotSVG
  let svgDiagram' = if null svgDiagram
        then let
            positions = -1000 `cons` map (5*_) source.cellPositions `snoc` (5*width + 1000)
            regions = zip source.regions (pairs positions) <#>
              \(Tuple region (Tuple start end)) ->
                let left = path true start (-1000) start 1010
                    right = path true end 1010 end (-1000)
                in svgRegion ("M"<>left<>"L"<>right<>"Z") region.display.colour
            lines = zip source.cells source.cellPositions <#> \(Tuple cell x) ->
              line true (5*x) (-1000) (5*x) 1010 cell.display.colour
            in regions <> lines
        else svgDiagram
  let
    highlighting = fromMaybe [] do
      matches <- mMatches
      let
        leftMatch = [matchOverlay (Just Source) []
            ("M2,-1000h-1002v"<>show (2000+height*10)<>"h1002Z")]
          ?? ([] `elem` matches.source)
        rightMatch = [matchOverlay (Just Target) []
            ("M"<>show(width*5-2)<>",-1000h1002v"<>show(2000+height*10)<>"h-1002Z")]
          ?? ([] `elem` matches.target)
        topMatches = matches.source >>= case _ of
          [wire] ->
            let x = fromMaybe 0 $ source.cellPositions !! wire
            in [matchOverlay (Just Source) [wire]
              ("M"<>show(x*5-2)<>",0l2,2l2,-2v-1000h-4Z")]
          _ -> []
        bottomMatches = matches.target >>= case _ of
          [wire] ->
            let x = fromMaybe 0 $ (topSlice g).cellPositions !! wire
            in [matchOverlay (Just Target) [wire]
              ("M"<>show(x*5-2)<>","<>show(height*10)<>"l2,-2l2,2v1000h-4Z")]
          _ -> []
        
      pure $ leftMatch <> rightMatch <> topMatches <> bottomMatches
  pure $ SVG.svg [SVG.viewBox 0 0 (5*width) (10*(max 1 height))] $ svgDiagram' <> highlighting

renderLineDiagram :: Maybe Matches -> Signature -> Diagram
                  -> OrError (H.ComponentHTML Query)
renderLineDiagram mMatches sig (Diagram {source:Just source,cells:[],dimension}) = do
  colour <- orError NoSource <<< head =<< getColours sig source
  let
    highlighting = fromMaybe [] do
      matches <- mMatches
      let
        leftMatch = [matchOverlay (Just Source) [] "M0,3l2,2l-2,2h-1000v-4Z"]
                    ?? ([] `elem` matches.source)
        midMatch = [matchOverlay Nothing [0] "M2,2v6h6v-6Z"] ?? ([0] `elem` matches.rewrite)
        rightMatch = [matchOverlay (Just Target) [] "M10,3l-2,2l2,2h1000v-4Z"]
                     ?? ([] `elem` matches.target)
      pure $ leftMatch <> midMatch <> rightMatch
      
  pure $ SVG.svg [SVG.viewBox 0 0 10 10] $
    [ line true (-1000) 5 1010 5 colour
    ] <> highlighting

renderLineDiagram mMatches sig (Diagram {source:Just source,cells: dCells,dimension}) = do
  -- pure cells :: OrError [(Int, Cell)]
  cells <- sequence $ sequence <$> map (getCell sig <<< _.id) <$> zipIndex dCells
  lines <- map concat <$> sequence $ cells <#> \(Tuple i cell) -> do
    sourceColour <- onlyColour =<< orError NoSource cell.source
    targetColour <- onlyColour =<< orError NoTarget cell.target
    let left = if i == 0 then -1000 else i*10
        mid = i * 10 + 5
        right = if i + 1 == length cells then i*10 + 1010 else i * 10 + 10
    pure
      [ line true left 5 mid 5 sourceColour
      , line true mid 5 right 5 targetColour
      , dot mid 5 (cellColour cell)
      ]
  
  pure $ SVG.svg [SVG.viewBox 0 0 (10*length cells) 10] $ lines <> highlighting
  where
    onlyColour diagram = orError NoSource <<< head =<< getColours sig diagram
    highlighting :: Array (H.ComponentHTML Query)
    highlighting = fromMaybe [] do
      matches <- mMatches
      let
        width = show $ 10 * length dCells
        h = show $ fromMaybe 6 do
          cell <- fromError $ getCell sig matches.id
          source <- cell.source
          pure $ length (diagramCells source) * 10 - 4
        leftMatch = [matchOverlay (Just Source) [] "M0,3l2,2l-2,2h-1000v-4Z"]
                    ?? ([] `elem` matches.source)
        midMatches = matches.rewrite >>= case _ of
          [x] -> [matchOverlay Nothing [x] ("M"<>show (x*10+2)<>",2v6h"<>h<>"v-6Z")]
          _   -> []
        rightMatch = [matchOverlay (Just Target) [] ("M"<>width<>",3l-2,2l2,2h1000v-4Z")]
                     ?? ([] `elem` matches.target)
      Just $ leftMatch <> midMatches <> rightMatch

renderLineDiagram _ _ _ = pure blankDiagram

getColours :: Signature -> Diagram -> OrError (Array Color)
getColours signature diagram =
  sequence $ map cellColour <$> getCell signature <$> _.id <$> diagramCells diagram

line :: Boolean -> Int -> Int -> Int -> Int -> Color -> H.ComponentHTML Query
line post x0 y0 x1 y1 = svgLine ("M" <> path post x0 y0 x1 y1)

line' :: String -> Color -> H.ComponentHTML Query
line' str = svgLine ("M" <> str)

path :: Boolean -> Int -> Int -> Int -> Int -> String
path post x0 y0 x1 y1
  | x0 == x1 || y0 == y1 = show x0 <> "," <> show y0 <> "L" <> show x1 <> "," <> show y1
  | post = show x0 <> "," <> show y0 <> "A"<>show (abs (x0-x1))<>",5,0,0,"<>flag<>","
      <> show x1 <> "," <> show y' <> "V" <> show y1
    where
      y' = if y1 > y0 then y0+5 else y0-5
      flag = if x0 > x1 ⊕ y0 > y1 then "0" else "1"
  | otherwise = show x0 <> "," <> show y0 <> "V" <> show y'
      <> "A"<>show (abs (x0-x1))<>",5,0,0,"<>flag<>"," <> show x1 <> "," <> show y1
    where
      y' = if y1 > y0 then y1-5 else y1+5
      flag = if x0 < x1 ⊕ y0 > y1 then "0" else "1"

svgLine :: String -> Color -> H.ComponentHTML Query
svgLine d colour =
  SVG.path
    [ SVG.d d
    , style do
        SC.fill (hsla 0.0 0.0 0.0 0.0)
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

matchOverlay :: Maybe Boundary -> Array Int -> String -> H.ComponentHTML Query
matchOverlay bound coords d = svgOverlay (Tuple bound coords) d darkgoldenrod

svgOverlay :: Tuple (Maybe Boundary) (Array Int) -> String -> Color -> H.ComponentHTML Query
svgOverlay target d colour =
  let {h,s,l,a} = toHSLA colour in
  SVG.path
    [ SVG.d d
    , HE.onClick (HE.input_ (PerformAttach target))
    , style do
        SC.fill (hsla h s l (a/2.0))
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

module Algorithms where

import Prelude
import Data.Foldable (and)
import Data.Unfoldable (fromMaybe) as Unfoldable
import Data.Tuple (Tuple(..))
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Control.MonadZero (guard)
import Data.Array ( cons, drop, foldl, init, last, length, modifyAt
                  , modifyAtIndices, null, range, replicate, snoc, take
                  , zipWith, (!!))
import Data.Traversable (sequence, find)
import Color (Color, black, white)
import Color.Scheme.X11 (blue, gray, green, orange, purple, red, teal, pink)
import Utilities
import Structures

getNumInputs :: Signature -> DiagramCell -> OrError Int
getNumInputs signature diagramCell = getNumInputs' signature diagramCell.id

getNumInputs' :: Signature -> CellID -> OrError Int
getNumInputs' signature cellId = do
    cell <- getCell signature cellId
    source <- orError NoSource cell.source
    pure $ length $ diagramCells source
  
getNumOutputs :: Signature -> DiagramCell -> OrError Int
getNumOutputs signature diagramCell = getNumOutputs' signature diagramCell.id

getNumOutputs' :: Signature -> CellID -> OrError Int
getNumOutputs' signature cellId = do
    cell <- getCell signature cellId
    target <- orError NoTarget cell.target
    pure $ length $ diagramCells target

identityDiag :: Diagram -> Diagram
identityDiag diag = Diagram newDiag
  where
     newDiag = { source: Just diag
               , cells: []
               , dimension: diagramDimension diag + 1
               }

newZeroCell :: Project -> Project
newZeroCell project = fromMaybe project $ fromError $ addCell Nothing Nothing project

newCell :: Diagram -> Diagram -> Project -> OrError Project
newCell source target project = do
  let slices = either (const []) id <<< getSlices project.signature
  guard' DifferentSources $ diagramSource source == diagramSource target
  guard' DifferentTargets $ last (slices source) == last (slices target)
  addCell (Just source) (Just target) project

addCell :: Maybe Diagram -> Maybe Diagram -> Project -> OrError Project
addCell source target project = do
  guard' BadDimension case source, target of
    Just s, Just t -> diagramDimension s == diagramDimension t
    Nothing, Nothing -> true
    _, _ -> false
  let dimension = 1 + maybe (-1) diagramDimension source
  let
    addToSignature :: Signature -> Signature
    addToSignature (Signature s@{sigma: Just sigma})
      | s.dimension > dimension =
          Signature (s { sigma = Just $ addToSignature sigma })
    addToSignature (Signature s@{cells, id: nextId})
      | s.dimension < dimension =
          addToSignature $ Signature
            { sigma: Just (Signature s)
            , cells: []
            , dimension: s.dimension + 1
            , id: 1
            }
      | otherwise =
          Signature (s { cells = cells `snoc` cell, id = nextId+1 })
          where
            cell =
              { source
              , target
              , id: CellID dimension nextId
              , invertible: false
              , name: show dimension <> "-cell " <> show nextId
              , singleThumbnail: dimension < 3
              , display: {colour: pickColour dimension nextId, rate: 1}
              }
  pure $ project { signature = addToSignature project.signature}

pickColour :: Int -> Int -> Color
pickColour dimension nextId =
  fromMaybe teal $ (_ !! nextId `mod` 3) =<< colours !! dimension `mod` 3
  where
    colours =
      [ [ red, green, black ]
      , [ orange, blue, pink ]
      , [ white, purple, gray ]
      ]

getCell :: Signature -> CellID -> OrError Cell
getCell (Signature s) cid@(CellID dim i)
  | s.dimension == dim = orError (BadCell cid) $ find (\cell -> cell.id == cid) s.cells
  | otherwise = flip getCell cid =<< orError (BadCell cid) s.sigma

updateSignature :: (Cell -> Cell) -> Int -> Int -> Signature -> Signature
updateSignature f dim i (Signature s)
  | s.dimension == dim = Signature s { cells = modifyAtIndices [i] f s.cells }
  | otherwise = Signature s { sigma = updateSignature f dim i <$> s.sigma }

rewrite :: Array Int -> Diagram -> Diagram -> Diagram -> OrError Diagram
rewrite coords baseDiagram sourceRewrite targetRewrite =
    addDiagram coords targetRewrite =<< removeDiagram coords baseDiagram sourceRewrite

addDiagram :: Array Int -> Diagram -> ReplacementDiagram -> OrError Diagram
addDiagram coords targetRewrite { diagram, offset }  =
    insertDiagram coords diagram $ alterCoords offset targetRewrite

removeDiagram :: Array Int -> Diagram -> Diagram -> OrError ReplacementDiagram
removeDiagram coords baseDiagram@(Diagram record) rewriteSource = do
    { before, after } <- orError BadKey $ splitAt (lastOrZero coords) (diagramCells baseDiagram)
    let offset = fromMaybe [] (init coords)
    let removedRewrite = drop (length $ diagramCells rewriteSource) after
    let finalDiagramCells = before <> removedRewrite
    pure { diagram: Diagram (record { cells = finalDiagramCells }), offset: offset }

insertDiagram :: Array Int -> Diagram -> Diagram -> OrError Diagram
insertDiagram coords baseDiagram@(Diagram record) targetRewrite = do
    { before, after } <- orError BadKey $ splitAt (lastOrZero coords) (diagramCells baseDiagram)
    let targetCells = diagramCells targetRewrite
    pure $ Diagram(record { cells = before <> targetCells <> after })

alterCoords :: Array Int -> Diagram -> Diagram
alterCoords offset (Diagram record) = Diagram $ record { cells = cells' }
  where
    cells' = map (alterCellCoords offset) $ record.cells

alterCellCoords :: Array Int -> DiagramCell -> DiagramCell
alterCellCoords offset cell = cell { key = zipWith (+) offset cell.key }

dimensionOfCell :: Cell -> Int
dimensionOfCell {source: Nothing} = 0
dimensionOfCell {source: Just source} = 1 + diagramDimension source

liftCell :: Cell -> Diagram
liftCell cell = Diagram
  { source: cell.source
  , cells: [{id: cell.id, key: key, box: Nothing}]
  , dimension: dimension
  }
  where
    dimension = dimensionOfCell cell
    key = replicate (max 0 (dimension-1)) 0

type Coords = Array Int

attach :: Signature -> Boundary -> Coords -> Diagram -> Diagram -> OrError Diagram
attach sig boundary coords attachDiagram baseDiagram  = do
    guard' BadDimension $ embeddingLevel >= 0
    if embeddingLevel == 0
      then appendCells sig baseDiagram attachDiagram boundary coords
      else appendLowerCells sig baseDiagram attachDiagram boundary coords
  where
    baseDimension = diagramDimension baseDiagram
    attachDimension  = diagramDimension attachDiagram
    embeddingLevel = baseDimension - attachDimension

appendCells :: Signature -> Diagram -> Diagram -> Boundary -> Coords -> OrError Diagram
appendCells sig baseDiagram attachDiagram Source coords = do
  appendedDiagram <- insertDiagram [0] baseDiagram $ alterCoords coords attachDiagram
  source <- orError NoSource $ diagramSource attachDiagram
  let position = lastOrZero coords
  let size = length $ diagramCells $ fromMaybe source $ last <=< fromError $ getSlices sig attachDiagram
  pure $ changeSource position size source appendedDiagram
appendCells sig baseDiagram attachDiagram Target coords =
    insertDiagram [length $ diagramCells baseDiagram] baseDiagram $
        alterCoords coords attachDiagram

updateKey :: forall f. Functor f => (Array Int -> f (Array Int)) -> DiagramCell -> f DiagramCell
updateKey fn diagramCell = (diagramCell { key = _}) <$> fn diagramCell.key

appendLowerCells :: Signature -> Diagram -> Diagram -> Boundary -> Coords -> OrError Diagram
appendLowerCells sig baseDiagram@(Diagram record) attachDiagram Source coords = do
  let attachDimension = diagramDimension attachDiagram - 1
  newCells <- sequence $ map (updateKey (orError BadDimension <<< modifyAt attachDimension (_+1))) record.cells
  source <- orError NoSource $ diagramSource baseDiagram
  diagramSource' <- attach sig Source coords attachDiagram source
  pure $ Diagram (record { source = Just diagramSource', cells = newCells })
appendLowerCells sig baseDiagram@(Diagram record) attachDiagram Target coords = do
  source <- orError NoSource $ diagramSource baseDiagram
  diagramSource' <- attach sig Target coords attachDiagram source
  pure $ Diagram ( record { source = Just diagramSource' })

changeSource :: Int -> Int -> Diagram -> Diagram -> Diagram
changeSource pos size newSource baseDiagram@(Diagram record) =
    Diagram record {source = source'}
  where
    {left, right} = splitInThree pos size (maybe [] diagramCells record.source)
    source' = do
      Diagram source <- record.source
      pure $ Diagram source {cells = left <> diagramCells newSource <> right}

slice :: Signature -> Diagram -> Int -> OrError Diagram
slice signature diagram height =
  foldl (applyRewrite signature) source rewrites
  where
    source = orError NoSource $ diagramSource diagram
    rewrites = take height $ diagramCells diagram  

applyRewrite :: Signature -> OrError Diagram -> DiagramCell -> OrError Diagram
applyRewrite signature thisSlice diagramCell = do
    sliceDiagram <- thisSlice
    let coords = diagramCell.key
    cell <- getCell signature diagramCell.id
    cSource <- orError NoSource cell.source
    cTarget <- orError NoTarget cell.target
    rewrite coords sliceDiagram cSource cTarget

getSlices :: Signature -> Diagram -> OrError (Array Diagram)
getSlices signature diagram =
    let sliceNum = length $ diagramCells diagram
        sliceHeights = range 0 sliceNum
     in sequence $ map (slice signature diagram) sliceHeights

match :: Signature -> Diagram -> Diagram -> Array (Array Int)
match signature baseDiagram matchDiagram
  | diagramDimension baseDiagram == 0 =
      do
        -- two zero cells match
        --if baseDiagram == matchDiagram then [[]] else []
        guard $ baseDiagram == matchDiagram
        pure []
  | diagramDimension baseDiagram == diagramDimension matchDiagram
    && null (diagramCells baseDiagram) && null (diagramCells matchDiagram) =
      case diagramSource baseDiagram, diagramSource matchDiagram of
        Just baseSource, Just matchSource ->
          (cons 0) <$> match signature baseSource matchSource
        _, _ -> []
  | otherwise =
      catch [] $ zipIndex <$> getSlices signature baseDiagram <#> \iSlices -> do
        Tuple i thisSlice <- iSlices
        
        source <- Unfoldable.fromMaybe $ diagramSource matchDiagram
        
        submatch <- match signature thisSlice source
        
        
        let baseCells  = diagramCells baseDiagram
            matchCells = diagramCells matchDiagram
        let aboveSlice = drop i baseCells
        let baseCellsToCheck = take (length matchCells) aboveSlice
        guard $ length matchCells == length baseCellsToCheck
        guard $ and $
          zipWith eqDiagramCell (map (alterCellCoords submatch) matchCells) baseCellsToCheck
      
        pure $ i `cons` submatch

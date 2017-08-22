module Algorithms where

import Prelude
import Data.Foldable (and)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Color (black)
import Control.MonadZero (guard)
import Data.Array (drop, init, length, modifyAt, snoc, zipWith, replicate, take, foldl, cons, range, zip, last, head, (!!), (..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Traversable (sequence, find)
import Control.MonadZero (guard)
import Color (Color, black, white)
import Color.Scheme.X11
import Utilities
import Structures

getNumInputs :: Signature -> DiagramCell -> Maybe Int
getNumInputs signature diagramCell = do
    cell <- (getCell signature diagramCell.id)
    source <- cell.source
    pure $ length $ diagramCells source
  
getNumOutputs :: Signature -> DiagramCell -> Maybe Int
getNumOutputs signature diagramCell = do
    cell <- (getCell signature diagramCell.id)
    target <- cell.target
    pure $ length $ diagramCells target

identityDiag :: Diagram -> Diagram
identityDiag diag = Diagram newDiag
  where
     newDiag = { source: Just diag
               , cells: []
               , dimension: diagramDimension diag + 1
               }

newZeroCell :: Project -> Project
newZeroCell project = fromMaybe project $ addCell Nothing Nothing project

newCell :: Diagram -> Diagram -> Project -> Maybe Project
newCell source target project = addCell (Just source) (Just target) project

addCell :: Maybe Diagram -> Maybe Diagram -> Project -> Maybe Project
addCell source target project = do
  guard case source, target of
    Just s, Just t -> diagramDimension s == diagramDimension t
    Nothing, Nothing -> true
    _, _ -> false
  let dimension = 1 + maybe (-1) diagramDimension source
  let
    addToSignature :: Signature -> Signature
    addToSignature (Signature s@{sigma: Just sigma})
      | s.dimension > dimension =
          Signature (s { sigma = Just $ addToSignature sigma })
    addToSignature (Signature s@{cells: (Cells cells), id: nextId})
      | s.dimension < dimension =
          addToSignature $ Signature
            { sigma: Just (Signature s)
            , cells: Cells []
            , dimension: s.dimension + 1
            , id: 1
            }
      | otherwise =
          Signature (s { cells = Cells (cells `snoc` cell), id = nextId+1 })
          where
            cell =
              { source
              , target
              , id: CellID dimension nextId
              , invertible: false
              , name: show dimension <> "-cell " <> show nextId
              , singleThumbnail: dimension < 2
              , display: {colour: pickColour dimension nextId, rate: 1}
              }
  pure $ project { signature = addToSignature project.signature}

pickColour :: Int -> Int -> Color
pickColour dimension nextId =
  maybeColour $ (_ !! nextId `mod` 3) =<< colours !! dimension `mod` 3
  where
    colours =
      [ [ red, green, black ]
      , [ orange, blue, white ]
      , [ yellow, purple, gray ]
      ]

getCell :: Signature -> CellID -> Maybe Cell
getCell (Signature s) cid@(CellID dim i)
  | s.dimension == dim = find (\cell -> cell.id == cid) $ getCells s.cells
  | otherwise = flip getCell cid =<< s.sigma

updateSignature :: (Cell -> Cell) -> Int -> Int -> Signature -> Maybe Signature
updateSignature f dim i sig
  | signatureDimension sig == dim =
      signatureCellArray' sig <$> modifyAt i f (signatureCellArray sig)
  | otherwise =
      signatureSigma' sig <$> sequence (updateSignature f dim i <$> signatureSigma sig)

rewrite :: Array Int -> Diagram -> Diagram -> Diagram -> Maybe Diagram
rewrite coords baseDiagram sourceRewrite targetRewrite =
    addDiagram coords targetRewrite =<< removeDiagram coords baseDiagram sourceRewrite

addDiagram :: Array Int -> Diagram ->  ReplacementDiagram -> Maybe Diagram
addDiagram coords targetRewrite { diagram, offset }  =
    insertDiagram coords diagram $ alterCoords offset targetRewrite

removeDiagram :: Array Int -> Diagram -> Diagram -> Maybe ReplacementDiagram
removeDiagram coords baseDiagram@(Diagram record) rewriteSource = do
    { before: preDiagram, after: postDiagram } <- splitAt (lastOrZero coords) (diagramCells baseDiagram)
    let offset = fromMaybe [] (init coords)
        removedRewrite = drop (length $ diagramCells rewriteSource) postDiagram
        finalDiagramCells = preDiagram <> removedRewrite
    pure { diagram: Diagram ( record { cells = finalDiagramCells }), offset: offset }

insertDiagram :: Array Int -> Diagram -> Diagram -> Maybe Diagram
insertDiagram coords baseDiagram@(Diagram record) targetRewrite = do
    { before: preDiagram, after: postDiagram } <- splitAt (lastOrZero coords) (diagramCells baseDiagram)
    let targetCells = diagramCells targetRewrite
    pure $ Diagram(record { cells = preDiagram <> targetCells <> postDiagram })

alterCoords :: Array Int -> Diagram -> Diagram
alterCoords offset (Diagram record) = Diagram $ record 
  { cells = map (alterCellCoords offset) $ record.cells }

alterCellCoords :: Array Int -> DiagramCell -> DiagramCell
alterCellCoords offset cell =
    cell { key = zipWith (+) offset cell.key }

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

attach :: Boundary -> Coords -> Diagram -> Diagram -> Maybe Diagram
attach boundary coords attachDiagram baseDiagram  = do
    guard $ embeddingLevel >= 0
    let appendFunction = if embeddingLevel == 0 then appendCells else appendLowerCells
    appendFunction baseDiagram attachDiagram boundary coords
  where
    baseDimension = diagramDimension baseDiagram
    attachDimension  = diagramDimension attachDiagram
    embeddingLevel = baseDimension - attachDimension

appendCells :: Diagram -> Diagram -> Boundary -> Coords -> Maybe Diagram
appendCells baseDiagram attachDiagram Source coords =
    let appendedDiagram = insertDiagram [0] baseDiagram $ alterCoords coords attachDiagram
     in changeSource <$> diagramSource attachDiagram <*> appendedDiagram
appendCells baseDiagram attachDiagram Target coords =
    insertDiagram [length $ diagramCells attachDiagram] baseDiagram $
        alterCoords coords attachDiagram

updateKey :: (Array Int -> Maybe (Array Int)) -> DiagramCell -> Maybe DiagramCell
updateKey fn diagramCell = (diagramCell { key = _}) <$> fn diagramCell.key

appendLowerCells :: Diagram -> Diagram -> Boundary -> Coords -> Maybe Diagram
appendLowerCells baseDiagram@(Diagram record) attachDiagram Source coords = do
    let attachDimension = diagramDimension attachDiagram - 1
    newCells <- sequence $ map (updateKey (modifyAt attachDimension (_+1))) record.cells
    diagramSource' <- attach Source coords attachDiagram =<< diagramSource baseDiagram
    pure $ Diagram (record { source = Just diagramSource', cells = newCells })
appendLowerCells baseDiagram@(Diagram record) attachDiagram Target coords = do
     diagramSource' <- attach Target coords attachDiagram =<< diagramSource baseDiagram
     pure $ Diagram ( record { source = Just diagramSource' })

changeSource :: Diagram -> Diagram -> Diagram
changeSource newSource baseDiagram@(Diagram record) =
    Diagram $ record { source = Just newSource }

type Height = Int

slice :: Signature -> Diagram -> Height -> Maybe Diagram
slice signature diagram height =
    let rewrites    = take height $ diagramCells diagram
        applyRewrite :: Maybe Diagram -> DiagramCell -> Maybe Diagram
        applyRewrite thisSlice diagramCell = do
            sliceDiagram <- thisSlice
            let coords = diagramCell.key
            cell <- getCell signature diagramCell.id
            cSource <- cell.source
            cTarget <- cell.target
            rewrite coords sliceDiagram cSource cTarget
    in foldl applyRewrite (diagramSource diagram) rewrites

getSlices :: Diagram -> Maybe (Array Diagram)
getSlices diagram =
    let sliceNum = length $ diagramCells diagram
        sliceHeight = range 0 $ sliceNum
        diagramList = replicate (sliceNum+ 1) diagram
     in sequence $ zipWith slice diagramList sliceHeight 

match :: Signature -> Diagram -> Diagram -> Array (Array Int)
match signature baseDiagram matchDiagram
  | diagramDimension baseDiagram == 0 =
      do
        -- two zero cells match
        --if baseDiagram == matchDiagram then [[]] else []
        guard $ baseDiagram == matchDiagram
        pure []
  | otherwise =
      do
        Tuple i maybeSlice <- mapWithIndex (\num _ -> Tuple num (slice signature baseDiagram num)) (diagramCells baseDiagram)
        -- match _ _ :: [[Int]]
        submatch <- fromMaybe [] do
          thisSlice <- maybeSlice
          source <- diagramSource matchDiagram
          pure $ match signature thisSlice source
        -- submatch :: [Int]
        let coords = i `cons` submatch
        let baseCells  = diagramCells baseDiagram
            matchCells = diagramCells matchDiagram
        let aboveSlice = drop i baseCells
        let baseCellsToCheck = take (length matchCells) aboveSlice
        guard $ and $ zipWith eqDiagramCell (map (alterCellCoords coords) matchCells) baseCellsToCheck
        
        -- ensure all rewrites above are same
        -- guard _
      
        pure coords

type GraphicalSlice =
  { cells :: Array Cell
  , rewrite :: Maybe { rewriteCell :: Cell, rewriteCoord :: Int }
  , cellPositions :: Array Int
  }

--takes a list of slices and a list of rewrites and constructs a graphical diagram
coreLoop :: Array Diagram -> Array DiagramCell -> Array GraphicalSlice
coreLoop diagrams dCells =
    foldl initialPassover [] $ zip diagrams (cons Nothing (Just <$> dCells))

--takes a slice and a rewrite and makes a graphical slice.
fixOffsets :: Array GraphicalSlice -> Tuple Diagram (Maybe DiagramCell) -> GraphicalSlice
fixOffsets graphicalSlices (Tuple diagram Nothing) = 
    { cells: map (\dCell -> dCell.cell) $ diagramCells diagram
    , rewrite: Nothing 
    , cellPositions: map (2 * _) $ range 0 (length (diagramCells diagram) -  1) 
    }
fixOffsets graphicalSlices (Tuple diagram (Just diagramCell)) =
    let gCells = map (\dCell -> dCell.cell) $ diagramCells diagram
        newCellPositions = calculateCellPositions graphicalSlices diagram diagramCell 
     in { cells: gCells, rewrite: diagramCell.cell, cellPositions: newCellPositions }

--Black Magic from Charlie's warped brain
-- Calculates cell positions for more outputs than inputs
calculateCellPositions :: GraphicalSlice -> Diagram -> DiagramCell -> Maybe (Array Int)
calculateCellPositions graphicalSlice diagram dCell = do
    changePoint <- last dCell.key --Changeover point
    graphicalSliceSplit <- splitAt changePoint graphicalSlice.cellPositions --prerewrite
    inputCount <- getNumInputs dCell
    dCellSplit <- splitAt length graphicalSliceSplit.after
    let leftBound = fromMaybe (-1 ) $ last graphicalSliceSplit.before 
    let rightBound = fromMaybe 1 $ head dCellSplit.after
    let centre = (fromMaybe 1 $ head dCellSplit.before) + (inputCount - 1)
    outputCount <- getNumOutputs dCell
    let leftOutput = centre - outputCount + 1
    let tempOutputs = map (\wire -> leftOutput + 2 * wire) 0..(outputCount-1)
    let leftShift = max 0 $ leftBound - leftOutput + 2
    let outputs = map (leftShift + _) tempOutputs
    let rightShift = leftShift + max 0 $ leftOutput + 2*outputCount - rightBound
    let rightWires = map (rightShift + _) dCellSplit.after
    pure $ Tuple centre $ graphicalSliceSplit.before <> outputs <> rightWires
     
--how many more output wires than input wires on diagramcell
diagramWireDiff :: DiagramCell -> Int
diagramWireDiff diagramCell = getNumOutputs diagramCell - getNumInputs diagramCell 

fixGraphics :: Array GraphicalSlice -> GraphicalSlice -> Array GraphicalSlice 
fixGraphics = ?hole1

initialPassover :: Array GraphicalSlice -> Tuple Diagram (Maybe DiagramCell) -> Array GraphicalSlice
initialPassover graphic newSlice =
    let fixedOffsets = fixOffsets newSlice graphic
        fixedGraphics = fixGraphics fixedOffsets graphic
     in fixedGraphics <> fixedOffsets

--transforms diagram into a graphical representation
transformGraphical :: Diagram -> Maybe (Array GraphicalSlice)
transformGraphical diagram = do
    sliceList <- getSlices diagram
    pure $ coreLoop sliceList (diagramCells diagram)
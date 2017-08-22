module Algorithms where

import Prelude
import Data.Foldable (and)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Color (black)
import Control.MonadZero (guard)
import Data.Array (drop, init, length, modifyAt, snoc, zipWith, replicate, take, foldl, cons, range, zip, last, head, (!!), (..))
import Data.Array (slice) as Array
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

slice :: Signature -> Diagram -> Int -> Maybe Diagram
slice signature diagram height =
  foldl (applyRewrite signature) (diagramSource diagram) rewrites
  where
    rewrites = take height $ diagramCells diagram  

applyRewrite :: Signature -> Maybe Diagram -> DiagramCell -> Maybe Diagram
applyRewrite signature thisSlice diagramCell = do
    sliceDiagram <- thisSlice
    let coords = diagramCell.key
    cell <- getCell signature diagramCell.id
    cSource <- cell.source
    cTarget <- cell.target
    rewrite coords sliceDiagram cSource cTarget

getSlices :: Signature -> Diagram -> Maybe (Array Diagram)
getSlices signature diagram =
    let sliceNum = length $ diagramCells diagram
        sliceHeight = range 0 $ sliceNum
        diagramList = replicate (sliceNum+ 1) diagram
     in sequence $ zipWith (slice signature) diagramList sliceHeight 

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

type Shifts = {leftBound :: Int, leftShift :: Int, rightBound :: Int, rightShift :: Int}

type GraphicalSlice =
  { cells :: Array Cell
  , rewrite :: Maybe { rewriteCell :: Cell, rewriteKey :: Int, rewriteCoord :: Int }
  , cellPositions :: Array Int
  }

--transforms diagram into a graphical representation
drawDiagram :: Signature -> Diagram -> Maybe (Array GraphicalSlice)
drawDiagram signature diagram = do
    sliceList <- getSlices signature diagram
    foldl (addGraphicalSlice signature) (pure []) $ zip slices (Nothing `cons` (Just <$> diagramCells diagram))

addGraphicalSlice :: Signature -> Maybe (Array GraphicalSlice) -> Tuple Diagram (Maybe DiagramCell) -> Maybe (Array GraphicalSlice)
addGraphicalSlice signature _ (Tuple slices Nothing) = do
  cells <- sequence $ map (getCell signature <<< dCellID) $ diagramCells slices
  pure
    [ { cells
      , rewrite: Nothing 
      , cellPositions: spaceNWires (length (diagramCells slices)) 
      }
    ]
addGraphicalSlice signature mGraphicalSlices (Tuple slices (Just diagramCell)) = do
    graphicalSlices <- mGraphicalSlices
    lastGraphicalSlice <- last graphicalSlices
    cells <- sequence $ map (getCell signature <<< dCellID) $ diagramCells slices
    cellPositions <- calculateCellPositions signature lastGraphicalSlice slices diagramCell
    key <- last diagramCell.key
    cell <- getCell signature diagramCell.id
    let fixedGraphics = fixGraphics graphicalSlices cellPositions.shifts
    pure $
      fixedGraphics `snoc`
        { cells
        , rewrite: Just { rewriteCell: cell, rewriteKey: key, rewriteCoord: cellPositions.centre }
        , cellPositions: cellPositions.positions
        }

spaceWires :: Array Int -> Array Int
spaceWires = map (\x -> 2*x + 1)

spaceNWiresFrom :: Int -> Int -> Array Int
spaceNWiresFrom a n = spaceWires (a .. (a+n-1))

-- Black Magic from Charlie's warped brain
calculateCellPositions :: Signature -> GraphicalSlice -> Diagram -> DiagramCell -> Maybe {shifts :: Shifts, centre :: Int, positions :: Array Int}
calculateCellPositions signature graphicalSlice diagram dCell = do
    key <- last dCell.key
    inputCount <- getNumInputs signature dCell
    outputCount <- getNumOutputs signature dCell
    let {left, mid, right} = splitInThree key inputCount graphicalSlice.cellPositions
    let leftBound = wiresRightBound 0 left
    let centre = leftBound + inputCount
    let leftBound' = centre - outputCount
    let leftShift = max 0 $ leftBound - leftBound'
    let outputs = spaceNWiresFrom (leftBound' + leftShift) outputCount
    let rightBound = wiresLeftBound infinity right
    let rightShift = leftShift + max 0 (leftBound + 2*outputCount - rightBound)
    let rightWires = map (rightShift + _) right
    let positions = left <> outputs <> rightWires
    pure $ {shifts: {leftBound, leftShift, rightBound, rightShift}, centre, positions}

splitInThree :: forall a. Int -> Int -> Array a -> {left :: Array a, mid :: Array a, right :: Array a}
splitInThree lsize msize arr =
  { left: take lsize arr
  , mid: Array.slice lsize (lsize + msize) arr
  , right: drop (lsize + msize) arr
  }

wiresLeftBound :: Int -> Array Int -> Int
wiresLeftBound default xs = maybe default (_-1) (head xs)

wiresRightBound :: Int -> Array Int -> Int
wiresRightBound default xs = maybe default (1+_) (last xs)

fixGraphics :: Array GraphicalSlice -> Shifts -> Array GraphicalSlice 
fixGraphics gSlices shifts = map applyShifts gSlices
  where
    applyShifts s@{cellPositions} = s{cellPositions = map applyShift cellPositions}
    applyShift n
      | n >= shifts.rightBound = n + shifts.rightShift
      | n > shifts.leftBound   = n + shifts.leftShift
      | otherwise              = n

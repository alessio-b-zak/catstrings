module Rendering where

import Prelude
import Data.Foldable (maximum)
import Data.Tuple (Tuple(..))
import Data.Maybe (fromMaybe, maybe)
import Data.Array (cons, foldl, head, last, length, snoc, tail, zip, (..))
import Data.Traversable (sequence)
import Utilities
import Structures
import Algorithms

type Shifts = {leftBound :: Int, leftShift :: Int, rightBound :: Int, rightShift :: Int}

type GraphicalSource e =
  { cells :: Array Cell
  , cellPositions :: Array Int
  , regions :: Array Cell
  | e }

type GraphicalSlice = GraphicalSource
                        ( rewriteCell :: Cell
                        , rewriteKey :: Int
                        , rewriteInputs :: Int
                        , rewriteOutputs :: Int
                        , rewriteCoord :: Int
                        )

type CellPositions = { shifts :: Shifts, inputCount :: Int, outputCount :: Int, centre :: Int, positions :: Array Int}

data GraphicalSlices = GraphicalSlices (GraphicalSource ()) (Array GraphicalSlice)

graphicalSlicesWidth :: GraphicalSlices -> Int
graphicalSlicesWidth (GraphicalSlices source slices) = fromMaybe 2 $ maximum $ width source `cons` map width slices
  where
    width :: forall e. GraphicalSource e -> Int
    width {cellPositions} = wiresRightBound 2 cellPositions

--transforms diagram into a graphical representation
drawDiagram :: Signature -> Diagram -> OrError GraphicalSlices
drawDiagram signature diagram = do
  sliceList <- getSlices signature diagram
  source <- orError NoSlice $ head sliceList
  rewrittenSlices <- orError NoSlice $ tail sliceList
  graphicalSource <- getGraphicalSource signature source
  let graphicalBasis = GraphicalSlices graphicalSource []
  foldl (addGraphicalSlice signature) (pure graphicalBasis) $ zip rewrittenSlices (diagramCells diagram)
--leftSource <- getCell signature =<< orError NoSource (_.id <$> (head <<< diagramCells =<< diagramSource =<< diagramSource diagram))

getGraphicalSource :: Signature -> Diagram -> OrError (GraphicalSource ())
getGraphicalSource signature source = do
  cells <- sequence $ map (getCell signature <<< _.id) $ diagramCells source
  regions <- getRegions signature source cells
  pure
    { cells
    , cellPositions: spaceNWires (length (diagramCells source)) 
    , regions
    }

getRegions :: Signature -> Diagram -> Array Cell -> OrError (Array Cell)
getRegions signature diagram cells = do
  left <- getCell signature =<< orError NoSource (
            _.id <$> (head <<< diagramCells =<< diagramSource diagram))
  rest <- getTargets signature cells
  pure $ left `cons` rest

getTargets :: Signature -> Array Cell -> OrError (Array Cell)
getTargets signature cells =
    --       ( C -> oe C                                                                              )
    --        ( DC        ->      oe C )     (m DC  -> oe DC)      (D     ->       m DC)     (C->m D)
  sequence $ (getCell signature <<< _.id <=< orError NoTarget <<< (head <<< diagramCells <=< _.target)) <$> cells

topSlice :: GraphicalSlices -> GraphicalSource ()
topSlice (GraphicalSlices source slices) = maybe source sliceToSource (last slices)

sliceToSource :: forall e . GraphicalSource e -> GraphicalSource ()
sliceToSource {cells, cellPositions, regions} = {cells, cellPositions, regions}

slicePairs :: GraphicalSlices -> Array (Tuple (GraphicalSource ()) GraphicalSlice)
slicePairs (GraphicalSlices source slices) =
  zip (source `cons` map sliceToSource slices) slices

addSlice :: GraphicalSlices -> GraphicalSlice -> GraphicalSlices
addSlice (GraphicalSlices source slices) newSlice = GraphicalSlices source (slices `snoc` newSlice)

addGraphicalSlice :: Signature -> OrError GraphicalSlices -> Tuple Diagram DiagramCell -> OrError GraphicalSlices
addGraphicalSlice signature mGraphicalSlices (Tuple dSlice diagramCell) = do
    gSlices <- mGraphicalSlices
    let gSlice = topSlice gSlices
    cells <- sequence $ map (getCell signature <<< _.id) $ diagramCells dSlice
    regions <- getRegions signature dSlice cells
    cellPositions <- calculateCellPositions signature gSlice dSlice diagramCell
    key <- orError BadDimension $ last diagramCell.key
    cell <- getCell signature diagramCell.id
    let fixedGraphics = fixGraphics gSlices cellPositions.shifts
    pure $ addSlice fixedGraphics
        { cells
        , regions
        , rewriteCell: cell
        , rewriteKey: key
        , rewriteInputs: cellPositions.inputCount
        , rewriteOutputs: cellPositions.outputCount
        , rewriteCoord: cellPositions.centre
        , cellPositions: cellPositions.positions
        }

spaceWires :: Array Int -> Array Int
spaceWires = map (\x -> 2*x + 1)

spaceNWires :: Int -> Array Int
spaceNWires n = spaceWires (0 ... n)

spaceNWiresFrom :: Int -> Int -> Array Int
spaceNWiresFrom a = map (a+_) <<< spaceNWires

-- Black Magic from Charlie's warped brain
calculateCellPositions :: Signature -> GraphicalSource () -> Diagram -> DiagramCell -> OrError CellPositions
calculateCellPositions signature gSlice diagram dCell = do
    key <- orError BadDimension $ last dCell.key
    inputCount <- getNumInputs signature dCell
    outputCount <- getNumOutputs signature dCell
    let {left, mid, right} = splitInThree key inputCount gSlice.cellPositions
    let leftBound = wiresRightBound 0 left
    let centre = leftBound + (max 1 inputCount)
    let leftBound' = centre - (max 1 outputCount)
    let leftShift = max 0 $ leftBound - leftBound'
    let outputs = spaceNWiresFrom (leftBound' + leftShift) outputCount
    let rightBound = wiresLeftBound infinity right
    let rightShift = leftShift + max 0 (leftBound + 2*(min 1 outputCount) - rightBound)
    let rightWires = map (rightShift + _) right
    let positions = left <> outputs <> rightWires
    pure $
      { shifts: {leftBound, leftShift, rightBound, rightShift}
      , inputCount
      , outputCount
      , centre: leftShift + centre
      , positions
      }

wiresLeftBound :: Int -> Array Int -> Int
wiresLeftBound default xs = maybe default (_-1) (head xs)

wiresRightBound :: Int -> Array Int -> Int
wiresRightBound default xs = maybe default (1+_) (last xs)

fixGraphics :: GraphicalSlices -> Shifts -> GraphicalSlices
fixGraphics (GraphicalSlices source slices) shifts = (GraphicalSlices (applyShifts source) (map applyShifts slices))
  where
    applyShifts :: forall r. GraphicalSource r -> GraphicalSource r
    applyShifts s@{cellPositions} = s{cellPositions = map applyShift cellPositions}
    applyShift n
      | n >= shifts.rightBound = n + shifts.rightShift
      | n > shifts.leftBound   = n + shifts.leftShift
      | otherwise              = n

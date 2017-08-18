module Structures where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..))
import Data.Foldable
import Data.Array
import Data.Generic (class Generic, gShow, gEq)
import Color

data CellID = CellID Int Int
derive instance eqCellID :: Eq CellID

type Project = 
  { diagram :: Maybe Diagram
  , signature :: Signature
  , cacheSourceTarget :: Maybe (Tuple Boundary Diagram)
  , initialized :: Boolean
  , viewControls :: ViewControls
  , selectedCell :: Maybe Cell
  } 

type DiagramCell = 
  { id :: CellID
  , key :: Array Int
  , box :: Maybe Box
  }

eqDiagramCell :: DiagramCell -> DiagramCell -> Boolean
eqDiagramCell diagramCell1 diagramCell2 =
    diagramCell1.id == diagramCell2.id &&
    diagramCell1.key == diagramCell2.key 

dCellID :: DiagramCell -> CellID
dCellID dCell = dCell.id

data Diagram = Diagram 
  { source :: Maybe Diagram
  , cells :: Array DiagramCell
  , dimension :: Int
  }

instance eqDiagram :: Eq Diagram where
  eq diagram1 diagram2 = 
      diagramSource diagram1 == diagramSource diagram2 &&
      (and $ zipWith eqDiagramCell (diagramCells diagram1) (diagramCells diagram2)) &&
      diagramDimension diagram1 == diagramDimension diagram2

emptyDiagram :: Diagram
emptyDiagram = Diagram {source: Nothing, cells: [], dimension: 0}

type ReplacementDiagram =
  { diagram :: Diagram
  , offset :: Array Int
  }

diagramSource :: Diagram -> Maybe Diagram
diagramSource (Diagram { source }) = source

diagramCells :: Diagram -> Array DiagramCell
diagramCells (Diagram { cells }) = cells

diagramDimension :: Diagram -> Int
diagramDimension (Diagram { dimension }) = dimension

data Signature = Signature 
  { cells :: Cells     -- Cells of the signature
  , sigma :: Maybe Signature -- Based on this signature
  , dimension :: Int -- Dimension of the signature
  , id :: Int -- Next id to use
  }

signatureCells :: Signature -> Cells
signatureCells (Signature s) = s.cells

signatureCells' :: Signature -> Cells -> Signature
signatureCells' (Signature s) cells = Signature $ s {cells = cells}

signatureCellArray :: Signature -> Array Cell
signatureCellArray (Signature {cells: Cells cs}) = cs

signatureCellArray' :: Signature -> Array Cell -> Signature
signatureCellArray' (Signature s) cells = Signature $ s {cells = Cells cells}

signatureSigma :: Signature -> Maybe Signature
signatureSigma (Signature s) = s.sigma

signatureSigma' :: Signature -> Maybe Signature -> Signature
signatureSigma' (Signature s) sigma = Signature $ s {sigma = sigma}

signatureDimension :: Signature -> Int
signatureDimension (Signature s) = s.dimension

signatureDimension' :: Signature -> Int -> Signature
signatureDimension' (Signature s) dimension = Signature $ s {dimension = dimension}

type Cell = 
  { source :: Maybe Diagram
  , target :: Maybe Diagram
  , id :: CellID
  , invertible :: Boolean
  , name :: String
  , singleThumbnail :: Boolean
  , display :: Display
  }

eqCell :: Cell -> Cell -> Boolean
eqCell cell1 cell2 =
    cell1.id == cell2.id
    && cell1.source == cell2.source
    && cell1.target == cell2.target
    && cell1.invertible == cell2.invertible

cellID :: Cell -> CellID
cellID cell = cell.id

cellColour :: Cell -> Color
cellColour cell = cell.display.colour

type Box = 
  { boxMin :: Array Int
  , boxMax :: Array Int
  , boxIgnore :: Boolean 
  }

type Display = 
  { colour :: Color
  , rate :: Int
  }

newtype Cells = Cells (Array Cell)
getCells :: Cells -> Array Cell
getCells (Cells cells) = cells

type ViewControls = 
  { project :: Int
  , slices :: Array Int
  }

data Boundary = Source | Target
derive instance genericBoundary :: Generic Boundary
instance eqBoundary :: Eq Boundary where eq = gEq
instance showBoundary :: Show Boundary where
  show Source = "Source"
  show Target = "Target"

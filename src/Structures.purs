module Structures where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Foldable (and)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Color (Color)
import Data.Array (zipWith)

data CellID = CellID Int Int
derive instance eqCellID :: Eq CellID
derive instance genericCellID :: Generic CellID _
instance showCellID :: Show CellID where show = genericShow

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

data Diagram = Diagram 
  { source :: Maybe Diagram
  , cells :: Array DiagramCell
  , dimension :: Int
  }


setCells :: Diagram -> Array DiagramCell -> Diagram
setCells (Diagram diagram) newCells= Diagram $ diagram {cells = newCells}

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
  { cells :: Array Cell -- Cells of the signature
  , sigma :: Maybe Signature -- Based on this signature
  , dimension :: Int -- Dimension of the signature
  , id :: Int -- Next id to use
  }

signatureCells :: Signature -> Array Cell
signatureCells (Signature s) = s.cells

signatureCells' :: Signature -> Array Cell -> Signature
signatureCells' (Signature s) cells = Signature $ s {cells = cells}

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

type ViewControls = 
  { project :: Int
  , slices :: Array Int
  }

data Boundary = Source | Target
derive instance eqBoundary :: Eq Boundary
derive instance genericBoundary :: Generic Boundary _
instance showBoundary :: Show Boundary where show = genericShow

data Error
  = BadCell CellID
  | BadDimension
  | BadKey
  | NoSource
  | NoTarget
  | NoSlice
  | DifferentSources
  | DifferentTargets
  | Other

derive instance genericError :: Generic Error _
instance showError :: Show Error where show = genericShow

type OrError = Either Error

orError :: forall a. Error -> Maybe a -> OrError a
orError error = maybe (Left error) Right

fromError :: forall a. OrError a -> Maybe a
fromError (Left a) = Nothing
fromError (Right b) = Just b

catch :: forall a. a -> OrError a -> a
catch default = fromMaybe default <<< fromError

guard' :: Error -> Boolean -> OrError Unit
guard' error cond
  | cond      = Right unit
  | otherwise = Left error

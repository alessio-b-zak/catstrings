module Structures where

import Prelude
import Data.Maybe (Maybe(..))
import Color

type Project = 
  { diagram :: Maybe Diagram
  , signature :: Signature
  , cacheSourceTarget :: Maybe Unit
  , initialized :: Boolean
  , viewControls :: ViewControls
  , selectedCell :: Maybe Cell
  } 

type DiagramCell = 
  { cell :: Cell
  , id :: String
  , key :: Array Int
  , box :: Maybe Box }

data Diagram = Diagram 
  { source :: Maybe Diagram
  , cells :: Array DiagramCell
  , dimension :: Int
  }

diagramSource :: Diagram -> Maybe Diagram
diagramSource (Diagram { source }) = source

diagramCells :: Diagram -> Array DiagramCell
diagramCells (Diagram { cells }) = cells

data Signature  = Signature 
  { cells :: Cells     -- Cells of the signature
  , sigma :: Maybe Signature -- Based on this signature
  , k :: Int -- Number of generators at this dimension
  , n :: Int -- Dimension of the signature
  }

type Cell = 
  { source :: Maybe Diagram
  , target :: Maybe Diagram
  , id :: String
  , invertible :: Boolean
  , name :: String
  , singleThumbnail :: Boolean
  , display :: Display
  } 

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

type ViewControls = 
  { vcProject :: Int
  , vcSlices :: Array Int
  } 

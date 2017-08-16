module Structures where

import Prelude
import Data.Maybe (Maybe)
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

data Signature  = Signature 
  { cells :: Cells     -- Cells of the signature
  , sigma :: Maybe Signature -- Based on this signature
  , k :: Int -- Number of generators at this dimension
  , n :: Int -- Dimension of the signature
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

signatureK :: Signature -> Int
signatureK (Signature s) = s.k

signatureK' :: Signature -> Int -> Signature
signatureK' (Signature s) k = Signature $ s {k = k}

signatureN :: Signature -> Int
signatureN (Signature s) = s.n

signatureN' :: Signature -> Int -> Signature
signatureN' (Signature s) n = Signature $ s {n = n}

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
  { project :: Int
  , slices :: Array Int
  } 

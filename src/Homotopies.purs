module Homtopies where

import Algorithms
import Prelude
import Structures

import Control.Apply(lift2)
import Data.Array (index, last, (!!), modifyAt, length)
import Data.Maybe (Maybe)
import Utilities (splitInThree)

type WireBoundaries = { leftBoundary :: Int, rightBoundary :: Int }

inputWireBoundaries :: Signature -> DiagramCell -> OrError WireBoundaries
inputWireBoundaries signature dCell = do
    dCellIndex <- orError Other (last $ dCell.key)
    inputWires <- getNumInputs signature dCell
    pure { leftBoundary : dCellIndex, rightBoundary : inputWires + dCellIndex - 1  }
            
outputWireBoundaries :: Signature -> DiagramCell -> OrError WireBoundaries
outputWireBoundaries signature dCell = do
    dCellIndex <- orError Other (last $ dCell.key)
    outputWires <- getNumOutputs signature dCell
    pure { leftBoundary : dCellIndex, rightBoundary : outputWires + dCellIndex - 1 }


checkBoundary :: WireBoundaries -> WireBoundaries -> Boolean
checkBoundary higherBoundary lowerBoundary =
    higherBoundary.rightBoundary < lowerBoundary.leftBoundary &&
    higherBoundary.leftBoundary > lowerBoundary.rightBoundary

--matches a type one homotopy where the coord is the highest rewrite
matchOneHomotopy :: Signature -> Int -> Diagram -> OrError Boolean
matchOneHomotopy signature coord diagram = do
    higherCell <- orError Other $ diagramCells diagram !! coord  
    lowerCell <- orError Other $ diagramCells diagram !! (coord - 1) 
    higherBoundaries <- inputWireBoundaries signature higherCell 
    lowerBoundaries <- outputWireBoundaries signature lowerCell
    pure $ checkBoundary higherBoundaries lowerBoundaries    

data VerticalLoc = Higher | Lower

type DCellComparable = {dCell :: DiagramCell, loc :: VerticalLoc }

getLeftmostCell :: DiagramCell -> DiagramCell -> OrError { leftCell :: DCellComparable, rightCell :: DCellComparable }
getLeftmostCell dCell1 dCell2 = do
    dCell1Horizontal <- orError Other $ last $ dCell1.key
    dCell2Horizontal <- orError Other $ last $ dCell2.key 
    if dCell1Horizontal < dCell2Horizontal
      then pure { leftCell: { dCell: dCell1, loc: Lower }, rightCell: { dCell: dCell2, loc: Higher } }
      else pure { leftCell: { dCell: dCell2, loc: Higher }, rightCell: { dCell: dCell1, loc: Lower } }

updateHorizontalCoord :: DCellComparable -> Int -> OrError DCellComparable 
updateHorizontalCoord dCellComp diff = do
    let dCell = dCellComp.dCell
    case dCellComp.loc of
        Higher -> do
              newKey <- orError BadKey $ modifyAt (length dCell.key - 1 ) (diff + _ ) dCell.key 
              pure $ dCellComp { dCell = dCell { key = newKey } }
        Lower -> do
              newKey <- orError BadKey $ modifyAt (length dCell.key - 1) (_ - diff) dCell.key
              pure $ dCellComp { dCell = dCell { key = newKey } }


diagramWireDiff :: Signature -> DiagramCell -> OrError Int
diagramWireDiff signature dCell =
    lift2 (-) (getNumInputs signature dCell) (getNumOutputs signature dCell) 

replaceDiagramCells :: DCellComparable -> DCellComparable -> Array DiagramCell
replaceDiagramCells dCell1 dCell2 =
    case dCell1.loc of
        Higher -> [dCell1.dCell, dCell2.dCell] 
        Lower -> [dCell2.dCell, dCell1.dCell]

swapAndAlter :: Signature -> Array DiagramCell -> OrError (Array DiagramCell)
swapAndAlter signature dCells = do
    firstCell <- orError Other $ dCells !! 1 
    secondCell <- orError Other $ dCells !! 2
    cellHorizontal <- getLeftmostCell firstCell secondCell
    wireDiff <- diagramWireDiff signature cellHorizontal.leftCell.dCell
    newRightCell <- updateHorizontalCoord cellHorizontal.rightCell wireDiff
    pure $ replaceDiagramCells cellHorizontal.leftCell newRightCell

rewriteOneHomotopy :: Signature -> Int -> Diagram -> OrError Diagram
rewriteOneHomotopy signature coord diagram = do
    let splitCells = splitInThree (coord + 1) 2 $ diagramCells diagram
    swappedCells <- swapAndAlter signature splitCells.mid
    pure $ setCells diagram $ splitCells.left <> swappedCells <> splitCells.right

    
    
    
    
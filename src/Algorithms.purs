module Algorithms where
  
import Utilities
import Structures
import Prelude
import Data.Maybe
import Data.Array
import Color (black)
import Data.Traversable
import Control.MonadZero

identityDiag :: Diagram -> Diagram
identityDiag diag = Diagram newDiag
  where
     newDiag = { source: Just diag
               , cells: []
               , dimension: diagramDimension diag + 1
               }

newZeroCell :: Project -> Project
newZeroCell project =
    let newZero = { source: Nothing
                  , target: Nothing
                  , id: "new 0 cell"
                  , invertible: false
                  , name: "new 0 cell"
                  , singleThumbnail: true
                  , display: {colour: black, rate: 1}
                  }
        updateSignature :: Signature -> Signature
        updateSignature (Signature s@{cells: (Cells cells), sigma: Nothing}) = 
           Signature $ s { cells = Cells (cells `snoc` newZero), k = s.k + 1 }
        updateSignature (Signature s@{sigma: Just sigma})   = Signature $ s { sigma = Just $ updateSignature sigma }
     in project { signature = updateSignature project.signature}

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

data Boundary = Source | Target

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
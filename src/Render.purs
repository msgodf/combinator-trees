module Render ( treeData
              , drawTree
              ) where

import Model

import Prelude (bind, pure, show, (<>), (*))
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (read, class IsForeign, readProp, write)
import Data.Array (tail)
import Control.Monad.Except (runExcept)
import D3.Tree (TreeNodeData(..),tree,hierarchyChildren,runTree,descendants)
import D3.Base (D3,Value(..))
import D3.Selection (rootSelect, append', attr, selectAll, data', enter, style, text, insert, remove)

-- This is used to match branch or leaf nodes passed to a callback
data BranchOrLeaf a = LeafNode | BranchNode a

instance branchOrLeafIsForeign :: IsForeign a => IsForeign (BranchOrLeaf a) where
  read x = case (runExcept (readProp "children" x)) of
        Left _ -> pure LeafNode
        Right ch -> pure (BranchNode ch)

fillBranchesAndLeaves :: Foreign -> String
fillBranchesAndLeaves f = case (runExcept ((read f) :: (F (BranchOrLeaf (Array (BranchOrLeaf (Tree String))))))) of
  Left err -> "blue"
  Right LeafNode -> "green"
  Right (BranchNode _) -> "red"

treeData :: Foreign
treeData = write (Branch
                        (Branch
                                (Leaf "B")
                                (Leaf "x"))
                        (Leaf "y"))

xyTranslate :: Foreign -> String
xyTranslate val =
  case (runExcept (read val)) of
       Left err -> "translate(0,0)"
       Right (TreeNodeData d :: (TreeNodeData (Tree String))) ->
             let dx = d.x * 400.0
                 dy = d.y * 200.0 in
             "translate(" <> (show dx) <> "," <> (show dy) <> ")"

diagonal :: forall a. (TreeNodeData a) -> (TreeNodeData a) -> String
diagonal (TreeNodeData source) (TreeNodeData dest) =
  let sx = source.x * 400.0
      sy = source.y * 200.0
      dx = dest.x * 400.0
      dy = dest.y * 200.0 in
      "M " <> (show sx) <> " " <> (show sy) <> " " <>
      "L " <> (show dx) <> " " <> (show dy)

parentChildLink :: Foreign -> String
parentChildLink s = do
  case (runExcept (read s)) of
    Left err -> ""
    Right (TreeNodeData source :: (TreeNodeData (Tree String))) -> case (source.parent) of
        Nothing -> ""
        Just (TreeNodeData parent :: (TreeNodeData (Tree String))) -> diagonal (TreeNodeData source)
                                                                               (TreeNodeData parent)

drawTree :: forall a e. Foreign -> Eff (d3 :: D3 | e) a
drawTree treeData' = do
     let width = "600px"
         height = "600px"

     treeMap <- tree

     h <- hierarchyChildren treeData' (_.children)

     theTree <- runTree treeMap h

     t <- descendants theTree

     existing <- rootSelect "div > div.drawing > svg"

     remove existing

     root <- rootSelect "div > div.drawing"
     svg <- append' root "svg"

     attr svg "width" (ValString width)
     attr svg "height" (ValString height)
     gr <- append' svg "g"
     attr gr "transform" (ValString "translate(100,100)")

     node <- selectAll gr "g.node"
     nodeData <- data' node t
     nodeEnter <- enter nodeData
     grn <- append' nodeEnter "g"
     attr grn "class" (ValString "node")

     attr grn "transform" (ValFn xyTranslate)

     circle <- append' grn "circle"
     attr circle "class" (ValString "node")
     attr circle "r" (ValString "10")
     style circle "fill" (ValFn fillBranchesAndLeaves)
     style circle "stroke" (ValString "#555")
     style circle "stroke-width" (ValString "2px")

     circleText <- append' grn "text"
     attr circleText "dy" (ValString "10px")
     attr circleText "x" (ValString "24")
     attr circleText "text-anchor" (ValString "end")
     text circleText (ValFn (\d -> d.data.name))

     case (tail t) of
          Nothing -> rootSelect ""
          Just links -> do
               linkNode <- selectAll gr "path.link"
               link <- data' linkNode links
               linkPaths <- enter link
               path <- insert linkPaths (ValString "path") "g"
               p <- attr path "class" (ValString "link")
               attr p "d" (ValFn parentChildLink)
               style p "stroke" (ValString "#555")
               style p "stroke-width" (ValString "3px")
               style p "fill" (ValString "none")

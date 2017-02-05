module Render (drawTree) where

import Model

import Prelude (bind, pure, show, (<>), (*), ($), const, (<<<))
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, ForeignError)
import Data.Foreign.Class (read, class IsForeign, readProp)
import Data.Array (tail)
import Data.List.NonEmpty (NonEmptyList)
import Control.Monad.Except (runExcept)
import D3.Tree (TreeNodeData(..),tree,hierarchyChildren,runTree,descendants)
import D3.Base (D3,Value(..))
import D3.Selection (rootSelect, append', attr, selectAll, data', enter, style, text, insert, remove)

-- This is used to match branch or leaf nodes passed to a callback
data BranchOrLeaf a = LeafNode | BranchNode a

type D3Tree = (BranchOrLeaf (Array (BranchOrLeaf (Tree String))))

instance branchOrLeafIsForeign :: IsForeign a => IsForeign (BranchOrLeaf a) where
  read x = case (runExcept (readProp "children" x)) of
        Left _ -> pure LeafNode
        Right ch -> pure (BranchNode ch)

convertToBranchOrLeaf :: Foreign -> Either (NonEmptyList ForeignError)
                                           D3Tree
convertToBranchOrLeaf = runExcept <<< read

colourBranchesAndLeaves :: forall a. (BranchOrLeaf a) -> String
colourBranchesAndLeaves LeafNode = "green"
colourBranchesAndLeaves (BranchNode _) = "red"

radiusBranchesAndLeaves :: forall a. (BranchOrLeaf a) -> String
radiusBranchesAndLeaves LeafNode = "10"
radiusBranchesAndLeaves (BranchNode _) = "3"

xyTranslate :: TreeNodeData (Tree String) -> String
xyTranslate (TreeNodeData d) = let dx = d.x * 400.0
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

parentChildLink :: (TreeNodeData (Tree String)) -> String
parentChildLink source@(TreeNodeData {parent: parent}) =
  maybe "" (diagonal source) parent

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
     attr grn "transform" (ValFn $ either (const "translate(0,0)")
                                          xyTranslate <<<
                                          runExcept <<<
                                          read)
     drawNode grn
     drawNodeText grn

     case (tail t) of
          Nothing -> rootSelect ""
          Just links -> drawLinks links gr

drawLinks :: forall a b e. a -> b -> Eff ( d3 :: D3 | e) b
drawLinks links parent = do
  linkNode <- selectAll parent "path.link"
  link <- data' linkNode links
  linkPaths <- enter link
  path <- insert linkPaths (ValString "path") "g"
  p <- attr path "class" (ValString "link")
  attr p "d" (ValFn $ either (const "")
                             parentChildLink <<<
                             runExcept <<<
                             read)
  style p "stroke" (ValString "#555")
  style p "stroke-width" (ValString "3px")
  style p "fill" (ValString "none")

drawNode :: forall a e. a -> Eff ( d3 :: D3 | e) a
drawNode parent = do
  circle <- append' parent "circle"
  attr circle "class" (ValString "node")
  attr circle "r" (ValFn $ either (const "5")
                                  radiusBranchesAndLeaves <<<
                                  convertToBranchOrLeaf)
  style circle "fill" (ValFn $ either (const "blue")
                                      colourBranchesAndLeaves <<<
                                      convertToBranchOrLeaf)
  style circle "stroke" (ValString "#555")
  style circle "stroke-width" (ValString "2px")

drawNodeText :: forall a e. a -> Eff ( d3 :: D3 | e) a
drawNodeText parent = do
  circleText <- append' parent "text"
  attr circleText "dy" (ValString "10px")
  attr circleText "x" (ValString "24")
  attr circleText "text-anchor" (ValString "end")
  text circleText (ValFn (\d -> d.data.name))

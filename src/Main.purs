module Main where

import Base (D3,Value(..))
import Selection (rootSelect,Selection,append',attr,selectAll,data',enter,style,text,insert)
import Tree (TreeNodeData(..),tree,hierarchyChildren,runTree,descendants)

import Prelude (class Show, Unit, Void, bind, pure, show, ($), (<>),(*),(/),(+))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), writeObject, fail)
import Data.Foreign.Class (class AsForeign, read, class IsForeign, (.=), readProp, write)
import Data.Array (tail,length)
import Control.Monad.Except (runExcept)

-- Helper function that calls console.log with anything
foreign import logAnythingImpl :: forall a e. (Fn1 a (Eff (console :: CONSOLE | e) Unit))
logAnything :: forall a e. a -> (Eff (console :: CONSOLE | e) Unit)
logAnything = (runFn1 logAnythingImpl)

data NodeData a = NodeData {data':: a}

instance nodeDataShow :: (Show a) => Show (NodeData a) where
  show (NodeData {data':data'}) = "data: " <> (show data')

instance nodeDataIsForeign :: (IsForeign a) => IsForeign (NodeData a) where
  read x = do
    data' <- readProp "data" x
    pure $ NodeData {data': data'}

data BranchOrLeaf a = LeafNode | BranchNode a

instance branchOrLeafIsForeign :: IsForeign a => IsForeign (BranchOrLeaf a) where
  read x = case (runExcept (readProp "children" x)) of
        Left _ -> pure LeafNode
        Right ch -> pure (BranchNode ch)


-- This is the type signaure that irks me the most
fillBranchesAndLeaves :: Foreign -> String
fillBranchesAndLeaves f = case (runExcept ((read f) :: (F (BranchOrLeaf (Array (BranchOrLeaf (SimpleTree String))))))) of
  Left err -> "blue"
  Right LeafNode -> "green"
  Right (BranchNode _) -> "red"

-- Leaf nodes are just values, branches are left and right subtrees
data SimpleTree a = Leaf a | Branch (SimpleTree a) (SimpleTree a)

instance simpleTreeAsForeign :: AsForeign a => AsForeign (SimpleTree a) where
  write (Leaf name) = writeObject ["name" .= name]
  write (Branch left right) = writeObject [ "name" .= ""
                                           ,"children" .= [left, right]]

instance simpleTreeIsForeign :: IsForeign a => IsForeign (SimpleTree a) where
  read x = do
    name <- readProp "name" x
    case (runExcept (readProp "children" x)) of
      Left _ -> pure $ Leaf name
      Right [left,right] -> pure $ Branch left right
      Right xs -> fail $ TypeMismatch "Array with two elements"
                                      ("Array with " <> (show $ length xs) <> " elements")

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
       Right (TreeNodeData d :: (TreeNodeData (SimpleTree String))) ->
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
    Right (TreeNodeData source :: (TreeNodeData (SimpleTree String))) -> case (source.parent) of
        Nothing -> ""
        Just (TreeNodeData parent :: (TreeNodeData (SimpleTree String))) -> diagonal (TreeNodeData source)
                                                                                     (TreeNodeData parent)

main :: forall e. Eff (d3 :: D3, console :: CONSOLE | e) (Selection Void)
main = do
     let width = "600px"
         height = "600px"

     treeMap <- tree

     h <- hierarchyChildren treeData  (\d -> d.children)

     theTree <- runTree treeMap h

     t <- descendants theTree

     root <- rootSelect "div.drawing"

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

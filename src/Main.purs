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
import Data.Foreign (Foreign, writeObject, ForeignError)
import Data.Foreign.Class (class AsForeign, read, class IsForeign, (.=), readProp, write)
import Data.Array (tail)

-- not sure how to define typeclass instances for extensible records - it works for show, but not IsForeign?

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

-- A simple type to deserialize to depending on the presence of a "children" field
data TND a = TNDN | TNDC (Array (TND a))

-- I think there's something important about the type of ch here.
instance tndIsForeign :: IsForeign a => IsForeign (TND a) where
  read x = case (readProp "children" x) of
        Left _ -> pure TNDN
        Right ch -> pure (TNDC ch)

fillBranchesAndLeaves :: Foreign -> String
fillBranchesAndLeaves f = case ((read f) :: (Either ForeignError (TND MyTree))) of
  Left err -> "blue"
  Right TNDN -> "green"
  Right (TNDC _) -> "red"

data MyTree = MyTree { name :: String,
                       children :: (Array MyTree) }

instance myTreeShow :: Show MyTree where
  show (MyTree {name:name,children:children}) = "name: " <> show name <> ", children: " <> show children

instance myTreeAsForeign :: AsForeign MyTree where
  write (MyTree { name: name, children: []}) = writeObject [ "name" .= name
                                ]
  write (MyTree { name: name, children: children}) = writeObject [ "name" .= name
                                , "children" .= children
                                ]

instance myTreeIsForeign :: IsForeign MyTree where
  read x = do
    name <- readProp "name" x
    -- Optional "children" element, gets converted into Maybe
    case (readProp "children" x) of
      Left _ -> pure $ MyTree { name: name, children: [] }
      Right children -> pure $ MyTree { name: name, children: children }

  -- {
  --   "name": "Top Level",
  --   "children": [
  --     {
  --       "name": "Level 2: A",
  --       "children": [
  --         { "name": "Son of A" },
  --         { "name": "Daughter of A" }
  --       ]
  --     },
  --     { "name": "Level 2: B" }
  --   ]
  -- };

data SimpleTree a = Leaf a | Branch a (Array (SimpleTree a))

simpleTToMyTree :: (SimpleTree String) -> MyTree
simpleTToMyTree (Branch x y) = (MyTree { name: x, children: (map simpleTToMyTree y)})
simpleTToMyTree (Leaf x) = (MyTree { name: x, children: [] })

treeData :: Foreign
treeData = write $
           simpleTToMyTree (Branch "TopLevel"
                                   [(Branch "Level 2: A"
                                            [(Leaf "Son of A"),
                                             (Leaf "Daughter of A")]),
                                   (Leaf "Level 2: B")])

xyTranslate :: Foreign -> String
xyTranslate val = case (read val) of
                       Left err -> "translate(0,0)"
                       Right (TreeNodeData d :: (TreeNodeData MyTree)) ->
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
      "C " <> (show ((sx + dx) / 2.0)) <> " " <> (show dy) <> ", "
           <> (show ((sx + dx) / 2.0)) <> " " <> (show dy) <> ", "
           <> (show dx) <> " " <> (show dy)

parentChildLink :: Foreign -> String
parentChildLink s = do
  case (read s) of
    Left err -> ""
    Right (TreeNodeData source :: (TreeNodeData MyTree)) -> case (source.parent) of
        Nothing -> ""
        Just (TreeNodeData parent :: (TreeNodeData MyTree)) -> diagonal (TreeNodeData source)
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
     style circle "fill" "black"
     style circle "fill" (ValFn fillBranchesAndLeaves)

     circleText <- append' grn "text"
     attr circleText "dy" (ValString ".35em")
     attr circleText "x" (ValString "13")
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
               attr p "d" (ValFn (\d -> diag d))
               attr p "d" (ValFn parentChildLink)
               style p "stroke" (ValString "#555")
               style p "stroke-width" (ValString "3px")
               style p "fill" (ValString "none")

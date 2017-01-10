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
import Data.Foreign (Foreign, writeObject)
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


data MyTree = MyTree { name :: String,
                       children :: Maybe (Array MyTree) }

instance myTreeShow :: Show MyTree where
  show (MyTree {name:name,children:children}) = "name: " <> show name <> ", children: " <> show children

instance myTreeAsForeign :: AsForeign MyTree where
  write (MyTree { name: name, children: Nothing}) = writeObject [ "name" .= name
                                ]
  write (MyTree { name: name, children: Just children}) = writeObject [ "name" .= name
                                , "children" .= children
                                ]

instance myTreeIsForeign :: IsForeign MyTree where
  read x = do
    name <- readProp "name" x
    -- Optional "children" element, gets converted into Maybe
    case (readProp "children" x) of
      Left _ -> pure $ MyTree { name: name, children: Nothing }
      Right children -> pure $ MyTree { name: name, children: (Just children) }

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

-- Would prefer to express the absence of children here
-- as an empty list, and get the serialization to JSON to
-- ignore it if it's empty.
treeData :: Foreign
treeData = write
            (MyTree { name: "Top Level",
                      children: (Just [
                                    (MyTree { name: "Level 2: A",
                                          children: (Just [(MyTree { name: "Son of A",
                                                                     children: Nothing }),
                                                           (MyTree {name: "Daughter of A", children: Nothing })])}),
                                    (MyTree { name: "Level 2: B",
                                              children: Nothing})])})

xyTranslate :: Foreign -> String
xyTranslate val = case (read val) of
                       Left err -> "translate(0,0)"
                       Right (TreeNodeData d :: (TreeNodeData MyTree)) ->
                         let dx = d.x * 300.0
                             dy = d.y * 300.0 in
                         "translate(" <> (show dy) <> "," <> (show dx) <> ")"

diagonal :: forall a. (TreeNodeData a) -> (TreeNodeData a) -> String
diagonal (TreeNodeData source) (TreeNodeData dest) =
  let sx = source.x * 300.0
      sy = source.y * 300.0
      dx = dest.x * 300.0
      dy = dest.y * 300.0 in
      "M " <> (show sy) <> " " <> (show sx) <> " " <>
      "C " <> (show ((sy + dy) / 2.0)) <> " " <> (show sx) <> ", "
           <> (show ((sy + dy) / 2.0)) <> " " <> (show dx) <> ", "
           <> (show dy) <> " " <> (show dx)

diag :: Foreign -> String
diag s = do
  case (read s) of
    Left err -> ""
    Right (TreeNodeData source :: (TreeNodeData MyTree)) -> case (source.parent) of
        Nothing -> ""
        Just (TreeNodeData parent :: (TreeNodeData MyTree)) -> diagonal (TreeNodeData source) (TreeNodeData parent)

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
               style p "stroke" "#555"
               style p "stroke-width" "3px"
               style p "fill" "none"

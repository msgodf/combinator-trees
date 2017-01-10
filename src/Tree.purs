module Tree where

import Prelude (bind,($),pure,(<>))
import Base (D3)
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn0, runFn0, Fn1, runFn1, Fn2, runFn2)
import Data.Show
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foreign (Foreign, writeObject, ForeignError)
import Data.Foreign.Class (class AsForeign, read, class IsForeign, (.=), readProp, write)

foreign import data HierarchyTree :: *
foreign import data Hierarchy :: *
foreign import data Node :: *

-- Data is the type of the data passed to d3.hierarchy() - usually not a Tree :) it'll be a JS type - it can be anything that can be passed (i.e. a single value, an object, an array, etc.) - the function given to hierarchy determines how to index it, and if present, then there is a "children" element to it
data TreeNodeData a = TreeNodeData {data' :: a, parent :: Maybe (TreeNodeData a), x :: Number, y :: Number}

instance treeNodeDataShow :: (Show a) => Show (TreeNodeData a) where
  show (TreeNodeData {data': data', parent: parent, x: x, y: y}) = "x: " <> (show x) <> "," <>
                                                                   "y: " <> (show y) <> ", " <>
                                                                   "data: " <> (show data') <> "," <>
                                                                   "parent: " <> (show parent)

instance treeNodeDataIsForeign :: (IsForeign a) => IsForeign (TreeNodeData a) where
  read x = do
    data' <- readProp "data" x
    x' <- readProp "x" x
    y' <- readProp "y" x
    case (readProp "parent" x) of
      Left _ -> pure $ TreeNodeData {data': data', x: x', y: y', parent: Nothing}
      Right p ->
        pure $ (TreeNodeData {data': data', x: x', y: y', parent: (Just p)})


instance treeNodeDataAsForeign :: AsForeign a => AsForeign (TreeNodeData a) where
  write (TreeNodeData {x: x, y: y, data': data', parent: Nothing}) =
    writeObject [ "x" .= x
                 ,"y" .= y
                 ,"data" .= write data'
                ]
  write (TreeNodeData {x:x, y:y, data':data', parent: (Just parent)}) =
    writeObject [ "x" .= x
                 ,"y" .= y
                 ,"data" .= write data'
                 ,"parent" .= write parent
                ]

foreign import treeImpl :: forall e. Fn0 (Eff (d3 :: D3 | e) HierarchyTree)

-- actually a tree is a function that can take a hierarchy root as a parameter
tree :: forall e. Eff (d3 :: D3 | e) HierarchyTree
tree = runFn0 treeImpl

-- calling d3.tree()(hierarchy) returns a Node
foreign import runTreeImpl :: forall a e. Fn2 HierarchyTree Hierarchy (Eff (d3 :: D3 | e) (TreeNodeData a))
runTree :: forall a e. HierarchyTree -> Hierarchy -> Eff (d3 :: D3 | e) (TreeNodeData a)
runTree = runFn2 runTreeImpl

foreign import nodesImpl :: forall a e. Fn1 HierarchyTree (Eff (d3 :: D3 | e) (Array (TreeNodeData a)))

nodes :: forall a e. HierarchyTree -> (Eff (d3 :: D3 | e) (Array (TreeNodeData a)))
nodes = runFn1 nodesImpl

foreign import sizeImpl :: forall e. Fn1 HierarchyTree (Eff (d3 :: D3 | e) (Array Int))

size :: forall e. HierarchyTree -> (Eff (d3 :: D3 | e) (Array Int))
size = runFn1 sizeImpl

foreign import hierarchyImpl :: forall a e. Fn1 a (Eff (d3 :: D3 | e) Hierarchy)

hierarchy :: forall a e. a -> Eff (d3 :: D3 | e) Hierarchy
hierarchy = runFn1 hierarchyImpl

foreign import hierarchyChildrenImpl :: forall a b c e. Fn2 a (b -> c) (Eff (d3 :: D3 | e) Hierarchy)

hierarchyChildren :: forall a b c e. a -> (b -> c) -> Eff (d3 :: D3 | e) Hierarchy
hierarchyChildren = runFn2 hierarchyChildrenImpl

-- It's interesting that the value that gets passed to this still has the function prototype... perhaps it's because we don't read and write it to a TreeNodeData

foreign import descendantsImpl :: forall a e. Fn1 (TreeNodeData a) (Eff (d3 :: D3 | e) (Array (TreeNodeData a)))

descendants :: forall a e. (TreeNodeData a) -> Eff (d3 :: D3 | e) (Array (TreeNodeData a))
descendants = runFn1 descendantsImpl

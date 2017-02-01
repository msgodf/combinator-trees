module D3.Selection
       ( rootSelect
       , attr
       , insert
       , style
       , append'
       , data'
       , selectAll
       , enter
       , text
       , remove
       , Selection
       ) where

import D3.Base (D3, Value)

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Prelude (class Show, (<>), show)

foreign import data Selection :: * -> *

foreign import rootSelectImpl :: forall c e. Fn1 String (Eff (d3 :: D3 | e) c)
rootSelect :: forall c e. String -> (Eff (d3 :: D3 | e) c)
rootSelect = runFn1 rootSelectImpl

foreign import attrImpl :: forall a b c e. Fn3 a String (Value b c) (Eff (d3 :: D3 | e) a)
attr :: forall a b c e. a -> String -> (Value b c) -> Eff (d3 :: D3 | e) a
attr = runFn3 attrImpl

foreign import insertImpl :: forall a b c e. Fn3 a (Value b c) String (Eff (d3 :: D3 | e) a)
insert :: forall a b c e. a -> (Value b c) -> String -> Eff (d3 :: D3 | e) a
insert = runFn3 insertImpl

foreign import styleImpl :: forall a b c e. Fn3 a String (Value b c) (Eff (d3 :: D3 | e) a)
style :: forall a b c e. a -> String -> (Value b c) -> Eff (d3 :: D3 | e) a
style = runFn3 styleImpl

foreign import appendImpl :: forall a e. Fn2 a String (Eff (d3 :: D3 | e) a)
append' :: forall a e. a -> String -> Eff (d3 :: D3 | e) a
append' = runFn2 appendImpl

foreign import dataImpl :: forall a b e. Fn2 a b (Eff (d3 :: D3 | e) a)
data' :: forall a b e. a -> b -> Eff (d3 :: D3 | e) a
data' = runFn2 dataImpl

foreign import enterImpl :: forall a e. Fn1 a (Eff (d3 :: D3 | e) a)
enter :: forall a e. a -> Eff (d3 :: D3 | e) a
enter = runFn1 enterImpl

foreign import textImpl :: forall a b c e. Fn2 a (Value b c) (Eff (d3 :: D3 | e) a)
text :: forall a b c e. a -> (Value b c) -> Eff (d3 :: D3 | e) a
text = runFn2 textImpl

foreign import selectAllImpl :: forall a e. Fn2 a String (Eff (d3 :: D3 | e) a)
selectAll :: forall a e. a -> String -> Eff (d3 :: D3 | e) a
selectAll = runFn2 selectAllImpl

foreign import mergeImpl :: forall a b e. Fn2 a b (Eff (d3 :: D3 | e) a)

merge :: forall a b e. a -> b -> Eff (d3 :: D3 | e) a
merge = runFn2 mergeImpl

foreign import removeImpl :: forall a e. Fn1 a (Eff (d3 :: D3 | e) a)

remove :: forall a e. a -> Eff (d3 :: D3 | e) a
remove = runFn1 removeImpl

instance showSelect :: Show a => Show (Selection a) where
  show x = "Selection" <> (show x)

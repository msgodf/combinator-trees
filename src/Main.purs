module Main where

import Base (D3,D3Eff)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)

data Value a = ValString String | ValFn (a -> a)

foreign import data Selection :: * -> *

-- TODO: select can actually take a string or a function, according to the docs
foreign import rootSelect :: String -> D3Eff (Selection Void)

foreign import rootSelectAll :: String -> D3Eff (Selection Void)

foreign import logAnythingImpl :: forall a e. (Fn1 a (Eff (console :: CONSOLE | e) Unit))
logAnything :: forall a e. a -> (Eff (console :: CONSOLE | e) Unit)
logAnything = (runFn1 logAnythingImpl)

foreign import attrImpl :: forall a e. Fn3 a String String (Eff (d3 :: D3 | e) a)
attr :: forall a e. a -> String -> String -> Eff (d3 :: D3 | e) a
attr = runFn3 attrImpl

foreign import appendImpl :: forall a e. Fn2 a String (Eff (d3 :: D3 | e) a)
append' :: forall a e. a -> String -> Eff (d3 :: D3 | e) a
append' = runFn2 appendImpl

foreign import dataImpl :: forall a b e. Fn2 a b (Eff (d3 :: D3 | e) a)
data' :: forall a b e. a -> b -> Eff (d3 :: D3 | e) a
data' = runFn2 dataImpl

foreign import enterImpl :: forall a e. Fn1 a (Eff (d3 :: D3 | e) a)
enter :: forall a e. a -> Eff (d3 :: D3 | e) a
enter = runFn1 enterImpl

foreign import textImpl :: forall a b e. Fn2 a (Value b) (Eff (d3 :: D3 | e) a)
text :: forall a b e. a -> (Value b) -> Eff (d3 :: D3 | e) a
text = runFn2 textImpl

foreign import selectAllImpl :: forall a e. Fn2 a String (Eff (d3 :: D3 | e) a)
selectAll :: forall a e. a -> String -> Eff (d3 :: D3 | e) a
selectAll = runFn2 selectAllImpl

instance showSelect :: Show a => Show (Selection a) where
  show x = "Selection" <> (show x)

main :: forall e. Eff (d3 :: D3, console :: CONSOLE | e) (Selection Void)
main = do
     root <- rootSelect "div.drawing"
     attr root "width" "600px"
     attr root "height" "300px"
     ee <- selectAll root "h1"
     w <- data' ee [1,2,3]
     v <- enter w
     o <- append' v "h1"
     text o (ValFn (\x -> x <> "o'clock"))

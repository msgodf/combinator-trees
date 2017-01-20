module Main ( main
            , drawTreeForExpression) where

import Parsing as P
import Render (drawTree, parseTreeToSimpleTree,treeData)
import D3.Base (D3)

import Prelude (Unit, bind, show, ($), (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Either (Either(..))
import Data.Foreign.Class (write)

-- Helper function that calls console.log with anything
foreign import logAnythingImpl :: forall a e. (Fn1 a (Eff (console :: CONSOLE | e) Unit))
logAnything :: forall a e. a -> (Eff (console :: CONSOLE | e) Unit)
logAnything = (runFn1 logAnythingImpl)

drawTreeForExpression :: forall e. String -> Eff (d3 :: D3, console :: CONSOLE | e) Unit
drawTreeForExpression x = case (P.parseTreeExpression x) of
  Left err -> log $ "Failed to parse expression " <> x <> ", because of error: " <> (show err)
  Right t -> do
                drawTree $ write $ parseTreeToSimpleTree t
                log "Parsed"


main :: forall a e. Eff (d3 :: D3, console :: CONSOLE | e) a
main = drawTree treeData

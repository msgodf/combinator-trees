module Main (main) where

import UI as UI
import Halogen as H

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Function.Uncurried (Fn1, runFn1)

-- Helper function that calls console.log with anything
foreign import logAnythingImpl :: forall a e. (Fn1 a (Eff (console :: CONSOLE | e) Unit))
logAnything :: forall a e. a -> (Eff (console :: CONSOLE | e) Unit)
logAnything = (runFn1 logAnythingImpl)

main :: forall e. Eff (H.HalogenEffects (UI.MyEffects e)) Unit
main = UI.main

module UI ( main
          , MyEffects) where

import Model (Tree(..), Symbol(..))
import Render (drawTree)
import Parsing (parseTreeExpression)
import D3.Base (D3)

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Foreign.Class (write)

import Control.Monad.Aff.Free (fromEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

type MyEffects eff = (console :: CONSOLE, d3 :: D3 | eff)

type State = { errorMessage :: Maybe String
             , tree :: Tree Symbol
             , expression :: String}

initialState :: State
initialState = { errorMessage: Nothing
               , tree: Leaf B
               , expression: ""}

reduceTree :: Tree Symbol -> Tree Symbol
reduceTree (Branch (Branch (Branch (Leaf B) x) y) z) = (Branch x (Branch y z)) -- Bluebird
reduceTree (Branch x y) = (Branch (reduceTree x) y)
reduceTree (Leaf x) = (Leaf x)

data Query a
  = ChangeExpression String a |
    ReduceTree a

ui :: forall e. H.Component State Query (Aff (MyEffects e))
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render n =
    HH.div_
      [ HH.div
        [ HP.class_ (HH.className "drawing") ] []
      , HH.p_
          [
            HH.input [ (HP.inputType HP.InputText)
                     , (HE.onValueInput (HE.input ChangeExpression))
                     , (HP.value n.expression)
                     ]
          , HH.button [
                       (HE.onClick (HE.input_ ReduceTree))] [HH.text "Reduce"]
          , HH.p [ HP.class_ (HH.className "error") ]
                 [ HH.text (maybe "" show n.errorMessage) ]
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (MyEffects e))
  eval (ChangeExpression value next) = do
    case (parseTreeExpression value) of
      Left parseError -> do
        H.modify (_ { errorMessage=Just $ show parseError,
                      expression=value
                    })
        pure next
      Right tree -> do
            H.modify (_ { errorMessage=Nothing,
                          tree=tree,
                          expression=value})
            fromEff $ drawTree $ write tree
            pure next
  eval (ReduceTree next) = do
    H.modify (\state -> state { expression=(show state.tree)
                              , tree=(reduceTree state.tree)})
    state <- H.get
    fromEff $ drawTree $ write state.tree
    pure next

main :: forall e. Eff (H.HalogenEffects (MyEffects e)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body

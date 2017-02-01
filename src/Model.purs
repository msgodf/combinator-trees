module Model where

import Prelude ((<>), bind, ($), pure, (<<<))
import Data.Show (class Show, show)
import Data.Either (Either(..))
import Data.Array (length)
import Data.Foreign (ForeignError(..), writeObject, fail)
import Data.Foreign.Class (class AsForeign, class IsForeign, (.=), readProp, write)
import Data.String (singleton)
import Control.Monad.Except (runExcept)

data Symbol = B | Variable Char

instance showSymbol :: Show Symbol where
  show B = "B"
  show (Variable x) = singleton x

instance asForeignSymbol :: AsForeign Symbol where
  write = write <<< show

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance showSymbolTree :: Show (Tree Symbol) where
  show (Leaf x) = show x
  show (Branch x y) = "(" <> (show x) <> (show y) <> ")"

instance asForeignTree :: AsForeign a => AsForeign (Tree a) where
  write (Leaf name) = writeObject ["name" .= name]
  write (Branch left right) = writeObject [ "name" .= ""
                                           ,"children" .= [left, right]]

instance isForeignTree :: IsForeign a => IsForeign (Tree a) where
  read x = do
    name <- readProp "name" x
    case (runExcept (readProp "children" x)) of
      Left _ -> pure $ Leaf name
      Right [left,right] -> pure $ Branch left right
      Right xs -> fail $ TypeMismatch "Array with two elements"
                                      ("Array with " <> (show $ length xs) <> " elements")

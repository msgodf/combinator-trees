module Parsing ( Tree(..)
               , Symbol
               , parseTreeExpression) where

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.Show (class Show)
import Data.String (toCharArray)
import Prelude ((<$), (<$>), (<>), ($), flip, map, show, pure)
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Combinators (between, chainl1, choice)
import Text.Parsing.Parser.String (char, string, class StringLike)

data Symbol = B | Variable Char

instance showSymbol :: Show Symbol where
  show B = "B"
  show (Variable x) = show x

bluebirdp :: Parser String Symbol
bluebirdp = B <$ (string "B")

variablep :: forall a. StringLike a => Parser a Symbol
variablep = Variable <$>
            (choice $
             map char $
             toCharArray "xyzwuvstpqmnlkji")

symbolp :: Parser String Symbol
symbolp = bluebirdp <|> variablep

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance showSymbolTree :: Show (Tree Symbol) where
  show (Leaf x) = show x
  show (Branch x y) = "(" <> (show x) <> (show y) <> ")"

leafParser :: Parser String (Tree Symbol)
leafParser = Leaf <$> symbolp

branchParser :: Parser String (Tree Symbol)
branchParser = fix (\p -> between (char '(')
                                  (char ')')
                                  (chainl1 (p <|> leafParser)
                                           (pure Branch)))

parseTreeExpression :: String -> Either ParseError (Tree Symbol)
parseTreeExpression = flip runParser
                           (chainl1 (branchParser <|> leafParser)
                                    (pure Branch))

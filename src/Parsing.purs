module Parsing (parseTreeExpression) where

import Model (Symbol(..), Tree(..))

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)

import Data.String (toCharArray)
import Prelude ((<$), (<$>), ($), flip, map, pure)
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Combinators (between, chainl1, choice)
import Text.Parsing.Parser.String (char, string, class StringLike)

bluebirdp :: Parser String Symbol
bluebirdp = B <$ (string "B")

variablep :: forall a. StringLike a => Parser a Symbol
variablep = Variable <$>
            (choice $
             map char $
             toCharArray "xyzwuvstpqmnlkji")

symbolp :: Parser String Symbol
symbolp = bluebirdp <|> variablep

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

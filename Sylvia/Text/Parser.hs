-- |
-- Module      : Sylvia.Text.Parser
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable

module Sylvia.Text.Parser
    (
      parseExp
    , ParseError
    ) where

import Control.Applicative
import Text.Parsec hiding ( (<|>), many )
import Sylvia.Model

parseExp :: String -> Either ParseError (Exp Integer)
parseExp = parse expression' "sylvia"
  where
    expression' = expression <* spaces -- Skip trailing spaces

type Parser = Parsec String ()

expression, term, bracketed, reference, abstraction :: Parser (Exp Integer)

expression = foldl1 App <$> many1 term

term = try spaces *> (bracketed <|> reference <|> abstraction)

bracketed = char '(' *> expression <* spaces <* char ')'
    <?> "bracketed term"

reference = Ref . read <$> many1 digit
    <?> "de Bruijn index"

abstraction = abstractIndex <$> (char '\\' *> expression)
    <?> "lambda abstraction"

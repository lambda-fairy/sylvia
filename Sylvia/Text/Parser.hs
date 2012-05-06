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
import Data.Void ( Void )
import Text.Parsec hiding ( (<|>), many )
import Sylvia.Model

parseExp :: String -> Either ParseError (Exp Void)
parseExp = parse expression' "sylvia"
  where
    expression' = verify' <$> expression 0 <* spaces -- Skip trailing spaces

type Parser = Parsec String ()

expression, term, bracketed, reference, abstraction :: Integer -> Parser (Exp Integer)

expression i = foldl1 App <$> many1 (term i)

term i = try spaces *> (bracketed i <|> reference i <|> abstraction i)

bracketed i = char '(' *> expression i <* spaces <* char ')'
    <?> "bracketed term"

reference i = Ref <$> (checkInRange =<< read <$> many1 digit)
    <?> "de Bruijn index"
  where
    checkInRange index
      | index < i = return index
      | otherwise = fail $ "index " ++ show index ++ " out of range"

abstraction i = abstractIndex <$> (char '\\' *> expression (i+1))
    <?> "lambda abstraction"

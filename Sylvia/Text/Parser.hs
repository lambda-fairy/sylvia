-- |
-- Module      : Sylvia.Text.Parser
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- Basic parser for zero-based de Bruijn index notation.
--
-- Here are some expressions that parse:
--
-- > identity: \ 0
-- > omega:    (\ 0 0) (\ 0 0)
-- > starling: \\ 2 0 (1 0)
-- > fixpoint: \ (\ 1 (0 0)) (\ 1 (0 0))
--
-- All variables must have a corresponding lambda, i.e. be bound
-- variables. This means expressions such as @\\ 9001@ will be rejected.

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

expression, term, bracketed, reference, abstraction
    :: Integer -- ^ The number of variables in scope.
    -> Parser (Exp Integer)

expression i = foldl1 App <$> many1 (term i)

term i = try spaces
    *> (bracketed i
    <|> reference i
    <|> abstraction i)

bracketed i = char '(' *> expression i <* spaces <* char ')'
    <?> "bracketed term"

reference i = Ref <$> (checkInRange =<< read <$> many1 digit)
    <?> "de Bruijn index"
  where
    -- Reject an index if it refers to a free variable
    checkInRange index
      | index < i = return index
      | otherwise = parserFail $ "index " ++ show index ++ " out of range"

abstraction i = abstract shiftUp <$> (oneOf "L\\" *> expression (i+1))
    <?> "lambda abstraction"

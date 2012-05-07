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
-- variables. This means expressions such as @\ 9000@ will be rejected.

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

-- Note:
-- That mysterious extra Integer argument is supposed to represent the
-- number of variables in scope. When the parser hits a lambda
-- abstraction it increments the value.

-- An expression is a list of terms, applied from left to right.
expression i = foldl1 App <$> many1 (term i)

-- A term is either...
term i = try spaces    -- (skipping leading spaces first, of course)
    *> (bracketed i    -- a parenthesized expression;
    <|> reference i    -- a variable reference;
    <|> abstraction i) -- or a lambda abstraction.

-- A bracketed expression: left bracket, expression, right bracket.
bracketed i = char '(' *> expression i <* spaces <* char ')'
    <?> "bracketed term"

-- A variable reference: read in a whole number, checking if it's a
-- valid index (see note).
reference i = Ref <$> (checkInRange =<< read <$> many1 digit)
    <?> "de Bruijn index"
  where
    checkInRange index
      | index < i = return index
      | otherwise = fail $ "index " ++ show index ++ " out of range"

-- A lambda expression: a backslash, followed by the function body.
abstraction i = abstractIndex <$> (char '\\' *> expression (i+1))
    <?> "lambda abstraction"

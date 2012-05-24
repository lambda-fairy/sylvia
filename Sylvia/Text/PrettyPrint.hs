-- |
-- Module      : Sylvia.Text.PrettyPrint
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- Convert an expression into a form that can be parsed by
-- "Sylvia.Text.Parser".

module Sylvia.Text.PrettyPrint
    (
      pprintExp
    ) where

import Data.Void ( Void, vacuous )
import Sylvia.Model

pprintExp :: Exp Void -> String
pprintExp = ($ "") . pprint . vacuous

pprint :: Exp Integer -> ShowS
pprint e = case e of
    Ref a   -> shows a
    Lam e'  -> str "(\\" . stackLams (flatten e') . str ")"
    App a b -> pprint a . str " " . case b of
        -- The right term only needs to be bracketed if it contains a
        -- function application
        App _ _ -> str "(" . pprint b . str ")"
        _       -> pprint b

-- | Allow consecutive lambda symbols without adding extra brackets.
stackLams :: Exp Integer -> ShowS
stackLams e = case e of
    Lam e' -> str "\\" . stackLams (flatten e')
    _      -> str " "  . pprint e

flatten :: Exp (Inc Integer) -> Exp Integer
flatten = fmap shiftDown

str :: String -> ShowS
str = (++)

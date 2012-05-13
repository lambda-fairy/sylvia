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
pprint exp = case exp of
    Ref a   -> shows a
    Lam e   -> str "(\\ " . pprint (flatten e) . str ")"
    App a b -> str "(". pprint a . str ") (" . pprint b . str ")"

flatten :: Exp (Inc Integer) -> Exp Integer
flatten = fmap shiftDown

str :: String -> ShowS
str = (++)

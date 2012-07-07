-- |
-- Module      : Sylvia.Renderer.Rhyme
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- Platform agnostic functions for calculating the rhyme.

module Sylvia.Renderer.Rhyme
    (
      Rhyme
    , rhyme
    ) where

import Sylvia.Model
import Sylvia.Renderer.Pair

type Rhyme = [Int]

rhyme :: Exp Int -> Rhyme
rhyme = ($ []) . go
  where
    go :: Exp Int -> ([Int] -> [Int])
    go e = case e of
        Ref x   -> (x:)
        Lam _   -> error "rhyme: lambdas not implemented"
        App a b -> go a . go b

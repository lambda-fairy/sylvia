-- |
-- Module      : Sylvia.Renderer.Core
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- Platform independent functions for calculating rhyme and rhythm.

module Sylvia.Renderer.Core
    (
    -- * Rhyme
      Rhyme
    , mkRhyme

    -- * Rhythm
    , Rhythm
    , mkRhythm
    ) where

import Sylvia.Model

-- | The list of variables referenced in an expression, from left to right.
type Rhyme = [Int]

-- | Perform a pre-order traversal of the expression tree, collecting
-- the indices on the way.
mkRhyme :: Exp Int -> Rhyme
mkRhyme = ($ []) . go
  where
    go :: Exp Int -> ([Int] -> [Int])
    go e = case e of
        Ref x   -> (x:)
        Lam _   -> error "rhyme: lambdas not implemented"
        App a b -> go a . go b

-- | The order of applications in an expression.
type Rhythm = [Int]

-- | Enumerate all the branches of the tree from bottom to top.
mkRhythm :: Exp Int -> Rhythm
mkRhythm e = case e of
    Ref _   -> []
    Lam _   -> error "rhythm: lambdas not implemented"
    App a b -> left ++ right ++ middle
      where
        left = mkRhythm a
        offset = length $ mkRhyme a
        middle = [offset - 1]
        right = map (+ offset) $ mkRhythm b

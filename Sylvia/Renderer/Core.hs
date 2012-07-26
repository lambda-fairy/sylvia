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
    , rhyme

    -- * Rhythm
    , Rhythm
    , rhythm
    ) where

import Sylvia.Model

-- | The list of variables referenced in an expression, from left to right.
type Rhyme = [Int]

-- | Perform a pre-order traversal of the expression tree, collecting
-- the indices on the way.
rhyme :: Exp Int -> Rhyme
rhyme = ($ []) . go
  where
    go :: Exp Int -> ([Int] -> [Int])
    go e = case e of
        Ref x   -> (x:)
        Lam _   -> error "rhyme: lambdas not implemented"
        App a b -> go a . go b

-- | The order of applications in an expression.
type Rhythm = [Int]

-- | Enumerate all the branches of the tree from bottom to top.
rhythm :: Exp Int -> Rhythm
rhythm e = case e of
    Ref _   -> []
    Lam _   -> error "rhythm: lambdas not implemented"
    App a b -> left ++ right ++ middle
      where
        left = rhythm a
        offset = rhymeSize a
        middle = [offset - 1]
        right = map (+ offset) $ rhythm b

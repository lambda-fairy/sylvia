-- |
-- Module      : Sylvia.Renderer.Impl
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- This module provides two things:
--
-- 1. An interface, 'RenderImpl', that all rendering methods must
--    implement.
--
-- 2. A menagerie of functions that use this interface.

module Sylvia.Renderer.Impl
    (
    -- * Interface
      RenderImpl(..)

    -- * Menagerie
    , renderRhyme
    , renderRhyme'
    ) where

import Data.Monoid

import Sylvia.Model
import Sylvia.Renderer.Pair

-- | An action that yields an image.
--
-- 'mempty' should yield an empty image and 'mappend' should stack two
-- images together.
class Monoid r => RenderImpl r where
    -- | Draw a line segment from one point to another.
    drawLine :: PInt -> PInt -> r

    -- | Draw a small circle centered at a point.
    drawDot :: PInt -> r

    -- | Translate the given image by a vector.
    relativeTo :: PInt -> r -> r

renderRhyme :: RenderImpl r => Exp Int -> r
renderRhyme = renderRhyme' . rhyme

rhyme :: Exp Int -> [Int]
rhyme = ($ []) . go
  where
    go :: Exp Int -> ([Int] -> [Int])
    go e = case e of
        Ref x   -> (x:)
        Lam _   -> error "rhyme: lambdas not implemented"
        App a b -> go a . go b

renderRhyme' :: RenderImpl r => [Int] -> r
renderRhyme' rs = mconcat $ zipWith renderOne rs [0..]
  where
    renderOne src dest = drawLine (0 :| -src) (1 :| dest - height + 1)
    height = length rs

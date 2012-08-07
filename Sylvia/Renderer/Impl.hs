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

import Control.Monad ( zipWithM_ )

import Sylvia.Renderer.Pair
import Sylvia.Renderer.Rhyme

-- | An implementation of a renderer.
class Monad m => RenderImpl m where
    -- | Draw a line segment from one point to another.
    drawLine :: PInt -> PInt -> m ()

    -- | Translate the given image by a vector.
    relativeTo :: PInt -> m a -> m a

renderRhyme :: RenderImpl m => Exp Int -> m ()
renderRhyme = renderRhyme' . rhyme

renderRhyme' :: RenderImpl m => Rhyme -> m ()
renderRhyme' rs = zipWithM_ renderOne rs [0..]
  where
    renderOne src dest = drawLine (0 :| -src) (1 :| dest - height + 1)
    height = length rs

-- |
-- Module      : Sylvia.Renderer.Impl
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable

module Sylvia.Renderer.Impl
    (
      RenderImpl(..)
    , renderRhyme
    ) where

import Control.Monad ( zipWithM_ )

import Sylvia.Renderer.Pair
import Sylvia.Renderer.Rhyme

-- | An implementation of a renderer.
class Monad m => RenderImpl m where
    -- | Draw a line segment from one point to another.
    drawLine :: PInt -> PInt -> m ()

    -- | Translate the image so its origin is at a certain point.
    relativeTo :: PInt -> m a -> m a

renderRhyme :: RenderImpl m => Rhyme -> m ()
renderRhyme rs = zipWithM_ renderOne rs [0..]
  where
    renderOne src dest = drawLine (0 :| -src) (1 :| dest - rhymeSize + 1)
    rhymeSize = length rs

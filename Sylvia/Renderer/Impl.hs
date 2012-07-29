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
    , renderAll
    , renderRhyme
    , renderRhythm
    ) where

import Control.Monad ( zipWithM_ )
import Data.List ( mapAccumL )
import Data.Maybe ( fromMaybe )
import qualified Data.Set as S

import Sylvia.Renderer.Core
import Sylvia.Renderer.Pair

-- | An implementation of a renderer.
class Monad m => RenderImpl m where
    -- | Draw a line segment from one point to another.
    drawLine :: PInt -> PInt -> m ()

    -- | Draw a small circle centered at a point.
    drawDot :: PInt -> m ()

    -- | Translate the given image by a vector.
    relativeTo :: PInt -> m a -> m a

renderAll :: RenderImpl m => RenderSpec -> m ()
renderAll rs = do
    renderRhyme (rhyme rs)
    renderRhythm (rhyme rs) (rhythm rs)

renderRhyme :: RenderImpl m => Rhyme -> m ()
renderRhyme ry = zipWithM_ renderOne ry [0..]
  where
    renderOne src dest = drawLine (0 :| -src) (1 :| dest - size + 1)
    size = length ry

renderRhythm :: RenderImpl m => Rhyme -> Rhythm -> m ()
renderRhythm ry = relativeTo (1 :| (-size) + 1) . squashAll
  where
    squashAll rt
      = let
            (endState, actionList)
                = mapAccumL squash (S.fromList [0 .. size-1])
                    $ zip [1..] rt
        in if S.size endState == 1
            then sequence_ actionList
                    >> drawLine (0 :| size-1) (S.findMin endState + 1 :| size-1)
            else error "renderRhythm: invalid rhythm"

    squash state (destX, srcY) = (state', rhymeLine >> verticalLine >> dot)
      where
        rhymeLine = drawLine (0 :| srcY) (destX :| srcY)
        verticalLine = drawLine (destX :| srcY) (destX :| destY)
        dot = drawDot (destX :| destY)
        destY = fromMaybe (error "renderRhythm: no lines to squash into")
                    $ lookupGT srcY state
        state' = S.delete srcY state

    size = length ry

-- | Find the first element in the set that is greater than the specified
-- value. If there are no elements that match, return 'Nothing'.
lookupGT :: Ord a => a -> S.Set a -> Maybe a
lookupGT x = minimum' . filterGT
  where
    minimum' = fmap fst . S.minView
    filterGT = snd . S.split x

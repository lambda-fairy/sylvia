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
    , renderRhythm
    ) where

import Data.Foldable ( foldMap )
import Data.Monoid

import Sylvia.Model
import Sylvia.Renderer.Pair

-- | An action that yields an image.
--
-- 'mempty' should yield an empty image and 'mappend' should stack two
-- images together.
class Monoid r => RenderImpl r where
    -- | Draw a dotted rectangle.
    drawBox :: PInt -- ^ Corner position
            -> PInt -- ^ Size
            -> r

    -- | Draw a line segment from one point to another.
    drawLine :: PInt -> PInt -> r

    -- | Draw a small circle centered at a point.
    drawDot :: PInt -> r

    -- | Translate the given image by a vector.
    relativeTo :: PInt -> r -> r

type Rhyme = [RhymeUnit]

-- | Specifies a /rhyme line/: a straight line protruding from the left
-- edge of a bounding box, connecting a variable to the sub-expression
-- that uses it.
--
-- @RhymeUnit index dest@ will be rendered as a line from (0, -index) to
-- (1, dest).
data RhymeUnit = RhymeUnit
    { _ruIndex :: Integer
    , _ruDest :: Int
    }

renderRhyme :: RenderImpl r => Rhyme -> r
renderRhyme = foldMap renderOne
  where
    renderOne (RhymeUnit src dest) = drawLine (0 :| fromInteger (-src)) (1 :| dest)

renderRhythm :: RenderImpl r => Exp Integer -> (r, PInt)
renderRhythm e = case e of
    Ref _   -> (mempty, 0 :| 0)
    Lam e'  -> (image, size)
      where
        (image, size') = renderRhythm $ fmap shiftDown e'
        size = size' |+| (2 :| 2)
    App a b -> (image, size)
      where
        image = mconcat $
            -- Two sub-expressions
            [ relativeTo (-1 :| aOffset) aImage
            , relativeTo (-1 :|       0) bImage
            -- Horizontal throat lines coming out of the sub-expressions
            , drawLine (-1 :| aOffset) (0 :| aOffset)
            , drawLine (-1 :|       0) (0 :|       0)
            -- Vertical line connecting the two
            , drawLine (0 :| aOffset) (0 :| 0)
            -- Application dot
            , drawDot (0 :| 0)
            ]
        (aImage, (aWidth :| aHeight)) = renderRhythm a
        (bImage, (bWidth :| bHeight)) = renderRhythm b
        aOffset = (-bHeight) - 1
        size = (max aWidth bWidth + 1) :| (aHeight + bHeight + 1)

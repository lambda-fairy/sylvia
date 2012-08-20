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
    , RhymeUnit(..)
    , renderRhythm
    , Result(..)
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
    { ruIndex :: Integer
    , ruDest  :: Int
    }
  deriving (Show)

renderRhyme :: RenderImpl r => Rhyme -> r
renderRhyme = foldMap renderOne
  where
    renderOne (RhymeUnit src dest) = drawLine (0 :| fromInteger (-src)) (1 :| dest)

data Result r = Result
    { resultImage :: r
    , resultSize  :: PInt
    , resultRhyme :: Rhyme
    }
  deriving (Show)

renderRhythm :: RenderImpl r => Exp Integer -> Result r
renderRhythm e = case e of
    Ref x   -> Result mempty (0 :| 0) [RhymeUnit x 0]
    Lam e'  -> Result image size rhyme
      where
        Result image size' rhyme = renderRhythm $ fmap shiftDown e'
        size = size' |+| (2 :| 2)
    App a b -> Result image size rhyme
      where
        image = mconcat $
            -- Draw the two sub-expressions
            [ aImage
            , bImage
            -- Horizontal throat lines coming out of the sub-expressions
            , drawLine (-1 :| aOffset) (0 :| aOffset)
            , drawLine (-1 :|       0) (0 :|       0)
            -- Connect them with a vertical line
            , drawLine (0 :| aOffset) (0 :| 0)
            -- Application dot
            , drawDot (0 :| 0)
            ]
        Result aImage (aWidth :| aHeight) aRhyme = relativeTo' (-1 :| aOffset) $ renderRhythm a
        Result bImage (bWidth :| bHeight) bRhyme = relativeTo' (-1 :|       0) $ renderRhythm b
        aOffset = (-bHeight) - 1
        size = (max aWidth bWidth + 1) :| (aHeight + bHeight + 1)
        rhyme = aRhyme ++ bRhyme

relativeTo' :: RenderImpl r => PInt -> Result r -> Result r
relativeTo' offset@(_ :| offsetY) (Result image size rhyme)
    = Result image' size rhyme'
  where
    image' = relativeTo offset image
    rhyme' = map (shiftRhyme offsetY) rhyme

    shiftRhyme :: Int -> RhymeUnit -> RhymeUnit
    shiftRhyme dy (RhymeUnit src dest) = RhymeUnit src (dest + dy)

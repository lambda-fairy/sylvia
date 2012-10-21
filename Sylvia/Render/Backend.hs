-- |
-- Module      : Sylvia.Render.Backend
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- This module provides an interface, 'Backend', that all rendering
-- methods must implement.

module Sylvia.Render.Backend
    (
    -- * An interface
      Backend(..)

    -- * Helper functions
    , drawDot
    , drawBox
    , stackHorizontally
    ) where

import Data.Monoid

import Sylvia.Render.Pair

-- | An action that yields an image.
--
-- 'mempty' should yield an empty image and 'mappend' should stack two
-- images together.
class Monoid r => Backend r where
    -- | Draw a dotted rectangle.
    drawDottedRectangle
        :: PInt -- ^ Corner position
        -> PInt -- ^ Size
        -> r

    -- | Draw a line segment from one point to another.
    drawLine :: PInt -> PInt -> r

    -- | Draw a line, but instead of drawing a diagonal line, draw a
    -- zigzag instead.
    drawZigzag :: PInt -> PInt -> r

    -- | Draw a simple circle segment, centered at a point.
    drawCircleSegment
        :: PInt   -- ^ Center point
        -> Double -- ^ Start angle, in radians
        -> Double -- ^ End angle, also in radians. Radians are cool.
        -> r

    -- | Translate the given image by a vector.
    relativeTo :: PInt -> r -> r

-- | Draw a full circle, centered at a point.
drawDot :: Backend r => PInt -> r
drawDot center = drawCircleSegment center 0 (2 * pi)

-- | Draw a box, complete with a throat and ear.
drawBox
    :: Backend r
    => PInt -- ^ Top-left corner point
    -> PInt -- ^ Size
    -> Int  -- ^ Y offset of ear and throat
    -> r
drawBox corner size throatY
    =  drawDottedRectangle corner size
    <> drawCircleSegment (corner |+| (    0 :| height + throatY)) (1 * rightAngle) (3 * rightAngle)
    <> drawCircleSegment (corner |+| (width :| height + throatY)) (3 * rightAngle) (1 * rightAngle)
  where
    width :| height = size
    rightAngle = pi / 2

-- | Take a list of images and line them up in a row.
stackHorizontally :: Backend r => [(r, PInt)] -> (r, PInt)
stackHorizontally = foldr step (mempty, 0 :| 0)
  where
    step (image', (w' :| h')) (image, (w :| h))
        = (relativeTo ((-w - 1) :| 0) image' <> image, (w + w' + 2) :| max h h')

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

import Control.Applicative
import Data.Foldable ( foldMap )
import Data.Monoid
import Debug.Trace

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
-- (1, dest), where (0, 0) is the position of the outer box's ear.
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
      -- ^ The rendered image.
    , resultSize  :: PInt
      -- ^ The size of the image's bounding box.
    , resultRhyme :: Rhyme
      -- ^ The expression's rhyme.
    , resultThroatY :: Int
      -- ^ The Y offset of the expression's ear and throat.
    }
  deriving (Show)

renderRhythm :: RenderImpl r => Exp Integer -> Result r
renderRhythm e = case e of
    Ref x   -> Result mempty (0 :| 0) [RhymeUnit x 0] 0
    Lam e'  -> renderLambda e'
    App a b -> Result image size rhyme bThroatY
      where
        image = mconcat $
            -- Draw the two sub-expressions
            [ aImage
            , bImage
            -- Extend the shorter sub-expression so it matches up with
            -- the bigger one
            , extendRhyme (-aWidth) (-bWidth) bRhyme
            -- Connect them with a vertical line
            , drawLine (0 :| aThroatY) (0 :| bThroatY)
            -- Application dot
            , drawDot (0 :| bThroatY)
            ]
        Result aImage aSize@(aWidth :| aHeight) aRhyme aThroatY
            = shiftY (-1 - bHeight) $ renderWithThroat bWidth a
        Result bImage bSize@(bWidth :| bHeight) bRhyme bThroatY
            = renderWithThroat 1 b
        size = (aWidth :| aHeight + bHeight + 1)
        rhyme = aRhyme ++ bRhyme

-- | Render an expression with a horizontal line sticking out of its
-- throat. Doesn't sound too comfortable, to be honest.
--
-- The 'resultSize' includes the length of this extra line.
renderWithThroat
    :: RenderImpl r
    => Int -- ^ Length of the throat line. This should be positive.
    -> Exp Integer -> Result r
renderWithThroat throatLength e = Result image' size' rhyme throatY
  where
    Result image size rhyme throatY = renderRhythm e
    -- Shift the main image to the left, then draw a line next to it
    image' = relativeTo (-throatLength :| 0) image <> throatLine
    throatLine = drawLine (-throatLength :| throatY) (0 :| throatY)
    size' = size |+| (throatLength :| 0)

-- | Render a lambda expression.
renderLambda :: RenderImpl r => Exp (Inc Integer) -> Result r
renderLambda e' = Result image size rhyme throatY
  where
    image = drawBox (0 :| 0) (negateP size) <> image'
    Result image' size' rhyme throatY = shiftY (-1) . renderWithThroat 1 $ fmap shiftDown e'
    size = size' |+| (1 :| 2)

-- | Shift an image vertically by a specified amount, changing the rhyme
-- and throat position to compensate.
shiftY :: RenderImpl r => Int -> Result r -> Result r
shiftY dy (Result image size rhyme throatY)
    = Result image' size rhyme' throatY'
  where
    image' = relativeTo (0 :| dy) image
    rhyme' = map shiftRhyme rhyme
    throatY' = throatY + dy

    shiftRhyme :: RhymeUnit -> RhymeUnit
    shiftRhyme (RhymeUnit src dest) = RhymeUnit src (dest + dy)

extendRhyme :: RenderImpl r => Int -> Int -> Rhyme -> r
extendRhyme srcX destX = foldMap $ drawLine
                                    <$> (srcX  :|) . ruDest
                                    <*> (destX :|) . ruDest

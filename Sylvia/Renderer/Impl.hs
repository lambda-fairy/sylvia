-- |
-- Module      : Sylvia.Renderer.Impl
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- This module provides three things:
--
-- 1. An interface, 'RenderImpl', that all rendering methods must
--    implement;
--
-- 2. A function, 'render', that uses the aforementioned interface to
--    draw a pretty picture;
--
-- 3. Another function, 'render'', that spews its internals all over the
--    place.

module Sylvia.Renderer.Impl
    (
    -- * An interface
      RenderImpl(..)

    -- * A function
    , render

    -- * Another function
    , render'
    , Result(..)
    , Rhyme
    , RhymeUnit(..)

    -- * Miscellany
    , stackHorizontally
    ) where

import Control.Applicative
import Data.Foldable ( foldMap )
import Data.List ( foldl' )
import Data.Monoid
import Data.Void ( Void, vacuous )

import Sylvia.Model
import Sylvia.Renderer.Pair

-- | An action that yields an image.
--
-- 'mempty' should yield an empty image and 'mappend' should stack two
-- images together.
class Monoid r => RenderImpl r where
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
drawDot :: RenderImpl r => PInt -> r
drawDot center = drawCircleSegment center 0 (2 * pi)

-- | Draw a box, complete with a throat and ear.
drawBox
    :: RenderImpl r
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

type Rhyme = [RhymeUnit]

-- | Specifies a /rhyme line/: a straight line protruding from the left
-- edge of a bounding box, connecting a variable to the sub-expression
-- that uses it.
data RhymeUnit = RhymeUnit
    { ruIndex :: Integer
    , ruDest  :: Int
    }
  deriving (Show)

-- | The result of a rendering operation.
data Result r = Result
    { resultImage :: r
      -- ^ The rendered image.
    , resultSize  :: PInt
      -- ^ The size of the image's bounding box in grid units, when all
      -- round things are removed.
    , resultRhyme :: Rhyme
      -- ^ The expression's rhyme.
    , resultThroatY :: Int
      -- ^ The Y offset of the expression's ear and throat, measured
      -- from the /bottom/ of its bounding box.
    }
  deriving (Show)

-- | Render an expression, returning an image along with its size.
render :: RenderImpl r => Exp Void -> (r, PInt)
render e =
    let Result image size rhyme _ = render' $ vacuous e
    in case rhyme of
        [] -> (image, size)
        _  -> error $ "render: the impossible happened -- "
                        ++ "extra free variables: " ++ show rhyme

-- | Render an expression, with extra juicy options.
render' :: RenderImpl r => Exp Integer -> Result r
render' e = case e of
    Ref x   -> Result mempty (0 :| 0) [RhymeUnit x 0] 0
    Lam e'  -> renderLambda e'
    App a b -> case a of
        Lam _ -> renderBeside a b
        _     -> renderAtop a b

renderBeside :: RenderImpl r => Exp Integer -> Exp Integer -> Result r
renderBeside a b = Result image size rhyme aThroatY
  where
    image = mconcat $
        [ aImage
        , bImage
        ]
    (Result aImage (aWidth :| aHeight) aRhyme aThroatY,
     Result bImage (bWidth :| bHeight) bRhyme bThroatY)
        = alignThroats
            (render' a)
            (shiftX (-aWidth) $ renderWithThroatLine False 1 b)
    size = (aWidth + bWidth :| max aHeight bHeight)
    rhyme = aRhyme ++ bRhyme

alignThroats :: RenderImpl r => Result r -> Result r -> (Result r, Result r)
alignThroats
    a@(Result _ aSize _ aThroatY)
    b@(Result _ bSize _ bThroatY)
    = (shiftY (minThroatY - aThroatY) a,
       shiftY (minThroatY - bThroatY) b)
  where
    minThroatY = min aThroatY bThroatY

renderAtop :: RenderImpl r => Exp Integer -> Exp Integer -> Result r
renderAtop a b = Result image size rhyme bThroatY
  where
    image = mconcat $
        -- Draw the two sub-expressions
        [ aImage
        , bImage
        -- Extend the lower sub-expression so it matches up with the
        -- bigger one
        , relativeTo (-bWidth :| 0)
            $ extendAcross (bWidth - aWidth) bRhyme
        -- Connect them with a vertical line
        , drawLine (0 :| aThroatY) (0 :| bThroatY)
        -- Application dot
        , drawDot (0 :| bThroatY)
        ]
    Result aImage (aWidth :| aHeight) aRhyme aThroatY
        = shiftY (-1 - bHeight) $ renderWithThroatLine False bWidth a
    Result bImage (bWidth :| bHeight) bRhyme bThroatY
        = renderWithThroatLine False 1 b
    size = (aWidth :| aHeight + bHeight + 1)
    rhyme = aRhyme ++ bRhyme

-- | Render an expression with a horizontal line sticking out of its
-- throat. Doesn't sound too comfortable, to be honest.
--
-- The 'resultSize' includes the length of this extra line.
renderWithThroatLine
    :: RenderImpl r
    => Bool -- ^ Whether the enclosing expression is a lambda.
    -> Int  -- ^ Length of the throat line. This should be positive.
    -> Exp Integer -> Result r
renderWithThroatLine outerIsLam lineLength e = Result image size rhyme throatY
  where
    Result image' size' rhyme throatY' = render' e
    -- Shift the main image to the left, then draw a line next to it
    image = relativeTo (-lineLength :| 0) image' <> throatLine
    throatLine = drawZigzag (-lineLength :| throatY') (0 :| throatY)
    throatY = if outerIsLam && containsLam e then throatY' - 1 else throatY'
    size = size' |+| (lineLength :| 0)

-- | Return whether there is a nested box touching the bottom edge of the
-- bounding box. If True, it means that everything in the image should
-- be shifted down one unit.
containsLam :: Exp a -> Bool
containsLam e = case e of
    Ref _ -> False
    Lam _ -> True
    App _ b -> containsLam b

-- | Render a lambda expression.
renderLambda :: RenderImpl r => Exp (Inc Integer) -> Result r
renderLambda e' = Result image size rhyme throatY
  where
    Result image' (innerWidth :| innerHeight) innerRhyme throatY
        = shiftY (-1) . renderWithThroatLine True 1 $ fmap shiftDown e'
    image = drawBox (negateP size) size throatY
            <> relativeTo (-width :| 0) rhymeImage
            <> image'
    (rhymeImage, rhyme) = renderRhyme throatY innerRhyme
    rhymeHeight = fromInteger . maximumOr 0 $ map ruIndex innerRhyme
    size@(width :| _) = (innerWidth + 1 :| (max innerHeight rhymeHeight) + 2)

-- | Like 'maximum', but returns a default value on an empty list rather
-- than throwing a hissy fit.
maximumOr :: Ord a => a -> [a] -> a
maximumOr def = foldl' max def

-- | Render an expression's rhyme.
renderRhyme
    :: RenderImpl r
    => Int        -- ^ Throat offset (see 'resultThroatY')
    -> Rhyme      -- ^ The inner expression's rhyme
    -> (r, Rhyme) -- ^ The resulting image, along with the outer rhyme
renderRhyme throatY innerRhyme = (foldMap renderOne innerRhyme, outerRhyme)
  where
    renderOne (RhymeUnit index dest) = drawLine (0 :| throatY - fromInteger index) (1 :| dest)
    outerRhyme =
        [ RhymeUnit (pred index) (throatY - fromInteger index)
        | RhymeUnit index _ <- innerRhyme
        , index > 0
        ]

-- | Shift an image horizontally by a specified amount.
shiftX :: RenderImpl r => Int -> Result r -> Result r
shiftX dx (Result image size rhyme throatY)
    = Result image' size rhyme throatY
  where
    image' = relativeTo (dx :| 0) image

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
    shiftRhyme (RhymeUnit index dest) = RhymeUnit index (dest + dy)

extendAcross :: RenderImpl r => Int -> Rhyme -> r
extendAcross dx = foldMap $ drawLine
                                <$> ( 0 :|) . ruDest
                                <*> (dx :|) . ruDest

-- | Take a list of images and line them up in a row.
stackHorizontally :: RenderImpl r => [(r, PInt)] -> (r, PInt)
stackHorizontally = foldr step (mempty, 0 :| 0)
  where
    step (image', (w' :| h')) (image, (w :| h))
        = (relativeTo ((-w - 1) :| 0) image' <> image, (w + w' + 2) :| max h h')

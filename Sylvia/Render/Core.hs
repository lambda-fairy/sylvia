{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Sylvia.Render.Core
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- This module provides the algorithm used to render expressions. It can
-- be called in two ways:
--
-- 1. A function, 'render', that uses "Sylvia.Render.Backend" to draw a
--    pretty picture;
--
-- 2. Another function, 'render'', that spews its internals all over the
--    place.

module Sylvia.Render.Core
    (
    -- * A function
      render

    -- * Another function
    , render'

    -- ** The result type
    , Result()
    , resultImage
    , resultSize
    , resultRhyme
    , resultThroatY

    -- ** Internal bits and pieces
    , Rhyme
    , RhymeUnit()
    , ruIndex
    , ruDest
    ) where

import Control.Applicative
import Data.Foldable (foldMap)
import Data.Lens.Common
import Data.Lens.Template
import Data.List (foldl')
import Data.Monoid
import Data.Void (Void, vacuous)

import Sylvia.Render.Backend
import Sylvia.Render.Pair
import Sylvia.Model

type Rhyme = [RhymeUnit]

-- | Specifies a /rhyme line/: a straight line protruding from the left
-- edge of a bounding box, connecting a variable to the sub-expression
-- that uses it.
data RhymeUnit = RhymeUnit
    { _ruIndex :: Integer
    , _ruDest  :: Int
    }
  deriving (Show)

$(makeLens ''RhymeUnit)

-- | The result of a rendering operation.
data Result r = Result
    { _resultImage :: r
      -- ^ The rendered image.
    , _resultSize  :: PInt
      -- ^ The size of the image's bounding box in grid units, when all
      -- round things are removed.
    , _resultRhyme :: Rhyme
      -- ^ The expression's rhyme.
    , _resultThroatY :: Int
      -- ^ The Y offset of the expression's ear and throat, measured
      -- from the /bottom/ of its bounding box.
    }
  deriving (Show)

$(makeLens ''Result)

-- | Render an expression, returning an image along with its size.
render :: Backend r => Exp Void -> (r, PInt)
render e =
    let Result image size rhyme _ = render' $ vacuous e
    in case rhyme of
        [] -> (image, size)
        _  -> error $ "render: the impossible happened -- "
                        ++ "extra free variables: " ++ show rhyme

-- | Render an expression, with extra juicy options.
render' :: Backend r => Exp Integer -> Result r
render' e = case e of
    Ref x   -> Result mempty (0 :| 0) [RhymeUnit x 0] 0
    Lam e'  -> renderLambda e'
    App a b -> case a of
        Lam _ -> renderBeside a b
        _     -> renderAtop a b

renderBeside :: Backend r => Exp Integer -> Exp Integer -> Result r
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

alignThroats :: Backend r => Result r -> Result r -> (Result r, Result r)
alignThroats
    a@(Result _ _ _ aThroatY)
    b@(Result _ _ _ bThroatY)
    = (shiftY' (minThroatY - aThroatY) a,
       shiftY' (minThroatY - bThroatY) b)
  where
    minThroatY = min aThroatY bThroatY

renderAtop :: Backend r => Exp Integer -> Exp Integer -> Result r
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
    :: Backend r
    => Bool -- ^ Whether the enclosing expression is a lambda.
    -> Int  -- ^ Length of the throat line. This should be positive.
    -> Exp Integer -> Result r
renderWithThroatLine outerIsLam lineLength e = Result image size rhyme throatY
  where
    Result innerImage innerSize rhyme innerThroatY = render' e
    -- Shift the main image to the left, then draw a line next to it
    image = relativeTo (-lineLength :| 0) innerImage <> throatLine
    throatLine = drawZigzag (-lineLength :| innerThroatY) (0 :| throatY)
    throatY = if outerIsLam && containsLam e then innerThroatY - 1 else innerThroatY
    size = innerSize |+| (lineLength :| 0)

-- | Return whether there is a nested box touching the bottom edge of the
-- bounding box. If True, it means that everything in the image should
-- be shifted down one unit, making the ear and throat lines zigzag.
containsLam :: Exp a -> Bool
containsLam e = case e of
    Ref _ -> False
    Lam _ -> True
    App _ b -> containsLam b

-- | Render a lambda expression.
renderLambda :: Backend r => Exp (Inc Integer) -> Result r
renderLambda e' = Result image size rhyme throatY
  where
    Result innerImage (innerWidth :| innerHeight) innerRhyme throatY
        = shiftY (-1) . renderWithThroatLine True 1 $ fmap shiftDown e'
    -- Draw a box around it
    image = drawBox (negateP size) size throatY
            <> relativeTo (-width :| 0) rhymeImage
            <> innerImage
    (rhymeImage, rhyme) = renderRhyme throatY innerRhyme
    rhymeHeight = fromInteger . maximumOr 0 $ map (getL ruIndex) innerRhyme
    size@(width :| _) = (innerWidth + 1 :| (max innerHeight rhymeHeight) + 2)

-- | Like 'maximum', but returns a default value on an empty list rather
-- than throwing a hissy fit.
maximumOr :: Ord a => a -> [a] -> a
maximumOr def = foldl' max def

-- | Render an expression's rhyme.
renderRhyme
    :: Backend r
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
shiftX :: Backend r => Int -> Result r -> Result r
shiftX dx = modL resultImage (relativeTo (dx :| 0))

-- | Shift an image vertically by a specified amount, changing the rhyme
-- and throat position to compensate.
shiftY :: Backend r => Int -> Result r -> Result r
shiftY dy
    = modL resultImage   (relativeTo (0 :| dy))
    . modL resultRhyme   (map (modL ruDest (+ dy)))
    . modL resultThroatY (+ dy)

-- | Like 'shiftY', but expand the image's bounding box rather than shifting it.
shiftY' :: Backend r => Int -> Result r -> Result r
shiftY' dy
    = modL resultSize (modL sndP (subtract dy)) -- dy is usually negative, so this /increases/ the size
    . shiftY dy

extendAcross :: Backend r => Int -> Rhyme -> r
extendAcross dx = foldMap $ drawLine
                                <$> ( 0 :|) . getL ruDest
                                <*> (dx :|) . getL ruDest

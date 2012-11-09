{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Sylvia.Render.Core
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- This module provides the algorithm used to render expressions.

module Sylvia.Render.Core
    (
    -- * A function
      render
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
    { _image :: r
      -- ^ The rendered image.
    , _size  :: PInt
      -- ^ The size of the image's bounding box in grid units, when all
      -- round things are removed.
    , _rhyme :: Rhyme
      -- ^ The expression's rhyme.
    , _midY :: Int
      -- ^ The Y offset of the expression's ear and throat, measured
      -- from the /bottom/ of its bounding box.
    }
  deriving (Show)

$(makeLens ''Result)

-- | Render an expression, returning an image along with its size.
render :: Backend r => Exp Void -> (r, PInt)
render e =
    let Result image' size' rhyme' _ = render' $ vacuous e
    in case rhyme' of
        [] -> (image', size')
        _  -> error $ "render: the impossible happened -- "
                        ++ "extra free variables: " ++ show rhyme'

-- | Render an expression, with extra juicy options.
render' :: Backend r => Exp Integer -> Result r
render' e = case e of
    Ref x   -> Result mempty (0 :| 0) [RhymeUnit x 0] 0
    Lam e'  -> renderLambda e'
    App a b -> case a of
        Lam _ -> renderBeside a b
        _     -> renderAtop a b

-- | Render two expressions next to each other.
renderBeside :: Backend r => Exp Integer -> Exp Integer -> Result r
renderBeside a b = Result image' size' rhyme' aMidY
  where
    image' = aImage <> bImage
    (Result aImage (aWidth :| aHeight) aRhyme aMidY,
     Result bImage (bWidth :| bHeight) bRhyme _)
        = alignMiddle
            (render' a)
            (shiftX (-aWidth) . extendThroatLine (1 :| 0) $ render' b)
    size' = (aWidth + bWidth :| max aHeight bHeight)
    rhyme' = bRhyme -- TODO

-- | Align two expressions so that their ears and throats are at the same
-- height.
alignMiddle :: Backend r => Result r -> Result r -> (Result r, Result r)
alignMiddle a b
    = (shiftY' (highest - a^.midY) a,
       shiftY' (highest - b^.midY) b)
  where
    highest = min (a^.midY) (b^.midY)

-- | Render the first expression above the second, connected with an
-- applicator dot.
renderAtop :: Backend r => Exp Integer -> Exp Integer -> Result r
renderAtop a b = Result image' size' rhyme' bMidY
  where
    image' = mconcat $
        -- Draw the two sub-expressions
        [ aImage
        , bImage
        -- Extend the lower sub-expression's rhyme lines so it matches up
        -- with the one above
        , relativeTo (-bWidth :| 0)
            $ extendAcross (bWidth - aWidth) bRhyme
        -- Connect them with a vertical line
        , drawLine (0 :| aMidY) (0 :| bMidY)
        -- Application dot
        , drawDot (0 :| bMidY)
        ]
    Result aImage (aWidth :| aHeight) aRhyme aMidY
        = shiftY (-1 - bHeight) . extendThroatLine (bWidth :| 0) $ render' a
    Result bImage (bWidth :| bHeight) bRhyme bMidY
        = extendThroatLine (1 :| 0) $ render' b
    size' = (aWidth :| aHeight + bHeight + 1)
    rhyme' = aRhyme ++ bRhyme

-- | Add a small line sticking out of an expression's throat, moving
-- the image and increasing the size to compensate.
extendThroatLine
    :: Backend r
    => PInt     -- ^ The endpoint of the line, relative to the throat.
    -> Result r
    -> Result r
extendThroatLine (dx :| dy) a
    = modL image (<> line)
    . setL midY outerMidY
    . shiftX' (-dx)
    $ a
  where
    line = drawZigzag (-dx :| a^.midY) (0 :| outerMidY)
    outerMidY = a^.midY + dy

-- | Render a lambda expression.
renderLambda :: Backend r => Exp (Inc Integer) -> Result r
renderLambda = renderLambda' . fmap shiftDown
  where
    renderLambda' e = enclose . extendRhyme . addLine $ render' e
      where
        addLine = extendThroatLine . (1 :|) $ if containsLam e then (-1) else 0

-- | Return whether there is a nested box touching the bottom edge of the
-- bounding box. If True, it means that everything in the image should
-- be shifted down one unit, making the throat line zigzag.
containsLam :: Exp a -> Bool
containsLam e = case e of
    Ref _ -> False
    Lam _ -> True
    App _ b -> containsLam b

-- | Put an expression in a box.
enclose :: Backend r => Result r -> Result r
enclose = enclose' . shiftY (-1)
  where
    enclose' a
        = modL image (<> drawBox (negateP boxSize) boxSize (a^.midY))
        . setL size  boxSize
        $ a
      where
        boxSize = a^.size |+| (0 :| 2)

-- | Render an expression's rhyme.
extendRhyme :: Backend r => Result r -> Result r
extendRhyme a
    = modL image (<> rhymeImage)
    . modL rhyme flattenRhyme
    . modL size  ((fstP ^+= 1) . (sndP ^%= max rhymeHeight))
    $ a
  where
    rhymeHeight = fromInteger . maximum0 $ map (^.ruIndex) (a^.rhyme)
    rhymeImage = relativeTo (-(a^.size^.fstP) :| 0) $ foldMap drawRhymeUnit (a^.rhyme)
      where drawRhymeUnit (RhymeUnit index dest)
                = drawLine (-1 :| makeOffset index) (0 :| dest)
    flattenRhyme inner =
        [ RhymeUnit (pred index) (makeOffset index)
        | RhymeUnit index _ <- inner, index > 0 ]
    makeOffset = (a^.midY -) . fromInteger

-- | Like 'maximum', but returns a zero on an empty list rather than
-- throwing a hissy fit.
maximum0 :: (Num a, Ord a) => [a] -> a
maximum0 = foldl' max 0

-- | Shift an image horizontally by a specified amount.
shiftX :: Backend r => Int -> Result r -> Result r
shiftX dx = modL image (relativeTo (dx :| 0))

-- | Like 'shiftX', but expand the image's bounding box rather than shifting it.
shiftX' :: Backend r => Int -> Result r -> Result r
shiftX' dx
    = modL size (fstP ^+= abs dx)
    . shiftX dx

-- | Shift an image vertically by a specified amount, changing the rhyme
-- and throat position to compensate.
shiftY :: Backend r => Int -> Result r -> Result r
shiftY dy
    = modL image (relativeTo (0 :| dy))
    . modL rhyme (map (ruDest ^+= dy))
    . modL midY  (+ dy)

-- | Like 'shiftY', but expand the image's bounding box rather than shifting it.
shiftY' :: Backend r => Int -> Result r -> Result r
shiftY' dy
    = modL size (sndP ^+= abs dy)
    . shiftY dy

extendAcross :: Backend r => Int -> Rhyme -> r
extendAcross dx = foldMap $ drawLine
                                <$> ( 0 :|) . getL ruDest
                                <*> (dx :|) . getL ruDest

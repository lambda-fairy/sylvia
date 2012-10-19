-- |
-- Module      : Sylvia.Render.Pair
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable
--
-- Strict pairs, and operations on them.

module Sylvia.Render.Pair
    (
    -- * Building and dismantling pairs
      P(..)
    , fstP
    , sndP

    -- * Fancy type aliases
    , PInt
    , PDouble

    -- * Operations on pairs
    , (|+|)
    , (|*|)
    , negateP
    , fromIntegralP
    ) where

import Control.Applicative
import Control.Comonad.Trans.Store
import Data.Lens.Common

-- | A strict pair.
data P a = !a :| !a
    deriving (Eq, Ord, Bounded, Read, Show)

infix 5 :|

instance Functor P where
    fmap f (x :| y) = f x :| f y

instance Applicative P where
    pure x = x :| x
    f :| g <*> x :| y = f x :| g y

-- | A pair of integers.
type PInt = P Int

-- | A pair of doubles.
type PDouble = P Double

-- | Add or multiply the corresponding values in two pairs.
(|+|), (|*|) :: Num a => P a -> P a -> P a
(|+|) = liftA2 (+)
(|*|) = liftA2 (*)

infixl 6 |+|
infixl 7 |*|

-- | Negate the contents of a pair.
negateP :: Num a => P a -> P a
negateP = fmap negate

-- | Apply 'fromIntegral' to the contents of a pair.
fromIntegralP :: (Integral a, Num b) => P a -> P b
fromIntegralP = fmap fromIntegral

-- | Lens on the first element.
fstP :: Lens (P a) a
fstP = Lens $ \(x :| y) -> store (\x' -> x' :| y) x

-- | Lens on the second element.
sndP :: Lens (P a) a
sndP = Lens $ \(x :| y) -> store (\y' -> x :| y') y

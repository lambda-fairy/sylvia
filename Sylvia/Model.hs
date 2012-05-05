{-# LANGUAGE Rank2Types #-}

-- |
-- Module      : Sylvia.Model
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable (Rank2Types)
--
-- The 'Exp' data type, for representing lambda expressions using De Bruijn
-- indices, plus basic abstraction and application operations.

module Sylvia.Model
    (
    -- * Types
      Inc(..)
    , Exp(..)

    -- * Abstracting
    , abstractName
    , abstractIndex
    , verify

    -- * Applying
    , matchName
    , matchIndex
    , apply
    , subst
    ) where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( Monad(..), ap )
import Data.Foldable ( Foldable(foldMap) )
import Data.Functor ( Functor(..), (<$>) )
import Data.Monoid ( Monoid(..) )
import Data.Traversable ( Traversable(traverse) )
import Data.Void ( Void )

-- | A structure for representing whole numbers.
--
-- The problem with referring to variables with plain integers is that
-- it's impossible to tell whether an expression refers to any free
-- variables. 'Inc' solves this by letting the type dictate the maximum
-- value of the number.
--
-- For example, 3 would be represented as @S (S (S x))@, where @x@ can
-- be any value. Since it is nested three levels deep, it would have the
-- type @Inc (Inc (Inc a))@.
data Inc a
    = O   -- ^ Zero
    | S a -- ^ Add one
  deriving (Eq, Ord, Read, Show)

instance Functor Inc where
    fmap = mapI

instance Applicative Inc where
    pure = return
    (<*>) = ap

instance Monad Inc where
    return = S
    val >>= f = joinI (mapI f val)

instance Foldable Inc where
    foldMap f O = mempty
    foldMap f (S x) = f x

instance Traversable Inc where
    traverse f O = pure O
    traverse f (S x) = S <$> f x

-- | Apply a function to the value inside the 'Inc', if it has one.
mapI :: (a -> b) -> Inc a -> Inc b
mapI f O = O
mapI f (S x) = S (f x)

-- | Remove one layer of nesting, projecting the inner value onto the outside.
joinI :: Inc (Inc a) -> Inc a
joinI (S (S x)) = S x
joinI _ = O

-- | A lambda expression.
data Exp a
    -- | Variable reference.
    = Ref a
    -- | Lambda abstraction.
    -- Since the function introduces a variable, the expression inside
    -- the lambda would have one more variable than the expression
    -- outside, hence the extra 'Inc'.
    | Lam (Exp (Inc a))
    -- | Function application.
    | App (Exp a) (Exp a)
  deriving (Eq, Ord, Read, Show)

instance Functor Exp where
    fmap = mapE

instance Applicative Exp where
    pure = return
    (<*>) = ap

instance Monad Exp where
    return = Ref
    exp >>= f = joinE (mapE f exp)

instance Foldable Exp where
    foldMap f exp = case exp of
        Ref x   -> f x
        Lam e   -> foldMap (foldMap f) e
        App a b -> foldMap f a `mappend` foldMap f b

instance Traversable Exp where
    traverse f exp = case exp of
        Ref x   -> Ref <$> f x
        Lam e   -> Lam <$> traverse (traverse f) e
        App a b -> App <$> traverse f a <*> traverse f b

-- | Apply a function to every leaf value in the tree.
mapE :: (a -> b) -> Exp a -> Exp b
mapE f exp = case exp of
    Ref x   -> Ref (f x)
    Lam e   -> Lam (mapE (mapI f) e)
    App a b -> App (mapE f a) (mapE f b)

-- | Flatten a nested expression by gluing its nodes onto the main tree.
joinE :: Exp (Exp a) -> Exp a
joinE exp = case exp of
    Ref x   -> x
    Lam e   -> Lam (joinE (mapE distE e))
    App a b -> App (joinE a) (joinE b)

-- | Convert a wrapped-up expression into an expression with wrapped-up
-- references.
distE :: Inc (Exp a) -> Exp (Inc a)
distE O = Ref O
distE (S x) = mapE S x

verify :: Exp a -> Maybe (Exp Void)
verify = traverse (const Nothing)

-- | Create a lambda abstraction by replacing a value with a reference to
-- the function's argument.
abstractName
    :: Eq a
    => a     -- ^ Argument name
    -> Exp a -- ^ Function body
    -> Exp a -- ^ Result
abstractName x = Lam . mapE (matchName x)

-- | If the value matches, return 'O'; otherwise, shift the value up by
-- one.
--
-- This is the inverse of 'subst': for any value of @x@,
-- @subst x . matchName x === id@
matchName
    :: Eq a
    => a     -- ^ Value to replace
    -> a     -- ^ Value to check
    -> Inc a -- ^ Result
matchName x y = if x == y then O else S y

-- | Create a lambda abstraction by linking up any de Bruijn indices
-- that match the lambda.
abstractIndex
    :: Integral a
    => Exp a
    -> Exp a
abstractIndex = Lam . mapE matchIndex

-- | Shift a de Bruijn index down by one. If it falls below zero,
-- replace it with 'O'.
matchIndex
    :: Integral a
    => a     -- ^ Index to check
    -> Inc a -- ^ Result
matchIndex index
  | index >  0 = S (index - 1)
  | index == 0 = O
  | otherwise = error "matchIndex: index out of range"

-- | Substitute a value into a function.
apply
    :: Exp a       -- ^ Argument value
    -> Exp (Inc a) -- ^ Function body
    -> Exp a       -- ^ Result
apply e = joinE . mapE (subst e . mapI Ref)

-- | Shift an 'Inc' down by one. If it falls below zero, replace it with
-- the value.
--
-- This is the inverse of the @matchX@ functions: for any value of @x@,
-- @subst x . matchName x === id@ and @subst x . matchIndex x === id@.
subst
    :: a     -- ^ Replacement value
    -> Inc a -- ^ Value to shift
    -> a     -- ^ Result
subst x O = x
subst x (S y) = y

{-# LANGUAGE Rank2Types #-}

-- |
-- Module      : Sylvia.Model.Exp
-- Copyright   : GPLv3
--
-- Maintainer  : chrisyco@gmail.com
-- Portability : portable (Rank2Types)
--
-- The 'Exp' data type, for representing lambda expressions using De Bruijn
-- indices.

module Sylvia.Model.Exp
    (
    -- * Types
      Inc(..)
    , Exp(..)

    -- * Abstracting and applying
    , abstract
    , match
    , apply
    , subst

    -- * Mapping and folding
    , mapI
    , joinI
    , mapE
    , joinE
    , foldE
    , gfoldE
    ) where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( Monad(..), ap )
import Data.Functor ( Functor(..), (<$>) )

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

instance Functor Inc where
    fmap = mapI

instance Applicative Inc where
    pure = return
    (<*>) = ap

instance Monad Inc where
    return = S
    val >>= f = joinI (mapI f val)

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

instance Functor Exp where
    fmap = mapE

instance Applicative Exp where
    pure = return
    (<*>) = ap

instance Monad Exp where
    return = Ref
    exp >>= f = joinE (mapE f exp)

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

-- | Create a lambda abstraction by replacing a value with a reference to
-- the function's argument.
abstract
    :: Eq a
    => a     -- ^ Argument name
    -> Exp a -- ^ Function body
    -> Exp a -- ^ Result
abstract x = Lam . mapE (match x)

-- | If the value matches, return 'O'; otherwise, shift the value up by
-- one.
--
-- This is the inverse of 'subst': for any value of @x@,
-- @subst x . match x === id@
match
    :: Eq a
    => a     -- ^ Value to replace
    -> a     -- ^ Value to check
    -> Inc a -- ^ Result
match x y = if x == y then O else S y

-- | Substitute a value into a function.
apply
    :: Exp a       -- ^ Argument value
    -> Exp (Inc a) -- ^ Function body
    -> Exp a       -- ^ Result
apply e = joinE . mapE (subst e . mapI Ref)

-- | Shift an 'Inc' down by one. If it falls below zero, replace it with
-- the value.
--
-- This is the inverse of 'match': for any value of @x@,
-- @subst x . match x === id@
subst
    :: a     -- ^ Replacement value
    -> Inc a -- ^ Value to shift
    -> a     -- ^ Result
subst x O = x
subst x (S y) = y

-- | Reduce an expression to a summary value, by replacing each node with
-- a function.
foldE
    :: (forall a. a -> t a)          -- ^ How to reduce a 'Ref'
    -> (forall a. t (Inc a) -> t a)  -- ^ 'Lam'
    -> (forall a. t a -> t a -> t a) -- ^ 'App'
    -> Exp b -> t b
foldE fr fl fa exp = case exp of
    Ref x   -> fr x
    Lam e   -> fl (recurse e)
    App a b -> fa (recurse a) (recurse b)
  where
    recurse = foldE fr fl fa

gfoldE
    :: (forall a. t a -> f a)
    -> (forall a. f (Inc a) -> f a)
    -> (forall a. f a -> f a -> f a)
    -> (forall a. Inc (t a) -> t (Inc a))
    -> Exp (t b) -> f b
gfoldE fr fl fa fk exp = case exp of
    Ref x   -> fr x
    Lam e   -> fl (recurse $ mapE fk e)
    App a b -> fa (recurse a) (recurse b)
  where
    recurse = gfoldE fr fl fa fk

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

mapI :: (a -> b) -> Inc a -> Inc b
mapI f O = O
mapI f (S x) = S (f x)

joinI :: Inc (Inc a) -> Inc a
joinI (S (S x)) = S x
joinI _ = O

-- | A lambda expression.
data Exp a
    = Ref a
    | Lam (Exp (Inc a))
    | App (Exp a) (Exp a)

instance Functor Exp where
    fmap = mapE

instance Applicative Exp where
    pure = return
    (<*>) = ap

instance Monad Exp where
    return = Ref
    exp >>= f = joinE (mapE f exp)

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

distE :: Inc (Exp a) -> Exp (Inc a)
distE O = Ref O
distE (S x) = mapE S x

abstract :: Eq a => a -> Exp a -> Exp a
abstract x = Lam . mapE (match x)

match :: Eq a => a -> a -> Inc a
match x y = if x == y then O else S y

apply :: Exp a -> Exp (Inc a) -> Exp a
apply e = joinE . mapE (subst e . mapI Ref)

subst :: a -> Inc a -> a
subst x O = x
subst x (S y) = y

foldE :: (forall a. a -> t a)
      -> (forall a. t (Inc a) -> t a)
      -> (forall a. t a -> t a -> t a)
      -> Exp b -> t b
foldE fr fl fa exp = case exp of
    Ref x   -> fr x
    Lam e   -> fl (recurse e)
    App a b -> fa (recurse a) (recurse b)
  where
    recurse = foldE fr fl fa

gfoldE :: (forall a. t a -> f a)
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

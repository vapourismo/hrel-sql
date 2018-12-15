{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Row
    ( Label (..)
    , Named (..)

    , RowKind

    , RowFunctor (..)
    , RowApplicative (..)
    , RowFoldable (..)
    , RowTraversable (..)
    , Row (..)

    , Proxy (..)
    , Single (..)
    , Product (..)
    , pattern (:*)
    )
where

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits         (KnownSymbol, Symbol)

import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.Kind            (Constraint, Type)
import Data.Proxy           (Proxy (..))

----------------------------------------------------------------------------------------------------
-- Helper types

data Label :: Symbol -> Type where
    Label :: KnownSymbol name => Label name

instance (a ~ b, KnownSymbol b) => IsLabel a (Label b) where
    fromLabel = Label

data Named :: (k -> Type) -> k -> Type where
    Named :: Label name -> f a -> Named f a

instance KnownSymbol name => IsLabel name (f a -> Named f a) where
    fromLabel = Named (Label @name)

----------------------------------------------------------------------------------------------------
-- Row classes

type RowKind k = (k -> Type) -> Type

class RowFunctor (row :: RowKind k) where
    mapRow :: (forall a. f a -> g a) -> row f -> row g

class RowFunctor row => RowApplicative (row :: RowKind k) where
    pureRow :: (forall a. f a) -> row f

class RowFoldable (row :: RowKind k) where
    foldMapRow :: Monoid m => (forall a. f a -> m) -> row f -> m

class (RowFunctor row, RowFoldable row) => RowTraversable (row :: RowKind k) where
    traverseRow :: Applicative g => (forall a. f a -> g (h a)) -> row f -> g (row h)

    type RowConstraint (c :: k -> Constraint) row :: Constraint

    traverseConstrainedRow
        :: (Applicative g, RowConstraint c row)
        => proxy c
        -> (forall a. c a => f a -> g (h a))
        -> row f
        -> g (row h)

class (RowApplicative row, RowTraversable row) => Row (row :: RowKind k) where
    nameFields :: row f -> row (Named f)

----------------------------------------------------------------------------------------------------
-- Empty row

instance RowFunctor Proxy where
    mapRow _ _ = Proxy

instance RowApplicative Proxy where
    pureRow _ = Proxy

instance RowFoldable Proxy where
    foldMapRow _ _ = mempty

instance RowTraversable Proxy where
    traverseRow _ _ = pure Proxy

    type RowConstraint c Proxy = ()

    traverseConstrainedRow _ _ _ = pure Proxy

instance Row Proxy where
    nameFields _ = Proxy

----------------------------------------------------------------------------------------------------
-- Singular row

newtype Single (a :: k) (f :: k -> Type) = Single {unSingle :: f a}

instance RowFunctor (Single a) where
    mapRow f (Single x) = Single (f x)

instance RowApplicative (Single a) where
    pureRow = Single

instance RowFoldable (Single a) where
    foldMapRow f (Single x) = f x

instance RowTraversable (Single a) where
    traverseRow f (Single x) = Single <$> f x

    type RowConstraint c (Single a) = c a

    traverseConstrainedRow _ f (Single x) = Single <$> f x

instance Row (Single a) where
    nameFields (Single x) = Single (#unSingle x)

----------------------------------------------------------------------------------------------------
-- Product row

pattern (:*) :: f a -> f b -> Product (Single a) (Single b) f
pattern (:*) lhs rhs = Pair (Single lhs) (Single rhs)

infixr 7 :*

instance (RowFunctor lhs, RowFunctor rhs) => RowFunctor (Product lhs rhs) where
    mapRow f (Pair lhs rhs) = Pair (mapRow f lhs) (mapRow f rhs)

instance (RowApplicative lhs, RowApplicative rhs) => RowApplicative (Product lhs rhs) where
    pureRow f = Pair (pureRow f) (pureRow f)

instance (RowFoldable lhs, RowFoldable rhs) => RowFoldable (Product lhs rhs) where
    foldMapRow f (Pair lhs rhs) = foldMapRow f lhs <> foldMapRow f rhs

instance (RowTraversable lhs, RowTraversable rhs) => RowTraversable (Product lhs rhs) where
    traverseRow f (Pair lhs rhs) = Pair
        <$> traverseRow f lhs
        <*> traverseRow f rhs

    type RowConstraint c (Product lhs rhs) = (RowConstraint c lhs, RowConstraint c rhs)

    traverseConstrainedRow proxy f (Pair lhs rhs) = Pair
        <$> traverseConstrainedRow proxy f lhs
        <*> traverseConstrainedRow proxy f rhs

instance (Row lhs, Row rhs) => Row (Product lhs rhs) where
    nameFields (Pair lhs rhs) = Pair (nameFields lhs) (nameFields rhs)

----------------------------------------------------------------------------------------------------
-- Sum row

instance (RowFunctor lhs, RowFunctor rhs) => RowFunctor (Sum lhs rhs) where
    mapRow f (InL x) = InL (mapRow f x)
    mapRow f (InR x) = InR (mapRow f x)

instance (RowApplicative lhs, RowApplicative rhs) => RowApplicative (Sum lhs rhs) where
    pureRow f = InR (pureRow f)

instance (RowFoldable lhs, RowFoldable rhs) => RowFoldable (Sum lhs rhs) where
    foldMapRow f (InL x) = foldMapRow f x
    foldMapRow f (InR x) = foldMapRow f x

instance (RowTraversable lhs, RowTraversable rhs) => RowTraversable (Sum lhs rhs) where
    traverseRow f (InL x) = InL <$> traverseRow f x
    traverseRow f (InR x) = InR <$> traverseRow f x

    type RowConstraint c (Sum lhs rhs) = (RowConstraint c lhs, RowConstraint c rhs)

    traverseConstrainedRow proxy f (InL x) = InL <$> traverseConstrainedRow proxy f x
    traverseConstrainedRow proxy f (InR x) = InR <$> traverseConstrainedRow proxy f x

instance (Row lhs, Row rhs) => Row (Sum lhs rhs) where
    nameFields (InL x) = InL (nameFields x)
    nameFields (InR x) = InR (nameFields x)

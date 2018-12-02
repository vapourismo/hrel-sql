{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , Record (..)
    )
where

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits         (KnownSymbol, Symbol)

import Data.Kind (Constraint, Type)

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

class RowApplicative (row :: RowKind k) where
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
-- Plain row

data Record :: [k] -> RowKind k where
    Unit :: Record '[] f
    (:*) :: f x -> Record xs f -> Record (x ': xs) f

instance RowFunctor (Record xs) where
    mapRow _ Unit      = Unit
    mapRow f (x :* xs) = f x :* mapRow f xs

instance RowApplicative (Record '[]) where
    pureRow _ = Unit

instance RowApplicative (Record xs) => RowApplicative (Record (x ': xs)) where
    pureRow x = x :* pureRow x

instance RowFoldable (Record xs) where
    foldMapRow _ Unit      = mempty
    foldMapRow f (x :* xs) = f x <> foldMapRow f xs

type family AllApply (xs :: [k]) (c :: k -> Constraint) :: Constraint where
    AllApply '[]       _ = ()
    AllApply (x ': xs) c = (c x, AllApply xs c)

instance RowTraversable (Record xs) where
    traverseRow _ Unit      = pure Unit
    traverseRow f (x :* xs) = (:*) <$> f x <*> traverseRow f xs

    type RowConstraint c (Record xs) = AllApply xs c

    traverseConstrainedRow _     _ Unit      = pure Unit
    traverseConstrainedRow proxy f (x :* xs) = (:*) <$> f x <*> traverseConstrainedRow proxy f xs

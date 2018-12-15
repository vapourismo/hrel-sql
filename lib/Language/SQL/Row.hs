{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
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
    , Dict (..)

    , RowFunctor (..)
    , RowApplicative (..)
    , RowDict (..)
    , RowFoldable (..)
    , RowTraversable (..)
    , Row (..)

    , traverseConstrainedRow
    , mapRowWithName
    , traverseRowWithName
    , foldMapRowWithName

    , Proxy (..)
    , Single (..)
    , Product (..)
    , pattern (:*)
    )
where

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits         (KnownSymbol, Symbol)

import Data.Functor.Compose  (Compose (..))
import Data.Functor.Const    (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product  (Product (..))
import Data.Kind             (Constraint, Type)
import Data.Proxy            (Proxy (..))

----------------------------------------------------------------------------------------------------
-- Helper types

data Label :: Symbol -> Type where
    Label :: KnownSymbol name => Label name

instance (a ~ b, KnownSymbol b) => IsLabel a (Label b) where
    fromLabel = Label

data Named :: k -> Type where
    Named :: Label name -> Named a

instance KnownSymbol name => IsLabel name (Named a) where
    fromLabel = Named (Label @name)

data Dict :: (k -> Constraint) -> k -> Type where
    Dict :: c a => Dict c a

----------------------------------------------------------------------------------------------------
-- Row classes

type RowKind k = (k -> Type) -> Type

class RowFunctor (row :: RowKind k) where
    mapRow :: (forall a. f a -> g a) -> row f -> row g

    default mapRow :: RowTraversable row => (forall a. f a -> g a) -> row f -> row g
    mapRow f row = runIdentity (traverseRow (Identity . f) row)

class RowFunctor row => RowApplicative (row :: RowKind k) where
    pureRow :: (forall a. f a) -> row f

    applyRow :: (forall a. f a -> g a -> h a) -> row f -> row g -> row h

class RowApplicative row => RowDict (row :: RowKind k) where
    type All (c :: k -> Constraint) row :: Constraint

    dictRow :: All c row => row (Dict c)

class RowFoldable (row :: RowKind k) where
    foldMapRow :: Monoid m => (forall a. f a -> m) -> row f -> m

    default foldMapRow :: (RowTraversable row, Monoid m) => (forall a. f a -> m) -> row f -> m
    foldMapRow f row = getConst (traverseRow (Const . f) row)

class (RowFunctor row, RowFoldable row) => RowTraversable (row :: RowKind k) where
    traverseRow :: Applicative g => (forall a. f a -> g (h a)) -> row f -> g (row h)
    traverseRow f = sequenceRow . mapRow (Compose . f)

    sequenceRow :: Applicative f => row (Compose f g) -> f (row g)
    sequenceRow = traverseRow getCompose

class (RowApplicative row, RowTraversable row) => Row (row :: RowKind k) where
    nameFields :: row Named

traverseConstrainedRow
    :: (Applicative g, RowDict row, RowTraversable row, All c row)
    => proxy c
    -> (forall a. c a => f a -> g (h a))
    -> row f
    -> g (row h)
traverseConstrainedRow (_ :: proxy c) f (row :: row f) =
    traverseRow (\(Pair Dict x) -> f x) (applyRow Pair (dictRow :: row (Dict c)) row)

mapRowWithName :: Row row => (forall name a. Label name -> f a -> g a) -> row f -> row g
mapRowWithName f = applyRow (\(Named label) row -> f label row) nameFields

traverseRowWithName
    :: (Row row, Applicative g)
    => (forall name a. Label name -> f a -> g (h a))
    -> row f
    -> g (row h)
traverseRowWithName f row =
    sequenceRow (applyRow (\(Named label) x -> Compose (f label x)) nameFields row)

foldMapRowWithName :: (Row row, Monoid m) => (forall name a. Label name -> f a -> m) -> row f -> m
foldMapRowWithName f row = getConst (traverseRowWithName (\label x -> Const (f label x)) row)

----------------------------------------------------------------------------------------------------
-- Empty row

instance RowFunctor Proxy where
    mapRow _ _ = Proxy

instance RowApplicative Proxy where
    pureRow _ = Proxy

    applyRow _ _ _ = Proxy

instance RowFoldable Proxy where
    foldMapRow _ _ = mempty

instance RowDict Proxy where
    type All c Proxy = ()

    dictRow = Proxy

instance RowTraversable Proxy where
    traverseRow _ _ = pure Proxy

instance Row Proxy where
    nameFields = Proxy

----------------------------------------------------------------------------------------------------
-- Singular row

newtype Single (a :: k) (f :: k -> Type) = Single {unSingle :: f a}

instance RowFunctor (Single a) where
    mapRow f (Single x) = Single (f x)

instance RowApplicative (Single a) where
    pureRow = Single

    applyRow f (Single x) (Single y) = Single (f x y)

instance RowFoldable (Single a) where
    foldMapRow f (Single x) = f x

instance RowDict (Single a) where
    type All c (Single a) = c a
    dictRow = Single Dict

instance RowTraversable (Single a) where
    traverseRow f (Single x) = Single <$> f x

instance Row (Single a) where
    nameFields = Single #unSingle

----------------------------------------------------------------------------------------------------
-- Product row

pattern (:*) :: f a -> f b -> Product (Single a) (Single b) f
pattern (:*) lhs rhs = Pair (Single lhs) (Single rhs)

infixr 7 :*

instance (RowFunctor lhs, RowFunctor rhs) => RowFunctor (Product lhs rhs) where
    mapRow f (Pair lhs rhs) = Pair (mapRow f lhs) (mapRow f rhs)

instance (RowApplicative lhs, RowApplicative rhs) => RowApplicative (Product lhs rhs) where
    pureRow f = Pair (pureRow f) (pureRow f)

    applyRow f (Pair ll lr) (Pair rl rr) = Pair (applyRow f ll rl) (applyRow f lr rr)

instance (RowFoldable lhs, RowFoldable rhs) => RowFoldable (Product lhs rhs) where
    foldMapRow f (Pair lhs rhs) = foldMapRow f lhs <> foldMapRow f rhs

instance (RowDict lhs, RowDict rhs) => RowDict (Product lhs rhs) where
    type All c (Product lhs rhs) = (All c lhs, All c rhs)

    dictRow = Pair dictRow dictRow

instance (RowTraversable lhs, RowTraversable rhs) => RowTraversable (Product lhs rhs) where
    traverseRow f (Pair lhs rhs) = Pair <$> traverseRow f lhs <*> traverseRow f rhs

instance (Row lhs, Row rhs) => Row (Product lhs rhs) where
    nameFields = Pair nameFields nameFields

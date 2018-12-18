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
    , Dict (..)

    , RowKind
    , Row (..)
    , buniqWithName
    , bfoldMapWithName
    , btraverseC

    , Proxy (..)
    , Single (..)
    , Product (..)
    , pattern (:*)
    )
where

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits         (KnownSymbol, Symbol)

import Data.Barbie             (ConstraintsB (..), FunctorB (..), ProductB (..), ProductBC (..),
                                TraversableB (..), bfoldMap)
import Data.Barbie.Constraints (Dict (..))
import Data.Functor.Product    (Product (..))
import Data.Kind               (Type)
import Data.Proxy              (Proxy (..))

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

----------------------------------------------------------------------------------------------------
-- Row classes

type RowKind k = (k -> Type) -> Type

class (TraversableB row, ProductB row) => Row (row :: RowKind k) where
    namedRow :: row Named

buniqWithName :: Row row => (forall name a. Label name -> f a) -> row f
buniqWithName f = bmap (\(Named label) -> f label) namedRow

bfoldMapWithName :: (Row row, Monoid m) => (forall name a. Label name -> f a -> m) -> row f -> m
bfoldMapWithName f row = bfoldMap (\(Pair (Named label) x) -> f label x) (bprod namedRow row)

btraverseC
    :: (TraversableB row, ConstraintsB row, AllB c row, Applicative g)
    => proxy c
    -> (forall a. c a => f a -> g (h a))
    -> row f
    -> g (row h)
btraverseC (_ :: proxy c) f row =
    btraverse (\(Pair (Dict :: Dict c a) (x :: f a)) -> f x) (baddDicts row)

----------------------------------------------------------------------------------------------------
-- Empty row

instance Row Proxy where
    namedRow = Proxy

----------------------------------------------------------------------------------------------------
-- Singular row

newtype Single (a :: k) (f :: k -> Type) = Single {unSingle :: f a}

instance FunctorB (Single a) where
    bmap f (Single x) = Single (f x)

instance TraversableB (Single a) where
    btraverse f (Single x) = Single <$> f x

instance ConstraintsB (Single a) where
    type AllB c (Single a) = c a

    baddDicts (Single x) = Single (Pair Dict x)

instance ProductB (Single a) where
    buniq = Single

    bprod (Single x) (Single y) = Single (Pair x y)

instance ProductBC (Single a) where
    bdicts = Single Dict

instance Row (Single a) where
    namedRow = Single #unSingle

----------------------------------------------------------------------------------------------------
-- Product row

pattern (:*) :: f a -> f b -> Product (Single a) (Single b) f
pattern (:*) lhs rhs = Pair (Single lhs) (Single rhs)

infixr 7 :*

instance (Row lhs, Row rhs) => Row (Product lhs rhs) where
    namedRow = Pair namedRow namedRow

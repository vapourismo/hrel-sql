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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Row
    ( RowKind
    , Row (..)
    , buniqWithName
    , bfoldMapWithName
    , btraverseC
    , pattern (:*)
    )
where

import Data.Barbie             (ConstraintsB (..), FunctorB (..), ProductB (..), TraversableB (..),
                                bfoldMap)
import Data.Barbie.Constraints (Dict (..))
import Data.Functor.Product    (Product (..))
import Data.Kind               (Type)
import Data.Proxy              (Proxy (..))

import Language.SQL.Types (Label (..), Named (..), Single (..))

----------------------------------------------------------------------------------------------------
-- Row classes

type RowKind k = (k -> Type) -> Type

class (TraversableB row, ProductB row) => Row (row :: RowKind k) where
    namedRow :: row Named

instance Row Proxy where
    namedRow = Proxy

instance Row (Single a) where
    namedRow = Single #unSingle

instance (Row lhs, Row rhs) => Row (Product lhs rhs) where
    namedRow = Pair namedRow namedRow

----------------------------------------------------------------------------------------------------
-- Utilities

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

pattern (:*) :: f a -> f b -> Product (Single a) (Single b) f
pattern (:*) lhs rhs = Pair (Single lhs) (Single rhs)

infixr 7 :*

{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeInType       #-}

module Language.SQL.Statement
    ( Statement (..)
    , select
    , restrict
    , join
    )
where

import Prelude hiding ((&&))

import Data.Barbie          (AllB, ConstraintsB, TraversableB)
import Data.Functor.Product (Product (..))
import Data.Kind            (Type)
import Data.String          (IsString (..))
import Data.Text            (Text)

import Language.SQL.Expression
import Language.SQL.Row

----------------------------------------------------------------------------------------------------
-- Statement type

data Statement :: RowKind Type -> Type where
    TableOnly :: Text -> Statement row

    Select
        :: (TraversableB source, ConstraintsB source, AllB Row source)
        => (source (Single Expression) -> row Expression)
        -> (source (Single Expression) -> Expression SqlBool)
        -> source Statement
        -> Statement row

instance IsString (Statement row) where
    fromString = TableOnly . fromString

----------------------------------------------------------------------------------------------------
-- Combinators

select
    :: Row row
    => (row Expression -> row' Expression)
    -> Statement row
    -> Statement row'
select selector = \case
    Select expand restrict source ->
        Select
            (selector . expand)
            restrict
            source

    source ->
        Select
            (selector . unSingle . unSingle)
            (const true)
            (Single source)

restrict
    :: Row row
    => (row Expression -> Expression SqlBool)
    -> Statement row
    -> Statement row
restrict restrictor = \case
    Select expand restrict source ->
        Select
            expand
            (\row -> restrict row && restrictor (expand row))
            source

    source ->
        Select
            (unSingle . unSingle)
            (restrictor . unSingle . unSingle)
            (Single source)

join
    :: (Row f, Row g)
    => (f Expression -> g Expression -> h Expression)
    -> (f Expression -> g Expression -> Expression SqlBool)
    -> Statement f
    -> Statement g
    -> Statement h
join expander restrictor lhs rhs = case (lhs, rhs) of
    (Select lhsExpand lhsRestrict lhsSource, Select rhsExpand rhsRestrict rhsSource) ->
        Select
            (\(Pair lhs rhs) -> expander (lhsExpand lhs) (rhsExpand rhs))
            (\(Pair lhs rhs) ->
                lhsRestrict lhs
                && rhsRestrict rhs
                && restrictor (lhsExpand lhs) (rhsExpand rhs))
            (Pair lhsSource rhsSource)

    (Select lhsExpand lhsRestrict lhsSource, TableOnly{}) ->
        Select
            (\(Pair lhs (Single rhs)) -> expander (lhsExpand lhs) (unSingle rhs))
            (\(Pair lhs (Single rhs)) ->
                lhsRestrict lhs && restrictor (lhsExpand lhs) (unSingle rhs))
            (Pair lhsSource (Single rhs))

    (TableOnly{}, Select rhsExpand rhsRestrict rhsSource) ->
        Select
            (\(Pair (Single lhs) rhs) -> expander (unSingle lhs) (rhsExpand rhs))
            (\(Pair (Single lhs) rhs) ->
                restrictor (unSingle lhs) (rhsExpand rhs) && rhsRestrict rhs)
            (Pair (Single lhs) rhsSource)

    _ ->
        Select
            (\(Single lhs :* Single rhs) -> expander lhs rhs)
            (\(Single lhs :* Single rhs) -> restrictor lhs rhs)
            (lhs :* rhs)

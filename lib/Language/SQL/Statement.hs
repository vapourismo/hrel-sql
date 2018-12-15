{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeInType       #-}

module Language.SQL.Statement where

import Prelude hiding ((&&))

import Data.Functor.Product (Product (..))
import Data.Kind            (Type)
import Data.Proxy           (Proxy (..))
import Data.String          (IsString (..))
import Data.Text            (Text)

import Language.SQL.Expression
import Language.SQL.Row

data Statement :: RowKind Type -> Type where
    TableOnly :: Text -> Statement row

    Select
        :: (RowTraversable source, RowConstraint Row source)
        => (source (Single Expression) -> row Expression)
        -> (source (Single Expression) -> Expression SqlBool)
        -> source Statement
        -> Statement row

instance RowFunctor Statement where
    mapRow _ (TableOnly name)                 = TableOnly name
    mapRow f (Select expand restrict sources) = Select (f . expand) restrict sources

instance RowApplicative Statement where
    pureRow x = Select (const x) (const true) Proxy

expandExpression :: Row row => Expression a -> row Expression
expandExpression exp =
    mapRow (\(Named name _) -> Access exp name) (nameFields (pureRow Proxy))

instance IsString (Statement row) where
    fromString = TableOnly . fromString

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

join2
    :: (Row f, Row g)
    => (f Expression -> g Expression -> h Expression)
    -> (f Expression -> g Expression -> Expression SqlBool)
    -> Statement f
    -> Statement g
    -> Statement h
join2 expander restrictor lhs rhs = case (lhs, rhs) of
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

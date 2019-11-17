{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}

module Language.SQL.Statement
    ( Statement (..)
    , Bound (..)
    , select
    , restrict
    , join
    , renderStatement
    )
where

import Prelude hiding ((&&))

import Data.Barbie          (AllB, ConstraintsB, TraversableB, bfoldMap, bmap)
import Data.Functor.Product (Product (..))
import Data.Kind            (Type)
import Data.List            (intersperse)
import Data.Proxy           (Proxy (..))
import Data.String          (IsString (..))

import           Language.SQL.Expression (Expression (..), renderExpression, true, (&&))
import qualified Language.SQL.Render     as Render
import           Language.SQL.Row        (pattern (:*), Row (..), RowKind, bfoldMapWithName,
                                          btraverseC)
import           Language.SQL.Types      (Name, Named (..), Single (..), labelName)

----------------------------------------------------------------------------------------------------
-- Statement type

newtype Bound (row :: RowKind Type) = Bound
    { fromBound :: Expression (row Expression) }

expandBound :: Row row => Bound row -> row Expression
expandBound (Bound boundExp) =
    bmap fieldExp namedRow
    where
        fieldExp :: Named a -> Expression a
        fieldExp (Named label) = Access boundExp label

data Statement :: RowKind Type -> Type where
    TableOnly :: Name -> Statement row

    Select
        :: (TraversableB source, ConstraintsB source, AllB Row source)
        => (source Bound -> row Expression)
        -> (source Bound -> Expression Bool)
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
            (selector . expandBound . unSingle)
            (const true)
            (Single source)

restrict
    :: Row row
    => (row Expression -> Expression Bool)
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
            (expandBound . unSingle)
            (restrictor . expandBound . unSingle)
            (Single source)

join
    :: (Row f, Row g)
    => (f Expression -> g Expression -> h Expression)
    -> (f Expression -> g Expression -> Expression Bool)
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
            (\(Pair lhs (Single rhs)) -> expander (lhsExpand lhs) (expandBound rhs))
            (\(Pair lhs (Single rhs)) ->
                lhsRestrict lhs && restrictor (lhsExpand lhs) (expandBound rhs))
            (Pair lhsSource (Single rhs))

    (TableOnly{}, Select rhsExpand rhsRestrict rhsSource) ->
        Select
            (\(Pair (Single lhs) rhs) -> expander (expandBound lhs) (rhsExpand rhs))
            (\(Pair (Single lhs) rhs) ->
                restrictor (expandBound lhs) (rhsExpand rhs) && rhsRestrict rhs)
            (Pair (Single lhs) rhsSource)

    _ ->
        Select
            (\(lhs :* rhs) -> expander (expandBound lhs) (expandBound rhs))
            (\(lhs :* rhs) -> restrictor (expandBound lhs) (expandBound rhs))
            (lhs :* rhs)

----------------------------------------------------------------------------------------------------
-- Render

data PreRenderedSource (a :: k) = PreRenderedSource Render.Rendered Name

prerenderSource
    :: (TraversableB source, ConstraintsB source, AllB Row source)
    => source Statement
    -> Render.Renderer (source PreRenderedSource)
prerenderSource statement =
    btraverseC (Proxy @Row) go statement
    where
        go statement = PreRenderedSource
            <$> renderStatement statement
            <*> Render.allocName "source"

renderStatement :: Row row => Statement row -> Render.Render
renderStatement = \case
    TableOnly name -> "TABLE ONLY " <> Render.name name

    Select expand restrict source -> do
        source <- prerenderSource source
        let boundSource = bmap (\(PreRenderedSource _ name) -> Bound (Variable name)) source
        mconcat
            [ "SELECT "
            , mconcat $ intersperse ", " $
                bfoldMapWithName
                    (\label exp -> [Render.alias (labelName label) (renderExpression exp)])
                    (expand boundSource)
            , " FROM "
            , mconcat $ intersperse ", " $
                bfoldMap
                    (\(PreRenderedSource rendered name) -> [Render.alias name (pure rendered)])
                    source
            , " WHERE "
            , renderExpression (restrict boundSource)
            ]

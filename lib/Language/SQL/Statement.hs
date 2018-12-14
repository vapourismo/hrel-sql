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

import Control.Monad.State.Strict (evalState, state)

import Data.Functor.Const (Const (..))
import Data.Kind          (Type)
import Data.Proxy         (Proxy (..))
import Data.String        (IsString (..))
import Data.Text          (Text, pack)

import Language.SQL.Expression
import Language.SQL.Row

newtype Captured (row :: RowKind Type) = Captured {unCapture :: row Expression}

instance RowFunctor Captured where
    mapRow f (Captured x) = Captured (f x)

instance RowApplicative Captured where
    pureRow = Captured

instance RowFoldable Captured where
    foldMapRow f (Captured x) = f x

instance RowTraversable Captured where
    traverseRow f (Captured x) = Captured <$> f x

    type RowConstraint c Captured = c Expression

    traverseConstrainedRow _ f (Captured x) = Captured <$> f x

instance Row Captured where
    nameFields (Captured x) = Captured (#unCapture x)

data Statement :: RowKind Type -> Type where
    TableOnly :: Text -> Statement row

    Select
        :: (RowTraversable source, RowConstraint Row source)
        => (source Captured -> row Expression)
        -> (source Captured -> Expression SqlBool)
        -> source Statement
        -> Statement row

instance RowFunctor Statement where
    mapRow _ (TableOnly name)                 = TableOnly name
    mapRow f (Select expand restrict sources) = Select (f . expand) restrict sources

instance RowApplicative Statement where
    pureRow x = Select (const x) (const true) Unit

fillSelector :: Row row => Expression a -> row Expression
fillSelector exp =
    mapRow (\(Named name _) -> Access exp name) (nameFields (pureRow (Const ())))

instance RowFoldable Statement where
    foldMapRow _ (TableOnly _)             = mempty
    foldMapRow f (Select expand _ sources) =
        f (expand (evalState (traverseConstrainedRow (Proxy @Row) go sources) (0 :: Word)))
        where
            go _ = state $ \index ->
                ( Captured (fillSelector (Variable (pack ('V' : show index))))
                , index + 1
                )

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
            (selector . unCapture . unSingle)
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
            (\row -> restrictor (expand row) && restrict row)
            source

    source ->
        Select
            (unCapture . unSingle)
            (restrictor . unCapture . unSingle)
            (Single source)

joinSome
    :: (RowTraversable source, RowConstraint Row source)
    => (source Captured -> row Expression)
    -> (source Captured -> Expression SqlBool)
    -> source Statement
    -> Statement row
joinSome = Select

join2
    :: (Row f, Row g)
    => (f Expression -> g Expression -> h Expression)
    -> (f Expression -> g Expression -> Expression SqlBool)
    -> Statement f
    -> Statement g
    -> Statement h
join2 expand restrict lhs rhs =
    joinSome
        (\(Captured lhs :* Captured rhs :* Unit) -> expand lhs rhs)
        (\(Captured lhs :* Captured rhs :* Unit) -> restrict lhs rhs)
        (lhs :* rhs :* Unit)

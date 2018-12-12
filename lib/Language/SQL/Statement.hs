{-# LANGUAGE GADTs            #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeInType       #-}

module Language.SQL.Statement where

import Control.Monad.State.Strict

import Data.Functor.Const
import Data.Kind          (Type)
import Data.Proxy
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

project
    :: Row row
    => (row Expression -> row' Expression)
    -> Statement row
    -> Statement row'
project selector source =
    Select
        (\(Captured exp :* Unit) -> selector exp)
        (const true)
        (source :* Unit)

restrict
    :: Row row
    => (row Expression -> Expression SqlBool)
    -> Statement row
    -> Statement row
restrict restrictor source =
    Select
        (\(Captured exp :* Unit) -> exp)
        (\(Captured exp :* Unit) -> restrictor exp)
        (source :* Unit)

join
    :: (Row source, RowConstraint Row source)
    => (source Captured -> row Expression)
    -> (source Captured -> Expression SqlBool)
    -> source Statement
    -> Statement row
join = Select

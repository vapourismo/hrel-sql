{-# LANGUAGE GADTs      #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

module Language.SQL.Statement where

import Data.Kind (Type)
import Data.Text (Text)

import Language.SQL.Expression (Expression, SqlBool)
import Language.SQL.Row        (Row, RowConstraint, RowKind, RowTraversable)

newtype Captured (row :: RowKind Type) = Captured {unCapture :: row Expression}

data Statement :: RowKind Type -> Type where
    TableOnly :: Text -> Statement row

    Select
        :: (RowTraversable source, RowConstraint Row source)
        => (source Captured -> row Expression)
        -> (source Captured -> Expression SqlBool)
        -> source Statement
        -> Statement row

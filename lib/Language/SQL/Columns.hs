{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Language.SQL.Columns
    ( Signature
    , (:::)
    , Columns
    , ColumnType
    )
where

import GHC.TypeLits (Symbol)

import Data.Kind (Type)

----------------------------------------------------------------------------------------------------
-- Columns

data Signature = Signature Symbol Type

type (:::) = 'Signature

type family Columns a :: [Signature]

type family FindColumnType (columns :: [Signature]) (name :: Symbol) :: Type where
    FindColumnType ((name ::: typ) ': _   ) name = typ
    FindColumnType (_              ': rest) name = FindColumnType rest name

type ColumnType a n = FindColumnType (Columns a) n

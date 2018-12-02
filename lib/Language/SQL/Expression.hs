{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.SQL.Expression
    ( SqlBool
    , SqlInt
    , SqlString
    , SqlReal

    , BinaryOp (..)
    , Callable (..)
    , Expression (..)

    , (==)
    , not
    , (&&)
    , (||)
    , true
    , false

    , Label (..)
    , (<.>)
    )
where

import Prelude (Bool (..), Double, Fractional (..), Integer, Num (..), (.))

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits         (KnownSymbol, Symbol)

import Data.Kind  (Type)
import Data.Ratio (denominator, numerator)
import Data.Text  (Text)
import Data.Vinyl (Rec (..))

import Language.SQL.Columns (ColumnType)

----------------------------------------------------------------------------------------------------
-- Base types

data SqlBool

data SqlInt

data SqlReal

data SqlString

----------------------------------------------------------------------------------------------------
-- Binary operators

newtype BinaryOp a b c = BinaryOp Text

----------------------------------------------------------------------------------------------------
-- Callables

newtype Callable (xs :: [Type]) r = Callable Text

----------------------------------------------------------------------------------------------------
-- Expression data type

data Expression a where
    IntegerLiteral :: Integer -> Expression SqlInt

    RealLiteral :: Double -> Expression SqlReal

    StringLiteral :: Text -> Expression SqlString

    BoolLiteral :: Bool -> Expression SqlBool

    Variable :: Text -> Expression a

    Infix :: BinaryOp a b c -> Expression a -> Expression b -> Expression c

    Access :: ColumnType a n ~ b => Expression a -> Label n -> Expression b

    Apply :: Callable xs r -> Rec Expression xs -> Expression r

----------------------------------------------------------------------------------------------------
-- Basic operations on 'Expression'

instance Num (Expression SqlInt) where
    (+) = Infix (BinaryOp "+")

    (-) = Infix (BinaryOp "-")

    (*) = Infix (BinaryOp "*")

    abs x = Apply (Callable "ABS") (x :& RNil)

    signum x = Apply (Callable "SIGN") (x :& RNil)

    fromInteger = IntegerLiteral

instance Num (Expression SqlReal) where
    (+) = Infix (BinaryOp "+")

    (-) = Infix (BinaryOp "-")

    (*) = Infix (BinaryOp "*")

    abs x = Apply (Callable "ABS") (x :& RNil)

    signum x = Apply (Callable "SIGN") (x :& RNil)

    fromInteger = RealLiteral . fromInteger

instance Fractional (Expression SqlReal) where
    (/) = Infix (BinaryOp "/")

    recip = (1 /)

    fromRational ratio = fromInteger (numerator ratio) / fromInteger (denominator ratio)

(==) :: Expression a -> Expression a -> Expression SqlBool
(==) = Infix (BinaryOp "=")

infix 4 ==

not :: Expression SqlBool -> Expression SqlBool
not x = Apply (Callable "NOT") (x :& RNil)

(&&) :: Expression SqlBool -> Expression SqlBool -> Expression SqlBool
(&&) = Infix (BinaryOp "AND")

infixr 3 &&

(||) :: Expression SqlBool -> Expression SqlBool -> Expression SqlBool
(||) = Infix (BinaryOp "OR")

infixr 2 ||

true :: Expression SqlBool
true = BoolLiteral True

false :: Expression SqlBool
false = BoolLiteral False

----------------------------------------------------------------------------------------------------
-- Field access

data Label (name :: Symbol) where
    Label :: KnownSymbol name => Label name

instance (KnownSymbol b, a ~ b) => IsLabel a (Label b) where
    fromLabel = Label

(<.>) :: ColumnType a n ~ b => Expression a -> Label n -> Expression b
(<.>) = Access

infixl 8 <.>

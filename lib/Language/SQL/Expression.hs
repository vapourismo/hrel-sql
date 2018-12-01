{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

data BinaryOp a r where
    And :: BinaryOp SqlBool SqlBool

    Or :: BinaryOp SqlBool SqlBool

    Equals :: BinaryOp a SqlBool

    Plus :: BinaryOp a a

    Minus :: BinaryOp a a

    Multiply :: BinaryOp a a

    Divide :: BinaryOp a a

----------------------------------------------------------------------------------------------------
-- Callables

data Callable xs r where
    Absolute :: Callable '[a] a

    Sign :: Callable '[a] a

    Not :: Callable '[SqlBool] SqlBool

----------------------------------------------------------------------------------------------------
-- Expression data type

data Expression a where
    IntegerLiteral :: Integer -> Expression SqlInt

    RealLiteral :: Double -> Expression SqlReal

    StringLiteral :: Text -> Expression SqlString

    BoolLiteral :: Bool -> Expression SqlBool

    Variable :: Text -> Expression a

    Infix :: BinaryOp a b -> Expression a -> Expression a -> Expression b

    Access :: ColumnType a n ~ b => Expression a -> Label n -> Expression b

    Apply :: Callable xs r -> Rec Expression xs -> Expression r

----------------------------------------------------------------------------------------------------
-- Basic operations on 'Expression'

instance Num (Expression SqlInt) where
    (+) = Infix Plus

    (-) = Infix Minus

    (*) = Infix Plus

    abs x = Apply Absolute (x :& RNil)

    signum x = Apply Sign (x :& RNil)

    fromInteger = IntegerLiteral

instance Num (Expression SqlReal) where
    (+) = Infix Plus

    (-) = Infix Minus

    (*) = Infix Plus

    abs x = Apply Absolute (x :& RNil)

    signum x = Apply Sign (x :& RNil)

    fromInteger = RealLiteral . fromInteger

instance Fractional (Expression SqlReal) where
    (/) = Infix Divide

    recip = (1 /)

    fromRational ratio = fromInteger (numerator ratio) / fromInteger (denominator ratio)

(==) :: Expression a -> Expression a -> Expression SqlBool
(==) = Infix Equals

infix 4 ==

not :: Expression SqlBool -> Expression SqlBool
not x = Apply Not (x :& RNil)

(&&) :: Expression SqlBool -> Expression SqlBool -> Expression SqlBool
(&&) = Infix And

infixr 3 &&

(||) :: Expression SqlBool -> Expression SqlBool -> Expression SqlBool
(||) = Infix Or

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

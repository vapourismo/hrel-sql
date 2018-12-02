{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.SQL.Expression
    ( SqlBool
    , SqlInt
    , SqlString
    , SqlReal

    , Expression (..)

    , (==)
    , not
    , (&&)
    , (||)
    , true
    , false
    )
where

import Prelude (Bool (..), Double, Fractional (..), Integer, Num (..), (.))

import Data.Kind   (Type)
import Data.Ratio  (denominator, numerator)
import Data.String (IsString (..))
import Data.Text   (Text)

import Language.SQL.Row

----------------------------------------------------------------------------------------------------
-- Base types

data SqlBool

data SqlInt

data SqlReal

data SqlString

----------------------------------------------------------------------------------------------------
-- Expression data type

data Expression :: Type -> Type where
    IntegerLiteral :: Integer -> Expression SqlInt

    RealLiteral :: Double -> Expression SqlReal

    StringLiteral :: Text -> Expression SqlString

    BoolLiteral :: Bool -> Expression SqlBool

    Variable :: Text -> Expression a

    Infix :: Text -> Expression a -> Expression b -> Expression c

    Access :: Expression a -> Label n -> Expression b

    Apply :: RowFoldable row => Text -> row Expression -> Expression r

----------------------------------------------------------------------------------------------------
-- Basic operations on 'Expression'

instance IsString (Expression SqlString) where
    fromString = StringLiteral . fromString

instance Num (Expression SqlInt) where
    (+) = Infix "+"

    (-) = Infix "-"

    (*) = Infix "*"

    abs x = Apply "ABS" (x :* Unit)

    signum x = Apply "SIGN" (x :* Unit)

    fromInteger = IntegerLiteral

instance Num (Expression SqlReal) where
    (+) = Infix "+"

    (-) = Infix "-"

    (*) = Infix "*"

    abs x = Apply "ABS" (x :* Unit)

    signum x = Apply "SIGN" (x :* Unit)

    fromInteger = RealLiteral . fromInteger

instance Fractional (Expression SqlReal) where
    (/) = Infix "/"

    recip = (1 /)

    fromRational ratio = fromInteger (numerator ratio) / fromInteger (denominator ratio)

(==) :: Expression a -> Expression a -> Expression SqlBool
(==) = Infix "="

infix 4 ==

not :: Expression SqlBool -> Expression SqlBool
not x = Apply "NOT" (x :* Unit)

(&&) :: Expression SqlBool -> Expression SqlBool -> Expression SqlBool
(&&) = Infix "AND"

infixr 3 &&

(||) :: Expression SqlBool -> Expression SqlBool -> Expression SqlBool
(||) = Infix "OR"

infixr 2 ||

true :: Expression SqlBool
true = BoolLiteral True

false :: Expression SqlBool
false = BoolLiteral False

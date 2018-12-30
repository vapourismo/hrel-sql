{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
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
    , (<)
    , (>)
    , (<=)
    , (>=)
    , not
    , (&&)
    , (||)
    , true
    , false

    , renderExpression
    )
where

import Prelude (Bool (..), Double, Fractional (..), Integer, Num (..), pure, ($), (.))

import GHC.TypeLits

import Data.Barbie (TraversableB, bfoldMap)
import Data.Kind   (Type)
import Data.List   (intersperse)
import Data.Monoid (mconcat, (<>))
import Data.Ratio  (denominator, numerator)
import Data.String (IsString (..))
import Data.Text   (Text)

import qualified Language.SQL.Render as Render
import           Language.SQL.Row    (Label (..), Single (..))

----------------------------------------------------------------------------------------------------
-- Base types

data SqlBool

data SqlInt

data SqlReal

data SqlString

----------------------------------------------------------------------------------------------------
-- Expression type

data Expression :: Type -> Type where
    IntegerLiteral :: Integer -> Expression SqlInt

    RealLiteral :: Double -> Expression SqlReal

    StringLiteral :: Text -> Expression SqlString

    BoolLiteral :: Bool -> Expression SqlBool

    Variable :: Render.Name -> Expression a

    Infix :: Text -> Expression a -> Expression b -> Expression c

    Access :: Expression a -> Label n -> Expression b

    Apply :: TraversableB row => Text -> row Expression -> Expression r

----------------------------------------------------------------------------------------------------
-- Basic operations on 'Expression'

instance IsString (Expression SqlString) where
    fromString = StringLiteral . fromString

instance Num (Expression SqlInt) where
    (+) = Infix "+"

    (-) = Infix "-"

    (*) = Infix "*"

    abs x = Apply "ABS" (Single x)

    signum x = Apply "SIGN" (Single x)

    fromInteger = IntegerLiteral

instance Num (Expression SqlReal) where
    (+) = Infix "+"

    (-) = Infix "-"

    (*) = Infix "*"

    abs x = Apply "ABS" (Single x)

    signum x = Apply "SIGN" (Single x)

    fromInteger = RealLiteral . fromInteger

instance Fractional (Expression SqlReal) where
    (/) = Infix "/"

    recip = (1 /)

    fromRational ratio = fromInteger (numerator ratio) / fromInteger (denominator ratio)

(==) :: Expression a -> Expression a -> Expression SqlBool
(==) = Infix "="

infix 4 ==

(>) :: Expression a -> Expression a -> Expression SqlBool
(>) = Infix ">"

infix 4 >

(<) :: Expression a -> Expression a -> Expression SqlBool
(<) = Infix "<"

infix 4 <

(>=) :: Expression a -> Expression a -> Expression SqlBool
(>=) = Infix ">="

infix 4 >=

(<=) :: Expression a -> Expression a -> Expression SqlBool
(<=) = Infix "<="

infix 4 <=

not :: Expression SqlBool -> Expression SqlBool
not x = Apply "NOT" (Single x)

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

----------------------------------------------------------------------------------------------------
-- Renderer

renderExpression :: Expression a -> Render.Renderer
renderExpression = \case
    IntegerLiteral int -> Render.showable int

    RealLiteral real -> Render.showable real

    StringLiteral string -> Render.string string

    BoolLiteral True -> "true"
    BoolLiteral _    -> "false"

    Variable name -> Render.name name

    Infix op lhs rhs -> Render.parens $ mconcat
        [ renderExpression lhs
        , " "
        , Render.fromText op
        , " "
        , renderExpression rhs
        ]

    Access exp label@Label -> mconcat
        [ Render.parens (renderExpression exp)
        , "."
        , fromString (symbolVal label)
        ]

    Apply name params ->
        Render.fromText name
        <> Render.parens (mconcat (intersperse "," (bfoldMap (pure . renderExpression) params)))

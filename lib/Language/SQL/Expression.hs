{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.SQL.Expression
    ( Expression (..)

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

import Prelude (Bool (..), Double, Fractional (..), Integer, Num (..), id, pure, ($), (.))

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits

import Data.Barbie (TraversableB, bfoldMap)
import Data.Kind   (Type)
import Data.List   (intersperse)
import Data.Monoid (mconcat, (<>))
import Data.Ratio  (denominator, numerator)
import Data.String (IsString (..))
import Data.Text   (Text)

import qualified Language.SQL.Render as Render
import           Language.SQL.Types  (Label (..), Name, Single (..))

----------------------------------------------------------------------------------------------------
-- Expression type

data Expression :: Type -> Type where
    IntegerLiteral :: Num a => Integer -> Expression a

    RealLiteral :: Fractional a => Double -> Expression a

    StringLiteral :: IsString a => Text -> Expression a

    BoolLiteral :: Bool -> Expression Bool

    Variable :: Name -> Expression a

    Infix :: Text -> Expression a -> Expression b -> Expression c

    Access :: Expression a -> Label n -> Expression b

    Apply :: TraversableB row => Text -> row Expression -> Expression r

----------------------------------------------------------------------------------------------------
-- Basic operations on 'Expression'

instance KnownSymbol name => IsLabel name (Expression a -> Expression b) where
    fromLabel exp = Access exp (Label @name)

instance IsString a => IsString (Expression a) where
    fromString = StringLiteral . fromString

instance Num a => Num (Expression a) where
    (+) = Infix "+"

    (-) = Infix "-"

    (*) = Infix "*"

    abs x = Apply "ABS" (Single x)

    signum x = Apply "SIGN" (Single x)

    fromInteger = IntegerLiteral

instance Fractional a => Fractional (Expression a) where
    (/) = Infix "/"

    recip = (1 /)

    fromRational ratio = fromInteger (numerator ratio) / fromInteger (denominator ratio)

(==) :: Expression a -> Expression a -> Expression Bool
(==) = Infix "="

infix 4 ==

(>) :: Expression a -> Expression a -> Expression Bool
(>) = Infix ">"

infix 4 >

(<) :: Expression a -> Expression a -> Expression Bool
(<) = Infix "<"

infix 4 <

(>=) :: Expression a -> Expression a -> Expression Bool
(>=) = Infix ">="

infix 4 >=

(<=) :: Expression a -> Expression a -> Expression Bool
(<=) = Infix "<="

infix 4 <=

not :: Expression Bool -> Expression Bool
not x = Apply "NOT" (Single x)

(&&) :: Expression Bool -> Expression Bool -> Expression Bool
(&&) = Infix "AND"

infixr 3 &&

(||) :: Expression Bool -> Expression Bool -> Expression Bool
(||) = Infix "OR"

infixr 2 ||

true :: Expression Bool
true = BoolLiteral True

false :: Expression Bool
false = BoolLiteral False

----------------------------------------------------------------------------------------------------
-- Renderer

renderExpression :: Expression a -> Render.Render
renderExpression = \case
    IntegerLiteral int -> Render.showable int

    RealLiteral real -> Render.showable real

    StringLiteral string -> Render.string (Render.fromText string)

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
        [ accessParens exp (renderExpression exp)
        , "."
        , fromString (symbolVal label)
        ]

    Apply name params ->
        Render.fromText name
        <> Render.parens (mconcat (intersperse "," (bfoldMap (pure . renderExpression) params)))

    where
        accessParens = \case
            Variable{} -> id
            _          -> Render.parens

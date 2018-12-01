{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SQL.Syntax
    ( expDoc )
where

import GHC.TypeLits

import qualified Data.Text        as Text
import           Data.Vinyl       (Rec (..))
import qualified Text.PrettyPrint as Pretty

import Language.SQL.Expression

binOpDoc :: BinaryOp a b -> Pretty.Doc
binOpDoc And      = "AND"
binOpDoc Or       = "OR"
binOpDoc Equals   = "="
binOpDoc Plus     = "+"
binOpDoc Minus    = "-"
binOpDoc Multiply = "*"
binOpDoc Divide   = "/"

callableDoc :: Callable xs r -> Rec Expression xs -> Pretty.Doc
callableDoc Absolute (x :& RNil) = "ABS" <> Pretty.parens (expDoc x)
callableDoc Not      (x :& RNil) = "NOT" <> Pretty.parens (expDoc x)
callableDoc Sign     (x :& RNil) = "SIGN" <> Pretty.parens (expDoc x)

quoteText :: Text.Text -> Pretty.Doc
quoteText =
    Pretty.text . Text.unpack . Text.concatMap insertQuote
    where
        insertQuote '\'' = "''"
        insertQuote x    = Text.singleton x

expDoc :: Expression a -> Pretty.Doc
expDoc = \case
    IntegerLiteral integer -> Pretty.integer integer

    RealLiteral real -> Pretty.double real

    BoolLiteral True -> "true"
    BoolLiteral _    -> "false"

    StringLiteral string -> quoteText string

    Variable name -> Pretty.text (Text.unpack name)

    Infix op lhs rhs -> Pretty.sep
        [ Pretty.parens (expDoc lhs)
        , binOpDoc op
        , Pretty.parens (expDoc rhs)
        ]

    Access subject field@Label -> Pretty.hcat
        [ Pretty.parens (expDoc subject)
        , "."
        , Pretty.text (symbolVal field)
        ]

    Apply callable params -> callableDoc callable params

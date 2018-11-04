{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SQL.Syntax
    ( BinaryOp (..)
    , Expression (..)
    , expDoc
    , Source (..)
    , Statement (..)
    , statementDoc
    )
where

import           Data.Foldable      (toList)
import           Data.List          (intersperse)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text          as Text
import qualified Text.PrettyPrint   as Pretty

data BinaryOp
    = And
    | Or
    | Equals
    | Plus
    deriving (Show, Eq)

binOpDoc :: BinaryOp -> Pretty.Doc
binOpDoc And    = "AND"
binOpDoc Or     = "OR"
binOpDoc Equals = "="
binOpDoc Plus   = "+"

data Expression
    = IntLit Integer
    | RealLit Double
    | BoolLit Bool
    | Var Text.Text
    | Access Expression Text.Text
    | Infix Expression BinaryOp Expression
    | Apply Text.Text [Expression]
    deriving (Show, Eq)

expDoc :: Expression -> Pretty.Doc
expDoc = \case
    IntLit integer -> Pretty.integer integer

    RealLit real -> Pretty.double real

    BoolLit True -> "true"
    BoolLit _    -> "false"

    Var name -> Pretty.text (Text.unpack name)

    Access subject field -> Pretty.hcat
        [ Pretty.parens (expDoc subject)
        , "."
        , Pretty.text (Text.unpack field)
        ]

    Infix lhs op rhs -> Pretty.sep
        [ Pretty.parens (expDoc lhs)
        , binOpDoc op
        , Pretty.parens (expDoc rhs)
        ]

    Apply fun params ->
        Pretty.text (Text.unpack fun)
        <> Pretty.parens (Pretty.hcat (intersperse ", " (map expDoc params)))

data Source
    = Simple
        { sourceAlias     :: Text.Text
        , sourceStatement :: Statement
        }
    | InnerJoin
        { sourceLeftName  :: Text.Text
        , sourceLeftExps  :: Statement
        , sourceRightName :: Text.Text
        , sourceRightExps :: Statement
        , sourceOnExp     :: Expression
        }
    deriving (Show, Eq)

sourceDoc :: Source -> Pretty.Doc
sourceDoc = \case
    Simple alias statement ->
        case statement of
            TableOnly name -> Pretty.sep
                [ Pretty.text (Text.unpack name)
                , "AS " <> Pretty.text (Text.unpack alias)
                ]

            _ -> Pretty.sep
                [ Pretty.parens (statementDoc statement)
                , "AS " <> Pretty.text (Text.unpack alias)
                ]

    InnerJoin lhsAlias lhs rhsAlias rhs condition -> Pretty.sep
        [ Pretty.parens (statementDoc lhs)
        , "AS " <> Pretty.text (Text.unpack lhsAlias)
        , "INNER JOIN"
        , Pretty.parens (statementDoc rhs)
        , "AS " <> Pretty.text (Text.unpack rhsAlias)
        , "ON"
        , Pretty.parens (expDoc condition)
        ]

data Statement
    = Select
        { selectExps      :: NonEmpty (Text.Text, Expression)
        , selectSource    :: [Source]
        , selectCondition :: Expression
        }
    | TableOnly
        { selectTable :: Text.Text }
    deriving (Show, Eq)

statementDoc :: Statement -> Pretty.Doc
statementDoc = \case
    Select exps mbSources mbCondition -> Pretty.vcat
        [ selectDoc exps
        , fromDoc mbSources
        , whereDoc mbCondition
        ]

    TableOnly name -> "TABLE " <> Pretty.text (Text.unpack name)

    where
        commaify []       = []
        commaify [last]   = [last]
        commaify (x : xs) = x <> Pretty.comma : commaify xs

        indentSection header items = Pretty.hang header 4 (Pretty.vcat (commaify items))

        selectDoc exps = indentSection "SELECT" (map selectExpDoc (toList exps))

        selectExpDoc (alias, exp) = Pretty.sep
            [ Pretty.parens (expDoc exp)
            , "AS " <> Pretty.text (Text.unpack alias)
            ]

        fromDoc []      = Pretty.empty
        fromDoc sources = indentSection "FROM" (commaify (map sourceDoc sources))

        whereDoc (BoolLit True) = Pretty.empty
        whereDoc condition      = Pretty.hang "WHERE" 4 (expDoc condition)

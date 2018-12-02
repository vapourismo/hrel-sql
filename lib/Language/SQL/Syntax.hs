{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Language.SQL.Syntax
    ( Builder
    , runBuilder
    , expDoc
    , statementDoc
    )
where

import GHC.TypeLits

import Control.Monad.State.Strict

import           Data.Functor.Const
import           Data.Functor.Product
import           Data.List            (intersperse)
import           Data.Proxy
import qualified Data.Text            as Text
import qualified Text.PrettyPrint     as Pretty

import Language.SQL.Expression
import Language.SQL.Row
import Language.SQL.Statement

type Builder = State Int

runBuilder :: Builder a -> a
runBuilder action = evalState action 0

allocName :: Builder Text.Text
allocName = Text.pack . ('B' :) . show <$> state (\idx -> (idx, idx + 1))

stringLiteral :: Text.Text -> Pretty.Doc
stringLiteral =
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

    StringLiteral string -> stringLiteral string

    Variable name -> Pretty.text (Text.unpack name)

    Infix op lhs rhs -> Pretty.sep
        [ Pretty.parens (expDoc lhs)
        , Pretty.text (Text.unpack op)
        , Pretty.parens (expDoc rhs)
        ]

    Access subject field@Label -> Pretty.hcat
        [ Pretty.parens (expDoc subject)
        , "."
        , Pretty.text (symbolVal field)
        ]

    Apply name params ->
        Pretty.text (Text.unpack name)
        <> Pretty.parens (Pretty.hcat (intersperse ", " (foldMapRow (pure . expDoc) params)))

selectStatement :: Row row => Expression (row Expression) -> row Expression
selectStatement exp =
    mapRow (\(Named name _) -> Access exp name) (nameFields (pureRow (Const ())))

prepareSource :: Row row => Statement row -> Builder (Product (Const Pretty.Doc) Captured row)
prepareSource (statement :: Statement row) = do
    name <- allocName
    doc  <- statementDoc statement
    pure $ Pair
        (Const (mkDoc name doc))
        (Captured (selectStatement (Variable name) :: row Expression))
    where
        mkDoc name doc = Pretty.sep
            [ Pretty.parens doc
            , "AS"
            , Pretty.text (Text.unpack name)
            ]

selectDoc :: Named Expression a -> Pretty.Doc
selectDoc (Named name@Label exp) = Pretty.sep
    [ Pretty.parens (expDoc exp)
    , "AS"
    , Pretty.text (symbolVal name)
    ]

statementDoc :: Row row => Statement row -> Builder Pretty.Doc
statementDoc = \case
    TableOnly name -> pure ("TABLE ONLY " <> Pretty.text (Text.unpack name))

    Select expand restrict sources -> do
        sources <- traverseConstrainedRow (Proxy @Row) prepareSource sources

        let binders = mapRow (\(Pair _ rhs) -> rhs) sources

        let selectClause =
                Pretty.hcat
                $ intersperse ", "
                $ foldMapRow (pure . selectDoc) (nameFields (expand binders))

        let fromClause =
                Pretty.hcat
                $ intersperse ", "
                $ foldMapRow (\(Pair (Const doc) _) -> pure doc) sources

        let whereClause = expDoc (restrict binders)

        pure $ Pretty.sep
            [ "SELECT"
            , selectClause
            , "FROM"
            , fromClause
            , "WHERE"
            , whereClause
            ]

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

import GHC.TypeLits (symbolVal)

import Control.Monad.State.Strict (State, evalState, state)

import           Data.Functor.Const   (Const (..))
import           Data.Functor.Product (Product (..))
import           Data.List            (intersperse)
import           Data.Proxy           (Proxy (..))
import qualified Data.Text            as Text
import qualified Text.PrettyPrint     as Pretty

import Language.SQL.Expression (Expression (..))
import Language.SQL.Row        (Label (..), Named (..), Row (..), RowFoldable (..), RowFunctor (..),
                                Single (..), foldMapRowWithName, traverseConstrainedRow)
import Language.SQL.Statement  (Statement (..))

type Builder = State Int

runBuilder :: Builder a -> a
runBuilder action = evalState action 0

allocName :: Builder String
allocName = ('B' :) . show <$> state (\idx -> (idx, idx + 1))

stringLiteral :: Text.Text -> Pretty.Doc
stringLiteral =
    Pretty.quotes . Pretty.text . Text.unpack . Text.concatMap insertQuote
    where
        insertQuote '\'' = "''"
        insertQuote x    = Text.singleton x

alias :: String -> Pretty.Doc -> Pretty.Doc
alias name doc = Pretty.sep
    [ Pretty.parens doc
    , "AS"
    , Pretty.text name
    ]

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
selectStatement exp = mapRow (\(Named name) -> Access exp name) nameFields

prepareSource :: Row row => Statement row -> Builder (Product (Const Pretty.Doc) (Single Expression) row)
prepareSource (statement :: Statement row) = do
    name <- allocName
    doc  <- statementDoc statement
    pure $ Pair
        (Const (alias name doc))
        (Single (selectStatement (Variable (Text.pack name)) :: row Expression))

selectDoc :: Label name -> Expression a -> Pretty.Doc
selectDoc name@Label exp = alias (symbolVal name) (expDoc exp)

statementDoc :: Row row => Statement row -> Builder Pretty.Doc
statementDoc = \case
    TableOnly name -> pure ("TABLE ONLY " <> Pretty.text (Text.unpack name))

    Select expand restrict sources -> do
        sources <- traverseConstrainedRow (Proxy @Row) prepareSource sources

        let binders = mapRow (\(Pair _ rhs) -> rhs) sources

        let selectClause =
                case foldMapRowWithName (\n e -> [selectDoc n e]) (expand binders) of
                    [] -> "SELECT NULL"
                    xs -> "SELECT " <> Pretty.hcat (intersperse ", " xs)

        let fromClause =
                case foldMapRow (\(Pair (Const doc) _) -> pure doc) sources of
                    [] -> mempty
                    xs -> "FROM " <> Pretty.hcat (intersperse ", " xs)

        let whereClause = "WHERE " <> expDoc (restrict binders)

        pure $ Pretty.sep
            [ selectClause
            , fromClause
            , whereClause
            ]

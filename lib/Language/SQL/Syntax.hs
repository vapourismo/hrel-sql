{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Language.SQL.Syntax
    ( Builder
    , runBuilder
    , expDoc
    , statementDoc
    )
where

import GHC.TypeLits

import Control.Monad.State.Strict

import           Data.List        (intersperse)
import qualified Data.Text        as Text
import           Data.Vinyl       (Rec (..), rtraverse)
import qualified Text.PrettyPrint as Pretty

import Language.SQL.Expression (BinaryOp (..), Callable (..), Expression (..), Label (..))
import Language.SQL.Selector
import Language.SQL.Statement

flatten :: (forall x. f x -> a) -> Rec f xs -> [a]
flatten _ RNil      = []
flatten f (x :& xs) = f x : flatten f xs

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

    Infix (BinaryOp op) lhs rhs -> Pretty.sep
        [ Pretty.parens (expDoc lhs)
        , Pretty.text (Text.unpack op)
        , Pretty.parens (expDoc rhs)
        ]

    Access subject field@Label -> Pretty.hcat
        [ Pretty.parens (expDoc subject)
        , "."
        , Pretty.text (symbolVal field)
        ]

    Apply (Callable name) params ->
        Pretty.text (Text.unpack name)
        <> Pretty.parens (Pretty.hcat (intersperse "," (flatten expDoc params)))

selectorDoc :: Rec SelectorExpression xs -> Pretty.Doc
selectorDoc selector =
    Pretty.hcat (intersperse ", " (flatten go selector))
    where
        go :: SelectorExpression x -> Pretty.Doc
        go (SelectorExpression label@Label exp) =
            Pretty.sep
                [ Pretty.parens (expDoc exp)
                , "AS"
                , Pretty.text (symbolVal label)
                ]

sourceDoc :: Statement a -> Builder Pretty.Doc
sourceDoc source = do
    name <- allocName
    doc  <- statementDoc source
    pure (Pretty.sep [doc, "AS", Pretty.text (Text.unpack name)])

statementDoc :: Statement a -> Builder Pretty.Doc
statementDoc = \case
    TableOnly name -> pure ("TABLE ONLY " <> Pretty.text (Text.unpack name))

    Select expand restrict sources -> do
        binder     <- rtraverse (const (Variable <$> allocName)) sources
        fromClause <- sequenceA (flatten sourceDoc sources)

        pure $ Pretty.sep
            [ "SELECT"
            , selectorDoc (expand binder)
            , "FROM"
            , Pretty.hcat (intersperse ", " fromClause)
            , "WHERE"
            , expDoc (restrict binder)
            ]

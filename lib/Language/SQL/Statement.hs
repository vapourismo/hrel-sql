{-# LANGUAGE GADTs #-}

module Language.SQL.Statement
    ( Statement (..) )
where

import Data.Text  (Text)
import Data.Vinyl (Rec (..))

import Language.SQL.Expression
import Language.SQL.Selector

data Statement a where
    TableOnly :: Text -> Statement a

    Select
        :: (Rec Expression xs -> Selector a)
        -> (Rec Expression xs -> Expression SqlBool)
        -> Rec Statement xs
        -> Statement a

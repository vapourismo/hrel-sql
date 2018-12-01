{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where

import Data.Vinyl (Rec (..))

import Language.SQL.Columns
import Language.SQL.Expression
import Language.SQL.Selector
import Language.SQL.Statement

data TestTable

type instance Columns TestTable = '[ "x" ::: SqlInt ]

testTable :: Statement TestTable
testTable = TableOnly "test_table"

query :: Statement (One SqlInt)
query =
    Select
        (\(testTable :& RNil) -> single (testTable <.> #x))
        (\_ -> true)
        (testTable :& RNil)

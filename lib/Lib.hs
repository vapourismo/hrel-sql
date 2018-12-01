{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Lib where

import GHC.TypeLits (KnownSymbol)

import Data.Text  (Text)
import Data.Vinyl (Rec (..))

import Language.SQL.Columns
import Language.SQL.Expression

newtype SelectorExpression sig = SelectorExpression (Expression (SignatureType sig))

type Selector a = Rec SelectorExpression (Columns a)

class HasSelector a xs where
    select' :: Expression a -> Rec SelectorExpression xs

instance HasSelector a '[] where
    select' _ = RNil

instance
    ( ColumnType a name ~ typ
    , KnownSymbol name
    , HasSelector a xs
    )
    => HasSelector a ((name ::: typ) ': xs)
    where
        select' exp = SelectorExpression (exp <.> Label @name) :& select' exp

type Selectable a = HasSelector a (Columns a)

select :: Selectable a => Expression a -> Selector a
select = select'

data Statement a where
    TableOnly :: Text -> Statement a

    Select
        :: (Expression a -> Selector b)
        -> (Expression a -> Expression SqlBool)
        -> Statement a
        -> Statement b

    Join
        :: (Expression a -> Expression b -> Selector c)
        -> (Expression a -> Expression b -> Expression Bool)
        -> Statement a
        -> Statement b
        -> Statement c

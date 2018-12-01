{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Selector
    ( SelectorExpression (..)
    , Selector
    , Selectable
    , select
    , One (..)
    , single
    )
where

import GHC.TypeLits (KnownSymbol)

import Data.Kind  (Type)
import Data.Vinyl (Rec (..))

import Language.SQL.Columns
import Language.SQL.Expression

----------------------------------------------------------------------------------------------------
-- Selectors

data SelectorExpression :: Signature -> Type where
    SelectorExpression :: Label name -> Expression typ -> SelectorExpression (name ::: typ)

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
        select' exp =
            SelectorExpression label (exp <.> label) :& select' exp
            where
                label = Label @name

type Selectable a = HasSelector a (Columns a)

select :: Selectable a => Expression a -> Selector a
select = select'

newtype One a = One {unOne :: a}

type instance Columns (One a) = '["one" ::: a]

single :: Expression a -> Selector (One a)
single exp = SelectorExpression (Label @"one") exp :& RNil

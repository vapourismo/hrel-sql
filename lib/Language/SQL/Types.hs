{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Language.SQL.Types
    ( Label (..)
    , labelName
    , Named (..)
    , fromNamed
    , Name (..)
    , Single(..)
    )
where

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits         (KnownSymbol, Symbol, symbolVal)

import Data.Barbie             (ConstraintsB (..), FunctorB (..), ProductB (..), ProductBC (..),
                                TraversableB (..))
import Data.Barbie.Constraints (Dict (..))
import Data.Functor.Product    (Product (..))
import Data.Kind               (Type)
import Data.String             (IsString (fromString))
import Data.Text               (Text)

----------------------------------------------------------------------------------------------------
-- Label

data Label :: Symbol -> Type where
    Label :: KnownSymbol name => Label name

instance (a ~ b, KnownSymbol b) => IsLabel a (Label b) where
    fromLabel = Label

labelName :: Label name -> Name
labelName label@Label = fromString (symbolVal label)

----------------------------------------------------------------------------------------------------
-- Named

data Named :: k -> Type where
    Named :: Label name -> Named a

instance KnownSymbol name => IsLabel name (Named a) where
    fromLabel = Named (Label @name)

fromNamed :: Named a -> Name
fromNamed (Named name@Label) = fromString (symbolVal name)

----------------------------------------------------------------------------------------------------
-- Name

newtype Name = Name
    { fromName :: Text }
    deriving (Show, Eq, Ord, IsString, Semigroup)

----------------------------------------------------------------------------------------------------
-- Single

newtype Single (a :: k) (f :: k -> Type) = Single {unSingle :: f a}

instance FunctorB (Single a) where
    bmap f (Single x) = Single (f x)

instance TraversableB (Single a) where
    btraverse f (Single x) = Single <$> f x

instance ConstraintsB (Single a) where
    type AllB c (Single a) = c a

    baddDicts (Single x) = Single (Pair Dict x)

instance ProductB (Single a) where
    buniq = Single

    bprod (Single x) (Single y) = Single (Pair x y)

instance ProductBC (Single a) where
    bdicts = Single Dict

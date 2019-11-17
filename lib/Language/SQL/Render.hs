{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.SQL.Render where

import Control.Monad.Trans.State.Strict

import qualified Data.Set    as Set
import           Data.String (IsString (..))
import qualified Data.Text   as Text

import Language.SQL.Types (Name (..))

newtype Rendered = Rendered
    { fromRendered :: Text.Text }
    deriving (Eq, Ord, IsString, Semigroup, Monoid)

newtype Renderer a = Renderer
    { unRenderer :: State (Set.Set Name) a }
    deriving (Functor, Applicative, Monad)

type Render = Renderer Rendered

runRender :: Render -> Text.Text
runRender (Renderer action) = fromRendered (evalState action Set.empty)

instance IsString a => IsString (Renderer a) where
    fromString = Renderer . pure . fromString

instance Semigroup a => Semigroup (Renderer a) where
    lhs <> rhs = (<>) <$> lhs <*> rhs

instance Monoid a => Monoid (Renderer a) where
    mempty = pure mempty

allocName :: Name -> Renderer Name
allocName prefix = Renderer $ do
    currentNames <- get
    let name = head
            [ name
            | index <- [1 :: Integer ..]
            , let name = prefix <> Name (fromString (show index))
            , not (Set.member name currentNames)
            ]

    modify (Set.insert name)
    pure name

fromText :: Text.Text -> Render
fromText = pure . Rendered

showable :: Show a => a -> Render
showable value = fromString (show value)

quote :: Char -> Render -> Render
quote delim = fmap $ \body ->
    Rendered (Text.replace (Text.singleton delim) (Text.pack [delim, delim]) (fromRendered body))

parens :: Render -> Render
parens inner = "(" <> inner <> ")"

string :: Render -> Render
string = quote '\''

name :: Name -> Render
name (Name name) = quote '"' (fromText name)

alias :: Name -> Render -> Render
alias aliasName body = parens body <> " AS " <> name aliasName

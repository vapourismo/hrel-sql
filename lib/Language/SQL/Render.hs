{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.SQL.Render where

import           Data.List   (find)
import           Data.Maybe  (fromJust)
import qualified Data.Set    as Set
import           Data.String (IsString (..))
import qualified Data.Text   as Text

newtype Name = Name Text.Text
    deriving (Show, Eq, Ord, IsString, Semigroup)

newtype Renderer = Renderer {unRenderer :: Set.Set Name -> Text.Text}
    deriving (Semigroup, Monoid)

instance IsString Renderer where
    fromString = fromText . Text.pack

fromText :: Text.Text -> Renderer
fromText text = Renderer (const text)

withName :: Name -> (Name -> Renderer) -> Renderer
withName name handle =
    Renderer (makeName >>= invoke)
    where
        generateName (Name prefix) usedNames =
            fromJust $ find (not . flip Set.member usedNames) $
                map (\index -> Name (prefix <> Text.pack (show index))) [1 :: Integer ..]

        makeName usedNames
            | Set.member name usedNames = generateName name usedNames
            | otherwise                 = name

        invoke name usedNames = unRenderer (handle name) (Set.insert name usedNames)

showable :: Show a => a -> Renderer
showable value = fromString (show value)

quote :: Char -> Text.Text -> Renderer
quote delim body =
    fromText (Text.replace (Text.singleton delim) (Text.pack [delim, delim]) body)

parens :: Renderer -> Renderer
parens inner = "(" <> inner <> ")"

string :: Text.Text -> Renderer
string = quote '\''

name :: Name -> Renderer
name (Name name) = quote '"' name

alias :: Name -> Renderer -> Renderer
alias (Name name) body = parens body <> quote '"' name


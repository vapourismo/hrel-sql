{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.SQL.Render where

import           Data.List   (find)
import           Data.Maybe  (fromJust)
import qualified Data.Set    as Set
import           Data.String (IsString)
import qualified Data.Text   as Text

newtype Name = Name Text.Text
    deriving (Show, Eq, Ord, IsString, Semigroup)

newtype Renderer a = Renderer {unRenderer :: Set.Set Name -> a}
    deriving (Functor, Applicative, Monad)

withName :: Name -> (Name -> Renderer a) -> Renderer a
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

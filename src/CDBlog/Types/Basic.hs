{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CDBlog.Types.Basic where

import           Data.Hashable
import           Data.String
import qualified Data.Text                      as T
import           Data.Time
import           Fmt
import           GHC.Generics
import           Text.Enum.Text


newtype Date = Date { _Date :: Day }
  deriving (Eq,Ord,Show)

instance Buildable Date where
  build (Date dy) = build $ show dy

instance IsString Date where
  fromString = Date . read


newtype HTML = HTML { _HTML :: Builder }
  deriving (Buildable,Eq,Generic,IsString,Ord,Show)

fromHTML :: HTML -> String
fromHTML = fmt . _HTML

newtype URL = URL { _URL :: T.Text }
  deriving (Buildable,Eq,Generic,Hashable,IsString,Ord,Show,TextParsable)

newtype Version = Version { _Version :: T.Text }
  deriving (Buildable,Eq,Generic,Hashable,IsString,Ord,Show,TextParsable)

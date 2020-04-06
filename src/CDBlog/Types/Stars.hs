{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}

module CDBlog.Types.Stars where

import           Fmt
import           Text.Enum.Text


data Stars
  = S_0
  | S_1
  | S_2
  | S_3
  | S_4
  | S_5
  | S_6
  deriving (Bounded,Enum,Eq,Ord,Show)
  deriving (Buildable,TextParsable) via UsingEnumText Stars
  deriving (EnumText)

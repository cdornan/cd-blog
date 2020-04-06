{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module CDBlog.Kit.Stars where

import           CDBlog.Types.Basic
import           CDBlog.Types.Stars
import qualified Data.Text                      as T
import           Fmt
import           Text.Enum.Text
import           Text.Heredoc


stars :: Stars -> HTML
stars s0 = HTML $ build $ subst g $ subst f template
  where
    f :: Stars -> Builder
    f s = case s<=s0 of
      True  -> "#ffd055"
      False -> "#222222"

    g :: Visibility -> Builder
    g _ = case s0==S_6 of
      True  -> "visible"
      False -> "hidden"

data Visibility
  = V_vis
  deriving (Bounded,Enum,Eq,Ord,Show)
  deriving (Buildable,TextParsable) via UsingEnumText Visibility
  deriving (EnumText)

template :: T.Text
template = [here|<svg height="25" width="23" class="cd-star">
  <polygon points="9.9, 1.1, 3.3, 21.78, 19.8, 8.58, 0, 8.58, 16.5, 21.78" style="fill-rule:nonzero; fill: <<1>>;"/>
</svg>
<svg height="25" width="23" class="cd-star">
  <polygon points="9.9, 1.1, 3.3, 21.78, 19.8, 8.58, 0, 8.58, 16.5, 21.78" style="fill-rule:nonzero; fill: <<2>>;"/>
</svg>
<svg height="25" width="23" class="cd-star">
  <polygon points="9.9, 1.1, 3.3, 21.78, 19.8, 8.58, 0, 8.58, 16.5, 21.78" style="fill-rule:nonzero; fill: <<3>>;"/>
</svg>
<svg height="25" width="23" class="cd-star">
  <polygon points="9.9, 1.1, 3.3, 21.78, 19.8, 8.58, 0, 8.58, 16.5, 21.78" style="fill-rule:nonzero; fill: <<4>>;"/>
</svg>
<svg height="25" width="23" class="cd-star">
  <polygon points="9.9, 1.1, 3.3, 21.78, 19.8, 8.58, 0, 8.58, 16.5, 21.78" style="fill-rule:nonzero; fill: <<5>>;"/>
</svg>
<svg height="25" width="23" class="cd-star">
  <polygon points="9.9, 1.1, 3.3, 21.78, 19.8, 8.58, 0, 8.58, 16.5, 21.78" style="fill-rule:nonzero; fill: <<6>>; visibility: <<vis>>;"/>
</svg>
|]

subst :: EnumText i => (i->Builder) -> T.Text -> T.Text
subst f = foldr (.) id
  [ T.replace var $ fmt $ f nm
    | nm <- [minBound..maxBound]
    , let var = fmt $ "<<"+|nm|+">>" :: T.Text
    ]

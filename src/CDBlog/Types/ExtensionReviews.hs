{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module CDBlog.Types.ExtensionReviews
  ( ExtensionReview(..)
  , ExtensionID(..)
  ) where

import           CDBlog.Kit.Stars
import           CDBlog.Types.Basic
import           CDBlog.Types.Stars
import           CDBlog.Types.IsSeries
import           Fmt
import           Text.Enum.Text


data ExtensionReview =
  ExtensionReview
    { _er_id      :: ExtensionID
    , _er_date    :: Date
    , _er_home    :: URL
    , _er_stars   :: Stars
    }
  deriving (Show)

data ExtensionID
  = EI_OverloadedStrings
  | EI_RecordWildCards
  deriving (Bounded,Enum,Eq,Ord,Show)
  deriving (Buildable,TextParsable) via UsingEnumText ExtensionID
  deriving (EnumText)

instance IsSeries ExtensionReview where
  seriesPosts = map mk [minBound..maxBound]
  seriesPostPath ExtensionReview{..} =
      fmt $ "posts/"+|_er_date|+"-er-"+|_er_id|+".html"

instance Buildable ExtensionReview where
  build er@ExtensionReview{..} =
      "<div class=cd-er-row>"
        +|exnm _er_id   |+"\n"
        +|date _er_date |+"\n"
        +|home _er_home |+"\n"
        +|strs _er_stars|+
      "</div>\n"
    where
      exnm :: Buildable a => a -> Builder
      exnm x = "<div class=cd-er-name>"+|x|+"</div>"

      date :: Buildable a => a -> Builder
      date x = "<div class=cd-er-date><a title='review article' href='"+|revw|+"'>"+|x|+"</a></div>"

      home :: Buildable a => a -> Builder
      home x = "<div class=cd-er-home><a title='GHC notes' href='"
                          +|x|+"'><span uk-icon='home'/></a></div>"

      strs :: Stars -> Builder
      strs x = mconcat
        [ "<a title='review article' class=cd-er-block-link href="+|revw|+">\n"
        , "<div class=cd-er-strs>\n"
        , build $ stars x
        , "</div>\n"
        , "</a>\n"
        ]

      revw :: Builder
      revw = "/"+|seriesPostPath er|+""


--------------------------------------------------------------------------------
-- mk
--------------------------------------------------------------------------------

-- | the database of our extensions reviews
mk :: ExtensionID -> ExtensionReview
mk pid = case pid of
  EI_OverloadedStrings ->
    ExtensionReview
      { _er_id      = pid
      , _er_date    = "2020-04-03"
      , _er_home    = "https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals"
      , _er_stars   = S_5
      }
  EI_RecordWildCards ->
    ExtensionReview
      { _er_id      = pid
      , _er_date    = "2020-04-18"
      , _er_home    = "https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RecordWildCards"
      , _er_stars   = S_4
      }

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module CDBlog.Types.StructureReviews
  ( StructureReview(..)
  , StructureID(..)
  ) where

import           CDBlog.Kit.Stars
import           CDBlog.Types.Basic
import           CDBlog.Types.Stars
import           CDBlog.Types.IsSeries
import           Fmt
import           Text.Enum.Text


data StructureReview =
  StructureReview
    { _sr_id      :: StructureID
    , _sr_date    :: Date
    , _sr_home    :: URL
    , _sr_stars   :: Stars
    }
  deriving (Show)

data StructureID
  = EI_Functor
  deriving (Bounded,Enum,Eq,Ord,Show)
  deriving (Buildable,TextParsable) via UsingEnumText StructureID
  deriving (EnumText)

instance IsSeries StructureReview where
  seriesPosts = map mk [minBound..maxBound]
  seriesPostPath StructureReview{..} =
      fmt $ "posts/"+|_sr_date|+"-sr-"+|_sr_id|+".html"

instance Buildable StructureReview where
  build sr@StructureReview{..} =
      "<div class=cd-sr-row>"
        +|exnm _sr_id     |+"\n"
        +|date _sr_date   |+"\n"
        +|home _sr_home   |+"\n"
        +|strs _sr_stars  |+
      "</div>\n"
    where
      exnm :: Buildable a => a -> Builder
      exnm x = "<div class=cd-sr-name>"+|x|+"</div>"

      date :: Buildable a => a -> Builder
      date x = "<div class=cd-sr-date><a title='the article' href='"
                                    +|revw|+"'>"+|x|+"</a></div>"

      home :: Buildable a => a -> Builder
      home x = "<div class=cd-sr-home><a title='Hackage page' href='"
                                    +|x|+"'><span uk-icon='home'/></a></div>"

      strs :: Stars -> Builder
      strs x = mconcat
        [ "<a title='the article' class=cd-sr-block-link href="+|revw|+">\n"
        , "<div class=cd-sr-strs>\n"
        , build $ stars x
        , "</div>\n"
        , "</a>\n"
        ]

      revw :: Builder
      revw = "/"+|seriesPostPath sr|+""


--------------------------------------------------------------------------------
-- mk
--------------------------------------------------------------------------------

-- | the database of our structure reviews
mk :: StructureID -> StructureReview
mk pid = case pid of
  EI_Functor ->
    StructureReview
      { _sr_id      = pid
      , _sr_date    = "2020-04-05"
      , _sr_home    = "https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html"
      , _sr_stars   = S_6
      }

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module CDBlog.Types.PackageReviews
  ( PackageReview(..)
  , PackageID(..)
  ) where

import           CDBlog.Kit.Stars
import           CDBlog.Types.Basic
import           CDBlog.Types.IsSeries
import           CDBlog.Types.Stars
import           Fmt
import           Text.Enum.Text


data PackageReview =
  PackageReview
    { _pr_id      :: PackageID
    , _pr_version :: Version
    , _pr_date    :: Date
    , _pr_home    :: URL
    , _pr_issues  :: URL
    , _pr_stars   :: Stars
    }
  deriving (Show)

data PackageID
  = PI_data_default
  | PI_heredoc
  deriving (Bounded,Enum,Eq,Ord,Show)
  deriving (Buildable,TextParsable) via UsingEnumText PackageID
  deriving (EnumText)

instance IsSeries PackageReview where
  seriesPosts = map mk [minBound..maxBound]
  seriesPostPath PackageReview{..} =
      fmt $ "posts/"+|_pr_date|+"-pr-"+|_pr_id|+".html"

instance Buildable PackageReview where
  build pr@PackageReview{..} =
      "<div class=cd-pr-row>"
        +|pknm _pr_id     |+"\n"
        +|vrsn _pr_version|+"\n"
        +|date _pr_date   |+"\n"
        +|home _pr_home   |+"\n"
        +|issu _pr_issues |+"\n"
        +|strs _pr_stars  |+
      "</div>\n"
    where
      pknm :: Buildable a => a -> Builder
      pknm x = "<div class=cd-pr-name>"+|x|+"</div>"

      vrsn :: Buildable a => a -> Builder
      vrsn x = "<div class=cd-pr-vrsn><a title='Hackage page' href='"+|hckg|+"'>"+|x|+"</a></div>"

      date :: Buildable a => a -> Builder
      date x = "<div class=cd-pr-date><a title='review article' href='"+|revw|+"'>"+|x|+"</a></div>"

      home :: Buildable a => a -> Builder
      home x = "<div class=cd-pr-home><a title='package home page' href='"
                                  +|x|+"'><span uk-icon='home'/></a></div>"

      issu :: Buildable a => a -> Builder
      issu x = "<div class=cd-pr-issu><a title='package issue tracker' href='"
                                  +|x|+"'><span uk-icon='warning'/></a></div>"

      strs :: Stars -> Builder
      strs x = mconcat
        [ "<a title='review article' class=cd-pr-block-link href="+|revw|+">\n"
        , "<div class=cd-pr-strs>\n"
        , build $ stars x
        , "</div>\n"
        , "</a>\n"
        ]

      hckg :: Builder
      hckg = "https://hackage.haskell.org/package/"
                                    +|_pr_id|+"-"+|_pr_version|+""
      revw :: Builder
      revw = "/"+|seriesPostPath pr|+""


--------------------------------------------------------------------------------
-- mk
--------------------------------------------------------------------------------

-- | the database of our package reviews
mk :: PackageID -> PackageReview
mk pid = case pid of
  PI_data_default ->
    PackageReview
      { _pr_id      = pid
      , _pr_version = "0.1.2.0"
      , _pr_date    = "2020-04-04"
      , _pr_home    = "https://github.com/mauke/data-default"
      , _pr_issues  = "https://github.com/mauke/data-default/issues"
      , _pr_stars   = S_5
      }
  PI_heredoc ->
    PackageReview
      { _pr_id      = pid
      , _pr_version = "0.2.0.0"
      , _pr_date    = "2020-04-21"
      , _pr_home    = "http://hackage.haskell.org/package/heredoc"
      , _pr_issues  = "http://hackage.haskell.org/package/heredoc"
      , _pr_stars   = S_5
      }

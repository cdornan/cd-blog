{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CDBlog.Review
  ( encapsulate
  , extensionReviewList
  , packageReviewList
  , structureReviewList
  ) where

import           CDBlog.Types
import           Control.Applicative
import           Data.Maybe
import           Fmt
import           Hakyll


-- | checks to see if post belongs to one of our series, returning its
-- HTML encapsulation if so
encapsulate :: Item a -> Compiler String
encapsulate itm = mk <$> er <*> pr <*> sr
  where
    mk e p s = e <|> p <|> s

    er = fromMaybe "" <$> extension_cap itm
    pr = fromMaybe "" <$> package_cap   itm
    sr = fromMaybe "" <$> structure_cap itm

-- | generate the post list for our extension reviews
extensionReviewList :: Item a -> Compiler String
extensionReviewList _ = fromHTML <$>
    seriesPostList (seriesPosts :: [ExtensionReview])

-- | generate the post list for our package reviews
packageReviewList :: Item a -> Compiler String
packageReviewList _ = fromHTML <$>
    seriesPostList (seriesPosts :: [PackageReview])

-- | generate the post list for our structure reviews
structureReviewList :: Item a -> Compiler String
structureReviewList _ = fromHTML <$>
    seriesPostList (seriesPosts :: [StructureReview])


--------------------------------------------------------------------------------
-- these encapsulation functions are currently identical but are expected to
-- diverge
--------------------------------------------------------------------------------

-- generate an HTML encapsulation for an extension review
extension_cap :: Item a -> Compiler (Maybe String)
extension_cap itm = fmap fromHTML <$> memberCap mk itm
  where
    mk :: ExtensionReview -> HTML
    mk pr = HTML $ fmt $ mconcat
      [ "<hr/>\n"
      , build pr
      , "<hr/>\n"
      ]

-- generate an HTML encapsulation for a package review
package_cap :: Item a -> Compiler (Maybe String)
package_cap itm = fmap fromHTML <$> memberCap mk itm
  where
    mk :: PackageReview -> HTML
    mk pr = HTML $ fmt $ mconcat
      [ "<hr/>\n"
      , build pr
      , "<hr/>\n"
      ]

-- generate an HTML encapsulation for an structure review
structure_cap :: Item a -> Compiler (Maybe String)
structure_cap itm = fmap fromHTML <$> memberCap mk itm
  where
    mk :: StructureReview -> HTML
    mk pr = HTML $ fmt $ mconcat
      [ "<hr/>\n"
      , build pr
      , "<hr/>\n"
      ]

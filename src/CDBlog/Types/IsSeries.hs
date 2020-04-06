{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CDBlog.Types.IsSeries where

import           CDBlog.Types.Basic
import           Data.Maybe
import           Fmt
import           Hakyll
import           System.FilePath


-- | for a to be a series it must be buildable into an HTML encapsulation
-- of a post in the series, we must be able enumerate each post in the
-- seris and be able to generate the URL path from the domeain
-- to the post
class Buildable a => IsSeries a where
  seriesPosts    :: [a]
  seriesPostPath :: a -> FilePath

-- | determines if the path of the article in Item is an extension review,
-- returning a  summary encapsulation if so
memberCap :: forall a x . IsSeries a
          => (a->HTML) -> Item x -> Compiler (Maybe HTML)
memberCap mk itm = return $ mk <$> is_review
  where
    is_review :: Maybe a
    is_review = listToMaybe
        [ er
          | er <- seriesPosts :: [a]
          , replaceExtension (seriesPostPath er) "md" == fp
          ]

    fp = toFilePath $ itemIdentifier itm

-- | convert series posts into Html
seriesPostList :: IsSeries a => [a] -> Compiler HTML
seriesPostList = return . HTML . fmt . mconcat . map ((<>"\n") . build)

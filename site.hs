--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Default
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith cfg $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend`
                    constField "description" "This is the post description"

            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderAtom feedConfig feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend`
                    constField "description" "This is the post description"

            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderRss feedConfig feedCtx posts

feedConfig :: FeedConfiguration
feedConfig =
  FeedConfiguration
    { feedTitle       = "chrisdornan.com"
    , feedDescription = "Chris Dornan's blog"
    , feedAuthorName  = "Chris Dornan"
    , feedAuthorEmail = "chris@chrisdornan.com"
    , feedRoot        = "http://chrisdornan.com"
    }


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat
    [ dateField "date" "%F"
    , defaultContext
    ]


--------------------------------------------------------------------------------
cfg :: Configuration
cfg = def
  { previewHost = "0.0.0.0"
  , previewPort = 8000
  }

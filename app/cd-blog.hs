{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           CDBlog
import           Data.Default
import           Hakyll
import           Hakyll.Web.Sass


main :: IO ()
main = hakyllWith cfg $ do

    match (fromList ["favicon.ico","apple-touch-icon.png"]) $ do
        route   idRoute
        compile copyFileCompiler

    match (fromRegex "^assets/scss/[^_].*.scss") $ do
        route $ setExtension "css"
        compile $ fmap compressCss <$> sassCompilerWith sass_options

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "assets/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["404.md","pages/about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["pages/extension-reviews.html"] $ do
        route     idRoute
        compile $ do
          let ctx = mconcat
                [ constField "title"               "Language Extension Reviews"
                , field      "extension-review-list"  extensionReviewList
                , defaultContext
                ]
          makeItem ""
              >>= loadAndApplyTemplate "templates/extension-reviews.html" ctx
              >>= loadAndApplyTemplate "templates/default.html"           ctx
              >>= relativizeUrls

    create ["pages/package-reviews.html"] $ do
        route     idRoute
        compile $ do
          let ctx = mconcat
                [ constField "title"                "Package Reviews"
                , field      "package-review-list"  packageReviewList
                , defaultContext
                ]
          makeItem ""
              >>= loadAndApplyTemplate "templates/package-reviews.html" ctx
              >>= loadAndApplyTemplate "templates/default.html"         ctx
              >>= relativizeUrls

    create ["pages/structure-reviews.html"] $ do
        route     idRoute
        compile $ do
          let ctx = mconcat
                [ constField "title"                  "Structure Reviews"
                , field      "structure-review-list"  structureReviewList
                , defaultContext
                ]
          makeItem ""
              >>= loadAndApplyTemplate "templates/structure-reviews.html" ctx
              >>= loadAndApplyTemplate "templates/default.html"         ctx
              >>= relativizeUrls

    create ["pages/archive.html"] $ do
        route idRoute
        compile $ do
            posts <- chronological =<< loadAll "posts/*"
            let ctx = mconcat
                  [ listField  "posts" postCtx $ pure posts
                  , constField "title" "Archives"
                  , defaultContext
                  ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = mconcat
                  [ listField   "posts" postCtx (return posts)
                  , constField  "title"   "Home"
                  , constField  "heading" "Posts"
                  , defaultContext
                  ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/**" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <>
                    constField "description" "This is the post description"

            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderAtom feedConfig feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <>
                    constField "description" "This is the post description"

            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderRss feedConfig feedCtx posts


--------------------------------------------------------------------------------
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
sass_options :: SassOptions
sass_options = defaultSassOptions
      { sassSourceMapEmbed = True
      , sassOutputStyle    = SassStyleCompressed
      , sassIncludePaths   = Just ["."]
      }


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat
    [ field     "capsule"     encapsulate
    , dateField "date"        "%F"
    , defaultContext
    ]


--------------------------------------------------------------------------------
cfg :: Configuration
cfg = def
  { previewHost = "0.0.0.0"
  , previewPort = 8000
  }

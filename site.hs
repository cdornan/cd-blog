{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Data.Default
import           Hakyll
import           Hakyll.Web.Sass


main :: IO ()
main = hakyllWith cfg $ do
    -- compile SASS/CSS
    depends <- makePatternDependency "assets/scss/**.scss"
    rulesExtraDependencies [depends] $ do
        match (fromRegex "^assets/scss/[^_].*.scss") $ do
            route $ setExtension "css"
            compile $ sassCompilerWith sass_options

    match "assets/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["pages/about.markdown"]) $ do
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

    create ["pages/archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
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
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
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
    [ dateField "date" "%F"
    , defaultContext
    ]


--------------------------------------------------------------------------------
cfg :: Configuration
cfg = def
  { previewHost = "0.0.0.0"
  , previewPort = 8000
  }

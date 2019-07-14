{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc
import           Text.Pandoc.Options
import qualified Data.Map as M
import qualified Data.Map.Lazy as ML
import           GHC.Generics

import           Control.Monad (zipWithM_)
import           Data.List (sortBy, intercalate)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (parseTimeM)
import           Data.Maybe (fromJust)
import           System.FilePath (takeFileName)
import           Data.Time.Format (defaultTimeLocale)
import qualified Data.Yaml as Y

import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Internal (preEscapedString)
import           Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A



hakyllConfig = defaultConfiguration { deployCommand = "bash -c 'cd _deploy && rm -rf * && cp -r ../_site/* . ; git add . ; $(git diff-index --quiet HEAD || git commit -m `date +%s`); $(git push origin gh-pages);'" }


data HaspotSetting = HaspotSetting {
  blog :: HaspotBlogSetting,
  author :: HaspotAuthorSetting
} deriving Generic

data HaspotBlogSetting = HaspotBlogSetting {
  title :: String,
  description:: String,
  root_url :: String,
  about_page_link :: String
} deriving Generic

data HaspotAuthorSetting = HaspotAuthorSetting {
  name :: String,
  email :: String,
  intro :: String
} deriving Generic

instance Y.FromJSON HaspotSetting
instance Y.FromJSON HaspotBlogSetting
instance Y.FromJSON HaspotAuthorSetting


main :: IO ()
main = do 
  maybeConfig <- Y.decodeFile "conf/setting.yml"
  case maybeConfig of
      Just conf -> hakyllSetting conf
      Nothing -> putStrLn "Configuration Error"

pandocMathCompiler =
    let defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = enableExtension Ext_tex_math_dollars defaultExtensions 
        newExtensions' = enableExtension Ext_tex_math_double_backslash defaultExtensions 
        newExtensions'' = enableExtension Ext_latex_macros defaultExtensions 
        newExtensions''' = enableExtension Ext_simple_tables defaultExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions''',
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

hakyllSetting :: HaspotSetting -> IO ()
hakyllSetting conf = do
    hakyllWith hakyllConfig $ do
      match "images/**" $ do
          route   idRoute
          compile copyFileCompiler
      
      match "fonts/*" $ do
          route   idRoute
          compile copyFileCompiler

      match (fromList ["robots.txt", "favicon.ico"]) $ do
          route idRoute
          compile copyFileCompiler

      match "stylesheets/*" $ do
          route   idRoute
          compile compressCssCompiler
      
      match "javascript/*" $ do
          route   idRoute
          compile copyFileCompiler

      match "posts/*" $ do
          route $ setExtension "html"
          compile $ pandocMathCompiler 
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= saveSnapshot "teaser"
              >>= loadAndApplyTemplate "templates/post-default.html"  ((blogCtx conf) `mappend` (authorCtx conf) `mappend` mathCtx `mappend` defaultContext)
              >>= relativizeUrls


      paginate 2 $ \index maxIndex itemsForPage -> do
          let id = if index == 1 
                      then fromFilePath "index.html"
                      else fromFilePath $ (show index) ++ "/index.html"

          create [id] $ do
              route idRoute
              compile $ do
                  let loadTeaser id = loadSnapshot id "teaser"
                                          >>= loadAndApplyTemplate "templates/teaser.html" defaultContext

                  item1 <- loadTeaser (head itemsForPage)
                  item2 <- loadTeaser (last itemsForPage)

                  let content = if length itemsForPage == 1 then [item1] else [item1, item2]
                  let postsCtx = if index == 0
                      then
                          listField "posts" postCtx (return content) `mappend`
                          field "navlinkolder" (\_ -> return $ indexNavLink index 1 maxIndex) `mappend`
                          field "currIndex" (\_ -> return $ show $ index) `mappend`
                          field "maxIndex" (\_ -> return $ show maxIndex) `mappend`
                          (blogCtx conf) `mappend`
                          (authorCtx conf) `mappend`
                          defaultContext
                      else if index == maxIndex 
                      then
                          listField "posts" postCtx (return content) `mappend`
                          field "navlinknewer" (\_ -> return $ indexNavLink index (-1) maxIndex) `mappend`
                          field "currIndex" (\_ -> return $ show $ index) `mappend`
                          field "maxIndex" (\_ -> return $ show maxIndex) `mappend`
                          (blogCtx conf) `mappend`
                          (authorCtx conf) `mappend`
                          defaultContext
                      else
                          listField "posts" postCtx (return content) `mappend`
                          field "navlinkolder" (\_ -> return $ indexNavLink index 1 maxIndex) `mappend`
                          field "navlinknewer" (\_ -> return $ indexNavLink index (-1) maxIndex) `mappend`
                          field "currIndex" (\_ -> return $ show $ index) `mappend`
                          field "maxIndex" (\_ -> return $ show maxIndex) `mappend`
                          (blogCtx conf) `mappend`
                          (authorCtx conf) `mappend`
                          defaultContext

                  makeItem ""
                      >>= loadAndApplyTemplate "templates/paginate.html" ((blogCtx conf) `mappend` (authorCtx conf) `mappend` postsCtx)
                      >>= relativizeUrls

      match "templates/*" $ compile templateCompiler


      create ["archive.html"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAllSnapshots  "posts/*" "teaser"
          let headingsCtx = listField "posts" postCtx (return posts) `mappend`
                            (blogCtx conf) `mappend`
                            defaultContext
          makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" headingsCtx
            >>= relativizeUrls


      create ["rss/feed.xml"] $ do
          route idRoute
          compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            let feedConfiguration = FeedConfiguration {
                  feedTitle = title $ blog conf
                , feedDescription = description $ blog conf
                , feedAuthorName = name $ author conf
                , feedAuthorEmail = email $ author conf
                , feedRoot = root_url $ blog conf
                }
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "teaser"
            renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"

authorCtx :: HaspotSetting -> Context a
authorCtx conf = field "author_name" ( \item -> do
                      metadata <- getMetadata $ itemIdentifier item
                      return $ name $ author conf
                      ) `mappend`
                 field "author_intro" ( \item -> do
                      metadata <- getMetadata $ itemIdentifier item
                      return $ intro $ author conf
                      )

blogCtx :: HaspotSetting -> Context a
blogCtx conf = field "blog_title" ( \item -> do
                      metadata <- getMetadata $ itemIdentifier item
                      return $ title $ blog conf
                      ) `mappend`
                 field "blog_description" ( \item -> do
                      metadata <- getMetadata $ itemIdentifier item
                      return $ description $ blog conf
                      ) `mappend`
                 field "about_page_link" ( \item -> do
                      metadata <- getMetadata $ itemIdentifier item
                      return $ about_page_link $ blog conf
                      ) `mappend`
                 field "root_url" ( \item -> do
                      metadata <- getMetadata $ itemIdentifier item
                      return $ root_url $ blog conf
                      )


pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }


paginate:: Int -> (Int -> Int -> [Identifier] -> Rules ()) -> Rules ()
paginate itemsPerPage rules = do
    identifiers <- getMatches "posts/*"

    let sorted = reverse $ sortBy byDate identifiers
        chunks = paginateEvery itemsPerPage sorted
        maxIndex = length chunks
        pageNumbers = take maxIndex [1..]
        process i is = rules i maxIndex is
    zipWithM_ process pageNumbers chunks
        where
            byDate id1 id2 =
                let fn1 = takeFileName $ toFilePath id1
                    fn2 = takeFileName $ toFilePath id2
                    parseTime' fn = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
                in compare ((parseTime' fn1) :: Maybe UTCTime) ((parseTime' fn2) :: Maybe UTCTime)


indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $ 
                   (preEscapedString lab)
        lab = if (d > 0) then "Older Posts &rarr;" else "&larr; Newer Posts"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "/index.html"
                    _ -> "/" ++ (show $ n + d) ++ "/"

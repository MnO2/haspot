--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc
import qualified Data.Map as M

import           Control.Monad (zipWithM_)
import           Data.List (sortBy, intercalate)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (parseTime)
import           System.FilePath (takeFileName)
import           System.Locale (defaultTimeLocale)

import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Internal (preEscapedString)
import           Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


--------------------------------------------------------------------------------

hakyllConfig = defaultConfiguration { deployCommand = "cd _deploy && rm -rf * && cp -r ../_site/* . && git add . && $(git diff-index --quiet HEAD || git commit -m `date +%s`) && git push origin master" }



main :: IO ()
main = hakyllWith hakyllConfig $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler
    
    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["robots.txt", "CNAME"]) $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    
    match "javascript/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "teaser"
            >>= loadAndApplyTemplate "templates/post-default.html"  (mathCtx `mappend` defaultContext)
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
                                        >>= relativizeUrls
                item1 <- loadTeaser (head itemsForPage)
                item2 <- loadTeaser (last itemsForPage)
                let body1 = itemBody item1
                    body2 = if length itemsForPage == 1 then "" else itemBody item2
                    postsCtx =
                        listField "posts" postCtx (return $ [item1, item2]) `mappend`
                        field "navlinkolder" (\_ -> return $ indexNavLink index 1 maxIndex) `mappend`
                        field "navlinknewer" (\_ -> return $ indexNavLink index (-1) maxIndex) `mappend`
                        field "currIndex" (\_ -> return $ show $ index) `mappend`
                        field "maxIndex" (\_ -> return $ show maxIndex) `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/paginate.html" postsCtx
                    >>= relativizeUrls


    match "templates/*" $ compile templateCompiler


    create ["rss/feed.xml"] $ do
        route idRoute
        compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
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
  return $ if "mathjax" `M.member` metadata
             then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
             else ""


pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "MnO2's Blog"
  , feedDescription = "MnO2's Blog"
  , feedAuthorName = "Paul Meng"
  , feedAuthorEmail = "mno2@mno2.org"
  , feedRoot = "http://blog.mno2.org"
  }


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
                    parseTime' fn = parseTime defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
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

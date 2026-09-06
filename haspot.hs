{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haspot: a static blog generator built on Hakyll.
--
-- Site-wide settings are read from @conf/setting.yml@ (see 'HaspotSetting').
-- Markdown posts under @posts/@ are rendered with Pandoc and assembled into
-- paginated index pages, an archive page, an Atom feed, a sitemap and a
-- robots.txt.  Everything else (images, stylesheets, the about page) is
-- copied through.
module Main (main) where

import           Data.Char           (isSpace)
import           Data.List           (isInfixOf, stripPrefix)
import           Data.Maybe          (fromMaybe)
import           Data.Time           (getCurrentTime, toGregorian, utctDay)
import qualified Data.Yaml           as Y
import           GHC.Generics        (Generic)
import           Hakyll
import           System.Exit         (exitFailure)
import           System.IO           (hPutStrLn, stderr)
import           Text.Pandoc.Options (Extension (..), HTMLMathMethod (..),
                                      ReaderOptions (..), WriterOptions (..),
                                      enableExtension)

--------------------------------------------------------------------------------
-- Settings

-- | Path of the YAML file holding the site settings.
settingsFile :: FilePath
settingsFile = "conf/setting.yml"

-- | Number of posts shown on each index page.
postsPerPage :: Int
postsPerPage = 2

-- | Top level of @conf/setting.yml@.
data HaspotSetting = HaspotSetting
    { blog   :: BlogSetting
    , author :: AuthorSetting
    } deriving (Generic, Show)

-- | The @blog:@ section.  Optional keys may be omitted from the YAML file.
data BlogSetting = BlogSetting
    { title                    :: String
    , description              :: String
    , root_url                 :: String        -- ^ e.g. @https://example.com@, no trailing slash
    , about_page_link          :: String        -- ^ root-relative, e.g. @/about@
    , google_analytics_id      :: Maybe String  -- ^ GA4 measurement id, e.g. @G-XXXXXXX@
    , google_site_verification :: Maybe String  -- ^ Search Console verification token
    } deriving (Generic, Show)

-- | The @author:@ section.
data AuthorSetting = AuthorSetting
    { name  :: String
    , email :: String
    , intro :: String
    } deriving (Generic, Show)

instance Y.FromJSON HaspotSetting
instance Y.FromJSON BlogSetting
instance Y.FromJSON AuthorSetting

--------------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
    result <- Y.decodeFileEither settingsFile
    case result of
        Left err -> do
            hPutStrLn stderr $ "haspot: cannot load " ++ settingsFile ++ ":\n"
                ++ Y.prettyPrintParseException err
            exitFailure
        Right conf -> do
            year <- currentYear
            hakyllWith hakyllConfig (rules conf year)

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
    { deployCommand = "./cmd_haspot.sh deploy" }

currentYear :: IO Integer
currentYear = do
    now <- getCurrentTime
    let (year, _, _) = toGregorian (utctDay now)
    return year

--------------------------------------------------------------------------------
-- Rules

rules :: HaspotSetting -> Integer -> Rules ()
rules conf year = do
    let siteCtx = siteContext conf year
        postCtx = postContext conf year

    match ("images/**" .||. "about/**" .||. "favicon.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "stylesheets/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateBodyCompiler

    -- Each post is rendered once; the "content" snapshot (body only) feeds
    -- the index pages, the archive, the sitemap and the Atom feed.
    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ pandocMathCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"
                    (pageTitleCtx <> constField "og_type" "article" <> mathCtx <> postCtx)
            >>= relativizeUrls

    -- index.html, 2/index.html, 3/index.html, ... newest first.
    paginate <- buildPaginateWith
        (fmap (paginateEvery postsPerPage) . sortRecentFirst)
        "posts/*"
        indexPageId

    paginateRules paginate $ \pageNum pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pat "content"
            let ctx = listField "posts" postCtx (return posts)
                   <> paginateContext paginate pageNum
                   <> mathCtxFor posts
                   <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" ctx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let ctx = constField "page_title" "Archive"
                   <> listField "posts" postCtx (return posts)
                   <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let ctx = listField "posts" postCtx (return posts) <> siteCtx
            makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" ctx

    create ["robots.txt"] $ do
        route idRoute
        compile $ makeItem "" >>= loadAndApplyTemplate "templates/robots.txt" siteCtx

    create ["rss/feed.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom (feedConfiguration conf) (postCtx <> bodyField "description") posts

-- | Where index page @n@ is written.  Page 1 is the site root.
indexPageId :: PageNumber -> Identifier
indexPageId 1 = "index.html"
indexPageId n = fromFilePath (show n ++ "/index.html")

--------------------------------------------------------------------------------
-- Compilers

-- | Pandoc with TeX math enabled in the reader (@$..$@, @$$..$$@,
-- @\\(..\\)@ and @\\[..\\]@) and emitted as MathJax-compatible markup.
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = pandocCompilerWith readerOptions writerOptions
  where
    readerOptions = defaultHakyllReaderOptions
        { readerExtensions = foldr enableExtension
            (readerExtensions defaultHakyllReaderOptions)
            [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
        }
    writerOptions = defaultHakyllWriterOptions
        { writerHTMLMathMethod = MathJax "" }

feedConfiguration :: HaspotSetting -> FeedConfiguration
feedConfiguration conf = FeedConfiguration
    { feedTitle       = title (blog conf)
    , feedDescription = description (blog conf)
    , feedAuthorName  = name (author conf)
    , feedAuthorEmail = email (author conf)
    , feedRoot        = root_url (blog conf)
    }

--------------------------------------------------------------------------------
-- Contexts

-- | Fields available on every page.  Ends with 'defaultContext', so it also
-- provides @$body$@, @$url$@, @$title$@ and any front-matter metadata.
siteContext :: HaspotSetting -> Integer -> Context String
siteContext conf year =
       constField    "blog_title"               (title b)
    <> optionalField "blog_description"         (Just (description b))
    <> constField    "root_url"                 (root_url b)
    <> constField    "about_page_link"          (about_page_link b)
    <> optionalField "blog_google_analytics_id" (google_analytics_id b)
    <> optionalField "google_site_verification" (google_site_verification b)
    <> constField    "author_name"              (name a)
    <> constField    "author_intro"             (intro a)
    <> constField    "current_year"             (show year)
    <> canonicalUrlField "canonical_url"        (root_url b)
    <> defaultContext
  where
    b = blog conf
    a = author conf

-- | Fields for a post, also used for each item inside @$for(posts)$@ loops.
-- Includes 'siteContext' because a loop body only sees its item's context.
postContext :: HaspotSetting -> Integer -> Context String
postContext conf year =
       dateField "date"    "%B %-d, %Y"
    <> dateField "isodate" "%Y-%m-%d"
    <> siteContext conf year

-- | The post title for @<title>@ and Open Graph tags.  Absent on pages
-- without a @title@ in their metadata, unlike 'defaultContext''s @title@,
-- which falls back to the file name (e.g. "index").
pageTitleCtx :: Context a
pageTitleCtx = field "page_title" $ \item -> do
    mtitle <- getMetadataField (itemIdentifier item) "title"
    maybe (noResult "page has no title metadata") return mtitle

-- | Present when the rendered post contains Pandoc math markup, so MathJax is
-- only loaded on pages that need it.
mathCtx :: Context String
mathCtx = field "mathjax" $ \item ->
    if hasMath (itemBody item) then return "true" else noResult "no math in post"

-- | Like 'mathCtx' but for a page listing several posts.
mathCtxFor :: [Item String] -> Context a
mathCtxFor posts = field "mathjax" $ \_ ->
    if any (hasMath . itemBody) posts then return "true" else noResult "no math on page"

hasMath :: String -> Bool
hasMath = isInfixOf "class=\"math"

-- | A constant field that is absent when the value is missing or blank, so
-- templates can guard it with @$if(key)$@.
optionalField :: String -> Maybe String -> Context a
optionalField key (Just value)
    | not (all isSpace value) = constField key value
optionalField key _ = field key $ \_ -> noResult ("no value for " ++ key)

-- | Absolute URL of the current page, with a trailing @index.html@ dropped so
-- the home page canonicalises to @root_url/@.
canonicalUrlField :: String -> String -> Context a
canonicalUrlField key root = field key $ \item -> do
    mroute <- getRoute (itemIdentifier item)
    case mroute of
        Nothing -> noResult ("no route for " ++ show (itemIdentifier item))
        Just r  -> return (root ++ stripIndexHtml (toUrl r))
  where
    stripIndexHtml u = fromMaybe u (stripSuffix "index.html" u)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse

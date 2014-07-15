--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.List              (isInfixOf)
import           System.FilePath.Posix  (takeBaseName, takeDirectory,
                                         (</>), splitFileName)
-- TODO
-- [X] RSS
-- [X] Proper link structure (e.g. no .md, no .html)
-- [-] Proper Homepage (n posts, first m sentences).
-- [X] Proper Archive Page.
-- [X] Proper Markdown syntax highlighting.
-- [X] Images linked & working.
-- [X] CSS.
-- [-] Test Google Analytics, RSS.
-- [X] Deploy.
-- [] "Link-type" post.
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "posts/*md" $ do
      route $ niceRoute `composeRoutes` (gsubRoute "^posts" (const "p"))
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

    match "posts/*html" $ do
      route $ niceRoute `composeRoutes` (gsubRoute "^posts" (const "p"))
      compile $ copyFileCompiler

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
                 loadAllSnapshots "posts/*" "content"
        renderRss notesRssConfig feedCtx posts

    create ["archives/index.html"] $ do
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
          >>= removeIndexHtml

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Notes"               `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
          >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    dateField "rdate" "%Y-%m-%d" `mappend`
    defaultContext

--------------------------------------------------------------------------------
notesRssConfig :: FeedConfiguration
notesRssConfig = FeedConfiguration
    { feedTitle       = "Notes"
    , feedDescription = ""
    , feedAuthorName  = "Isaac Hodes"
    , feedAuthorEmail = "isaachodes@gmail.com"
    , feedRoot        = "http://blog.isaachodes.io"
    }

--------------------------------------------------------------------------------
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident

--------------------------------------------------------------------------------
-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
  (dir, "index.html") | isLocal dir -> dir
                      | otherwise   -> url
  _                                 -> url
  where isLocal uri = not (isInfixOf "://" uri)

--------------------------------------------------------------------------------

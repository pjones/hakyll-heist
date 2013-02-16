--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll
import qualified Hakyll.Web.Heist as H

--------------------------------------------------------------------------------
main :: IO ()
main =
  do heist <- H.loadDefaultHeist "templates"
     hakyll $ do
       match "images/*" $ do
           route   idRoute
           compile copyFileCompiler

       match "css/*" $ do
           route   idRoute
           compile compressCssCompiler

       match (fromList ["about.rst", "contact.markdown"]) $ do
           route   $ setExtension "html"
           compile $ pandocCompiler
               >>= H.applyTemplate heist "basic" defaultContext
               >>= relativizeUrls

       match "posts/*" $ do
           route $ setExtension "html"
           compile $ pandocCompiler
               >>= H.applyTemplate heist "post" postCtx
               >>= relativizeUrls

       -- create ["archive.html"] $ do
       --     route idRoute
       --     compile $ do
       --         let archiveCtx =
       --                 field "posts" (\_ -> postList recentFirst) `mappend`
       --                 constField "title" "Archives"              `mappend`
       --                 defaultContext

       --         makeItem ""
       --             >>= H.applyTemplate heist "achive" archiveCtx
       --             >>= H.applyTemplate heist "basic"  archiveCtx
       --             >>= relativizeUrls

       -- match "index.html" $ do
       --     route idRoute
       --     compile $ do
       --         let indexCtx = field "posts" $ \_ -> postList (take 3 . recentFirst)

       --         getResourceBody
       --             >>= applyAsTemplate indexCtx
       --             >>= H.applyTemplate heist "basic" postCtx
       --             >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter <$> loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

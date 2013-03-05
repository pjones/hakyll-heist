{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the package hakyll-heist. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/hakyll-heist/LICENSE.  No
part of hakyll-heist package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Hakyll.Web.Heist
  ( loadDefaultHeist
  , loadHeist
  , applyTemplate
  , applyTemplateList
  , applyJoinTemplateList
  , Content
  , SpliceT
  , State
  ) where

--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (toByteString)
import           Control.Error (runEitherT)
import           Control.Monad (liftM)
import           Control.Monad.Reader (ReaderT(..), ask)
import           Control.Monad.Trans (lift)
import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import           Text.XmlHtml (elementAttrs)
import           Data.ByteString.UTF8 (toString)
import           Data.List (intersperse)
import qualified Data.Text as T

--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Template.Context

--------------------------------------------------------------------------------
import           Heist
import qualified Heist.Compiled as C

--------------------------------------------------------------------------------
type Content a = (Context a, Item a)
type SpliceT a = ReaderT (Content a) Compiler
type State   a = HeistState (SpliceT a)

--------------------------------------------------------------------------------
-- | Load all of the templates from the given directory and return an
-- initialized 'HeistState' using the given splices (in addition to
-- the default splices and the @hakyll@ splice).
loadHeist :: FilePath
          -- ^ Directory containing the templates.
          -> [(T.Text, C.Splice (SpliceT a))]
          -- ^ List of compiled Heist slices.
          -> [(T.Text, AttrSplice (SpliceT a))]
          -- ^ List of Heist attribute slices.
          -> IO (HeistState (SpliceT a))
loadHeist baseDir a b = do
    tState <- runEitherT $ do
        templates <- loadTemplates baseDir
        let splices' = [("hakyll", hakyllSplice)] ++ a
            attrs = [("url", urlAttrSplice)] ++ b
            hc = HeistConfig [] defaultLoadTimeSplices splices' attrs templates
        initHeist hc
    either (error . concat) return tState

--------------------------------------------------------------------------------
-- | Load all of the templates from the given directory and return an
-- initialized 'HeistState' with the default splices.
loadDefaultHeist :: FilePath -> IO (HeistState (SpliceT a))
loadDefaultHeist baseDir = loadHeist baseDir [] []

--------------------------------------------------------------------------------
-- | Apply a Heist template to a Hakyll 'Item'.  You need a
-- 'HeistState' from either the 'loadDefaultHeist' function or the
-- 'loadHeist' function.
applyTemplate :: HeistState (SpliceT a)  -- ^ HeistState
              -> ByteString              -- ^ Template name
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item String)  -- ^ Resulting item
applyTemplate state name context item = do
    case C.renderTemplate state name of
      Nothing    -> fail badTplError
      Just (r,_) -> do builder <- runReaderT r (context, item)
                       let body = toString $ toByteString builder
                       return $ itemSetBody body item
    where badTplError = "failed to render template: " ++ toString name

--------------------------------------------------------------------------------
-- | Render the given list of 'Item's with the given Heist template
-- and return everything concatenated together.
applyTemplateList :: HeistState (SpliceT a)  -- ^ HeistState
                  -> ByteString
                  -> Context a
                  -> [Item a]
                  -> Compiler String
applyTemplateList = applyJoinTemplateList ""

--------------------------------------------------------------------------------
-- | Render the given list of 'Item's with the given Heist template.
-- The content of the items is joined together using the given string
-- delimiter.
applyJoinTemplateList :: String
                      -> HeistState (SpliceT a)  -- ^ HeistState
                      -> ByteString
                      -> Context a
                      -> [Item a]
                      -> Compiler String
applyJoinTemplateList delimiter state name context items = do
    items' <- mapM (applyTemplate state name context) items
    return $ concat $ intersperse delimiter $ map itemBody items'

--------------------------------------------------------------------------------
-- Internal function to render the @hakyll@ splice given fields inside
-- of a 'Context'.
hakyllSplice :: C.Splice (SpliceT a)
hakyllSplice = do
    node <- getParamNode
    return $ C.yieldRuntimeText $ do
        (context, item) <- lift ask
        let context' f = unContext (context <> missingField) f item
        case lookup "field" $ elementAttrs node of
          Nothing -> fail fieldError
          Just f  -> liftStr $ context' $ T.unpack f
    where fieldError = "The `hakyll' splice is missing the `field' attribute"
          liftStr s  = lift $ lift $ liftM T.pack s

--------------------------------------------------------------------------------
-- Attribute splice: changes a bare @url@ attribute to a complete
-- @href@ attribute using the URL from the current 'Context'.
urlAttrSplice :: AttrSplice (SpliceT a)
urlAttrSplice _ = do
  (context, item) <- lift ask
  let url = unContext (context <> missingField) "url" item
  val <- lift $ lift $ liftM T.pack url
  return $ ("href", val) : []

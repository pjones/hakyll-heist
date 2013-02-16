{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package hakyll-heist. It is subject
to the license terms in the LICENSE file found in the top-level
directory of this distribution and at git://pmade.com/vimeta/LICENSE.
No part of hakyll-heist package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Hakyll.Web.Heist
  ( loadHeist
  , loadDefaultHeist
  , applyTemplate
  ) where

--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (toByteString)
import           Control.Error (runEitherT)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (ReaderT(..), ask)
import           Control.Monad.Trans (lift)
import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import           Text.XmlHtml (elementAttrs)
import           Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Web.Template.Context

--------------------------------------------------------------------------------
import           Heist
import qualified Heist.Compiled as C

--------------------------------------------------------------------------------
type Content a = (Context a, Item a)
type SpliceT a = ReaderT (Content a) Compiler

--------------------------------------------------------------------------------
loadHeist :: FilePath -- ^ Directory containing the templates.
          -> [(T.Text, C.Splice (SpliceT a))] -- ^ List of Heist slices.
          -> IO (HeistState (SpliceT a))
loadHeist baseDir splices = do
  tmap <- runEitherT $ do
      templates <- loadTemplates baseDir
      let splices' = [("hakyll", hakyllSplice)] ++ splices
          hc = HeistConfig [] defaultLoadTimeSplices splices' [] templates
      initHeist hc
  either (error . concat) return tmap

--------------------------------------------------------------------------------
loadDefaultHeist :: FilePath -> IO (HeistState (SpliceT a))
loadDefaultHeist = (flip loadHeist) []

--------------------------------------------------------------------------------
applyTemplate :: HeistState (SpliceT a)  -- ^ HeistState
              -> ByteString              -- ^ Template name
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item String)  -- ^ Resulting item
applyTemplate state name context item = do
  case C.renderTemplate state name of
    Nothing    -> undefined
    Just (r,m) -> do builder <- runReaderT r (context, item)
                     let body = toString $ toByteString builder
                     return $ itemSetBody body item

--------------------------------------------------------------------------------
hakyllSplice :: C.Splice (SpliceT a)
hakyllSplice = do node <- getParamNode
                  return $ C.yieldRuntimeText $ do
                    (context, item) <- lift ask
                    let context' f = unContext (context <> missingField) f item
                    case lookup "field" $ elementAttrs node of
                      Nothing    -> undefined
                      Just field -> lift $ lift $ liftM T.pack $ context' (T.unpack field)

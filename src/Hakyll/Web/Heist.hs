{-

This file is part of the Haskell package hakyll-heist. It is subject
to the license terms in the LICENSE file found in the top-level
directory of this distribution and at git://pmade.com/vimeta/LICENSE.
No part of hakyll-heist package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Hakyll.Web.Heist
  (
  ) where

--------------------------------------------------------------------------------
import           Control.Error (runEitherT)
import qualified Data.Text as T

--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler (Compiler(..))
import           Hakyll.Core.Identifier (Identifier(..))
import           Hakyll.Core.Item (Item(..))

--------------------------------------------------------------------------------
import           Heist
import qualified Heist.Compiled as C

--------------------------------------------------------------------------------
loadHeist :: FilePath -- ^ Directory containing the templates.
          -> [(T.Text, C.Splice IO)] -- ^ List of Heist slices.
          -> IO (HeistState IO)
loadHeist baseDir splices = do
  tmap <- runEitherT $ do
      templates <- loadTemplates baseDir
      let hc = HeistConfig [] defaultLoadTimeSplices splices [] templates
      initHeist hc
  either (error . concat) return tmap

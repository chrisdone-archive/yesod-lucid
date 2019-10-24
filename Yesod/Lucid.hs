{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Lucid support for Yesod.
--
-- Example Handler for a route, using Lucid to generate html,
-- including a rendered url:
--
-- > import Yesod.Lucid
-- > import Lucid
-- >
-- > getExampleR :: Handler Html
-- > getExampleR = commuteHtmlT $
-- >   do url <- route
-- >      p_ $ a_ [href_ (url ExampleR)] "self link"

module Yesod.Lucid
  ( route
  , commuteHtmlT -- Impure (using the yesod handler monad).
  , relaxHtmlT -- Pure Html generators.
  ) where

import Data.Functor.Identity
import Control.Monad.Reader
import Data.Text (Text)
import Lucid.Base
import Yesod.Core (ToTypedContent, MonadHandler, ToContent, Route,
                   HandlerSite, HasContentType(..))
import qualified Yesod.Core as Y

route :: MonadHandler m => HtmlT m (Route (HandlerSite m) -> Text)
route = lift Y.getUrlRender

instance ToTypedContent (Html ()) where
  toTypedContent m = Y.TypedContent (getContentType (Just m)) (Y.toContent m)

instance ToContent (Html ()) where
  toContent html = Y.ContentBuilder (runIdentity (execHtmlT html)) Nothing

instance HasContentType (Html ()) where
  getContentType _ = "text/html"

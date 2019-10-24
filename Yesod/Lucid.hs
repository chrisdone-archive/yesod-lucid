{-# LANGUAGE TypeFamilies #-}
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
  ( commuteHtmlT -- Impure (using the yesod handler monad).
  , relaxHtmlT -- Pure Html generators.
  , htmlWithUrl -- Pure Html with url.
  , Lucid
  , getUrl
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Text (Text)
import Lucid.Base
import Yesod.Core (HandlerFor, ToTypedContent, ToContent, Route, HasContentType(..))

import qualified Yesod.Core as Y

class MonadUrl m where
  type Site m :: *
  getUrl :: m (Route (Site m) -> Text)

type Lucid site = HtmlT (Reader (Route site -> Text))

htmlWithUrl ::
     Y.MonadHandler m => HtmlT (Reader (Route (Y.HandlerSite m) -> Text)) () -> m (Html ())
htmlWithUrl m = do
  renderer <- Y.getUrlRender
  pure (runReader (commuteHtmlT m) renderer)

instance MonadUrl (HtmlT (HandlerFor site)) where
  type Site (HtmlT (HandlerFor site)) = site
  getUrl = lift Y.getUrlRender

instance MonadUrl (HtmlT (Reader (Route site -> Text))) where
  type Site (HtmlT (Reader (Route site -> Text))) = site
  getUrl = lift ask

instance ToTypedContent (Html ()) where
  toTypedContent m = Y.TypedContent (getContentType (Just m)) (Y.toContent m)

instance ToContent (Html ()) where
  toContent html = Y.ContentBuilder (runIdentity (execHtmlT html)) Nothing

instance HasContentType (Html ()) where
  getContentType _ = "text/html"

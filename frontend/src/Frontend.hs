{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Prelude hiding (id, (.))

import Control.Category
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.Javascript.JSaddle
import Reflex.Dom.Core

import Obelisk.Frontend
import Obelisk.Route.Frontend

import Static

import Common.Route

import Frontend.Css (appCssStr)

title :: Text
title = "Sridhar Ratnakumar"

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = do
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text title
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
      el "style" $ text appCssStr
  , _frontend_body = subRoute_ $ \r -> do
        resp <- prerender (pure never) $ fetchContent $ backendRoute $ case r of
          Route_Landing -> BackendRoute_GetPage "landing.md"
          Route_Page s -> BackendRoute_GetPage $ s <> ".md"
        content :: Dynamic t Text <- holdDyn "Loading..." resp
        divClass "ui container" $ do
          divClass "ui top attached inverted header" $ do
            el "h1" $ elAttr "a" ("href" =: "/") $ text title
          divClass "ui attached segment" $
            elAttr "div" ("id" =: "content") $ do
              divClass "markdown" $ do
                prerender blank $ void $ elDynHtml' "div" content
  , _frontend_title = \_ -> title
  , _frontend_notFoundRoute = \_ -> Route_Landing :/ ()
  }
  where
    Right backendRouteValidEncoder = checkEncoder $ obeliskRouteEncoder backendRouteComponentEncoder backendRouteRestEncoder
    backendRoute r = T.intercalate "/" $ fst $ _validEncoder_encode backendRouteValidEncoder $ ObeliskRoute_App r :/ ()

-- TODO: Move to Widget.hs

aLink :: DomBuilder t m => m () -> m (Event t ())
aLink body = click' $ el' "a" body

click'
  :: (HasDomEvent t target 'ClickTag, Functor m)
  => m (target, a)
  -> m (Event t (DomEventType target 'ClickTag))
click' = fmap (domEvent Click . fst)

-- TODO: change this to toplevel dynamic
fetchContent ::
  ( PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadJSM (Performable m)
  , HasJSContext (Performable m)
  )
  => Text -> m (Event t Text)
fetchContent url = do
  let req = xhrRequest "GET" url def
  pb <- getPostBuild
  asyncReq <- performRequestAsync (tag (constant req) pb)
  pure $ fmap (fromMaybe "    fetchMarkdown: Unknown error" . _xhrResponse_responseText) asyncReq

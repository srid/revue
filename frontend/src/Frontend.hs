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
import Data.Monoid hiding ((<>))
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
  { _frontend_head = subRoute_ $ \r -> do
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
      el "style" $ text appCssStr
      -- FIXME: Title should actually come from the Yaml metadata, but we fetch
      -- content in _frontend_body; how to access that Dynamic from here?
      el "title" $ text $ case r of
        Route_Page s -> s <> " - " <> title
        Route_Landing -> title
  , _frontend_body = subRoute_ $ \r -> do
        resp <- prerender (pure never) $ fetchContent $ backendRoute $ case r of
          Route_Landing -> BackendRoute_GetPage "landing.md"
          Route_Page s -> BackendRoute_GetPage $ s <> ".md"
        -- Workaround the fetchContent bug by using holdUniqDyn
        content :: Dynamic t Text <- holdUniqDyn =<< holdDyn "Loading..." resp

        divClass "ui container" $ do
          divClass "ui top attached inverted header" $ do
            evt <- click' $ el' "h1" $ text title
            tellEvent $ Endo (const $ Route_Landing :/ ()) <$ evt
          divClass "ui attached segment" $
            elAttr "div" ("id" =: "content") $ do
              divClass "markdown" $ do
                prerender blank $ void $ elDynHtml' "div" content
          divClass "ui secondary bottom attached segment" $ do
            divClass "footer" $ do
              elAttr "a" ("href" =: projectUrl) $ text "Powered by Haskell"
  -- TODO: Why are these titles not being set at all? See also `_frontend_head`
  , _frontend_title = const "WIP" -- \case
      -- Route_Page s :/ () -> s <> " - " <> title
      -- _ -> "boo"
  , _frontend_notFoundRoute = \_ -> Route_Landing :/ ()
  }
  where
    projectUrl = "https://github.com/srid/revue" :: Text
    Right backendRouteValidEncoder = checkEncoder $ obeliskRouteEncoder backendRouteComponentEncoder backendRouteRestEncoder
    backendRoute r = T.intercalate "/" $ fst $ _validEncoder_encode backendRouteValidEncoder $ ObeliskRoute_App r :/ ()

-- TODO: Move to Widget.hs

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
  -- FIXME: Why is asyncReq firing 3 times?
  responses <- performRequestAsync $ req <$ pb
  pure $ fmap (fromMaybe "    fetchContent: Unknown error" . _xhrResponse_responseText) responses

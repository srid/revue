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
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Monoid hiding ((<>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- import Language.Javascript.JSaddle
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Static

import Common.Route

import Frontend.Css (appCssStr)
import Frontend.Markdown

-- TODO: As soon as obelisk backend routing is ready, move content as markdown
-- to the backend (or have backend fetch it from elsewhere).

title :: Text
title = "Sridhar Ratnakumar"

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = do
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text title
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
      el "style" $ text appCssStr
  , _frontend_body = pageTemplate $ subRoute_ $
        divClass "markdown" . markdownView . getRouteMarkdown
  , _frontend_title = \_ -> title
  , _frontend_notFoundRoute = \_ -> Route_Landing :/ ()
  }

pageTemplate :: (DomBuilder t m, EventWriter t (Endo (R Route)) m) => m a -> m a
pageTemplate page = divClass "ui container" $ do
  divClass "ui top attached inverted header" $ el "h1" $ text title
  divClass "ui attached segment" $
    elAttr "div" ("id" =: "content") $ page

getRouteMarkdown :: Route a -> Text
getRouteMarkdown = \case
  Route_Landing -> T.decodeUtf8 landingMd

landingMd :: ByteString
landingMd = $(embedFile "static/markdown/landing.md")

-- TODO: Move to Widget.hs

aLink :: DomBuilder t m => m () -> m (Event t ())
aLink body = click' $ el' "a" body

click'
  :: (HasDomEvent t target 'ClickTag, Functor m)
  => m (target, a)
  -> m (Event t (DomEventType target 'ClickTag))
click' = fmap (domEvent Click . fst)

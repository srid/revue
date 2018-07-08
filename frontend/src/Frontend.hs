{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Monoid hiding ((<>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core

import Obelisk.Route.Frontend
import Static


import Common.Route

import Frontend.Css (appCssStr)
import Frontend.Markdown

-- TODO: As soon as obelisk backend routing is ready, move content as markdown
-- to the backend (or have backend fetch it from elsewhere).

title :: Text
title = "Sridhar Ratnakumar"

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text title
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
      el "style" $ text appCssStr
    body = runRouteViewT routeComponentEncoder routeRestEncoder
      (\_ -> title) (\_ -> Route_Landing :/ ()) $ subRoute_ $ \case
        Route_Landing -> landingPage
        Route_About -> aboutPage

pageTemplate :: (DomBuilder t m, EventWriter t (Endo (R Route)) m) => m a -> m a
pageTemplate page = divClass "ui container" $ do
  divClass "ui top attached inverted header" $ el "h1" $ text title
  divClass "ui attached segment" $
    elAttr "div" ("id" =: "content") $ page

landingPage :: (DomBuilder t m, EventWriter t (Endo (R Route)) m) => m ()
landingPage = pageTemplate $ do
  -- el "p" $ do
  --   l <- aLink $ text "About"
  --   tellEvent $ Endo (const $ Route_About :/ ()) <$ l
  divClass "markdown" $ markdownView
    "# Welcome!\n\
    \\n\
    \This website is work in progress. Meanwhile you may \n\
    \[look at my resume](https://stackoverflow.com/story/sridca) or\n\
    \[look at the source code](https://github.com/srid/revue) for this site."

-- TODO: remove
aboutPage :: (DomBuilder t m, EventWriter t (Endo (R Route)) m) => m ()
aboutPage = pageTemplate $ do
  el "h2" $ text "About - Unused"
  el "p" $ do
    l <- aLink $ text "Home"
    tellEvent $ Endo (const $ Route_Landing :/ ()) <$ l

-- TODO: Move to Widget.hs

aLink :: DomBuilder t m => m () -> m (Event t ())
aLink body = click' $ el' "a" body

click'
  :: (HasDomEvent t target 'ClickTag, Functor m)
  => m (target, a)
  -> m (Event t (DomEventType target 'ClickTag))
click' = fmap (domEvent Click . fst)

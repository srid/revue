{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Semigroup ((<>))
import Data.Monoid hiding ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core

import Obelisk.Route.Frontend
import Static

import Common.Route

import Frontend.Css (appCssStr)


frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    title = "Revue"
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
  divClass "ui top attached inverted header" $ text "Revue"
  divClass "ui attached segment" $
    elAttr "div" ("id" =: "content") $ page

landingPage :: (DomBuilder t m, EventWriter t (Endo (R Route)) m) => m ()
landingPage = pageTemplate $ do
  el "h2" $ text "Welcome!"
  el "p" $ do
    l <- aLink $ text "About"
    tellEvent $ Endo (const $ Route_About :/ ()) <$ l

aboutPage :: (DomBuilder t m, EventWriter t (Endo (R Route)) m) => m ()
aboutPage = pageTemplate $ do
  el "h2" $ text "About"
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

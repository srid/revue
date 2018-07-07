{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Semigroup ((<>))
import Data.Monoid hiding ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

import Obelisk.Route.Frontend
import Static

import Common.Route

import Frontend.Css (appCssStr)

-- TODO: As soon as obelisk backend routing is ready, move content as markdown
-- to the backend (or have backend fetch it from elsewhere).

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
  divClass "ui top attached inverted header" $ el "h1" $ text "Revue"
  divClass "ui attached segment" $
    elAttr "div" ("id" =: "content") $ page

landingPage :: (DomBuilder t m, EventWriter t (Endo (R Route)) m) => m ()
landingPage = pageTemplate $ do
  el "h2" $ text "Welcome!"
  el "p" $ do
    l <- aLink $ text "About"
    tellEvent $ Endo (const $ Route_About :/ ()) <$ l
  el "p" $ text loremIpsum

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

loremIpsum :: Text
loremIpsum = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

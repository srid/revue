{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import Reflex.Dom.Core

import Obelisk.Route.Frontend

import Common.Route

import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    title = "Revue"
    head' = do
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text title
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
      -- el "style" $ text appCssStr
    body = elAttr "div" ("id" =: "content") $
      runRouteViewT routeComponentEncoder routeRestEncoder
        (\_ -> title) (\_ -> Route_Landing :/ ()) $ subRoute_ $ \case
          Route_Landing -> text "landing"
          Route_About -> text "about"

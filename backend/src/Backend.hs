{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
import Data.FileEmbed
import Data.Functor.Identity
import qualified Data.Text.Encoding as T
import Snap

import Reflex.Dom.Core

import Obelisk.Backend as Ob

import Common.Route

import Backend.Markdown (markdownView)

landingMd :: ByteString
landingMd = $(embedFile "static/markdown/landing.md")

backend :: Backend BackendRoute Route
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> serve $ \case
      BackendRoute_GetPage :=> Identity () -> do
        ((), v) <- liftIO $ renderStatic $ do
          markdownView $ T.decodeUtf8 landingMd
        writeBS v
  }

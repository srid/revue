{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap

import Reflex.Dom.Core

import Obelisk.Backend as Ob

import Common.Route

import Backend.Markdown (markdownView)

getSource :: FilePath -> Maybe ByteString
getSource s = fmap snd $ flip find pageContent $ \(fn, _) -> fn == s

backend :: Backend BackendRoute Route
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> serve $ \case
      BackendRoute_GetPage :=> Identity f -> do
        -- TODO: Don't use head; and then securely traverse the path.
        let fname = T.unpack (head f) <> ".md"
        case getSource fname of
          Nothing ->
            putResponse $ setResponseCode 404 emptyResponse
          Just content -> do
            (_page, html) <- liftIO $ renderStatic $ do
              markdownView $ T.decodeUtf8 content
            writeBS html
  }

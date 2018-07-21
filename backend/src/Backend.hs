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
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Snap

import Reflex.Dom.Core

import Obelisk.Backend as Ob

import Common.Api
import Common.Route

import Backend.Markdown (markdownView)

sources :: [(FilePath, ByteString)]
sources = $(embedDir sourceDir)

getSource :: FilePath -> Maybe ByteString
getSource s = fmap snd $ flip find sources $ \(fn, _) -> fn == s

backend :: Backend BackendRoute Route
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> serve $ \case
      BackendRoute_GetPage s :=> Identity () -> do
        liftIO $ T.putStrLn s
        let Just content = getSource $ T.unpack s
        ((), v) <- liftIO $ renderStatic $ do
          markdownView $ T.decodeUtf8 content
        writeBS v
  }

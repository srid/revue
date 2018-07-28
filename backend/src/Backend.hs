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
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Snap
import System.Directory
import System.Environment
import System.FilePath

import Reflex.Dom.Core

import Obelisk.Backend as Ob

import Common.Route

import Backend.Markdown (markdownView)

-- TODO: Move to common
landingFile :: FilePath
landingFile = "landing.md"

getContentDirectory :: IO FilePath
getContentDirectory
  = check =<< canonicalizePath =<< maybe defaultDir pure =<< lookupEnv "REVUE_CONTENT_DIR"
  where
    defaultDir = pure "../"
    check d = liftIO (doesDirectoryExist d) >>= \case
      False -> fail $ "content directory does not exist: " <> d
      True -> liftIO (doesFileExist $ d </> landingFile) >>= \case
        False -> fail "no landing.md found"
        True -> pure d

backend :: Backend BackendRoute Route
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> do
      contentDir <- getContentDirectory
      liftIO $ putStrLn $ "Serving content from: " <> contentDir
      serve $ \case
        BackendRoute_GetPage :=> Identity f -> do
          -- TODO: Don't use head; and then securely traverse the path.
          let fname = contentDir </> T.unpack (head f) <> ".md"
          liftIO $ putStrLn fname
          liftIO (doesFileExist fname) >>= \case
            False ->
              putResponse $ setResponseCode 404 emptyResponse
            True -> do
              content <- liftIO $ T.readFile fname
              (_page, html) <- liftIO $ renderStatic $ markdownView content
              writeBS html
  }

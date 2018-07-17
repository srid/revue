{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity

import Obelisk.Backend as Ob

import Common.Route

backend :: Backend BackendRoute Route
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> serve $ \case
      BackendRoute_NoOp :=> Identity () -> pure ()
  }

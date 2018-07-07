{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

import Obelisk.Route

import Control.Monad.Except
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import Data.Universe

data Route :: * -> * where
  Route_Landing :: Route ()
  Route_About :: Route ()

instance Universe (Some Route) where
  universe =
    [ Some.This Route_Landing
    , Some.This Route_About
    ]

routeComponentEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse (Some Route) (Maybe Text)
routeComponentEncoder = enum1Encoder $ \case
  Route_Landing -> Nothing
  Route_About -> Just "about"

routeRestEncoder
  :: (Applicative check, MonadError Text parse)
  => Route a -> Encoder check parse a PageName
routeRestEncoder = Encoder . pure . \case
  Route_Landing -> endValidEncoder mempty
  Route_About -> endValidEncoder mempty

deriveGShow ''Route
deriveGEq ''Route
deriveGCompare ''Route

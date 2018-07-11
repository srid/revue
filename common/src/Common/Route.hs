{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

import Obelisk.Route

import Control.Monad.Except
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import Data.Universe

data Route :: * -> * where
  Route_Landing :: Route ()

instance Universe (Some Route) where
  universe =
    [ Some.This Route_Landing
    ]

instance Universe (R Route) where
  universe =
    [ Route_Landing :/ ()
    ]

routeComponentEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse (Some Route) (Maybe Text)
routeComponentEncoder = enum1Encoder $ \case
  Route_Landing -> Nothing

routeRestEncoder
  :: (Applicative check, MonadError Text parse)
  => Route a -> Encoder check parse a PageName
routeRestEncoder = Encoder . pure . \case
  Route_Landing -> endValidEncoder mempty

deriveGShow ''Route
deriveGEq ''Route
deriveGCompare ''Route

instance ShowTag Route Identity where
  showTaggedPrec = \case
    Route_Landing -> showsPrec

instance EqTag Route Identity where
  eqTagged = \case
    Route_Landing -> \_ -> (==)

instance OrdTag Route Identity where
  compareTagged = \case
    Route_Landing -> \_ -> compare

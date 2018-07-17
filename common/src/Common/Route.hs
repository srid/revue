{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

import Prelude hiding ((.))

import Obelisk.Route

import Control.Category
import Control.Monad.Except
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Functor.Sum
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import Data.Universe

backendRouteEncoder
  :: ( check ~ parse
     , MonadError Text parse
     )
  => Encoder check parse (R (Sum BackendRoute (ObeliskRoute Route))) PageName
backendRouteEncoder = Encoder $ do
  let myComponentEncoder = (backendRouteComponentEncoder `shadowEncoder` obeliskRouteComponentEncoder routeComponentEncoder) . someSumEncoder
  myObeliskRestValidEncoder <- checkObeliskRouteRestEncoder routeRestEncoder
  checkEncoder $ pathComponentEncoder myComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_NoOp -> endValidEncoder mempty
    InR obeliskRoute -> runValidEncoderFunc myObeliskRestValidEncoder obeliskRoute

--TODO: Should we rename `Route` to `AppRoute`?
data BackendRoute :: * -> * where
  --TODO: How do we do routes with strongly-typed results?
  BackendRoute_NoOp :: BackendRoute ()

backendRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some BackendRoute) (Maybe Text)
backendRouteComponentEncoder = enum1Encoder $ \case
  BackendRoute_NoOp -> Just "noop"

backendRouteRestEncoder :: (Applicative check, MonadError Text parse) => BackendRoute a -> Encoder check parse a PageName
backendRouteRestEncoder = Encoder . pure . \case
  BackendRoute_NoOp-> endValidEncoder mempty

instance Universe (Some BackendRoute) where
  universe =
    [ Some.This BackendRoute_NoOp
    ]

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

instance ShowTag Route Identity where
  showTaggedPrec = \case
    Route_Landing -> showsPrec

instance EqTag Route Identity where
  eqTagged = \case
    Route_Landing -> \_ -> (==)

instance OrdTag Route Identity where
  compareTagged = \case
    Route_Landing -> \_ -> compare

deriveGShow ''Route
deriveGEq ''Route
deriveGCompare ''Route
deriveGShow ''BackendRoute
deriveGEq ''BackendRoute
deriveGCompare ''BackendRoute

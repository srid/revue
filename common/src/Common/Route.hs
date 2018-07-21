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
import Data.ByteString (ByteString)
import Data.Dependent.Sum
import Data.FileEmbed
import Data.Functor.Identity
import Data.Functor.Sum
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import qualified Data.Text as T
import Data.Universe
import System.FilePath (FilePath)

import Common.Api

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
      BackendRoute_GetPage _ -> endValidEncoder mempty
    InR obeliskRoute -> runValidEncoderFunc myObeliskRestValidEncoder obeliskRoute

--TODO: Should we rename `Route` to `AppRoute`?
data BackendRoute :: * -> * where
  --TODO: How do we do routes with strongly-typed results?
  BackendRoute_GetPage :: Text -> BackendRoute ()

backendRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some BackendRoute) (Maybe Text)
backendRouteComponentEncoder = enumEncoder $ \case
  Some.This (BackendRoute_GetPage s) -> Just s

backendRouteRestEncoder :: (Applicative check, MonadError Text parse) => BackendRoute a -> Encoder check parse a PageName
backendRouteRestEncoder = Encoder . pure . \case
  BackendRoute_GetPage _ -> endValidEncoder mempty

pages :: [FilePath]
pages = $(embedDirListing sourceDir)

-- | Gets the user facing route path for a page.
routeForPage :: FilePath -> Text
routeForPage = fst . T.breakOn ".md" . T.pack

-- FIXME: This should live in the backend, however unfortunately full ghc
-- build if the content exists only in one of common and backend packages.
-- To workaround, we move all TH stuff to the package where the content lives.
pageContent :: [(FilePath, ByteString)]
pageContent = $(embedDir sourceDir)

instance Universe (Some BackendRoute) where
  universe = Some.This . BackendRoute_GetPage . T.pack <$> pages

data Route :: * -> * where
  Route_Landing :: Route ()
  Route_Page :: Text -> Route ()

instance Universe (Some Route) where
  universe =
    [ Some.This Route_Landing
    ] <> (Some.This . Route_Page . routeForPage <$> pages)

instance Universe (R Route) where
  universe =
    [ Route_Landing :/ ()
    ]

routeComponentEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse (Some Route) (Maybe Text)
routeComponentEncoder = enum1Encoder $ \case
  Route_Landing -> Nothing
  Route_Page s -> Just s

routeRestEncoder
  :: (Applicative check, MonadError Text parse)
  => Route a -> Encoder check parse a PageName
routeRestEncoder = Encoder . pure . \case
  Route_Landing -> endValidEncoder mempty
  Route_Page _ -> endValidEncoder mempty

instance ShowTag Route Identity where
  showTaggedPrec = \case
    Route_Landing -> showsPrec
    Route_Page _ -> showsPrec

instance EqTag Route Identity where
  eqTagged = \case
    Route_Landing -> \_ -> (==)
    Route_Page _ -> \_ -> (==)

instance OrdTag Route Identity where
  compareTagged = \case
    Route_Landing -> \_ -> compare
    Route_Page _ -> \_ -> compare

deriveGShow ''Route
deriveGEq ''Route
deriveGCompare ''Route
deriveGShow ''BackendRoute
deriveGEq ''BackendRoute
deriveGCompare ''BackendRoute

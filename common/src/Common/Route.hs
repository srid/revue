{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

import Prelude hiding ((.))

import Control.Category
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Functor.Sum
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (FilePath)

import Obelisk.Route
import Obelisk.Route.TH

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
      BackendRoute_GetPage -> pathOnlyValidEncoder -- singlePathOnlyValidEncoder $ fmap T.pack pages
    InR obeliskRoute -> runValidEncoderFunc myObeliskRestValidEncoder obeliskRoute

--TODO: Should we rename `Route` to `AppRoute`?
data BackendRoute :: * -> * where
  --TODO: How do we do routes with strongly-typed results?
  BackendRoute_GetPage :: BackendRoute [Text]

data Route :: * -> * where
  Route_Home :: Route ()
  Route_Page :: Route [Text]

backendRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some BackendRoute) (Maybe Text)
backendRouteComponentEncoder = enumEncoder $ \case
  Some.This BackendRoute_GetPage-> Just "get-page"

backendRouteRestEncoder :: (Applicative check, MonadError Text parse) => BackendRoute a -> Encoder check parse a PageName
backendRouteRestEncoder = Encoder . pure . \case
  BackendRoute_GetPage -> pathOnlyValidEncoder -- singlePathOnlyValidEncoder $ fmap T.pack pages

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

routeComponentEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse (Some Route) (Maybe Text)
routeComponentEncoder = enumEncoder $ \case
  Some.This Route_Home -> Nothing
  Some.This Route_Page -> Just "page"

routeRestEncoder
  :: (Applicative check, MonadError Text parse)
  => Route a -> Encoder check parse a PageName
routeRestEncoder = Encoder . pure . \case
  Route_Home -> endValidEncoder mempty
  Route_Page -> pathOnlyValidEncoder -- singlePathOnlyValidEncoder $ fmap routeForPage pages

concat <$> mapM deriveRouteComponent
  [ ''Route
  , ''BackendRoute
  ]

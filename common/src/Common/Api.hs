{-# LANGUAGE DeriveGeneric #-}
module Common.Api where

import GHC.Generics

import Data.Aeson

import Data.Text (Text)

sourceDir :: FilePath
sourceDir = "page"

data Page = Page
  { _page_title :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Page
instance ToJSON Page


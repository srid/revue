{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Css (appCssStr) where

import Prelude hiding (all, rem)

import Clay
import Data.String.Conv (toS)
import Data.Text (Text)

appCssStr :: Text
appCssStr = toS $ render appCss

appThemeColor :: Color
appThemeColor = lightblue

appCss :: Css
appCss = do
  importUrl "https://fonts.googleapis.com/css?family=Roboto"

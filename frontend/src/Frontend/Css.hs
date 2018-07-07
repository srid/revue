{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Css (appCssStr) where

import Data.Semigroup ((<>))
import Prelude hiding (all, rem)

import Clay
import Data.String.Conv (toS)
import Data.Text (Text)

appCssStr :: Text
appCssStr = toS $ render appCss

themeColor :: Color
themeColor = lightgreen

themeLighterColor :: Color
themeLighterColor = "#e9fce9"

mainFont :: (Text, Text)
mainFont = ("Comfortaa:700", "Comfortaa")

headerFont :: (Text, Text)
headerFont = ("Chela+One", "Chela One")

appCss :: Css
appCss = do
  importUrl $ "https://fonts.googleapis.com/css?family=" <> fst mainFont
  importUrl $ "https://fonts.googleapis.com/css?family=" <> fst headerFont

  "#content" ? do
    important $ fontFamily [snd mainFont] [sansSerif]

  "h1, h2, h3, h4, h5, h6" ? do
    important $ fontFamily [snd headerFont] [cursive]

  h1 ? do
    textAlign center
    important $ color themeColor


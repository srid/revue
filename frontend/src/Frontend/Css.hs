{-# LANGUAGE MultiParamTypeClasses #-}
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

-- themeLighterColor :: Color
-- themeLighterColor = "#e9fce9"

mainFont :: (Text, Text)
mainFont = ("IBM+Plex+Serif:400,400i", "IBM Plex Serif")

headerFont :: (Text, Text)
headerFont = ("IBM+Plex+Sans", "IBM Plex Sans")

monoFont :: (Text, Text)
monoFont = ("IBM+Plex+Mono", "IBM Plex Mono")

appCss :: Css
appCss = do
  importUrl $ "https://fonts.googleapis.com/css?family=" <> fst mainFont
  importUrl $ "https://fonts.googleapis.com/css?family=" <> fst headerFont
  importUrl $ "https://fonts.googleapis.com/css?family=" <> fst monoFont

  "#content" ? do
    important $ fontFamily [snd mainFont] [sansSerif]

  "h1, h2, h3, h4, h5, h6, .footer" ? do
    important $ fontFamily [snd headerFont] [cursive]

  "h1" ? do
    cursor pointer

  "code, pre, tt" ? do
    important $ fontFamily [snd monoFont] [monospace]

  ".header h1" ? do
    textAlign center
    important $ color themeColor

  ".markdown" ? do
    "pre" ? do
      marginBottom $ em 1
      backgroundColor "#eee"
      width $ pct 50
      sym margin auto
      sym padding $ em 1

    ".footer a" ? do
      textAlign center

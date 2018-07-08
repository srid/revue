{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.Markdown where

import Control.Foldl hiding (mconcat)
import Control.Monad (forM_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (Link)

import qualified Text.MMark as MMark
import Text.MMark.Extension (Block (..), Inline (..))
import qualified Text.URI as URI

import Common.Route (Route)

markdownView :: (DomBuilder t m, EventWriter t (Endo (R Route)) m) => Text -> m ()
markdownView source = case MMark.parse "<nofile>" source of
  Left errs -> elClass "tt" "markdown-error" $
    text $ T.pack (MMark.parseErrorsPretty source errs)
  Right r -> MMark.runScannerM r $ FoldM (const renderBlock) blank pure
  where
    renderBlock = \case
      ThematicBreak -> el "tt" $ text "TODO: ThematicBreak"
      Heading1 xs -> el "h1" $ renderInlines xs
      Heading2 xs -> el "h2" $ renderInlines xs
      Heading3 xs -> el "h3" $ renderInlines xs
      Heading4 xs -> el "h4" $ renderInlines xs
      Heading5 xs -> el "h5" $ renderInlines xs
      Heading6 xs -> el "h6" $ renderInlines xs
      CodeBlock info xs -> elClass "code" (fromMaybe "" info) $ text xs
      Naked xs -> el "tt" $ do
        text $ "TODO: Naked"
        renderInlines xs
      Paragraph xs -> el "p" $ renderInlines xs
      Blockquote bs -> el "blockquote" $ flip forM_ renderBlock bs
      OrderedList _start _bs -> el "ol" $ el "tt" $ text "TODO: OrderedList"
      UnorderedList _bs -> el "ol" $ el "tt" $ text "TODO: UnorderedList"
      Table _ _ -> el "tt" $ text "TODO: Table"
    renderInlines = flip forM_ renderInline . NE.toList
    renderInline = \case
      Plain s -> text s
      LineBreak -> el "tt" $ text "TODO: LineBreak"
      Emphasis xs -> el "em" $ renderInlines xs
      Strong xs -> el "strong" $ renderInlines xs
      Strikeout xs -> el "strike" $ renderInlines xs
      Subscript xs -> el "sub" $ renderInlines xs
      Superscript xs -> el "sup" $ renderInlines xs
      CodeSpan s -> el "code" $ text s
      Link xs dest title -> referringElement "a" "href" "title" dest title $ renderInlines xs
      Image xs dest title -> referringElement "img" "src" "alt" dest title $ renderInlines xs
    referringElement t refAttr titleAttr dest title = elAttr t attrs
      where
        attrs = mconcat $ catMaybes
          [ Just $ refAttr =: URI.render dest
          , (titleAttr =:) <$> title
          ]



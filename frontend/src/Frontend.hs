{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex.Dom.Core

import Common.Api
import Data.Monoid ((<>))
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" attrsCSS blank
    body = do
      elAttr "div" ("class" =: "column") $ do
        text "Welcome to Obelisk!"
        el "p" $ text $ T.pack commonStuff
        elAttr "img" ("src" =: static @"obelisk.jpg") blank
        el "div" $ text "ok"
        el "div" $ do
          el "p" $ text "Reflex-Dom is:"
          el "ul" $ do
            el "li" $ text "Fun"
            el "li" $ text "Not difficult"
            el "li" $ text "Efficient"
      elAttr "div" ("class" =: "column") $ do
        el "h1" $ text "A link to DuckDuckGo in a new tab"
        elAttr "a" attrsLink $ text "DuckDuckGo"
        elClass "h2" "sub-title" $ text "This is a sub title"
        rec dynBool <- toggle False evClick
            let dynAttrs = attrsColorToggle <$> dynBool
            elDynAttr "h1" dynAttrs $ text "Changing color"
            evClick <- button "Change Color"
        return ()
        el "br" blank
        el "h2" $ text "Counter"
        button "Click me" >>= count >>= display
        el "br" blank
        rec el "h2" $ text "Counter as a fold"
            numbs <- foldDyn (+) (0 :: Int) (1 <$ evIncr)
            evIncr <- button "Increment"
            el "span" $ display numbs
        return ()

attrsCSS :: Map.Map T.Text T.Text
attrsCSS = ("href" =: static @"style.css") <> ("rel" =: "stylesheet")

attrsLink :: Map.Map T.Text T.Text
attrsLink = ("target" =: "_blank") <> ("href" =: "http://duckduckgo.com")

attrsColorToggle :: Bool -> Map.Map T.Text T.Text
attrsColorToggle b = "style" =: ("color: " <> color b)
  where
    color True = "red"
    color _ = "green"

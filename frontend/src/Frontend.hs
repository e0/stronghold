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
        el "h1" $ text "Welcome to Obelisk!"
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
        el "h1" $ text "Intro examples"
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
        rec el "h2" $ text "Combining Events with leftmost"
            counts <-
              foldDyn (+) (0 :: Int) $ leftmost [1 <$ evIncr, -1 <$ evDecr]
            el "div" $ display counts
            evIncr <- button "Increment"
            evDecr <- button "Decrement"
        return ()
        rec el "h2" $ text "Combining Events with mergeWith"
            dynCount <-
              foldDyn (+) (0 :: Int) (mergeWith (+) [1 <$ evIncr, -1 <$ evDecr])
            el "div" $ display dynCount
            evIncr <- button "Increment"
            evDecr <- button "Decrement"
        return ()
        rec el "h2" $ text "Using foldDyn with function application"
            dynNum <-
              foldDyn ($) (0 :: Int) $
              leftmost [(+ 1) <$ evIncr, (+ (-1)) <$ evDecr, const 0 <$ evReset]
            el "div" $ display dynNum
            evIncr <- button "Increment"
            evDecr <- button "Decrement"
            evReset <- button "Reset"
        return ()
      elAttr "div" ("class" =: "column") $ do
        el "h1" $ text "Predefined input widgets"
        el "h2" $ text "Text Input - Configuration"
        el "h3" $ text "Basic"
        ti <- textInput $ def
        dynText $ value ti
        el "h3" $ text "Max length 7"
        t1 <- textInput $ def & attributes .~ constDyn ("maxlength" =: "7")
        dynText $ value t1
        el "h3" $ text "Initial value"
        t2 <- textInput $ def & textInputConfig_initialValue .~ "initial"
        dynText $ value t2
        el "h3" $ text "Input hint"
        t3 <-
          textInput $
          def & attributes .~ constDyn ("placeholder" =: "type here")
        dynText $ value t3
        el "h3" $ text "Password"
        t4 <- textInput $ def & textInputConfig_inputType .~ "password"
        dynText $ value t4
        el "h3" $ text "Multiple attributes (hint + max length)"
        t5 <-
          textInput $
          def & attributes .~
          constDyn ("placeholder" =: "max 7" <> "maxlength" =: "7")
        dynText $ value t5
        el "h3" $ text "Numeric field with initial value"
        t6 <-
          textInput $
          def & textInputConfig_inputType .~ "number" &
          textInputConfig_initialValue .~
          "0"
        dynText $ value t6

attrsCSS :: Map.Map T.Text T.Text
attrsCSS = ("href" =: static @"style.css") <> ("rel" =: "stylesheet")

attrsLink :: Map.Map T.Text T.Text
attrsLink = ("target" =: "_blank") <> ("href" =: "http://duckduckgo.com")

attrsColorToggle :: Bool -> Map.Map T.Text T.Text
attrsColorToggle b = "style" =: ("color: " <> color b)
  where
    color True = "red"
    color _ = "green"

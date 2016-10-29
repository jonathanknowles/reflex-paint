{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Paint.WebInterface where

import Control.Monad (forM_)

import Data.Char (chr)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)

import GHCJS.DOM.Types (TouchEvent)

import Reflex
import Reflex.Dom

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

css = T.encodeUtf8 $ T.unlines
    [ ""
    , "* {margin: 0px; padding 0px; border: 0px}"
    , "body {background-color: #000000; color: #ffffff}"
    ]

main :: IO ()
main = mainWidgetWithCss css $ swipeArea

type Width  = Int
type Height = Int

data RichTouchEvent = TouchStart
                    | TouchMove
                    | TouchEnd
                    | MouseDown
                    | MouseUp
                    | TouchCancel deriving Show

swipeArea :: forall t m . MonadWidget t m => m ()
swipeArea = do
    rec (e, _) <- elAttr' "div" ("class" =: "swipe-area") $
            el "p" $ dyn lastFewTouchEvents'

        let touchEvents :: Event t (NonEmpty RichTouchEvent)
            touchEvents = mergeList [ const TouchStart  <$> domEvent Touchstart  e
                                    , const TouchEnd    <$> domEvent Touchend    e
                                    , const TouchCancel <$> domEvent Touchcancel e
                                    , const TouchMove   <$> domEvent Touchmove   e
                                    , const MouseDown   <$> domEvent Mousedown   e
                                    , const MouseUp     <$> domEvent Mouseup     e
                                    ]

        let accum :: NonEmpty a -> [a] -> [a]
            accum new past = take 20 $ toList new <> past
        (lastFewTouchEvents :: Dynamic t [RichTouchEvent]) <- foldDyn accum [] touchEvents

        let lastFewTouchEvents' :: Dynamic t (m ())
            lastFewTouchEvents' = fmap unorderedList lastFewTouchEvents
    return ()


screenXPixels :: Int
screenXPixels = 412 -- 1440
screenYPixels :: Int
screenYPixels = 732 -- 2560


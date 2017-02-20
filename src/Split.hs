{-# LANGUAGE TemplateHaskell #-}
module Split (Split(Split), Direction, handleSplitEvent, drawSplit) where

import Data.Label
import Brick.Widgets.Core

import Window

data Direction = Horizontal | Vertical

data Split =
  Split { _direction :: Maybe Direction
        , _left :: Maybe Split
        , _right :: Maybe Split
        , _window :: Maybe Window
        }

mkLabel ''Split

handleSplitEvent split ev =
  case ev of
    _ -> do
      let Just win = get window split
      set window (Just (handleWindowEvent win ev)) split

drawSplit split width height x y = do
  case (get window split) of
    Just win -> drawWindow win width height x y
    Nothing -> str "foo"


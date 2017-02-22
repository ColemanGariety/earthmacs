{-# LANGUAGE TemplateHaskell #-}
module Split (Split(Split),
              Direction,
              getWindow,
              handleSplitEvent,
              drawSplit) where

import Data.Label
import qualified Graphics.Vty as V
import qualified Brick.Types as T
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

getWindow = get window

handleSplitEvent split ev (width, height) =
  let Just win = get window split
  in set window (Just (handleWindowEvent win ev (width, height))) split

drawSplit :: Split -> t -> t1 -> t2 -> t3 -> T.Widget Name
drawSplit split width height x y = do
  case (get window split) of
    Just win -> drawWindow win width height x y
    Nothing -> str "foo"


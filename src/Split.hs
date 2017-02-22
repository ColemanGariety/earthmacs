{-# LANGUAGE TemplateHaskell #-}
module Split (Split(Split),
              Direction,
              getWindow,
              handleSplitEvent,
              splitEast,
              drawSplit) where

import Data.Label
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Brick.Widgets.Core
import Graphics.Vty hiding (Mode, showCursor)

import Window
import Util

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
  case get window split of
    Just win -> set window (Just (handleWindowEvent win ev (width, height))) split
    Nothing -> split

drawSplit :: Split -> t -> t1 -> t2 -> t3 -> T.Widget Name
drawSplit split width height x y = do
  case (get window split) of
    Just win -> drawWindow win width height x y
    Nothing -> (drawSplit (eliminate (get left split)) width height x y) <+>
               (drawSplit (eliminate (get right split)) width height x y)

splitEast split =
  set window Nothing $
  set direction (Just Horizontal) $
  set left (Just (set window (Just (set name (WindowID 1) (eliminate (get window split)))) split)) $
  set right (Just (set window (Just (set name (WindowID 2) (eliminate (get window split)))) split)) split
  

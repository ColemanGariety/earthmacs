{-# LANGUAGE TemplateHaskell #-}
module Split (Split(Split),
              Direction,
              splitEast,
              direction,
              left,
              right,
              window,
              handleSplitEvent,
              drawSplit) where

import Control.Lens
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

makeLenses ''Split

drawSplit :: Split -> t -> t1 -> t2 -> t3 -> T.Widget Name
drawSplit split width height x y = do
  case split^.window of
    Just win -> drawWindow win width height x y
    Nothing -> (drawSplit (eliminate (split^.right)) width height x y) <+>
               (drawSplit (eliminate (split^.right)) width height x y)

splitEast split =
  set window Nothing $
  set direction (Just Horizontal) $
  set left (Just (set window (Just (set name (WindowID 1) (eliminate (split^.window)))) split)) $
  set right (Just (set window (Just (set name (WindowID 2) (eliminate (split^.window)))) split)) split
  
handleSplitEvent split ev (width, height) =
  case split^.window of
    Just win -> set window (Just (handleWindowEvent win ev (width, height))) split
    Nothing -> split

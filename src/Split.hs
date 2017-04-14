{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Split (Split(Split),
              Direction(Horizontal, Vertical),
              direction,
              left,
              right,
              window,
              drawSplit) where

import Data.Data
import Control.Lens
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Brick.Widgets.Core
import Graphics.Vty hiding (Mode, showCursor)
import qualified Brick.Focus as F

import Buffer
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

instance Plated Split where
  plate f (Split d l r w) = (\l' r' -> Split d l' r' w) <$> traverse f l <*> traverse f r

drawSplit buffers split focusRing = do
  case split^.window of
    Just win -> let i = win^.bufferIndex
                    focused = (eliminate $ F.focusGetCurrent focusRing) == (win^.name)
                in drawWindow (win, buffers!!(win^.bufferIndex)) focused
    Nothing -> case split^.direction of
                 Just Horizontal -> (drawSplit buffers (eliminate (split^.left)) focusRing) <+>
                               (drawSplit buffers (eliminate (split^.right)) focusRing)
                 Just Vertical -> (drawSplit buffers (eliminate (split^.left)) focusRing) <=>
                             (drawSplit buffers (eliminate (split^.right)) focusRing)

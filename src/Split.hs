{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Split (Split(Split),
              Direction,
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
  
drawSplit split width height x y = do
  case split^.window of
    Just win -> drawWindow win width height x y
    Nothing -> (drawSplit (eliminate (split^.left)) width height x y) <+>
               (drawSplit (eliminate (split^.right)) width height x y)

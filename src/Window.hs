{-# LANGUAGE TemplateHaskell #-}
module Window (Window(Window), Split(Split), Mode(Normal), Direction, Name, handleWindowEvent, drawWindow) where

import Data.Label
import Graphics.Vty hiding (Mode, showCursor)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Brick.Widgets.Core
import Brick.Widgets.Border.Style
import Brick.Widgets.Border

import Buffer

data Direction = Horizontal | Vertical

data Mode = Normal | Insert

data Name = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Ord, Show, Eq)

data Split =
  Split { _direction :: Maybe Direction
        , _left :: Maybe Split
        , _right :: Maybe Split
        , _parent :: Maybe Split
        , _window :: Maybe Window
        }

data Window =
  Window { _split :: Split
         , _buffer :: Buffer
         , _cursor :: (Int, Int)
         , _scroll_y :: Int
         , _column :: Int
         , _mode :: Mode
         , _mark :: Maybe (Int, Int)
         }

mkLabel ''Split
mkLabel ''Window

handleWindowEvent window ev =
  case ev of
    V.EvKey (V.KChar 'h') [] -> modify cursor (\(x,y) -> (x - 1, y)) window
    V.EvKey (V.KChar 'j') [] -> modify cursor (\(x,y) -> (x, y + 1)) window
    V.EvKey (V.KChar 'k') [] -> modify cursor (\(x,y) -> (x, y - 1)) window
    V.EvKey (V.KChar 'l') [] -> modify cursor (\(x,y) -> (x + 1, y)) window
    _ -> set buffer (handleBufferEvent (get buffer window) ev) window

drawWindow window width height x y = do
  let string = drawBuffer (get buffer window)
  return $
    withBorderStyle unicodeBold $
    border $
    C.center $
    (viewport V1 T.Vertical) $
    showCursor V1 (T.Location (get cursor window)) $
    string

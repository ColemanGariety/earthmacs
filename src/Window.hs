{-# LANGUAGE TemplateHaskell #-}
module Window (Window(Window), Split(Split), Mode(Normal), Direction, Name, handleWindowEvent, drawWindow) where

import Data.Label
import Graphics.Vty hiding (Mode)
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
         , _x :: Int
         , _y :: Int
         , _scroll_y :: Int
         , _column :: Int
         , _mode :: Mode
         , _mark :: Maybe (Int, Int)
         }

mkLabel ''Split
mkLabel ''Window

handleWindowEvent window ev =
  case ev of
    V.EvKey (V.KChar 'h') [] -> modify x (\x -> x - 1) window
    V.EvKey (V.KChar 'j') [] -> modify y (+1) window
    V.EvKey (V.KChar 'k') [] -> modify y (\y -> y - 1) window
    V.EvKey (V.KChar 'l') [] -> modify x (+1) window
    _ -> set buffer (handleBufferEvent (get buffer window) ev) window

-- drawSplit split width height x y =
--   case get direction split of
--     Just direction -> do
--       case direction of
--         Horizontal -> return $ C.center ((viewport View T.Vertical $ str "left") <+> (viewport View T.Vertical $ str "right"))
--         Vertical -> return $ C.center ((viewport View T.Vertical $ str "top") <+> (viewport View T.Vertical $ str "bottom"))
--     Nothing -> do
--       let Just activeWindow = get window split
--       let activeBuffer = get buffer activeWindow
--       let w = viewport View T.Vertical $ str (head (getLns activeBuffer))
--       return $ withBorderStyle Brick.Widgets.Border.Style.unicodeBold $ Brick.Widgets.Border.border $ C.center $ w

drawWindow window width height x y = do
  return . withBorderStyle unicodeBold . border . C.center $ drawBuffer (get buffer window)

{-# LANGUAGE TemplateHaskell #-}
module Window (Window(Window),
               Mode(Normal),
               Name,
               handleWindowEvent,
               drawWindow) where

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

data Mode = Normal | Insert

data Name = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Ord, Show, Eq)

data Window =
  Window { _buffer :: Buffer
         , _cursor :: (Int, Int)
         , _scroll_y :: Int
         , _column :: Int
         , _mode :: Mode
         , _mark :: Maybe (Int, Int)
         }

mkLabel ''Window

handleWindowEvent window ev =
  updateCursor $ case (get mode window) of
    Normal ->
      case ev of
        V.EvKey (V.KChar '0') [] -> moveBol window
        V.EvKey (V.KChar 'A') [] -> append window
        V.EvKey (V.KChar 'h') [] -> moveLeft window 
        V.EvKey (V.KChar 'i') [] -> insert window
        V.EvKey (V.KChar 'j') [] -> moveDown window
        V.EvKey (V.KChar 'k') [] -> moveUp window
        V.EvKey (V.KChar 'l') [] -> moveRight window
        V.EvKey (V.KChar 'x') [] -> forwardDelete window
        _ -> window
    Insert ->
      case ev of
        V.EvKey V.KEsc [] -> leaveInsertMode window
        V.EvKey V.KBS [] -> backwardDelete window
        V.EvKey V.KEnter [] -> addLineAtCursor window
        _ -> let (V.EvKey (V.KChar char) []) = ev
             in addCharAtCursor char window

drawWindow window width height x y = do
  let string = drawBuffer (get buffer window)
  withBorderStyle unicodeBold $
    border $
    C.center $
    (viewport V1 T.Vertical) $
    showCursor V1 (T.Location (get cursor window)) $
    string

updateCursor window =
  let (x, y) = get cursor window
      l = lineAt (get buffer window) (x, y)
  in case (get mode window) of
       Insert -> set cursor (max 0 (min (length l) x), y) window
       Normal -> set cursor (max 0 (min ((length l) - 1) x), y) window

-- these will be available to the user --

moveLeft = modify cursor (\(x,y) -> (x - 1, y))
moveDown = modify cursor (\(x,y) -> (x, y + 1))
moveUp = modify cursor (\(x,y) -> (x, y - 1))
moveRight = modify cursor (\(x,y) -> (x + 1, y))
moveBol window = set cursor (0, (snd (get cursor window))) window
moveEol window = set cursor (length (lineAt (get buffer window)(get cursor window)),
                             (snd (get cursor window))) window

append :: Window -> Window
append window = moveEol $ set mode Insert window

insert :: Window -> Window
insert = set mode Insert

forwardDelete :: Window -> Window
forwardDelete window =
  let (x, y) = get cursor window
  in if x == 0
     then window
     else let win = removeCharAtCursor window
          in moveRight win
  
backwardDelete :: Window -> Window
backwardDelete window =
  let (x, y) = get cursor window
  in if x == 0
     then if y == 0
          then window
          else let win = set buffer (deleteLineAt (get buffer window) (x, y)) window
               in moveEol (moveUp win)
     else removePrevCharAtCursor window

leaveInsertMode :: Window -> Window
leaveInsertMode window =
  let win = set mode Normal window
  in moveLeft win

addCharAtCursor :: Char -> Window -> Window
addCharAtCursor char window =
  let win = set buffer (insertCharAt (get buffer window) char (get cursor window)) window
  in moveRight win
  
addLineAtCursor :: Window -> Window
addLineAtCursor window =
  let win = set buffer (insertLineAt (get buffer window) (get cursor window)) window
  in moveBol (moveDown win)

removeCharAtCursor :: Window -> Window
removeCharAtCursor window =
  let win = set buffer (deleteCharAt (get buffer window) (get cursor window)) window
  in moveLeft win

removePrevCharAtCursor :: Window -> Window
removePrevCharAtCursor window =
  let (x, y) = get cursor window
      win = set buffer (deleteCharAt (get buffer window) (x - 1, y)) window
  in moveLeft win

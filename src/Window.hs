{-# LANGUAGE TemplateHaskell #-}
module Window (Window(Window),
               Mode(Normal),
               Name(V1),
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
import Brick.Main

import Buffer

data Mode = Normal | Insert | Delete deriving (Show, Eq)

data Name = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 deriving (Ord, Show, Eq)

data Window =
  Window { _buffer :: Buffer
         , _cursor :: (Int, Int)
         , _scroll :: (Int, Int)
         , _mode :: Mode
         , _mark :: Maybe (Int, Int)
         }

mkLabel ''Window

handleWindowEvent window ev (width, height) =
  updateCursor $
  updateViewport (width, height) $
  updateBuffer $
  case (get mode window) of
    Normal ->
      case ev of
        V.EvKey (V.KChar '0') [] -> moveBol window
        V.EvKey (V.KChar 'A') [] -> append window
        V.EvKey (V.KChar 'b') [MCtrl] -> moveUpBy (height - 1) window
        V.EvKey (V.KChar 'd') [] -> deleteMode window
        V.EvKey (V.KChar 'f') [MCtrl] -> moveDownBy (height - 1) window
        V.EvKey (V.KChar 'h') [] -> moveLeft window
        V.EvKey (V.KChar 'i') [] -> insertMode window
        V.EvKey (V.KChar 'j') [] -> moveDown window
        V.EvKey (V.KChar 'k') [] -> moveUp window
        V.EvKey (V.KChar 'l') [] -> moveRight window
        V.EvKey (V.KChar 'x') [] -> forwardDelete window
        _ -> window
    Insert ->
      case ev of
        V.EvKey V.KEsc [] -> normalMode window
        V.EvKey V.KBS [] -> backwardDelete window
        V.EvKey V.KEnter [] -> addLineAtCursor window
        _ -> let (V.EvKey (V.KChar char) []) = ev
             in addCharAtCursor char window
    Delete ->
      case ev of
        V.EvKey (V.KChar 'd') [] -> removeLineAtCursor window
        _ -> normalMode window

drawWindow :: Window -> t -> t1 -> t2 -> t3 -> T.Widget Name
drawWindow window width height x y = do
  let (x, y) = get cursor window
      (scrollX, scrollY) = get scroll window
  withBorderStyle unicodeBold $
    border $
    C.center $
    reportExtent V1 $
    (viewport V1 T.Vertical) $
    showCursor V1 (T.Location (x, y - scrollY)) $
    drawBuffer (get buffer window) (scrollX, scrollY)

updateBuffer :: Window -> Window
updateBuffer window =
  let ls = getLines (get buffer window)
  in if length ls == 0
     then set buffer (oneLine (get buffer window)) window
     else window

updateCursor :: Window -> Window
updateCursor window =
  let (x, y) = get cursor window
      ls = getLines (get buffer window)
      yCap = (length ls) - 1
      l = lineAt (get buffer window) (x, updatedY)
      xCap = case (get mode window) of
               Insert -> length l
               _ -> (length l) - 1
      updatedX = max 0 (min xCap x)
      updatedY = max 0 (min yCap y)
  in set cursor (updatedX, updatedY) window

updateViewport :: (t, Int) -> Window -> Window
updateViewport (width, height) window =
  let (cursorX, cursorY) = get cursor window
      (scrollX, scrollY) = get scroll window
      yMax = ((length (getLines (get buffer window))) - height)
      yMin = 0
  in if (cursorY - scrollY >= height)
     then modify scroll (\(x, y) -> (x, min yMax (cursorY - height + 1))) window
     else if (cursorY - scrollY) < 0
          then modify scroll (\(x, y) -> (x, max yMin cursorY)) window
          else window

-- these will be available to the user --

moveLeft :: Window -> Window
moveLeft = modify cursor (\(x,y) -> (x - 1, y))
moveDown :: Window -> Window
moveDown = modify cursor (\(x,y) -> (x, y + 1))
moveUp :: Window -> Window
moveUp = modify cursor (\(x,y) -> (x, y - 1))
moveRight :: Window -> Window
moveRight = modify cursor (\(x,y) -> (x + 1, y))
moveBol :: Window -> Window
moveBol window = set cursor (0, (snd (get cursor window))) window
moveEol :: Window -> Window
moveEol window = set cursor (length (lineAt (get buffer window)(get cursor window)),
                             (snd (get cursor window))) window

moveDownBy 0 window = window
moveDownBy height window = moveDownBy (height - 1) (moveDown window)

moveUpBy 0 window = window
moveUpBy height window = moveUpBy (height - 1) (moveUp window)

append :: Window -> Window
append window = moveEol $ set mode Insert window

insertMode :: Window -> Window
insertMode = set mode Insert
deleteMode :: Window -> Window
deleteMode = set mode Delete
normalMode :: Window -> Window
normalMode window =
  let win = set mode Normal window
  in moveLeft win


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

addCharAtCursor :: Char -> Window -> Window
addCharAtCursor char window =
  let win = set buffer (insertCharAt (get buffer window) char (get cursor window)) window
  in moveRight win
  
addLineAtCursor :: Window -> Window
addLineAtCursor window =
  let win = set buffer (insertLineAt (get buffer window) (get cursor window)) window
  in moveBol (moveDown win)

removeLineAtCursor :: Window -> Window
removeLineAtCursor window =
  let win = set buffer (deleteLineAt (get buffer window) (get cursor window)) window
  in normalMode win

removeCharAtCursor :: Window -> Window
removeCharAtCursor window =
  let win = set buffer (deleteCharAt (get buffer window) (get cursor window)) window
  in moveLeft win

removePrevCharAtCursor :: Window -> Window
removePrevCharAtCursor window =
  let (x, y) = get cursor window
      win = set buffer (deleteCharAt (get buffer window) (x - 1, y)) window
  in moveLeft win

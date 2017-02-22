{-# LANGUAGE TemplateHaskell #-}
module Window (Window(Window),
               Mode(Normal),
               Name(WindowID),
               getName,
               handleWindowEvent,
               drawWindow) where

import Data.Label
import Graphics.Vty hiding (Mode, showCursor)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Brick.Widgets.Core as C hiding (getName)
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import Brick.Main

import Buffer

data Mode = Normal | Insert | Delete | Visual deriving (Show, Eq)

data Name = WindowID Integer deriving (Ord, Show, Eq)

data Window =
  Window { _buffer :: Buffer
         , _cursor :: (Int, Int)
         , _column :: Int
         , _scroll :: (Int, Int)
         , _mode :: Mode
         , _mark :: Maybe (Int, Int)
         , _name :: Name
         }

mkLabel ''Window

handleWindowEvent window ev (width, height) =
  updateScroll (width, height) $
  updateCursor $
  updateBuffer $
  case (get mode window) of
    Normal ->
      case ev of
        V.EvKey V.KEnter [] -> moveDown window
        V.EvKey (V.KChar '$') [] -> moveEol window
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
        V.EvKey (V.KChar 'o') [] -> addLine (insertMode window)
        V.EvKey (V.KChar 'O') [] -> addLineAbove (insertMode window)
        V.EvKey (V.KChar 'x') [] -> forwardDelete window
        _ -> window
    Insert ->
      case ev of
        V.EvKey V.KEsc [] -> normalMode window
        V.EvKey V.KBS [] -> backwardDelete window
        V.EvKey V.KEnter [] -> addLine window
        V.EvKey (V.KChar char) [] -> addChar char window
        _ -> normalMode window
    Delete ->
      case ev of
        V.EvKey (V.KChar 'd') [] -> removeLine window
        _ -> normalMode window
    Visual ->
      case ev of
        V.EvKey (V.KChar 'h') [] -> moveLeft window
        V.EvKey (V.KChar 'j') [] -> moveDown window
        V.EvKey (V.KChar 'k') [] -> moveUp window
        V.EvKey (V.KChar 'l') [] -> moveRight window
        _ -> normalMode window

getName = get name

drawWindow :: Window -> t -> t1 -> t2 -> t3 -> T.Widget Name
drawWindow window width height x y =
  let (x, y) = get cursor window
      (scrollX, scrollY) = get scroll window
      n = get name window
  in withBorderStyle unicodeBold $
    border $
    C.center $
    reportExtent n $
    (viewport n T.Vertical) $
    showCursor n (T.Location (x, y - scrollY)) $
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
      l = lineAt (get buffer window) updatedY
      ls = getLines (get buffer window)
      yCap = (length ls) - 1
      xCap = case (get mode window) of
               Insert -> length l
               _ -> (length l) - 1
      updatedX = max 0 (min xCap x)
      updatedY = max 0 (min yCap y)
  in set cursor (updatedX, updatedY) window

updateScroll :: (t, Int) -> Window -> Window
updateScroll (width, height) window =
  let (cursorX, cursorY) = get cursor window
      (scrollX, scrollY) = get scroll window
      ls = (getLines (get buffer window))
      yMax = max 0 (length ls - height)
      yMin = 0
  in if (cursorY - scrollY >= height)
     then set scroll (cursorX, min yMax (cursorY - height + 1)) window
     else if (cursorY - scrollY) < 0
          then set scroll (max 0 cursorX, max yMin cursorY) window
          else window

-- these will be available to the user --

moveLeft :: Window -> Window
moveLeft window =
  let win = modify cursor (\(x,y) -> (x - 1, y)) window
  in modify column (subtract 1) win

moveDown :: Window -> Window
moveDown window = modify cursor (\(x,y) -> (get column window, y + 1)) window

moveUp :: Window -> Window
moveUp window = modify cursor (\(x,y) -> (get column window, y - 1)) window

moveRight :: Window -> Window
moveRight window =
  let win = modify cursor (\(x,y) -> (x + 1, y)) window
  in modify column (+1) win

moveBol :: Window -> Window
moveBol window =
  let win = set cursor (0, (snd (get cursor window))) window
  in set column 0 win

moveEol :: Window -> Window
moveEol window =
  let (x, y) = get cursor window
      l =  eol (get buffer window) y
      win = set cursor (l, y) window
  in set column l win

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
     else let win = removeChar window
          in moveRight win
  
backwardDelete :: Window -> Window
backwardDelete window =
  let (x, y) = get cursor window
  in if x == 0
     then if y == 0
          then window
          else let win = set buffer (deleteLineAt (get buffer window) (x, y)) window
               in moveEol (moveUp win)
     else removePrevChar window

addChar :: Char -> Window -> Window
addChar char window =
  let win = set buffer (insertCharAt (get buffer window) char (get cursor window)) window
  in moveRight win
  
addLine :: Window -> Window
addLine window =
  let (x, y) = get cursor window
      win = set buffer (insertLineAt (get buffer window) (x, y)) window
  in moveBol (moveDown win)
  
addLineAbove :: Window -> Window
addLineAbove window =
  let (x, y) = get cursor window
      win = set buffer (insertLineAt (get buffer window) (x, y - 1)) window
  in moveBol win

removeLine :: Window -> Window
removeLine window =
  set buffer
  (deleteLineAt (get buffer window) (get cursor window))
  (normalMode window)

removeChar :: Window -> Window
removeChar window =
  let win = set buffer (deleteCharAt (get buffer window) (get cursor window)) window
  in moveLeft win

removePrevChar :: Window -> Window
removePrevChar window =
  let (x, y) = get cursor window
      win = set buffer (deleteCharAt (get buffer window) (x - 1, y)) window
  in moveLeft win

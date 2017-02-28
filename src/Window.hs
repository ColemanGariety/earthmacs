{-# LANGUAGE TemplateHaskell #-}
module Window (Window(Window),
               Mode(Normal),
               Name(WindowID),
               name,
               _name,
               handleWindowEvent,
               drawWindow) where

import Control.Lens
import Graphics.Vty hiding (Mode, showCursor)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Types as T
import Brick.Widgets.Core as C
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import Brick.Main

import Buffer

data Mode = Normal | Insert | ReplaceChar | Delete | Visual deriving (Show, Eq)

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

makeLenses ''Window

handleWindowEvent :: Window -> Event -> (t, Int) -> Window
handleWindowEvent window ev (width, height) =
  updateScroll (width, height) $
  updateCursor $
  updateBuffer $
  case window^.mode of
    Normal ->
      case ev of
        EvKey KEnter [] -> moveDown window
        EvKey (KChar '$') [] -> moveEol window
        EvKey (KChar '0') [] -> moveBol window
        EvKey (KChar 'a') [] -> moveRight $ insertMode window
        EvKey (KChar 'A') [] -> append window
        EvKey (KChar 'b') [MCtrl] -> moveUpBy (height - 1) window
        EvKey (KChar 'd') [] -> deleteMode window
        EvKey (KChar 'f') [MCtrl] -> moveDownBy (height - 1) window
        EvKey (KChar 'h') [] -> moveLeft window
        EvKey (KChar 'i') [] -> insertMode window
        EvKey (KChar 'j') [] -> moveDown window
        EvKey (KChar 'k') [] -> moveUp window
        EvKey (KChar 'l') [] -> moveRight window
        EvKey (KChar 'r') [] -> replaceCharMode window
        EvKey (KChar 'o') [] -> addLine (insertMode window)
        EvKey (KChar 'O') [] -> addLineAbove (insertMode window)
        EvKey (KChar 'x') [] -> forwardDelete window
        _ -> window
    ReplaceChar ->
      case ev of
        EvKey (KChar char) [] -> replaceChar char window
        _ -> normalMode window
    Insert ->
      case ev of
        EvKey KEsc [] -> normalMode window
        EvKey KBS [] -> backwardDelete window
        EvKey KEnter [] -> addLine window
        EvKey (KChar char) [] -> addChar char window
        _ -> normalMode window
    Delete ->
      case ev of
        EvKey (KChar 'd') [] -> removeLine window
        _ -> normalMode window
    Visual ->
      case ev of
        EvKey (KChar 'h') [] -> moveLeft window
        EvKey (KChar 'j') [] -> moveDown window
        EvKey (KChar 'k') [] -> moveUp window
        EvKey (KChar 'l') [] -> moveRight window
        _ -> normalMode window

drawWindow :: Window -> t -> t1 -> t2 -> t3 -> T.Widget Name
drawWindow window width height x y =
  let (x, y) = window^.cursor
      (scrollX, scrollY) = window^.scroll
      n = window^.name
  in withBorderStyle unicodeBold $
    border $
    C.center $
    reportExtent n $
    (viewport n T.Vertical) $
    showCursor n (T.Location (x, y - scrollY)) $
    drawBuffer (window^.buffer) (scrollX, scrollY)

updateBuffer :: Window -> Window
updateBuffer window =
  let ls = window^.buffer^.lns
  in if length ls == 0
     then set buffer (oneLine (window^.buffer)) window
     else window

updateCursor :: Window -> Window
updateCursor window =
  let (x, y) = window^.cursor
      l = lineAt (window^.buffer) updatedY
      ls = window^.buffer^.lns
      yCap = (length ls) - 1
      xCap = case window^.mode of
               Insert -> length l
               _ -> (length l) - 1
      updatedX = max 0 (min xCap x)
      updatedY = max 0 (min yCap y)
  in set cursor (updatedX, updatedY) window

updateScroll :: (t, Int) -> Window -> Window
updateScroll (width, height) window =
  let (cursorX, cursorY) = window^.cursor
      (scrollX, scrollY) = window^.scroll
      ls = window^.buffer^.lns
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
  let win = over cursor (\(x,y) -> (x - 1, y)) window
  in over column (subtract 1) win

moveDown :: Window -> Window
moveDown window = over cursor (\(x,y) -> (window^.column, y + 1)) window

moveUp :: Window -> Window
moveUp window = over cursor (\(x,y) -> (window^.column, y - 1)) window

moveRight :: Window -> Window
moveRight window =
  let win = over cursor (\(x,y) -> (x + 1, y)) window
  in over column (+1) win

moveBol :: Window -> Window
moveBol window =
  let win = set cursor (0, (snd (window^.cursor))) window
  in set column 0 win

moveEol :: Window -> Window
moveEol window =
  let (x, y) = window^.cursor
      l =  eol (window^.buffer) y
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

replaceCharMode :: Window -> Window
replaceCharMode = set mode ReplaceChar

forwardDelete :: Window -> Window
forwardDelete window =
  let (x, y) = window^.cursor
  in if x == 0
     then window
     else let win = removeChar window
          in moveRight win
  
backwardDelete :: Window -> Window
backwardDelete window =
  let (x, y) = window^.cursor
  in if x == 0
     then if y == 0
          then window
          else let win = set buffer (deleteLineAt (window^.buffer) (x, y)) window
               in moveEol (moveUp win)
     else removePrevChar window

addChar :: Char -> Window -> Window
addChar char window =
  let win = set buffer (insertCharAt (window^.buffer) char (window^.cursor)) window
  in moveRight win
  
addLine :: Window -> Window
addLine window =
  let (x, y) = window^.cursor
      win = set buffer (insertLineAt (window^.buffer) (x, y)) window
  in moveBol (moveDown win)
  
addLineAbove :: Window -> Window
addLineAbove window =
  let (x, y) = window^.cursor
      win = set buffer (insertLineAt (window^.buffer) (x, y - 1)) window
  in moveBol win

removeLine :: Window -> Window
removeLine window =
  set buffer
  (deleteLineAt (window^.buffer) (window^.cursor))
  (normalMode window)

removeChar :: Window -> Window
removeChar window =
  let win = set buffer (deleteCharAt (window^.buffer) (window^.cursor)) window
  in moveLeft win

removePrevChar :: Window -> Window
removePrevChar window =
  let (x, y) = window^.cursor
      win = set buffer (deleteCharAt (window^.buffer) (x - 1, y)) window
  in moveLeft win

replaceChar :: Char -> Window -> Window
replaceChar char window =
  let win = addChar char (forwardDelete window)
  in normalMode win

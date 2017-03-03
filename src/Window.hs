{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Window (Window(Window),
               Mode(Normal),
               Name(WindowID),
               name,
               bufferIndex,
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

data Name = WindowID Int deriving (Ord, Show, Eq)

data Window =
  Window { _bufferIndex :: Int
         , _cursor :: (Int, Int)
         , _column :: Int
         , _scroll :: (Int, Int)
         , _mode :: Mode
         , _mark :: Maybe (Int, Int)
         , _name :: Name
         }

makeLenses ''Window

handleWindowEvent :: Event -> (t, Int) -> (Window, Buffer) -> (Window, Buffer)
handleWindowEvent ev (width, height) (window, buffer) =
  updateScroll (width, height) $
  updateCursor $
  updateBuffer $
  case window^.mode of
    Normal ->
      case ev of
        EvKey KEnter [] -> moveDown (window, buffer)
        EvKey (KChar '$') [] -> moveEol (window, buffer)
        EvKey (KChar '0') [] -> moveBol (window, buffer)
        EvKey (KChar 'a') [] -> moveRight $ insertMode (window, buffer)
        EvKey (KChar 'A') [] -> append (window, buffer)
        EvKey (KChar 'b') [MCtrl] -> moveUpBy (height - 1) (window, buffer)
        EvKey (KChar 'd') [] -> deleteMode (window, buffer)
        EvKey (KChar 'f') [MCtrl] -> moveDownBy (height - 1) (window, buffer)
        EvKey (KChar 'h') [] -> moveLeft (window, buffer)
        EvKey (KChar 'i') [] -> insertMode (window, buffer)
        EvKey (KChar 'j') [] -> moveDown (window, buffer)
        EvKey (KChar 'k') [] -> moveUp (window, buffer)
        EvKey (KChar 'l') [] -> moveRight (window, buffer)
        EvKey (KChar 'r') [] -> replaceCharMode (window, buffer)
        EvKey (KChar 'o') [] -> addLine $ insertMode (window, buffer)
        EvKey (KChar 'O') [] -> addLineAbove (insertMode (window, buffer))
        EvKey (KChar 'x') [] -> forwardDelete (window, buffer)
        _ -> (window, buffer)
    ReplaceChar ->
      case ev of
        EvKey (KChar char) [] -> replaceChar char (window, buffer)
        _ -> normalMode (window, buffer)
    Insert ->
      case ev of
        EvKey KEsc [] -> normalMode (window, buffer)
        EvKey KBS [] -> backwardDelete (window, buffer)
        EvKey KEnter [] -> addLine (window, buffer)
        EvKey (KChar char) [] -> addChar char (window, buffer)
        _ -> normalMode (window, buffer)
    Delete ->
      case ev of
        EvKey (KChar 'd') [] -> removeLine (window, buffer)
        _ -> normalMode (window, buffer)
    Visual ->
      case ev of
        EvKey (KChar 'h') [] -> moveLeft (window, buffer)
        EvKey (KChar 'j') [] -> moveDown (window, buffer)
        EvKey (KChar 'k') [] -> moveUp (window, buffer)
        EvKey (KChar 'l') [] -> moveRight (window, buffer)
        _ -> normalMode (window, buffer)

drawWindow :: (Window, Buffer) -> T.Widget Name
drawWindow (window, buffer) =
  let (x, y) = window^.cursor
      (scrollX, scrollY) = window^.scroll
      n = window^.name
  in withBorderStyle unicodeBold $
    border $
    C.center $
    reportExtent n $
    (viewport n T.Vertical) $
    showCursor n (T.Location (x, y - scrollY)) $
    drawBuffer buffer (scrollX, scrollY)

updateBuffer :: (t, Buffer) -> (t, Buffer)
updateBuffer (window, buffer) =
  let ls = buffer^.lns
  in if length ls == 0
     then (window, oneLine buffer)
     else (window, buffer)

updateCursor :: (Window, Buffer) -> (Window, Buffer)
updateCursor (window, buffer) =
  let (x, y) = window^.cursor
      l = lineAt buffer updatedY
      ls = buffer^.lns
      yCap = (length ls) - 1
      xCap = case window^.mode of
               Insert -> length l
               _ -> (length l) - 1
      updatedX = max 0 (min xCap x)
      updatedY = max 0 (min yCap y)
  in (set cursor (updatedX, updatedY) window, buffer)

updateScroll :: (t, Int) -> (Window, Buffer) -> (Window, Buffer)
updateScroll (width, height) (window, buffer) =
  let (cursorX, cursorY) = window^.cursor
      (scrollX, scrollY) = window^.scroll
      ls = buffer^.lns
      yMax = max 0 (length ls - height)
      yMin = 0
  in if (cursorY - scrollY >= height)
     then (set scroll (cursorX, min yMax (cursorY - height + 1)) window, buffer)
     else if (cursorY - scrollY) < 0
          then (set scroll (max 0 cursorX, max yMin cursorY) window, buffer)
          else (window, buffer)

-- -- window stuff --

moveLeft :: (Window, t) -> (Window, t)
moveLeft (window, buffer) =
  let win = over cursor (\(x,y) -> (x - 1, y)) window
  in (over column (subtract 1) win, buffer)

moveDown :: (Window, t) -> (Window, t)
moveDown (window, buffer) = (over cursor (\(x,y) -> (window^.column, y + 1)) window, buffer)

moveUp :: (Window, t) -> (Window, t)
moveUp (window, buffer) = (over cursor (\(x,y) -> (window^.column, y - 1)) window, buffer)

moveRight :: (Window, t) -> (Window, t)
moveRight (window, buffer) =
  let win = over cursor (\(x,y) -> (x + 1, y)) window
  in (over column (+1) win, buffer)

moveBol :: (Window, t) -> (Window, t)
moveBol (window, buffer) =
  let win = set cursor (0, (snd (window^.cursor))) window
  in (set column 0 win, buffer)

moveEol :: (Window, Buffer) -> (Window, Buffer)
moveEol (window, buffer) =
  let (x, y) = window^.cursor
      l =  eol buffer y
      win = set cursor (l, y) window
  in (set column l win, buffer)

moveDownBy :: (Num t, Eq t) => t -> (Window, t1) -> (Window, t1)
moveDownBy 0 (window, buffer) = (window, buffer)
moveDownBy height (window, buffer) = moveDownBy (height - 1) (moveDown (window, buffer))

moveUpBy :: (Num t, Eq t) => t -> (Window, t1) -> (Window, t1)
moveUpBy 0 (window, buffer) = (window, buffer)
moveUpBy height (window, buffer) = moveUpBy (height - 1) (moveUp (window, buffer))

append :: (Window, Buffer) -> (Window, Buffer)
append (window, buffer) = moveEol (insertMode (window, buffer))

insertMode :: (Window, t) -> (Window, t)
insertMode (window, buffer) = (set mode Insert window, buffer)

deleteMode :: (Window, t) -> (Window, t)
deleteMode (window, buffer) = (set mode Delete window, buffer)

normalMode :: (Window, t) -> (Window, t)
normalMode (window, buffer) = moveLeft (set mode Normal window, buffer)

replaceCharMode :: (Window, t) -> (Window, t)
replaceCharMode (window, buffer) = (set mode ReplaceChar window, buffer)

forwardDelete :: (Window, Buffer) -> (Window, Buffer)
forwardDelete (window, buffer) =
  let (x, y) = window^.cursor
  in if x == 0
     then (window, buffer)
     else moveRight $ removeChar (window, buffer)

-- (window, buffer) stuff
  
backwardDelete :: (Window, Buffer) -> (Window, Buffer)
backwardDelete (window, buffer) =
  let (x, y) = window^.cursor
  in if x == 0
     then if y == 0
          then (window, buffer)
          else moveEol (moveUp (window, buffer))
     else removePrevChar (window, buffer)

addChar :: Char -> (Window, Buffer) -> (Window, Buffer)
addChar char (window, buffer) =
  (fst (moveRight (window, buffer)), insertCharAt buffer char (window^.cursor))
  
addLine :: (Window, Buffer) -> (Window, Buffer)
addLine (window, buffer) =
  let (x, y) = window^.cursor
  in (fst (moveBol (moveDown (window, buffer))), insertLineAt buffer (x, y))
  
addLineAbove :: (Window, Buffer) -> (Window, Buffer)
addLineAbove (window, buffer) =
  let (x, y) = window^.cursor
  in (fst $ moveBol (window, buffer), insertLineAt buffer (x, y - 1))

removeLine :: (Window, Buffer) -> (Window, Buffer)
removeLine (window, buffer) = (fst (normalMode (window, buffer)), deleteLineAt buffer (window^.cursor))

removeChar :: (Window, Buffer) -> (Window, Buffer)
removeChar (window, buffer) =
  (fst $ moveLeft (window, buffer), deleteCharAt buffer (window^.cursor))

removePrevChar :: (Window, Buffer) -> (Window, Buffer)
removePrevChar (window, buffer) =
  let (x, y) = window^.cursor
  in (fst $ moveLeft (window, buffer), deleteCharAt buffer (x - 1, y))

replaceChar :: Char -> (Window, Buffer) -> (Window, Buffer)
replaceChar char (window, buffer) = moveLeft $ addChar char (forwardDelete (window, buffer))

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
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Types as T
import Brick.Util (fg, bg, on)
import qualified Brick.AttrMap as A
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

punctuation = [' ', ',', '.', ';']

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
        EvKey (KChar 'b') [] -> movePrevBow (window, buffer)
        EvKey (KChar 'b') [MCtrl] -> moveUpBy (height - 1) (window, buffer)
        EvKey (KChar 'd') [] -> deleteMode (window, buffer)
        EvKey (KChar 'e') [] -> moveNextEow (window, buffer)
        EvKey (KChar 'f') [MCtrl] -> moveDownBy (height - 1) (window, buffer)
        EvKey (KChar 'h') [] -> moveLeft (window, buffer)
        EvKey (KChar 'i') [] -> insertMode (window, buffer)
        EvKey (KChar 'j') [] -> moveDown (window, buffer)
        EvKey (KChar 'J') [] -> joinNextLine (window, buffer)
        EvKey (KChar 'k') [] -> moveUp (window, buffer)
        EvKey (KChar 'l') [] -> moveRight (window, buffer)
        EvKey (KChar 'r') [] -> replaceCharMode (window, buffer)
        EvKey (KChar 'o') [] -> addLineBelow $ insertMode (window, buffer)
        EvKey (KChar 'O') [] -> addLineAbove $ insertMode (window, buffer)
        EvKey (KChar 'w') [] -> moveNextBow (window, buffer)
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
        EvKey (KChar 'd') [] -> moveBol $ removeLine (window, buffer)
        _ -> normalMode (window, buffer)
    Visual ->
      case ev of
        EvKey (KChar 'h') [] -> moveLeft (window, buffer)
        EvKey (KChar 'j') [] -> moveDown (window, buffer)
        EvKey (KChar 'k') [] -> moveUp (window, buffer)
        EvKey (KChar 'l') [] -> moveRight (window, buffer)
        _ -> normalMode (window, buffer)

drawWindow :: (Window, Buffer) -> Bool -> T.Widget Name
drawWindow (window, buffer) focused =
  let (x, y) = window^.cursor
      (scrollX, scrollY) = window^.scroll
      n = window^.name
      borderMappings = if focused
                       then [(B.borderAttr, V.blue `on` V.brightBlack)]
                       else []
  in withBorderStyle unicodeBold $
     C.updateAttrMap (A.applyAttrMappings borderMappings) $
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

-- window stuff

moveLeft :: (Window, Buffer) -> (Window, Buffer)
moveLeft (window, buffer) =
  let win = over cursor (\(x,y) -> (x - 1, y)) window
  in (over column (subtract 1) win, buffer)

moveDown :: (Window, Buffer) -> (Window, Buffer)
moveDown (window, buffer) = (over cursor (\(x,y) -> (window^.column, y + 1)) window, buffer)

moveUp :: (Window, Buffer) -> (Window, Buffer)
moveUp (window, buffer) = (over cursor (\(x,y) -> (window^.column, y - 1)) window, buffer)

moveRight :: (Window, Buffer) -> (Window, Buffer)
moveRight (window, buffer) =
  let win = over cursor (\(x,y) -> (x + 1, y)) window
  in (over column (+1) win, buffer)

moveBol :: (Window, Buffer) -> (Window, Buffer)
moveBol (window, buffer) =
  let win = set cursor (0, (snd (window^.cursor))) window
  in (set column 0 win, buffer)

-- windowbuffer stuff

moveEol :: (Window, Buffer) -> (Window, Buffer)
moveEol (window, buffer) =
  let (x, y) = window^.cursor
      l =  eol buffer y
      win = set cursor (l, y) window
  in (set column l win, buffer)

moveDownBy :: (Num t, Eq t) => t -> (Window, Buffer) -> (Window, Buffer)
moveDownBy 0 (window, buffer) = (window, buffer)
moveDownBy height (window, buffer) = moveDownBy (height - 1) (moveDown (window, buffer))

moveUpBy :: (Num t, Eq t) => t -> (Window, Buffer) -> (Window, Buffer)
moveUpBy 0 (window, buffer) = (window, buffer)
moveUpBy height (window, buffer) = moveUpBy (height - 1) (moveUp (window, buffer))

append :: (Window, Buffer) -> (Window, Buffer)
append (window, buffer) = moveEol (insertMode (window, buffer))

insertMode :: (Window, Buffer) -> (Window, Buffer)
insertMode (window, buffer) = (set mode Insert window, buffer)

deleteMode :: (Window, Buffer) -> (Window, Buffer)
deleteMode (window, buffer) = (set mode Delete window, buffer)

normalMode :: (Window, Buffer) -> (Window, Buffer)
normalMode (window, buffer) = moveLeft (set mode Normal window, buffer)

replaceCharMode :: (Window, Buffer) -> (Window, Buffer)
replaceCharMode (window, buffer) = (set mode ReplaceChar window, buffer)

forwardDelete :: (Window, Buffer) -> (Window, Buffer)
forwardDelete (window, buffer) =
  let (x, y) = window^.cursor
  in if x == 0
     then removeChar (window, buffer)
     else moveRight $ removeChar (window, buffer)

-- (window, buffer) stuff
  
backwardDelete :: (Window, Buffer) -> (Window, Buffer)
backwardDelete (window, buffer) =
  let (x, y) = window^.cursor
  in if x == 0
     then if y == 0
          then (window, buffer)
          else (fst $ moveEol $ moveUp (window, buffer), joinLinesAt buffer (x, y))
     else removePrevChar (window, buffer)

joinNextLine :: (Window, Buffer) -> (Window, Buffer)
joinNextLine (window, buffer) =
  let (x, y) = window^.cursor
  in if y + 1 == eof buffer
     then moveEol (window, buffer)
     else (fst $ moveEol (window, buffer), joinLinesAt buffer (eol buffer y, y + 1))

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
  in (fst $ moveBol (window, buffer), insertLineAt buffer (0, y))

addLineBelow :: (Window, Buffer) -> (Window, Buffer)
addLineBelow (window, buffer) =
  let (x, y) = window^.cursor
  in (fst $ moveDown $ moveBol (window, buffer), insertLineAt buffer (eol buffer y, y))

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
replaceChar char (window, buffer) = normalMode $ addChar char (forwardDelete (window, buffer))

moveNextBow :: (Window, Buffer) -> (Window, Buffer)
moveNextBow (window, buffer) = (set cursor (nextBow buffer (window^.cursor)) window, buffer)

movePrevBow :: (Window, Buffer) -> (Window, Buffer)
movePrevBow (window, buffer) = (set cursor (prevBow buffer (window^.cursor)) window, buffer)

moveNextEow :: (Window, Buffer) -> (Window, Buffer)
moveNextEow (window, buffer) = (set cursor (nextEow buffer (window^.cursor)) window, buffer)

-- does't work yet
-- movePrevEow :: (Window, Buffer) -> (Window, Buffer)
-- movePrevEow (window, buffer) = (set cursor (prevEow buffer (window^.cursor)) window, buffer) 

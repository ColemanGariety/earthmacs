{-# LANGUAGE TemplateHaskell #-}
module Editor (Editor(Editor),
               drawEditor,
               focusRing,
               handleEvent) where

import System.Random hiding (split)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad (replicateM)
import Data.List
import Data.Monoid
import Graphics.Vty
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Focus as F

import Split
import Window
import Buffer
import Util

data Editor =
  Editor { _buffers :: [Buffer]
         , _split :: Split
         , _focusRing :: F.FocusRing Name
         }

makeLenses ''Editor

handleEvent :: Editor -> T.BrickEvent Name e -> T.EventM Name (T.Next Editor)
handleEvent state (T.VtyEvent ev) =
  let Just n = F.focusGetCurrent (state^.focusRing)
      Just focusedWindow = getFocusedWindow n (state^.split)
      focusedBuffer = (state^.buffers)!!(focusedWindow^.bufferIndex)
  in case ev of
       EvKey (KChar 'c') [MCtrl] -> M.halt state
       EvKey (KChar 'L') [MMeta] -> M.continue $ splitTowards Horizontal n state
       EvKey (KChar 'H') [MMeta] -> M.continue $ splitTowards Horizontal n state
       EvKey (KChar 'J') [MMeta] -> M.continue $ splitTowards Vertical n state
       EvKey (KChar 'K') [MMeta] -> M.continue $ splitTowards Vertical n state
       EvKey KBackTab [] -> M.continue $ over focusRing F.focusPrev state
       EvKey (KChar '\t') [] -> M.continue $ over focusRing F.focusNext state
       _ -> do
         (width, height) <- getExtent n
         let (newWindow, newBuffer) = handleWindowEvent ev (width, height) (focusedWindow, focusedBuffer)
             bufferUpdater newBuffer buffers =
               let (as, bs) = splitAt (newWindow^.bufferIndex) buffers
               in as ++ [newBuffer] ++ (tail bs)
             windowUpdater newWindow split =
               case split^.window of
                 Just win -> if win^.name == n
                             then set window (Just newWindow) split
                             else split
                 Nothing -> split
         M.continue $ set buffers (bufferUpdater newBuffer (state^.buffers)) $ over split (transform (windowUpdater newWindow)) state

drawEditor state = [drawSplit (state^.buffers) (state^.split)]

getExtent n = do
  mExtent <- M.lookupExtent n
  let (width, height) = case mExtent of
                          Just (T.Extent _ _ (width, height) _) -> (width, height)
                          Nothing -> (0, 0)
  return (width, height) 

getFocusedWindow n s@(Split d l r w) = find (\(Window _ _ _ _ _ _ name) -> name == n) (traverseSplit [] s)

getWindowNames n s@(Split d l r w) = map (\(Window _ _ _ _ _ _ name) -> name) (traverseSplit [] s)

traverseSplit acc (Split _ (Just l) (Just r) Nothing) = acc ++ (traverseSplit acc l) ++ (traverseSplit acc r)
traverseSplit acc (Split _ _ _ (Just w)) = acc ++ [w]

splitTowards d n state = do
  let c = sum $ map (\(WindowID x) -> x) $ getWindowNames n (state^.split)
  let splitUpdater split =
        case split^.window of
          Just win -> if win^.name == n
                      then doSplit split
                      else split
          Nothing -> split
      doSplit split =
        set window Nothing $
        set direction (Just d) $
        set left (Just (Split Nothing Nothing Nothing (Just (set name (WindowID (c + 1)) (eliminate (split^.window)))))) $
        set right (Just (Split Nothing Nothing Nothing (Just (set name (WindowID (c + 2)) (eliminate (split^.window)))))) split
  updateFocusRing n $ over split (transform splitUpdater) state

updateFocusRing n state = set focusRing (F.focusRing (getWindowNames n (state^.split))) state

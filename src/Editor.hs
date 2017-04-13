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
      Just focusedWindow = getWindowByName n (state^.split)
      focusedBuffer = (state^.buffers)!!(focusedWindow^.bufferIndex)
  in case ev of
       EvKey (KChar 'c') [MCtrl] -> M.halt state
       EvKey (KChar 'q') [MCtrl] -> M.continue $ unsplit n state
       EvKey (KChar 'L') [MMeta] -> M.continue $ splitAs Horizontal n state
       EvKey (KChar 'H') [MMeta] -> M.continue $ splitAs Horizontal n state
       EvKey (KChar 'J') [MMeta] -> M.continue $ splitAs Vertical n state
       EvKey (KChar 'K') [MMeta] -> M.continue $ splitAs Vertical n state
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

-- returns all the splits as a list
getSplits :: [Window] -> Split -> [Window]
getSplits acc (Split _ (Just l) (Just r) Nothing) = acc ++ (getSplits acc l) ++ (getSplits acc r)
getSplits acc (Split _ _ _ (Just w)) = acc ++ [w]

getWindowByName :: Name -> Split -> Maybe Window
getWindowByName n s@(Split d l r w) = find (\(Window _ _ _ _ _ _ name) -> name == n) (getSplits [] s)

getWindowNames :: Split -> [Name]
getWindowNames s@(Split d l r w) = map (\(Window _ _ _ _ _ _ name) -> name) (getSplits [] s)

-- the first name in the ring will be focused
-- the variable n will designate the first
setFocusRing :: t -> Editor -> Editor
setFocusRing n state = set focusRing (F.focusRing (getWindowNames (state^.split))) state

-- not implemented
unsplit :: Name -> Editor -> Editor
unsplit n state = state

-- this could be cleaner probably
splitAs :: Direction -> Name -> Editor -> Editor
splitAs d n state = do
  let lastId = sum $ map (\(WindowID x) -> x) $ getWindowNames (state^.split)
      splitUpdater split@(Split _ _ _ (Just win)) = if win^.name == n then createSplit split else split
      splitUpdater split@(Split _ _ _ (Nothing)) = split
      createSplit split =
        set window Nothing $
        set direction (Just d) $
        set left (Just (Split Nothing Nothing Nothing (Just (set name (WindowID (lastId + 1)) (eliminate (split^.window)))))) $
        set right (Just (Split Nothing Nothing Nothing (Just (set name (WindowID (lastId + 2)) (eliminate (split^.window)))))) $
        split
  setFocusRing n $ over split (transform splitUpdater) state


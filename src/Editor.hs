{-# LANGUAGE TemplateHaskell #-}
module Editor (Editor(Editor),
               EditorMode(EditorNormal),
               drawEditor,
               focusRing,
               handleEvent) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad (replicateM)
import Data.List
import Data.Monoid
import Data.Maybe
import Graphics.Vty
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Focus as F
import Brick.Widgets.Core

import Drawer
import Split
import Window
import Buffer
import Util

data EditorMode = EditorNormal | EditorExecute | EditorFindFiles

data Editor =
  Editor { _buffers :: [Buffer]
         , _split :: Split
         , _focusRing :: F.FocusRing Name
         , _drawer :: Maybe Drawer
         , _editorMode :: EditorMode
         }

makeLenses ''Editor

handleEvent :: Editor -> T.BrickEvent Name e -> T.EventM Name (T.Next Editor)
handleEvent state (T.VtyEvent ev) =
  let Just n = F.focusGetCurrent (state^.focusRing)
      Just focusedWindow = getWindowByName n (state^.split)
      focusedBuffer = (state^.buffers)!!(focusedWindow^.bufferIndex)
  in case state^.editorMode of
       EditorExecute ->
         case ev of
           EvKey (KChar 'f') [MCtrl] -> M.continue $ editorFindFilesMode n state
           _ -> M.continue $ editorNormalMode state
       EditorFindFiles ->
         case ev of
           EvKey (KChar 'g') [MCtrl] -> M.continue $ editorNormalMode state
           _ -> M.continue $ handleDrawerEvent state
       EditorNormal ->
         case ev of
           EvKey (KChar 'x') [MCtrl] -> M.continue $ editorExecuteMode state
           EvKey (KChar 'c') [MCtrl] -> M.halt state
           EvKey (KChar 'q') [MCtrl] -> M.continue $ unsplit n state
           EvKey (KChar 'L') [MMeta] -> M.continue $ splitAs Horizontal n state
           EvKey (KChar 'H') [MMeta] -> M.continue $ splitAs Horizontal n state
           EvKey (KChar 'J') [MMeta] -> M.continue $ splitAs Vertical n state
           EvKey (KChar 'K') [MMeta] -> M.continue $ splitAs Vertical n state
           EvKey KBackTab [] -> M.continue $ over focusRing F.focusPrev state
           EvKey (KChar '\t') [] -> M.continue $ over focusRing F.focusNext state
           -- other events get delegated to the window
           -- a (Window, Buffer) pair is transformed
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
             M.continue $ set buffers (bufferUpdater newBuffer (state^.buffers)) $ over split (transform (windowUpdater newWindow)) $ state

drawEditor state =
  let Just n = F.focusGetCurrent (state^.focusRing)
  in if isJust (state^.drawer)
     then [drawSplit (state^.buffers) (state^.split) (state^.focusRing) <=>
           drawDrawer n (state^.drawer) (state^.focusRing)]
     else [drawSplit (state^.buffers) (state^.split) (state^.focusRing)]

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

-- this rebuilds the focus ring by traversing
-- the tree for the window names.
-- the first name in the ring will be focused
-- the variable n will designate the first.
setFocusRing n state =
  let names = getWindowNames (state^.split)
      (hs, ts) = splitAt (eliminate (findIndex (\name -> name == n) names)) names
  in set focusRing (F.focusRing (head ts : (hs ++ tail ts))) state

-- not implemented
unsplit :: Name -> Editor -> Editor
unsplit n state =
  let splitUpdater split@(Split _ _ _ (Just win)) = split
      splitUpdater split@(Split _ (Just l) (Just r) (Nothing)) =
        if isJust (l^.window) && ((eliminate (l^.window))^.name) == n
        then doUnsplit l (eliminate (l^.window))
        else if isJust (r^.window) && ((eliminate (r^.window))^.name) == n
             then doUnsplit r (eliminate (r^.window))
             else split
      doUnsplit s win =
        set window (Just win) $
        set direction Nothing $
        set left Nothing $
        set right Nothing $
        s
  in over split (transform splitUpdater) state
  
-- this could be cleaner probably
splitAs :: Direction -> Name -> Editor -> Editor
splitAs d n state =
  let lastId = sum $ map (\(WindowID x) -> x) $ getWindowNames (state^.split)
      nextId = (WindowID (lastId + 1))
      nextId' = (WindowID (lastId + 2))
      splitUpdater split@(Split _ _ _ (Just win)) = if win^.name == n then createSplit split else split
      splitUpdater split@(Split _ _ _ (Nothing)) = split
      createSplit split =
        set window Nothing $
        set direction (Just d) $
        set left (Just (Split Nothing Nothing Nothing (Just (set name nextId (eliminate (split^.window)))))) $
        set right (Just (Split Nothing Nothing Nothing (Just (set name nextId' (eliminate (split^.window)))))) $
        split
  in setFocusRing nextId $ over split (transform splitUpdater) state

editorFindFilesMode n state =
  set drawer (Just (Drawer "foo" ["bar"] 0 0)) $
  set focusRing (F.focusRing [DrawerID]) $
  set editorMode EditorFindFiles $
  state
  
editorNormalMode state =
  set editorMode EditorNormal $
  set drawer Nothing $
  setFocusRing (WindowID 0) $
  state
  
editorExecuteMode state =
  set editorMode EditorExecute $
  set drawer Nothing $
  state

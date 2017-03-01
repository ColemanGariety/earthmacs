{-# LANGUAGE TemplateHaskell #-}
module Editor (Editor(Editor), drawEditor, handleEvent) where

import Control.Lens
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
  in case ev of
       EvKey (KChar 'c') [MCtrl] -> M.halt state
       EvKey (KChar 'L') [MMeta] -> M.continue $ splitEast n state
       EvKey (KChar 'h') [MMeta] -> M.continue $ over focusRing F.focusPrev state
       EvKey (KChar 'l') [MMeta] -> M.continue $ over focusRing F.focusNext state
       _ -> do
         (width, height) <- getExtent n
         let go split =
               case split^.window of
                 Just win -> if win^.name == n
                             then set window (Just (handleWindowEvent ev (width, height) win)) split
                             else split
                 Nothing -> split
         M.continue $ over split (transform go) state

drawEditor :: Monad m => Editor -> m (T.Widget Name)
drawEditor state = do
  return $ drawSplit (state^.split) 10 10 0 0

getExtent n = do
  mExtent <- M.lookupExtent n
  let (width, height) = case mExtent of
                          Just (T.Extent _ _ (width, height) _) -> (width, height)
                          Nothing -> (0, 0)
  return (width, height) 

splitEast n state =
  case state^.split^.window of
    Just win -> if win^.name == n
                then over focusRing F.focusNext $
                     over split doSplit state
                else state
    Nothing -> state
    where doSplit split =
            set window Nothing $
            set left (Just (Split Nothing Nothing Nothing (Just (set name (WindowID 1) (eliminate (split^.window)))))) $
            set right (Just (Split Nothing Nothing Nothing (Just (set name (WindowID 2) (eliminate (split^.window)))))) split

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
       EvKey (KChar 'L') [MMeta] -> M.continue $ set split (splitEast (state^.split)) state
       _ -> do
         (width, height) <- getExtent n
         M.continue $ set split (handleSplitEvent (state^.split) ev (width, height)) state

drawEditor :: Monad m => Editor -> m (T.Widget Name)
drawEditor state = do
  return $ drawSplit (state^.split) 10 10 0 0

getExtent n = do
  mExtent <- M.lookupExtent n
  let (width, height) = case mExtent of
                          Just (T.Extent _ _ (width, height) _) -> (width, height)
                          Nothing -> (0, 0)
  return (width, height) 

{-# LANGUAGE TemplateHaskell #-}
module Editor (Editor(Editor), drawEditor, handleEvent) where

import Data.Label
import qualified Data.Label.Base as LB
import Data.Monoid
import Graphics.Vty
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Graphics.Vty as V

import Split
import Window
import Buffer

data Editor =
  Editor { _buffers :: [Buffer]
         , _split :: Split
         }

mkLabel ''Editor

handleEvent :: Editor -> T.BrickEvent Name e -> T.EventM Name (T.Next Editor)
handleEvent state (T.VtyEvent ev) =
  case ev of
    V.EvKey (V.KChar 'c') [MCtrl] -> M.halt state
    _ -> do
      M.continue $ set split (handleSplitEvent (get split state) ev) state

drawEditor state = do
  return $ drawSplit (get split state) 10 10 0 0

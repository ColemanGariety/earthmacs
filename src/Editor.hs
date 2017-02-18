{-# LANGUAGE TemplateHaskell #-}
module Editor (Editor(Editor), drawEditor, handleEvent) where

import Data.Label
import qualified Data.Label.Base as LB
import Data.Monoid
import Graphics.Vty
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Graphics.Vty as V

import Buffer
import Window

data Editor =
  Editor { _buffers :: [Buffer]
         , _split :: Split
         , _active :: Window
         }

mkLabel ''Editor

handleEvent :: Editor -> T.BrickEvent Name e -> T.EventM Name (T.Next Editor)
handleEvent state (T.VtyEvent ev) =
  case ev of
    V.EvKey (V.KChar 'c') [MCtrl] -> M.halt state
    _ -> do
      M.continue $ set active (handleWindowEvent (get active state) ev) state

drawEditor state = do
  drawWindow (get active state) 50 50 0 0

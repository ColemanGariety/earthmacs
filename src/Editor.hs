{-# LANGUAGE TemplateHaskell #-}
module Editor (Editor(Editor), drawEditor, handleEvent) where

import Data.Label
import qualified Data.Label.Base as LB
import Data.Monoid
import Graphics.Vty
import qualified Brick.Types as T
import qualified Brick.Main as M

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
  let Just w = getWindow (get split state)
      n = get name w
  in case ev of
       EvKey (KChar 'c') [MCtrl] -> M.halt state
       EvKey (KChar 'L') [MMeta] -> do M.continue $ set split (splitEast (get split state)) state
       _ -> do
         mExtent <- M.lookupExtent n
         let (width, height) = case mExtent of
                                 Just (T.Extent _ _ (width, height) _) -> (width, height)
                                 Nothing -> (0, 0)
         M.continue $ set split (handleSplitEvent (get split state) ev (width, height)) state

drawEditor :: Monad m => Editor -> m (T.Widget Name)
drawEditor state = do
  return $ drawSplit (get split state) 10 10 0 0

{-# LANGUAGE TemplateHaskell #-}
module Drawer (Drawer(Drawer),
               handleDrawerEvent,
               drawDrawer) where

import Control.Lens
import Brick.Widgets.Core
import Brick.Widgets.Border
import qualified Brick.Types as T
import Window

data Drawer =
  Drawer { _value :: FilePath
         , _options :: [String]
         , _index :: Integer
         , _scroll :: Integer
         }

makeLenses ''Drawer

handleDrawerEvent state = state

drawDrawer n (Just drawer) focusRing =
  showCursor DrawerID (T.Location (length (drawer^.value), 0)) $
  str (drawer^.value)

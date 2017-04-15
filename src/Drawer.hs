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
  Drawer { _value :: String
         , _options :: [String]
         , _index :: Integer
         , _scroll :: Integer
         }

makeLenses ''Drawer

handleDrawerEvent state = state

drawDrawer n (Just drawer) focusRing  =
  showCursor DrawerID (T.Location (0, 0)) $
  str (drawer^.value)

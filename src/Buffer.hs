{-# LANGUAGE TemplateHaskell #-}
module Buffer (Buffer(Buffer), drawBuffer, handleBufferEvent) where

import Data.Label
import qualified Brick.Types as T
import Brick.Widgets.Core
import Brick.Widgets.Border.Style
import Brick.Widgets.Border

data Buffer =
  Buffer { _lns :: [String]
         , _path :: String
         }

mkLabel ''Buffer

drawBuffer buffer = str (head (get lns buffer))

handleBufferEvent buffer ev =
  case ev of
    _ -> set lns ["bar"] buffer

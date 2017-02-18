{-# LANGUAGE TemplateHaskell #-}
module Buffer (Buffer(Buffer), Name(View), getLns, drawBuffer, handleBufferEvent) where

import Data.Label
import qualified Brick.Types as T
import Brick.Widgets.Core
import Brick.Widgets.Border.Style
import Brick.Widgets.Border

data Name = View deriving (Ord, Show, Eq)

data Buffer =
  Buffer { _lns :: [String]
         , _path :: String
         }

mkLabel ''Buffer

updateFoo = set lns ["foo"]

getLns buffer = get lns buffer

drawBuffer buffer = viewport View T.Vertical $ str (head (getLns buffer))

handleBufferEvent buffer ev =
  case ev of
    _ -> set lns ["bar"] buffer

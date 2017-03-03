module Main where

import Control.Lens
import System.Directory
import Control.Monad (void)
import Brick.Main as M
import qualified Brick.Focus as F
import Brick.AttrMap (attrMap)
import qualified Graphics.Vty as V

import Split
import Editor
import Buffer
import Window

getInitialState :: Editor
getInitialState =
  Editor [welcomeBuffer] rootSplit (F.focusRing [WindowID 0])
  where welcomeBuffer = Buffer ["Earthmacs welcomes you in."] getCurrentDirectory
        welcomeWindow = Window 0 (0, 0) 0 (0, 0) Normal Nothing (WindowID 0)
        rootSplit = Split Nothing Nothing Nothing (Just welcomeWindow)

chooseCursor = F.focusRingCursor (^.focusRing)

app :: M.App Editor e Name
app =
  M.App { M.appDraw = drawEditor
        , M.appStartEvent = return
        , M.appHandleEvent = handleEvent
        , M.appAttrMap = const $ attrMap V.defAttr []
        , M.appChooseCursor = chooseCursor
        }

main :: IO ()
main = void $ M.defaultMain app getInitialState

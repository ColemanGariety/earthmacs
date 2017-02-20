module Main where

import Control.Monad (void)
import Brick.Main as M
import Brick.AttrMap (attrMap)
import qualified Graphics.Vty as V

import Split
import Editor
import Buffer
import Window

getInitialState :: Editor
getInitialState =
  Editor [welcomeBuffer] rootSplit 
  where welcomeBuffer = Buffer ["Earthmacs welcomes you in."] "/home/coleman/Git"
        welcomeWindow = Window welcomeBuffer (0, 0) (0, 0) Normal Nothing
        rootSplit = Split Nothing Nothing Nothing (Just welcomeWindow)

app :: M.App Editor e Name
app =
  M.App { M.appDraw = drawEditor
        , M.appStartEvent = return
        , M.appHandleEvent = handleEvent
        , M.appAttrMap = const $ attrMap V.defAttr []
        , M.appChooseCursor = M.showFirstCursor
        }

main :: IO ()
main = void $ M.defaultMain app getInitialState

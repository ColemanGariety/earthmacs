{-# LANGUAGE TemplateHaskell #-}
module Buffer (Buffer(Buffer),
               drawBuffer,
               oneLine,
               eol,
               lns,
               charAt,
               lineAt,
               insertCharAt,
               deleteCharAt,
               insertLineAt,
               deleteLineAt) where

import Control.Lens
import Data.List
import qualified Brick.Types as T
import Brick.Widgets.Core

data Buffer =
  Buffer { _lns :: [String]
         , _path :: IO FilePath
         }

makeLenses ''Buffer

drawBuffer buffer (scrollX, scrollY) = str (unlines (drop scrollY (buffer^.lns)))

eol :: Buffer -> Int -> Int
eol buffer y = length $ lineAt buffer y

oneLine :: Buffer -> Buffer
oneLine = set lns [""] 

charAt :: Buffer -> (Int, Int) -> Char
charAt buffer (x, y) = ((buffer^.lns)!!y)!!x

lineAt :: Buffer -> Int -> String
lineAt buffer y = (buffer^.lns)!!y

insertLineAt :: Buffer -> (t, Int) -> Buffer
insertLineAt buffer (x, y) =
  let (as, bs) = splitAt (y + 1) (buffer^.lns)
  in set lns (as ++ [[]] ++ bs) buffer

deleteLineAt :: Buffer -> (t, Int) -> Buffer
deleteLineAt buffer (x, y) =
  let (as, bs) = splitAt (y + 1) (buffer^.lns)
  in set lns ((init as) ++ bs) buffer

insertCharAt :: Buffer -> Char -> (Int, Int) -> Buffer
insertCharAt buffer char (x, y) =
  let (as, bs) = splitAt y (buffer^.lns)
  in let (ys, zs) = splitAt x (lineAt buffer y)
     in set lns (as ++ [ys ++ [char] ++ zs] ++ (tail bs)) buffer

deleteCharAt :: Buffer -> (Int, Int) -> Buffer
deleteCharAt buffer (x, y) =
  let (as, bs) = splitAt y (buffer^.lns)
  in let (ys, zs) = splitAt x (lineAt buffer y)
     in set lns (as ++ [ys ++ (tail zs)] ++ (tail bs)) buffer

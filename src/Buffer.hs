{-# LANGUAGE TemplateHaskell #-}
module Buffer (Buffer(Buffer),
               drawBuffer,
               oneLine,
               getLines,
               getPath,
               eol,
               charAt,
               lineAt,
               insertCharAt,
               deleteCharAt,
               insertLineAt,
               deleteLineAt) where

import Data.Label
import Data.List
import qualified Brick.Types as T
import Brick.Widgets.Core

data Buffer =
  Buffer { _lns :: [String]
         , _path :: IO FilePath
         }

mkLabel ''Buffer

drawBuffer buffer (scrollX, scrollY) = str (unlines (drop scrollY (get lns buffer)))

eol :: Buffer -> Int -> Int
eol buffer y = length $ lineAt buffer y

oneLine :: Buffer -> Buffer
oneLine buffer = set lns [""] buffer

getLines :: Buffer -> [String]
getLines = get lns

getPath :: Buffer -> IO FilePath
getPath = get path

charAt :: Buffer -> (Int, Int) -> Char
charAt buffer (x, y) = ((get lns buffer)!!y)!!x

lineAt :: Buffer -> Int -> String
lineAt buffer y = (get lns buffer)!!y

insertLineAt :: Buffer -> (t, Int) -> Buffer
insertLineAt buffer (x, y) =
  let (as, bs) = splitAt (y + 1) (get lns buffer)
  in set lns (as ++ [[]] ++ bs) buffer

deleteLineAt :: Buffer -> (t, Int) -> Buffer
deleteLineAt buffer (x, y) =
  let (as, bs) = splitAt (y + 1) (get lns buffer)
  in set lns ((init as) ++ bs) buffer

insertCharAt :: Buffer -> Char -> (Int, Int) -> Buffer
insertCharAt buffer char (x, y) =
  let (as, bs) = splitAt y (get lns buffer)
  in let (ys, zs) = splitAt x (lineAt buffer y)
     in set lns (as ++ [ys ++ [char] ++ zs] ++ (tail bs)) buffer

deleteCharAt :: Buffer -> (Int, Int) -> Buffer
deleteCharAt buffer (x, y) =
  let (as, bs) = splitAt y (get lns buffer)
  in let (ys, zs) = splitAt x (lineAt buffer y)
     in set lns (as ++ [ys ++ (tail zs)] ++ (tail bs)) buffer

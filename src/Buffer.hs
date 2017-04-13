{-# LANGUAGE TemplateHaskell #-}
module Buffer (Buffer(Buffer),
               drawBuffer,
               oneLine,
               eol,
               lns,
               nextBow,
               prevBow,
               charAt,
               lineAt,
               insertCharAt,
               deleteCharAt,
               insertLineAt,
               deleteLineAt) where

import Control.Lens
import Data.List
import Data.Char
import Util
import qualified Brick.Types as T
import Brick.Widgets.Core

data Buffer =
  Buffer { _lns :: [String]
         , _path :: IO FilePath
         }

makeLenses ''Buffer

drawBuffer :: Buffer -> (t, Int) -> T.Widget n
drawBuffer buffer (scrollX, scrollY) = str (unlines (drop scrollY (buffer^.lns)))

eol :: Buffer -> Int -> Int
eol buffer y = length $ lineAt buffer y

nextBow :: Buffer -> (Int, Int) -> Int
nextBow buffer (x, y) =
  let (as, bs) = splitAt x (lineAt buffer y)
      a = eliminate $ findIndex (\c -> isSpace c) bs
      (cs, ds) = splitAt a bs
      b = eliminate $ findIndex (\c -> not (isSpace c)) ds
  in x + a + b

prevBow :: Buffer -> (Int, Int) -> Int
prevBow buffer (x, y) =
  let (as, bs) = splitAt x (lineAt buffer y)
      a = eliminate $ findIndex (\c -> isSpace c) (reverse as)
      (cs, ds) = splitAt a bs
      b = eliminate $ findIndex (\c -> not (isSpace c)) (reverse cs)
  in x - a - b

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

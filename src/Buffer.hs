{-# LANGUAGE TemplateHaskell #-}
module Buffer (Buffer(Buffer),
               drawBuffer,
               oneLine,
               eol,
               eof,
               lns,
               nextBow,
               nextEow,
               prevBow,
               charAt,
               lineAt,
               insertCharAt,
               deleteCharAt,
               insertLineAt,
               joinLinesAt,
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

eof :: Buffer -> Int
eof buffer = length $ buffer^.lns

-- in these word-location equations
-- go checks for whitespace
-- go' traverses the buffer
-- all four are slightly different

nextBow :: Buffer -> (Int, Int) -> (Int, Int)
nextBow buffer (sx, sy) = go False (sx, sy)
  where go found (x, y)
          | found && (not (isSpace (charAt buffer (x, y)))) = (x, y)
          | not found && isSpace (charAt buffer (x, y)) = go' True (x, y)
          | otherwise = go' found (x, y)
        go' found (x, y)
          | x + 1 >= eol buffer y && y + 1 >= eof buffer = (x, y)
          | x + 1 >= eol buffer y = go True (0, y + 1)
          | otherwise = go found (x + 1, y)
          
nextEow :: Buffer -> (Int, Int) -> (Int, Int)
nextEow buffer (sx, sy) = go' False (sx, sy)
  where go found (x, y)
          | found && x + 1 >= eol buffer y = (x, y)
          | found && isSpace (charAt buffer (x, y)) = (x - 1, y)
          | not found && not (isSpace (charAt buffer (x, y))) = go' True (x, y)
          | otherwise = go' found (x, y)
        go' found (x, y)
          | x + 1 >= eol buffer y && y + 1 >= eof buffer = (x, y)
          | x + 1 >= eol buffer y = go True (0, y + 1)
          | otherwise = go found (x + 1, y)
        
prevBow :: Buffer -> (Int, Int) -> (Int, Int)
prevBow buffer (sx, sy) = go' False (sx, sy)
  where go found (x, y)
          | found && x - 1 <= 0 = (x - 1, y)
          | found && isSpace (charAt buffer (x, y)) = (x + 1, y)
          | not found && not (isSpace (charAt buffer (x, y))) = go' True (x, y)
          | otherwise = go' found (x, y)
        go' found (x, y)
          | x - 1 <= 0 && y <= 0 = (x, y)
          | x - 1 <= 0 = go False ((eol buffer (y - 1)) - 1, y - 1)
          | otherwise = go found (x - 1, y)

-- BEGIN WIP...
  
-- prevEow :: Buffer -> (Int, Int) -> (Int, Int)
-- prevEow buffer (sx, sy) = go False (sx, sy)
--   where go found (x, y)
--           | found && isSpace (charAt buffer (x, y)) = (x - 1, y)
--           | not found && not (isSpace (charAt buffer (x, y))) = go' True (x, y)
--           | otherwise = go' found (x, y)
--         go' found (x, y)
--           | x - 1 <= 0 && y - 1 <= 0 = (x - 1, y)
--           | x - 1 <= 0 = go True (0, y - 1)
--           | otherwise = go found (x - 1, y)

-- END WIP

oneLine :: Buffer -> Buffer
oneLine = set lns [""] 

charAt :: Buffer -> (Int, Int) -> Char
charAt buffer (x, y) = ((buffer^.lns)!!y)!!x

lineAt :: Buffer -> Int -> String
lineAt buffer y = (buffer^.lns)!!y

insertLineAt :: Buffer -> (Int, Int) -> Buffer
insertLineAt buffer (x, y) =
  let (as, bs) = splitAt (y + 1) (buffer^.lns)
      (ys, zs) = splitAt x (lineAt buffer y)
  in set lns ((init as) ++ [ys] ++ [zs] ++ bs) buffer

joinLinesAt :: Buffer -> (Int, Int) -> Buffer
joinLinesAt buffer (x, y) =
  let (as, bs) = splitAt y (buffer^.lns)
  in set lns ((init as) ++ [(last as) ++ (head bs)] ++ (tail bs)) buffer

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

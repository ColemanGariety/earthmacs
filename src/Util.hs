module Util (eliminate, justOrZero) where

-- only use when you are SURE it isn't a maybe
-- but it really ought to be two data types
eliminate :: Maybe a -> a
eliminate (Just a) = a

justOrZero (Just a) = a
justOrZero Nothing = 0

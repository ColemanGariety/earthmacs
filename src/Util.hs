module Util (eliminate) where

-- only use when you are SURE it isn't a maybe
eliminate :: Maybe a -> a
eliminate (Just a) = a

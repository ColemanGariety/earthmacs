module Util (eliminate) where

eliminate :: Maybe a -> a
eliminate (Just a) = a

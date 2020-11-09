{-# LANGUAGE LinearTypes #-}

module LinearSeq where

bad :: a %1 -> ()
bad x = seq x ()

{-# LANGUAGE LinearTypes #-}

module LinearSeq where

bad :: a ->. ()
bad x = seq x ()

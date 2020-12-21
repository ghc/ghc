{-# LANGUAGE NoIncomplete #-}

module A where

-- should warn about non-exhaustive pattern match
w :: String -> String
w x = let (_:_) = x in "1"

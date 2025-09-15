{-# LANGUAGE ViewPatterns #-}

module Bug where

f :: Maybe a -> Bool
f (id->Nothing)  = False
f (id->(Just _)) = True

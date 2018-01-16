{-# LANGUAGE RebindableSyntax #-}

module Bug where

foo :: (a, b) -> ()
foo x | (_,_) <- x = ()

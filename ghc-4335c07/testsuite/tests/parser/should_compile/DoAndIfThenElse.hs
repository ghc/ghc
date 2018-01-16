
{-# LANGUAGE DoAndIfThenElse #-}

module DoAndIfThenElse where

foo :: IO ()
foo = do if True
         then return ()
         else return ()

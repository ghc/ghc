
{-# LANGUAGE NoDoAndIfThenElse #-}

module NoDoAndIfThenElse where

foo :: IO ()
foo = do if True
         then return ()
         else return ()

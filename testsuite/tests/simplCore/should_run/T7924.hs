{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Exception (throwIO, Exception)
import Control.Monad (when)
import Data.Typeable (Typeable)

data Boom = Boom deriving (Show, Typeable)
instance Exception Boom

main = do
    args <- return []

    -- Should throw this exception.
    when (length args /= 1) (throwIO Boom)

    -- With -O, instead throws this one from head [].
    let n = read (head args)
    print (n :: Int)

    return ()
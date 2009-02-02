
-- trace #2913

{-# LANGUAGE DeriveDataTypeable #-}

import qualified Control.Exception as New
import qualified Control.OldException as Old

import Data.Typeable

data MyException = MyException
    deriving (Eq, Show, Typeable)

instance New.Exception MyException

main :: IO ()
main = (New.throwIO MyException
            `Old.catch`
            (\e -> do putStrLn ("Old got " ++ show e)
                      Old.throw e)
       ) `New.catch` (\e -> putStrLn ("New got " ++ show (e :: MyException)))


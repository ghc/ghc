{-# LANGUAGE FunctionalDependencies #-}  {-# LANGUAGE FlexibleInstances #-}

module T7171 where

import T7171a
import Data.ByteString

-- this works
-- test1 :: [Int] -> [Int]
-- test1 = test

-- this fails
test2 :: ByteString -> ByteString
test2 = test

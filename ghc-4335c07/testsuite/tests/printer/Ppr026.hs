{-# Language OverloadedStrings #-}
-- from https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html

import Data.String

n :: Num a => a
n = 43

f  :: Fractional a => a
f = 03.1420

-- foo :: Text
foo :: Data.String.IsString a => a
foo = "hello\n there"

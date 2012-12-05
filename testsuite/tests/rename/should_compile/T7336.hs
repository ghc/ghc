module T7336 ( U, T ) where

data U = MkU deriving Show
-- Want to report MkU as unused

data T = MkT deriving Read
-- But MkT is not unused; we might
-- parse a string to generate it

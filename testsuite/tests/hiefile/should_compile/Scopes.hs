{-# LANGUAGE RecordWildCards #-}
module Scopes where
data T = C { x :: Int, y :: Char }

-- Verify that names generated from record construction are in scope
foo = C { x = 1 , y = 'a' }

-- Verify that record wildcards are in scope
sdaf :: T
sdaf = C{..}
  where
    x = 1
    y = 'a'

{-# OPTIONS -fglasgow-exts -dcore-lint #-}

-- The dict-bindings attached to an IPBinds 
-- need not be in recursive order.  This is
-- a long-standing bug, which lasted up to 
-- and including GHC 6.4.2

module Bug795(foo) where

data Arg = E Integer | T Bool deriving (Eq, Show)

foo :: Integer -> [Arg] -> IO String
foo 1 as = do { let ?err = "my custom error"
               ; let ws = (show (firstE as))
               ; return (show (firstE as)) }

firstE :: (?err :: String) => [Arg] -> Integer
firstE = error "urk"

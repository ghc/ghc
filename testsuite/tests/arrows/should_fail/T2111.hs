{-# OPTIONS_GHC -XArrows -XRecursiveDo#-}
-- Test Trac #2111

module Foo where

foo = do { rec { x <- undefined -< x }; undefined -< x }

bar1 = do { rec { x <- return ('a':x); }; putStrLn (take 20 x) }

bar2 = mdo { rec { x <- return ('a':x); }; putStrLn (take 20 x) }

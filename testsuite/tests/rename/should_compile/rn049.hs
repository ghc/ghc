-- GHC 6.4.1 said
--  test.hs:1:5:
--     Warning: accepting non-standard pattern guards 
--		(-fglasgow-exts to suppress this message)
--         [x <- ((1 * 2) + 3) * 4, undefined]
-- Note the wrongly-parenthesised expression

{-# LANGUAGE Haskell98 #-}

module ShouldCompile where

main | x <- 1*2+3*4 = x
 
{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

-- A stripped down functional-dependency 
-- example that causes GHC 4.08.1 to crash with:
-- "basicTypes/Var.lhs:194: Non-exhaustive patterns in function readMutTyVar"
-- Reported by Thomas Hallgren Nov 00


foo = dup 1 >>= print

foreign import "dup" primDup :: Int -> IO Int
--dup :: Int -> IO Int               -- not needed with (1), needed with (2)
dup = call primDup                   -- ghc crashes here with (1), ok with (2)

class Call    c h | c -> h where call  :: c -> h            -- (1) problematic
--class Call    c h          where call  :: c -> h          -- (2) ok

class Result  c h | c -> h where fromC :: c -> IO h

instance Result c h => Call (IO c) (IO h) where call f = fromC =<< f
instance Call c h => Call (Int->c) (Int->h) where call f = call . f

instance Result Int Int where fromC = return


{-
The example is perhaps too stripped down to illustrate the purpose of these
classes, but the idea is that the class "Call" should relate suitably declared
low-level prim_types in foreign imports to sensible, high-level Haskell types,
allowing high level functions to be obtained by simply applying the method
"call" to the primitive function, as in the definition of dup above, without
having to explicitly give also the type of the high level function.
-}

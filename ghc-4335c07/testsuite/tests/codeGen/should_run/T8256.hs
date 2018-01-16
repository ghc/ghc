{-# LANGUAGE MagicHash, UnboxedTuples , ScopedTypeVariables #-}

module Main where


import GHC.Prim
import GHC.Types
import Foreign
import Foreign.Ptr
import GHC.Ptr

wrapFetch :: (a -> State# RealWorld -> State# RealWorld) -> (a -> IO ())
wrapFetch prefetch  a = IO (\ s -> (# prefetch a s, ()#))







main :: IO ()
main = do
    (ptr :: Ptr Int) <- malloc
    wrapFetch (\ (Ptr adr)-> prefetchAddr3# adr 0# ) ptr
    wrapFetch prefetchValue1# (1 ::Int)
    wrapFetch prefetchValue2# "hiiii"
    wrapFetch prefetchValue3# (Just "testing")
    wrapFetch prefetchValue0# (error "this shouldn't get evaluated")
    --  -- ^^ this is to make sure it doesn't force thunks!
    --incontrast,
    --wrapFetch prefetchValue0#  $! (error "this shouldn't get evaluated")
    -- would trigger an exception
    putStrLn "success"

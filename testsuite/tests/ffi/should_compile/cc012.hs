{-# LANGUAGE ForeignFunctionInterface #-}
-- !!! test that infix operators can be exported with foreign export,
-- and that we can export something which isn't defined in this module.
module ShouldCompile where
foreign export ccall "plusInt" (+) :: Int -> Int -> Int


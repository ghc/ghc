module ShouldCompile where
import Foreign
foreign import ccall "dynamic" imp :: FunPtr Int -> Int
f1 a = imp a + 1
f2 a = imp a + 2

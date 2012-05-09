{-# LANGUAGE ForeignFunctionInterface, CPP #-}
import Foreign
import Foreign.C

type IOF = Int -> IO Int

foreign import ccall "wrapper" wrap_f_io :: IOF -> IO (FunPtr IOF)
foreign import ccall "dynamic" f_io :: FunPtr IOF -> IOF

-- The value of n needs to be adjusted to avoid overflowing the
-- C stack.  n is the number of times the f calls itself, and each
-- C call allocates a bit over 16 kB on a 64 bit processor.
-- (Remember that there is no tail call optimization of foreign functions.)
-- A typical C stack is 8 MB, so n = 400 will allocate about 4.8 MB
-- on a 64 bit system.  If you have a 128 bit processor you'll have to
-- reduce it.
--
-- Under ghci this test segfaults for smaller n, probably
-- because more of the C stack is allocated for other use than
-- when compiled.
--
-- On *nix systems, the C stack size can be examined and changed by
-- the "ulimit -s" command.

n = 300

f :: Int -> IO Int
f 0 = return 42
f n = do 
  f' <- wrap_f_io f
  f_io f' (n-1)

main = f n >>= print

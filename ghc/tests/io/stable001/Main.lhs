\begin{code}
module Main(main) where

--import PreludeGlaST
--old:import GHCio(stThen)
--old:import PreludeGlaMisc

main =	makeStablePtr test	>>= \ stablePtr ->
	((_casm_GC_ ``SaveAllStgRegs(); test1(%0); RestoreAllStgRegs();'' stablePtr)
						:: PrimIO ())
				>>= \ _ ->
	return ()

test :: IO Int
test =
	let f x = sum [1..x]
	    f :: Int -> Int
	in 
	_ccall_ printf
	      "The stable pointer has just been used to print this number %d\n" (f 100)
				>>= \ _ ->
	return 5
\end{code}

This is a rather exciting experiment in using the new call
@makeStablePtr#@ and @performIO@. It doesn't do much but it took an
incredible effort to get it to do it.

\begin{code}[C-code]
#define NULL_REG_MAP
#include "stgdefs.h"

int
test1( stableIOPtr )
  StgStablePtr stableIOPtr;
{
  int i;
  int result;

  printf("Using stable pointer %x\n", stableIOPtr);

  for( i = 0; i != 10; i = i + 1 ) {
    printf( "Calling stable pointer for %dth time\n", i );
    performIO( stableIOPtr );
    printf( "Returned after stable pointer\n" );
  }

  return 1;
}
\end{code}

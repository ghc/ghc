module Main (main, hputc) where

import IO

main = _casm_GC_ ``rts_evalIO(
			rts_apply(
		  	  &Main_hputc_closure,
		  	  rts_mkChar('x')
			  ),
		        NULL
	     	   );'' :: IO ()

hputc :: Char -> IO ()
hputc c = hPutChar stdout c >> hPutChar stdout '\n'

foreign export hputc :: Char -> IO ()

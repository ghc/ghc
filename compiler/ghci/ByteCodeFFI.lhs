%
% (c) The University of Glasgow 2001-2008
%

ByteCodeGen: Generate machine-code sequences for foreign import

\begin{code}
module ByteCodeFFI ( moan64, newExec ) where

import Outputable
import System.IO
import Foreign
import Foreign.C

moan64 :: String -> SDoc -> a
moan64 msg pp_rep
   = unsafePerformIO (
        hPutStrLn stderr (
        "\nGHCi's bytecode generation machinery can't handle 64-bit\n" ++
        "code properly yet.  You can work around this for the time being\n" ++
        "by compiling this module and all those it imports to object code,\n" ++
        "and re-starting your GHCi session.  The panic below contains information,\n" ++
        "intended for the GHC implementors, about the exact place where GHC gave up.\n"
        )
     )
     `seq`
     pprPanic msg pp_rep

newExec :: Storable a => [a] -> IO (FunPtr ())
newExec code
   = alloca $ \pcode -> do
        ptr <- _allocateExec (fromIntegral $ codeSize undefined code) pcode
        pokeArray ptr code
        code <- peek pcode
        return (castPtrToFunPtr code)
   where
   codeSize :: Storable a => a -> [a] -> Int
   codeSize dummy array = sizeOf(dummy) * length array

foreign import ccall unsafe "allocateExec"
  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)  
\end{code}


%
% (c) The University of Glasgow 2001-2008
%

ByteCodeGen: Generate machine-code sequences for foreign import

\begin{code}
module ByteCodeFFI ( moan64 ) where

import Outputable
import System.IO
import System.IO.Unsafe

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
\end{code}


import Control.Exception as CE

import Data.Typeable
import Data.Dynamic

data MyException = MyException deriving (Typeable,Show) 

exception_uncaught    = CE.throwDyn MyException
exceptionIO_uncaught  = CE.throwIO (DynException (toDyn MyException))

exception_caught = CE.try exceptionIO_uncaught
module Control.Monad.X.Utils where

-- | This is a private module and is not to be imported
-- directly by non-library modules.


import Prelude(return,fail,(.))
import Control.Monad(MonadPlus(..))
import Control.Monad.X.Trans

-- has base
inBase' m = lift (inBase m)

-- monad 
return' x = lift (return x)
fail' msg = lift (fail msg)

-- reader
ask'      :: (MonadTrans t, MonadReader r m) => t m r
ask'          = lift ask
local' map f  = map (local f)

-- writer
tell' w                 = lift (tell w)
listen1' mk unmk add m  = mk (do (x,w) <- listen (unmk m)
                                 return (add w x))
listen2' mk unmk add m  = mk (\s -> do (x,w) <- listen (unmk m s)
                                       return (add w x))

-- state
get'      :: (MonadTrans t, MonadState s m) => t m s
get'      = lift get
put' s    = lift (put s)

-- error
throwError' e             = lift (throwError e)
catchError1' mk unmk m h  = mk (catchError (unmk m) (unmk . h))
catchError2' mk unmk m h  = mk (\y -> catchError (unmk m y) (\e -> unmk (h e) y))

-- mplus
mzero'    :: (MonadTrans t, MonadPlus m) => t m a
mzero'              = lift mzero
mplus1' mk unmk m n = mk (mplus (unmk m) (unmk n))
mplus2' mk unmk m n = mk (\y -> unmk m y `mplus` unmk n y)

-- cont
callCC1' mk unmk ret f  = mk (callCC (\br -> unmk (f (\a -> lift (br (ret a))))))
callCC2' mk unmk ret f  = mk (\s -> callCC (\br -> unmk (f (\a -> lift (br (ret a s)))) s))





-- Trac #4306
-- Check that the worker for 'upd' has only one argument

module T4306 where

data D = D {-# UNPACK #-} !Double {-# UNPACK #-} !Double  
data UPD = UPD {-# UNPACK #-} !Double D

upd (UPD _ (D x _)) = sqrt $! (x*x + x*x + sin x + x*x + x*x + cos x + x*x + x*x + tan x +
                               x*x + x*x + sin x + x*x + x*x + cos x + x*x + x*x + tan x +
                               x*x + x*x + sin x + x*x + x*x + cos x + x*x + x*x + tan x)
                               -- make the rhs large enough to be worker/wrapperred

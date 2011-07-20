-- !!! tests for ArityErr
module TcFail where

data A a b = B (A a)

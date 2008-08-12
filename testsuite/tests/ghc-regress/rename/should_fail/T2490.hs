-- Trac #2490
module ShouldFail where

-- All these sections are illegal

f x = [ (`head` x, ())
      , (+ x, ())
      , ((), + x)
      , ((), + x, ())
      , ((), x +) ]

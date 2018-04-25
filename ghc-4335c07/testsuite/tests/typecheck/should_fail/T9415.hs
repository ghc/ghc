module T9415 where

class D a => C a where
  meth :: D a => ()
class C a => D a

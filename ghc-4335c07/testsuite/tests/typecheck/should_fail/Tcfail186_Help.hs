module Tcfail186_Help where

type PhantomSyn a  = Int

f = (\_ -> 2) :: PhantomSyn a -> Int

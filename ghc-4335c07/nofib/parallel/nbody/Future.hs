module Future (Eval(..), Future, runEval, rseq, fork, join, deep) where

import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies

data Future a = Future a

fork :: Eval a -> Eval (Future a)
fork a = do a' <- rpar (runEval a); return (Future a')

join :: Future a -> Eval a
join (Future a) = a `pseq` return a

deep :: NFData a => Eval a -> Eval a
deep m = do a <- m; rnf a `pseq` return a

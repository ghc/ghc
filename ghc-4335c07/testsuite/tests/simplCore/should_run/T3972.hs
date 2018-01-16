
module Main (main) where

import T3972A (Expr(E10), Token, spanning, getSpan)

import Control.Monad
import System.Exit
import System.IO

main :: IO ()
main = do h <- openFile "T3972.o" ReadMode
          s <- hFileSize h
          hClose h
          -- size is just under 8k on amd64/Linux in 6.13, but was
          -- around 3.5M in 6.12. Let's try >20k as the test for
          -- having regressed.
          when (s > 20000) $ do
              hPutStrLn stderr ("T3972.o is too big! " ++ show s)
              exitFailure

makeTupleOrExpr :: [Expr] -> Maybe Token -> Expr
makeTupleOrExpr [e] Nothing = e
makeTupleOrExpr es@(_:_) (Just t) = E10 (spanning es t)
makeTupleOrExpr es@(_:_) Nothing  = E10 (getSpan es)


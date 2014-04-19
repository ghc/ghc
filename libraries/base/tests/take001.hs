-- Test for bug #1219, F/B rule for take was too strict
import System.Environment
main = do
 (n:_) <- getArgs
 print (map (const 'x') (take (read n) (undefined:undefined)))

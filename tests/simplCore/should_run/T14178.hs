import System.Exit
import Control.Monad.Trans.State.Strict

eval :: Int -> State Int a -> a
eval p = fst . flip runState p

advance :: Int -> State Int ()
advance = modify' . (+)

loc :: State Int Int
loc = get

emit1 :: State Int ()
emit1 = advance 1

emitN :: Int -> State Int ()
-- adding in the 0 case, breaks with HEAD. 8.2.1 is fine with it.
-- emitN 0 = advance 0
emitN 0 = pure ()
emitN n = advance n

align8 :: State Int ()
align8 = do
  bits <- (`mod` 8) <$> loc
  emitN (8 - bits)

main :: IO ()
main = do
  let p = eval 0 (emit1 >> align8 >> loc)
  putStrLn $ show p
  if p == 8
    then putStrLn "OK" >> exitSuccess
    else putStrLn "FAIL" >> exitFailure

module Main (main) where
import GHC.Exts.Heap (GenClosure(..), getClosureData)
import T27005_aux (T(..), S(..), f)
import System.Exit

main :: IO ()
main = do
    let !t = MkT (MkS (pure ()) 0)
    dup <- f t
    c <- getClosureData dup
    case c of
      ThunkClosure{} -> putStrLn "FAIL" >> exitFailure
      APClosure{}    -> putStrLn "FAIL" >> exitFailure
      _              -> putStrLn "OK"

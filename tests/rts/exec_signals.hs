import System.Process
import System.Posix.Signals
import Control.Monad(when)

data SigState = Ignored | Default | Handled
    deriving (Eq, Read, Show)

data ChildInfo = ChildInfo {
        masked :: [(Int,Bool)],
        handlers :: [(Int, SigState)] }
    deriving (Read, Show)

main = do out <- readProcess "./exec_signals_child" [] ""
          let ci = read out :: ChildInfo
              blockedSigs = [x | (x, True) <- masked ci]
              ignoredSigs = [x | (x, Ignored) <- handlers ci]
          when (not $ null blockedSigs) $
            putStrLn ("signals " ++ show blockedSigs ++ " are blocked")
          when (not $ null ignoredSigs) $
            putStrLn ("signals " ++ show ignoredSigs ++ " are ignored")

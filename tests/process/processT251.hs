import Control.Exception
import GHC.IO.Exception
import System.Environment
import System.Exit
import System.Process

main :: IO ()
main = do
    args <- getArgs
    case args of
      []         -> parent
      ["child"]  -> child
      ["child2"] -> child2
      _          -> fail "unknown mode"

parent :: IO ()
parent = do
    putStrLn "parent start"
    (_, _, _, phdl) <- createProcess $ (proc "./processT251" ["child"]) { std_in = NoStream }
    ExitSuccess <- waitForProcess phdl
    putStrLn "parent done"

child :: IO ()
child = do
    putStrLn "child start"
    (_, _, _, phdl) <- createProcess $ (proc "./processT251" ["child2"]) { std_in = NoStream }
    ExitSuccess <- waitForProcess phdl
    putStrLn "child done"

child2 :: IO ()
child2 = do
    putStrLn "child2 start"
    -- Unfortunate, there isn't a reliable way to test that stdin has been closed.
    -- Afterall, if any file is opened in the child, it may reuse the
    -- supposedly-closed fd 0. In particular this tends to happen in the
    -- threaded RTS, since the event manager's control pipe is opened during
    -- RTS initialzation.
    putStrLn "child2 done"


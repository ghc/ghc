import Posix

-- This test is an example of where something more sophisticated than runstdtest
-- is required, as its interactive.

main = do
    installHandler keyboardSignal (Catch doCtrlC) Nothing
    ta  <- getTerminalAttributes stdInput
    case (controlChar ta Interrupt) of
      Nothing -> fixMe ta
      Just x  -> continue x

fixMe ta = do
    putStr "Oops...no interrupt character?\nI can fix that...\n"
    setTerminalAttributes stdInput (withCC ta (Interrupt, '\ETX')) Immediately
    ta   <- getTerminalAttributes stdInput
    case (controlChar ta Interrupt) of
      Nothing -> putStr "...Then again, maybe I can't\n"
      Just x -> continue x

continue x =
    putStr "Press '"
    putStr (ccStr x)
    putStr "'.\n"
    awaitSignal Nothing
    putStr "How did I get here?\n"

doCtrlC =
    putStr "Caught an interrupt.\n"

ccStr '\DEL' = "^?"
ccStr x 
  | x >= ' ' = [x]
  | otherwise = ['^', (toEnum (fromEnum x + fromEnum '@'))]

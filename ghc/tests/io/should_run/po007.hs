import Posix

main = 
    installHandler keyboardSignal (Catch doCtrlC) Nothing >>
    getTerminalAttributes stdInput >>= \ ta ->
    case (controlChar ta Interrupt) of
      Nothing -> fixMe ta
      Just x -> continue x

fixMe ta =
    putStr "Oops...no interrupt character?\nI can fix that...\n" >>
    setTerminalAttributes stdInput (withCC ta (Interrupt, '\ETX')) Immediately >>
    getTerminalAttributes stdInput >>= \ ta ->
    case (controlChar ta Interrupt) of
      Nothing -> putStr "...Then again, maybe I can't\n"
      Just x -> continue x

continue x =
    putStr "Press '" >>
    putStr (ccStr x) >>
    putStr "'.\n" >>
    awaitSignal Nothing >>
    putStr "How did I get here?\n"

doCtrlC =
    putStr "Caught an interrupt.\n"

ccStr '\DEL' = "^?"
ccStr x 
  | x >= ' ' = [x]
  | otherwise = ['^', (toEnum (fromEnum x + fromEnum '@'))]

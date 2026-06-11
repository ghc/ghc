import System.Exit
import System.IO

main = do
    print ((f id2) (10 + thirty_two))
    hPutStrLn stderr "deliberate breakage to test summary output"
    exitWith (ExitFailure 3)
  where
    f x = g x
      where
        g x = h x
          where
            h x = x

    thirty_two :: Int
    thirty_two = 32

id2 x = x

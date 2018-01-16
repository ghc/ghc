
import System.Posix.User

main :: IO ()
main = getUserEntryForName "thisIsNotMeantToExist" >> return ()

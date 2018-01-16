
import System.Posix.User

main :: IO ()
main = getGroupEntryForName "thisIsNotMeantToExist" >> return ()

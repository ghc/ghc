-- test that none of System.Posix.User.get* fail
import Control.Exception as Exception
import System.Posix.User

check :: Show a => a -> Bool
check a = show a == show a

p :: Show a => String -> IO a -> IO ()
p s m = (do putStr (s ++ ": ")
            c <- fmap check m
            putStrLn $ if c then "OK" else "I am the pope!")
        `Exception.catch` (\e -> putStrLn ("ERROR: " ++ show (e::SomeException)))

main :: IO ()
main = do p "getRealUserID"        $ getRealUserID
          p "getRealGroupID"       $ getRealGroupID
          p "getEffectiveUserID"   $ getEffectiveUserID
          p "getEffectiveGroupID"  $ getEffectiveGroupID
          p "getGroups"            $ getGroups
          --p "getLoginName"         $ getLoginName
          p "getEffectiveUserName" $ getEffectiveUserName
          p "getGroupEntryForID"   $ getRealGroupID >>= getGroupEntryForID
          p "getGroupEntryForName" $ getRealGroupID >>= getGroupEntryForID >>= getGroupEntryForName . groupName
          p "getAllGroupEntries"   $ getAllGroupEntries
          p "getUserEntryForID"    $ getRealUserID >>= getUserEntryForID
          --p "getUserEntryForName"  $ getLoginName >>= getUserEntryForName
          p "getAllUserEntries"    $ getAllUserEntries

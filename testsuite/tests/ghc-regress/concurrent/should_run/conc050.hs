{-
  $Id: conc050.hs,v 1.1 2005/03/21 13:59:36 simonmar Exp $

  Implements a simple directory service that handles
  insert and delete commands using STMs.
-}

module Main
where
import Control.Concurrent
import Control.Concurrent.STM
import System.Environment
import Control.Monad

type Key = Int 

type Value = Int

type DirectoryEntry = (Key, Value)

type DirectoryEntryList = [DirectoryEntry]

-- The service handles add and remove commands
data DirectoryCommand = DirectoryAdd Key Value | DirectoryRemove Key

type DirectoryChannel = TChan DirectoryCommand

type DirectoryTable = TVar DirectoryEntryList

type DirectoryCommandCount = TVar Int

-- The service's state
data DirectoryState = DirectoryState {
  chan :: DirectoryChannel, 
  table :: DirectoryTable, 
  count :: DirectoryCommandCount }

{-
  Return True if a DirectoryEntry's key equals
  the specified key.
-}
keyEquals :: Key -> DirectoryEntry -> Bool
keyEquals k e = (fst e) == k

{-
  Return True if a DirectoryEntry's key does not equal
  the specified key.
-}
keyNotEquals :: Key -> DirectoryEntry -> Bool
keyNotEquals k e = (fst e) /= k

{-
  Print a DirectoryEntryList to stdout.
-}
dumpDirectoryEntryList :: DirectoryEntryList -> IO ()
dumpDirectoryEntryList [] = return ()
dumpDirectoryEntryList (x:xs)
 = do putStrLn ((show (fst x)) ++ " " ++ (show (snd x)))
      dumpDirectoryEntryList xs

{-
  Print a DirectoryTable to stdout.
-}
dumpDirectoryTable :: DirectoryTable -> IO ()
dumpDirectoryTable t
 = do l <- atomically (do {l <- readTVar t; writeTVar t l; return l})
      putStrLn ("table length = " ++ (show (length l)))
      -- dumpDirectoryEntryList l

{-
  Add a DirectoryEntry to a DirectoryTable verifying
  that the key does not already exist in the table.
-}
addDirectoryTable :: DirectoryTable -> DirectoryEntry -> IO ()
addDirectoryTable t e@(key,value)
 = do atomically (do l <- readTVar t
                     if filter (keyEquals key) l == [] 
                       then writeTVar t (e:l)
                       else writeTVar t l)
      -- putStrLn ("added (" ++ (show (fst e)) ++ "," ++ (show (snd e)) ++ ")")

{-
  Insert a DirectoryCommand into a DirectoryChannel.
-}
postCommand :: DirectoryChannel -> DirectoryCommand -> IO ()
postCommand c cmd = atomically (writeTChan c cmd)

{-
  Remove a DirectoryEntry from a DirectoryTable.
-}
removeDirectoryTable :: DirectoryTable -> Key -> IO ()
removeDirectoryTable t k
 = atomically (do l <- readTVar t 
                  let newl = filter (keyNotEquals k) l
                  writeTVar t newl)
      -- putStrLn ("removed " ++ (show k))

{-
  Find a DirectoryEntry in a DirectoryTable.
-}
findDirectoryTable :: DirectoryTable -> Key -> IO DirectoryEntryList 
findDirectoryTable t k
 = do l <- atomically (do l <- readTVar t 
                          writeTVar t l 
                          return l)
      let fl = filter (keyEquals k) l
      return fl

{-
  Increment the DirectoryCommandCount.
-}
incDirectoryCommandCount :: DirectoryCommandCount -> IO ()
incDirectoryCommandCount cnt
 = atomically (do i <- readTVar cnt; writeTVar cnt (i+1))

{-
  Read the DirectoryCommandCount.
-}
readDirectoryCommandCount :: DirectoryCommandCount -> IO Int
readDirectoryCommandCount cnt
 = do i <- atomically (do i <- readTVar cnt
                          writeTVar cnt i
                          return i)
      return i

{-
  Process that constantly searches the DirectoryTable
  for a DirectoryKey of 1 and prints whether it found it.
-}
directoryFinder :: DirectoryState -> TVar Bool -> IO ()
directoryFinder state done
 = do cc <- readDirectoryCommandCount cnt
      l <- findDirectoryTable t 1
{-
      if l /= [] then 
         putStr "found"
       else 
         putStr "not found"
      putStrLn (" " ++ (show cc))
-}

      b <- atomically (readTVar done)
      if b then return ()
	   else directoryFinder state done
   where
     t = table state
     cnt = count state

{-
  Process that constantly prints the contents of
  the DirectoryTable.
-}
directoryDumper :: DirectoryState -> IO ()
directoryDumper state
 = do let t = table state
      dumpDirectoryTable t
      directoryDumper state

{-
  Process that reads commands from the DirectoryChannel
  and executes them.
-}
directoryListener :: Int -> DirectoryState -> TVar Bool -> IO ()
directoryListener 0 state done = atomically (writeTVar done True)
directoryListener n state done
 = do cmd <- atomically (do {cmd <- readTChan c; return cmd})
      case cmd of
        (DirectoryAdd k v) -> addDirectoryTable t (k,v)
        (DirectoryRemove k) -> removeDirectoryTable t k
      incDirectoryCommandCount cnt
      directoryListener (n-1) state done
   where
     c = chan state
     t = table state
     cnt = count state

{-
  Process that constantly posts DirectoryAdd
  commands to the DirectoryChannel.
-}
directoryPoster1 :: Int -> DirectoryState -> IO ()
directoryPoster1 0 state = return ()
directoryPoster1 n state
 = do let c = chan state
      postCommand c (DirectoryAdd 1 2)
      directoryPoster1 (n-1) state

{-
  Process that constantly posts DirectoryRemove
  commands to the DirectoryChannel.
-}
directoryPoster2 :: Int -> DirectoryState -> IO ()
directoryPoster2 0 state = return ()
directoryPoster2 n state
 = do let c = chan state
      postCommand c (DirectoryRemove 1)
      directoryPoster2 (n-1) state

{-
  The DirectoryService main process.
-}
directoryService
 = do [s] <- getArgs
      let n = read s :: Int

      c <- atomically (newTChan)
      t <- atomically (newTVar [])
      cnt <- atomically (newTVar 0)
      let state = DirectoryState c t cnt

      done <- atomically (newTVar False)
      forkIO (directoryListener (n*2) state done)
      forkIO (directoryPoster1 n state)
      forkIO (directoryPoster2 n state)
      directoryFinder state done
      -- directoryDumper state

main
  = directoryService

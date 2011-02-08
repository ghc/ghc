{-# LANGUAGE BangPatterns,CPP #-}
module MVarListLockCoupling where

import Control.Monad
import Data.IORef
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import Data.Time


-- #define USE_UNPACK
-- #define USE_STRICT

#if defined(USE_UNPACK)
#define UNPACK(p) {-# UNPACK #-} !(p)
#elif defined(USE_STRICT)
#define UNPACK(p) !(p)
#else
#define UNPACK(p) p
#endif


data List a = Node { val :: a
                   , next :: UNPACK(MVar (List a)) }
            | Null
            | Head { next :: UNPACK(MVar (List a)) } deriving Eq

data ListHandle a = ListHandle { headList :: IORef (MVar (List a)), 
                             tailList :: IORef (MVar (List a)) }


-- we assume a static head pointer, pointing to the first node which must be Head
-- the deleted field of Head is always False, it's only there to make some of the code
-- more uniform
-- tail points to the last node which must be Null

-- head is static, therefore IORef
-- tail will be adjusted, therefore MVar


type Iterator a = IORef (MVar (List a))

-- iterators are private

-------------------------------------------
-- auxilliary functions



while b cmd = if b then do {cmd; while b cmd}
              else return ()

repeatUntil cmd = do { b <- cmd; if b then return ()
                                  else repeatUntil cmd }

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORef ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

atomicWrite :: IORef a -> a -> IO ()
atomicWrite ptr x =
   atomicModifyIORef ptr (\ _ -> (x,()))


----------------------------------------------
-- functions operating on lists


-- we create a new list
newList :: IO (ListHandle a)
newList = 
   do null <- newMVar Null
      hd <- newMVar (Head {next = null })
      hdPtr <- newIORef hd
      tailPtr <- newIORef null
      return (ListHandle {headList = hdPtr, tailList = tailPtr})


-- we add a new node, by overwriting the null tail node
-- we only need to adjust tailList but not headList because
-- of the static Head
-- we return the location of the newly added node
addToTail :: Eq a => ListHandle a -> a -> IO ()
addToTail (ListHandle {tailList = tailPtrPtr}) x =
   do null <- newMVar Null
      tailPtr <- readIORef tailPtrPtr
      takeMVar tailPtr
      writeIORef tailPtrPtr null 
      putMVar tailPtr (Node {val = x, next = null})


find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle { headList = head }) x =
  let go prevPtr prevNode =
           do let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- takeMVar curPtr
              case curNode of
                Node {val = y, next = nextNode } ->
                   if (x == y)
                   then -- node found 
                      do putMVar prevPtr prevNode
                         putMVar curPtr curNode
                         return True
                   else 
                      do putMVar prevPtr prevNode
                         go curPtr curNode -- continue
                Null -> do putMVar prevPtr prevNode
                           putMVar curPtr curNode
                           return False -- reached end of list
  in do startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle { headList = head }) x =
  let go prevPtr prevNode =
        do do let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- takeMVar curPtr
              case curNode of
                Node {val = y, next = nextNode } ->
                   if (x == y)
                   then -- delink node
                     do case prevNode of
                          Node {} -> do putMVar prevPtr (Node {val = val prevNode, 
                                                               next = nextNode})
                                        putMVar curPtr curNode
                                        return True
                          Head {} -> do putMVar prevPtr (Head {next = nextNode})
                                        putMVar curPtr curNode
                                        return True
                   else do putMVar prevPtr prevNode                         
                           go curPtr curNode -- continue
                Null -> do putMVar curPtr curNode
                           putMVar prevPtr prevNode                         
                           return False -- reached end of list

  in do startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode



--printing and counting

printList :: Show a => ListHandle a -> IO ()
printList (ListHandle {headList = ptrPtr}) =
  do startptr <- (
          do ptr <- readIORef ptrPtr
             Head {next = startptr} <- readMVar ptr
             return startptr)
     printListHelp startptr


printListHelp :: Show a => MVar (List a) -> IO ()
printListHelp curNodePtr =
   do { curNode <- readMVar curNodePtr
      ; case curNode of
          Null -> putStr "Nil"
          Node {val = curval, next = curnext} ->
             do { putStr (show curval  ++ " -> ")
                ;  printListHelp curnext }
      } 

cntList :: Show a => ListHandle a -> IO Int
cntList (ListHandle {headList = ptrPtr}) =
  do startptr <- (
          do ptr <- readIORef ptrPtr
             Head {next = startptr} <- readMVar ptr
             return startptr)
     cntListHelp startptr 0


cntListHelp :: Show a => MVar (List a) -> Int -> IO Int
cntListHelp curNodePtr i =
   do { curNode <- readMVar curNodePtr
      ; case curNode of
          Null -> return i
          Node {val = curval, next = curnext} -> 
                cntListHelp curnext (i+1)
      } 

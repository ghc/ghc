
> module BackList2 where

> import Data.IORef
> import Control.Concurrent
> --import Control.Concurrent.STM

> import RefInterface

> data List r a = Node { val :: a, next :: (r (List r a)) }
>               | Head { next :: r (List r a) }
>               | DelNode (r (List r a))                  -- backpointer
>               | Null

> data ListHandle r a = ListHandle { headList :: r (r (List r a)),
>                            tailList :: r (r (List r a)) }


  abbreviations

> newAtomic x = atomicRef (newRef x)

> readAtomic x = atomicRef (readRef x)

> writeAtomic ptr x = atomicRef (writeRef ptr x)


> -- we create a new list
> newList :: Ref r m => IO (ListHandle r a)
> newList = 
>   do null <- newAtomic Null
>      hd <- newAtomic (Head {next = null })
>      hdPtr <- newAtomic hd
>      tailPtr <- newAtomic null
>      return (ListHandle {headList = hdPtr, tailList = tailPtr})



> find :: (Eq a, Ref r m) => ListHandle r a -> a -> IO Bool
> find (ListHandle {headList = ptrPtr})  i = 
>   do startPtr <- atomicRef (readRef ptrPtr)
>      find2 startPtr i


NOTE: I thought we can catch the 'head' case here,
      so we don't need to check for 'head' in findNode,
      but this leads to a non-exhaustive pattern failure

> {-
>  do startptr <- atomically (
>          do ptr <- readTVar ptrPtr
>             Head {next = startptr} <- readTVar ptr
>             return startptr)
>     find2 startptr i
> -}

> find2 :: (Eq a, Ref r m) => r (List r a) -> a -> IO Bool
> find2 curNodePtr i =  
>     do 
>     { comp <- atomicRef (findNode curNodePtr i)  
>     ; comp }


We atomically check for lhs and guards

f1 @ node(X,P,Next) \ find(X,P,R) <=> R=tt..
f2 @ nil(P) \ find(Y,P,R) <=> R=ff.
f3 @ node(X,P,Next) \ find(Y,P,R) <=> X /= Y | find(Y,Next,R).
f4 @ nodeDel(P,Q) \ find(X,P,R) <=> find(X,Q,R).



> findNode :: (Eq a, Ref r m) => r (List r a) -> a -> m (IO Bool)
> findNode curNodePtr x = do 
>   { curNode <- readRef curNodePtr
>   ; case curNode of
>       Node {val = y, next = q} ->
>         if (y == x) then return (return True) -- f1
>         else return (find2 q x) -- f3       
>       Null -> return (return False)    -- f2
>       Head p-> return (find2 p x)  --error "findNode: impossible"
>       DelNode p -> return (find2 p x)  -- f4
>  }


> -- we add a new node, by overwriting the null tail node
> -- we only need to adjust tailList but not headList because
> -- of the static Head
> -- we return the location of the newly added node
> addToTail :: Ref r m => ListHandle r a -> a -> IO (r (List r a))
> addToTail (ListHandle {tailList = tailPtrPtr}) x =
>   do tPtr <- atomicRef (
>                do null <- newRef Null
>                   tailPtr <- readRef tailPtrPtr
>                   writeRef tailPtr (Node {val = x, next = null})
>                   writeRef tailPtrPtr null
>                   return tailPtr
>              )
>      return tPtr


> delete :: (Eq a, Ref r m) => ListHandle r a -> a -> IO Bool
> delete (ListHandle {headList = ptrPtr})  i = 
>  do prevPtr <- atomicRef (readRef ptrPtr)
>     delete2 prevPtr i


> delete2 :: (Eq a, Ref r m) => r (List r a) -> a -> IO Bool
> delete2 prevPtr i = 
>     do 
>     { comp <- atomicRef (deleteNode prevPtr i)
>     ; comp }


> deleteNode :: (Eq a,Ref r m) => r (List r a) -> a -> m (IO Bool)
> deleteNode prevPtr x = 
>     do
>    { prevNode <- readRef prevPtr
>    ; case prevNode of
>        Null -> error "impossible case"
>        DelNode q -> return (delete2 q x) 
>        nodeOrhead ->
>         do let curPtr = next prevNode 
>            curNode <- readRef curPtr
>            case curNode of
>              Node {val = y, next= nextNode} ->
>               if (y /= x) 
>               then return (delete2 curPtr x) 
>               else -- (delink node)
>                 do writeRef curPtr (DelNode prevPtr)   -- add backpointer
>                    case prevNode of
>                      Head {} -> do writeRef prevPtr (Head {next = nextNode})
>                                    return (return True)
>                      Node {} -> do writeRef prevPtr
>                                       (Node {val = val prevNode, next = nextNode})
>                                    return (return True)
>              Null -> return (return False)
>              DelNode _ -> retryRef      
>                   -- means that the prevNode points to next of delNode
>                   -- we simply retry therefore
>      }


printing and counting

> printList :: (Show a, Ref r m) => ListHandle r a -> IO ()
> printList (ListHandle {headList = ptrPtr}) =
>  do startptr <- atomicRef (
>          do ptr <- readRef ptrPtr
>             Head {next = startptr} <- readRef ptr
>             return startptr)
>     printListHelp startptr


> printListHelp :: (Show a, Ref r m) => r (List r a) -> IO ()
> printListHelp curNodePtr =
>   do { curNode <- atomicRef (readRef curNodePtr)
>      ; case curNode of
>          Null -> putStr "Nil"
>          Node {val = curval, next = curnext} ->
>             do { putStr (show curval  ++ " -> ")
>                ;  printListHelp curnext }
>          DelNode curnext ->
>             printListHelp curnext 
>      } 

> cntList :: (Show a, Ref r m) => ListHandle r a -> IO Int
> cntList (ListHandle {headList = ptrPtr}) =
>  do startptr <- atomicRef (
>          do ptr <- readRef ptrPtr
>             Head {next = startptr} <- readRef ptr
>             return startptr)
>     cntListHelp startptr 0

> cntListHelp :: (Show a, Ref r m) => r (List r a) -> Int -> IO Int
> cntListHelp curNodePtr i =
>   do { curNode <- atomicRef (readRef curNodePtr)
>      ; case curNode of
>          Null -> return i
>          Node {val = curval, next = curnext} ->
>             cntListHelp curnext (i+1)
>          DelNode curnext ->
>             cntListHelp curnext i
>      }


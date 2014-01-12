> {-# LANGUAGE BangPatterns,CPP #-}
> module IOList where

-- #define USE_UNPACK
-- #define USE_STRICT

#if defined(USE_UNPACK)
#define UNPACK(p) {-# UNPACK #-} !(p)
#elif defined(USE_STRICT)
#define UNPACK(p) !(p)
#else
#define UNPACK(p) p
#endif

> import Data.IORef


> data List a = Node { val :: a, next :: UNPACK(IORef (List a))}   
>           | Null
>           | Head {next :: UNPACK(IORef (List a)) }

> data ListHandle a = ListHandle { headList :: UNPACK(IORef (IORef (List a))),
>                            tailList :: UNPACK(IORef (IORef (List a))) }

> atomically = \x -> x


> -- we create a new list
> newList :: IO (ListHandle a)
> newList = 
>   do null <- newIORef Null
>      hd <- newIORef (Head {next = null })
>      hdPtr <- newIORef hd
>      tailPtr <- newIORef null
>      return (ListHandle {headList = hdPtr, tailList = tailPtr})
  
> find ::  Eq a => ListHandle a -> a -> IO Bool
> find (ListHandle {headList = ptrPtr})  i =  do 
>  ptr <- readIORef ptrPtr
>  Head {next = startptr} <- readIORef ptr
>  find2 startptr i 
>   where 
>    find2 :: Eq a => IORef (List a) -> a -> IO Bool
>    find2 curNodePtr i = do
>    { curNode <- readIORef curNodePtr
>    ; case curNode of
>        Null -> return False  -- we've reached the end of the list
>                              -- element not found
>        Node {val = curval, next = curnext} ->
>          if (curval == i) then return True -- element found
>          else find2 curnext i              -- keep searching
>    }
 

> -- we add a new node, by overwriting the null tail node
> -- we only need to adjust tailList but not headList because
> -- of the static Head
> -- we return the location of the newly added node
> addToTail :: ListHandle a -> a -> IO (IORef (List a))
> addToTail (ListHandle {tailList = tailPtrPtr}) x =
>   do tPtr <- atomically (
>                do null <- newIORef Null
>                   tailPtr <- readIORef tailPtrPtr
>                   writeIORef tailPtr (Node {val = x, next = null})
>                   writeIORef tailPtrPtr null
>                   return tailPtr
>              )
>      return tPtr


> delete :: Eq a => ListHandle a -> a -> IO Bool
> delete (ListHandle {headList = ptrPtr})  i = 
>  atomically (
>          do startptr <- readIORef ptrPtr
>             delete2 startptr i)
>   where
>    delete2 :: Eq a => IORef (List a) -> a -> IO Bool
>    delete2 prevPtr i =
>     do
>    { prevNode <- readIORef prevPtr
>    ; let curNodePtr = next prevNode --  head/node have both next 
>    ; curNode <- readIORef curNodePtr
>    ; case curNode of
>        Null -> return False  -- we've reached the end of the list
>                              -- element not found
>        Node {val = curval, next = nextNode} ->
>          if (curval /= i) 
>           then delete2 curNodePtr i         -- keep searching
>           else 
>               -- delete element (ie delink node)
>               do { case prevNode of
>                      Head {} -> do writeIORef prevPtr (Head {next = nextNode})
>                                    return True
>                      Node {} -> do writeIORef prevPtr
>                                       (Node {val = val prevNode, next = nextNode})
>                                    return True
>                  }
>     }


printing and counting

> printList :: Show a => ListHandle a -> IO ()
> printList (ListHandle {headList = ptrPtr}) =
>   do startptr <- (
>          do ptr <- readIORef ptrPtr
>             Head {next = startptr} <- readIORef ptr
>             return startptr)
>      printListHelp startptr


> printListHelp :: Show a => IORef (List a) -> IO ()
> printListHelp curNodePtr =
>   do { curNode <- readIORef curNodePtr
>      ; case curNode of
>          Null -> putStr "Nil"
>          Node {val = curval, next = curnext} ->
>             do { putStr (show curval  ++ " -> ")
>                ;  printListHelp curnext }
>      } 

> cntList :: Show a => ListHandle a -> IO Int
> cntList (ListHandle {headList = ptrPtr}) =
>  do startptr <- (
>          do ptr <- readIORef ptrPtr
>             Head {next = startptr} <- readIORef ptr
>             return startptr)
>     cntListHelp startptr 0


> cntListHelp :: Show a => IORef (List a) -> Int -> IO Int
> cntListHelp curNodePtr i =
>   do { curNode <- readIORef curNodePtr
>      ; case curNode of
>          Null -> return i
>          Node {val = curval, next = curnext} -> 
>                cntListHelp curnext (i+1)
>      } 

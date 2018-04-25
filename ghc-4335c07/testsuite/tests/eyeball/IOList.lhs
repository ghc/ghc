> {-# LANGUAGE BangPatterns,CPP #-}
> module IOList (delete) where

Goal: we want all the IORef boxes to go away in the "delete" operation
below.  There are two versions of the code: one using the record
selector "next", the other using a hand-written record selector
"myNext" (see the use in delete).  Currently (6.10), neither version
gives good code, but for different reasons.  The record selector
version is not inlined, and the myNext version gives rise to a join
point that takes the reboxed IORef as an argument.

#define USE_UNPACK
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

> {-# INLINE [0] myNext #-}
> myNext :: List a -> IORef (List a)
> myNext Node{next=n} = n
> myNext Head{next=n} = n
> myNext Null = error "null"

> data ListHandle a = ListHandle { headList :: UNPACK(IORef (IORef (List a))),
>                            tailList :: UNPACK(IORef (IORef (List a))) }
  
> delete :: Eq a => ListHandle a -> a -> IO Bool
> delete (ListHandle {headList = ptrPtr})  i = 
>          do startptr <- readIORef ptrPtr
>             delete2 startptr i
>   where
>    delete2 :: Eq a => IORef (List a) -> a -> IO Bool
>    delete2 prevPtr i =
>     do
>    { prevNode <- readIORef prevPtr
>    ; let curNodePtr = next {- or: myNext -} prevNode --  head/node have both next 
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

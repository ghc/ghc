{-# LANGUAGE CPP, DataKinds, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-x-partial #-}
module NativeOpenInstalledReverseProducer
    ( worker, twin, wrapper, wrongTail, wrongHead, wrongAcc, wrongOwner
    , wrongWrapper, forceAcc, UserReverse
    ) where

{-# NOINLINE [2] worker #-}
worker :: [a] -> [a] -> [a]
worker [] acc = acc
worker (x : xs) acc = worker xs (x : acc)

{-# NOINLINE [2] twin #-}
twin :: [a] -> [a] -> [a]
twin [] acc = acc
twin (x : xs) acc = twin xs (x : acc)

wrapper :: [a] -> [a]
wrapper xs = worker xs []

wrongTail :: [a] -> [a] -> [a]
wrongTail [] acc = acc
wrongTail (x : xs) acc = wrongTail (x : xs) (x : acc)

wrongHead :: [a] -> [a] -> [a]
wrongHead [] acc = acc
wrongHead (_ : xs) acc = wrongHead xs (head acc : acc)

wrongAcc :: [a] -> [a] -> [a]
wrongAcc [] acc = acc
wrongAcc (x : xs) _ = wrongAcc xs [x]

wrongOwner :: [a] -> [a] -> [a]
wrongOwner [] acc = acc
wrongOwner (x : xs) acc = twin xs (x : acc)

wrongWrapper :: [a] -> [a]
wrongWrapper xs = worker xs xs

forceAcc :: [a] -> [a] -> [a]
forceAcc [] acc = acc
forceAcc (x : xs) acc = acc `seq` forceAcc xs (x : acc)

type family UserReverse (xs :: [a]) (acc :: [a]) :: [a] where
    UserReverse '[] acc = acc
    UserReverse (x ': xs) acc = UserReverse xs (x ': acc)

#ifdef CHANGE_WORKER
{-# RULES "drop reverse traversal" [~1] forall xs acc. worker xs acc = acc #-}
#endif
#ifdef SWAP_WORKER
{-# RULES "replace reverse wrapper worker" [~1] forall xs. worker xs [] = twin xs [] #-}
#endif

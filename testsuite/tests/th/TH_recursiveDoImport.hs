{-# LANGUAGE RecursiveDo #-}
module TH_recursiveDoImport where
import Data.IORef
import Language.Haskell.TH

data SelfRef = SelfRef (IORef (IORef SelfRef))

recIO :: ExpQ
recIO = [e|
    do rec r1 <- newIORef r2
           r2 <- newIORef (SelfRef r1)
       readIORef r2 |]

mdoIO :: ExpQ
mdoIO = [e|
    mdo r1 <- return r2
        r2 <- return (const 1 r1)
        return r1 |]

emptyRecIO :: ExpQ
emptyRecIO = [e|
   do rec {}
      return () |]

-- !!! triggered a temporary bug in freeStablePtr around 20020424

module Main where
import Foreign.StablePtr (newStablePtr, freeStablePtr)

data Foo = A | B | C | D

main :: IO ()
main = do aSPtr <- newStablePtr A
          bSPtr <- newStablePtr B
          cSPtr <- newStablePtr C
          cSPtr' <- newStablePtr C
          freeStablePtr aSPtr
          freeStablePtr bSPtr
          freeStablePtr cSPtr
          freeStablePtr cSPtr'
          aSPtr <- newStablePtr A
          bSPtr <- newStablePtr B
          cSPtr <- newStablePtr C
          dSPtr <- newStablePtr D
          print "Hello World"
      

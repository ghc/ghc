{-# LANGUAGE RecursiveDo #-}
import Data.IORef
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import TH_recursiveDoImport

main = testRec >> testMdo

testRec = do
    putStrLn $(lift . pprint =<< recIO)
    -- Test that we got the expected structure.
    SelfRef r1 <- $(recIO)
    r2 <- readIORef r1
    SelfRef r1' <- readIORef r2
    print (r1 == r1')

testMdo =
    putStrLn $(lift . pprint =<< mdoIO)

{-# LANGUAGE CPP #-}

#include "MachDeps.h"


import StrictPrim
import Type
import Natural


main :: IO ()
main = do
    let (a, b) = (1234, 2345)
        (na, nb) = (mkSingletonNat a, mkSingletonNat b)
        nc = timesNatural na nb

    print $ fromNatural na
    print $ fromNatural nb
    print $ fromNatural nc
    checkEtaCount


checkEtaCount :: IO ()
checkEtaCount = do
    text <- readFile "Natural.dump-simpl"
    let etaCount = length . filter (== "eta") $ words text
    if etaCount > 0
        then error $ "Error : Eta count (" ++ show etaCount ++ ") should 0."
        else putStrLn "Test passed!"


mkSingletonNat :: Word -> Natural
mkSingletonNat x = runStrictPrim mkNat
  where
    mkNat :: StrictPrim s Natural
    mkNat = do
        marr <- newWordArray 1
        writeWordArray marr 0 x
        narr <- unsafeFreezeWordArray marr
        return $ Natural 1 narr


fromNatural :: Natural -> Word
fromNatural (Natural _ arr) = indexWordArray arr 0

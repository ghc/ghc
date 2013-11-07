{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Integer.GMP.Internals (Integer(..), gcdInt, gcdInteger, gcdExtInteger, lcmInteger, powInteger, powModInteger, powModSecInteger, recipModInteger, nextPrimeInteger, testPrimeInteger, sizeInBaseInteger, importIntegerFromByteArray, importIntegerFromAddr, exportIntegerToMutableByteArray, exportIntegerToAddr)
    where

import GHC.Integer.Type


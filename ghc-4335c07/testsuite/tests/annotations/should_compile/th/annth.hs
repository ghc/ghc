{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import AnnHelper
import TestModule
import TestModuleTH
import System.IO

main = do
  $(do
    anns <- traverseModuleAnnotations
    runIO $ print (anns :: [String])
    anns <- reifyAnnotations (AnnLookupName 'testValue)
    runIO $ print (anns :: [String])
    anns <- reifyAnnotations (AnnLookupName 'testValueTH)
    runIO $ print (anns :: [String])
    anns <- reifyAnnotations (AnnLookupName ''TestType)
    runIO $ print (anns :: [String])
    anns <- reifyAnnotations (AnnLookupName ''TestTypeTH)
    runIO $ print (anns :: [String])
    anns <- reifyAnnotations (AnnLookupName 'TestType)
    runIO $ print (anns :: [String])
    anns <- reifyAnnotations (AnnLookupName 'TestTypeTH)
    runIO $ print (anns :: [String])
    [| return () |] )
  hFlush stdout

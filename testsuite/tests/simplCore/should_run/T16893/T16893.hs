{-# LANGUAGE DataKinds #-}

module Main (main) where

import Complex

badComplex :: Complex 'OpenType
badComplex =
  Complex
    { complexInner =
        OpenComplex
          { openComplexSource = undefined
          }
    }

segFaultTrigger :: IO ()
segFaultTrigger =
  closeComplex badComplex `seq` pure ()

main :: IO ()
main = segFaultTrigger

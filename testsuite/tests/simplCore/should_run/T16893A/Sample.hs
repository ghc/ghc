{-# LANGUAGE DataKinds #-}

module Sample where

import Complex


badComplex :: Complex 'OpenType
badComplex =
  Complex
    { complexAddress = zeroAddress
    , complexInner =
        OpenComplex
          { openComplexSource = zeroHash
          , openComplexNumber = 0
          }
    }

segFaultTrigger :: IO ()
segFaultTrigger =
  closeComplex badComplex `seq` pure ()


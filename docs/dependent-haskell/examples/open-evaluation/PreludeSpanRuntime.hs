{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeApplications #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeSpanRuntime (showSpan) where

import Data.Proxy (Proxy(..))
import PreludeFilterAppendRuntime (decisionsForNot)
import PreludeSpanProof (rejoinSpan, spanAcceptsHead, spanAcceptsSuffix)

-- | Show the projection and reassembly proofs and observe both lists.
showSpan :: foreach (xs :: [Bool]) -> String
showSpan xs =
    show ( spanAcceptsHead (Proxy @'[]), spanAcceptsSuffix (Proxy @'[])
         , rejoinSpan (decisionsForNot xs)
         , take 1 prefix, take 1 suffix, take 2 (prefix ++ suffix)
         )
  where
    (prefix, suffix) = span not xs

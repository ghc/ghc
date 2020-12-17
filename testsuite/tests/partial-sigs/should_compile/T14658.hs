{-# LANGUAGE PartialTypeSignatures, ImplicitParams #-}

module T14658 where

import GHC.Stack

-- Test ensures that the final 'f' has a CallStack constraint
-- as specified in the partial type signature
foo :: (?loc :: CallStack, _) => a -> a -> Bool
foo x y = x==y

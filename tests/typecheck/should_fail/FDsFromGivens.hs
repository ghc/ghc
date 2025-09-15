{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, GADTs #-}

module FDsFromGivens where

class C a b | a -> b where
   cop :: a -> b -> ()

{- Failing, as it righteously should! It's inaccessible code -}
-- But (c.f. test T5236) we no longer reject this in the
-- typechecker (see #12466)
-- Instead we get a redundant pattern-match warning,
-- in the post-typechecking pattern-match checks
g1 :: (C Char [a], C Char Bool) => a -> ()
g1 x = ()


{-# LANGUAGE ExplicitForAll #-}
module TH_ExplicitForAllRules_a (decls, hsToTh) where

import Language.Haskell.TH

decls = [d| {-# RULES "example" forall a. forall (x :: a). id x = x #-} |]

hsToTh = do
  decls' <- runQ decls
  mapM (print . ppr) decls'

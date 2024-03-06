{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module T16293b where

import Control.Monad
import GHC.Exts
import Language.Haskell.TH

f :: ()
f = $(do TyConI (DataD _ _ targs _ _ _) <- reify ''Proxy#
         let arity = length targs
         unless (arity == 1) $
           fail $ "Unexpected arity for Proxy#: " ++ show arity
         [| () |])

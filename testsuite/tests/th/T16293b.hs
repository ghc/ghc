{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module T16293b where

import Control.Monad
import GHC.Exts
import Language.Haskell.TH

f :: ()
f = $(do PrimTyConI _ arity _ <- reify ''Proxy#
         unless (arity == 1) $
           fail $ "Unexpected arity for Proxy#: " ++ show arity
         [| () |])

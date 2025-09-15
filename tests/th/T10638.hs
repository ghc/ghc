{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, QuasiQuotes, MagicHash #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import GHC.Exts

{-
   the prim and javascript calling conventions do not support
   headers and the static keyword.
-}

-- check that quasiquoting roundtrips successfully and that the declaration
-- does not include the static keyword
test1 :: String
test1 = $(do (ds@[ForeignD (ImportF _ _ p _ _)]) <-
               [d| foreign import prim "test1" cmm_test1 :: Int# -> Int# |]
             addTopDecls ds
             case p of
              "test1" -> return (LitE . stringL $ p)
              _       -> error $ "unexpected value: " ++ show p
         )

-- check that constructed prim imports with the static keyword are rejected
test2 :: String
test2 = $(do t <- [t| Int# -> Int# |]
             cmm_test2 <- newName "cmm_test2"
             addTopDecls
               [ForeignD (ImportF Prim Safe "static test2" cmm_test2 t)]
             [| test1 |]
         )

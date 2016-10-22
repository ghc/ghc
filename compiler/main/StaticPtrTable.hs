-- | Code generation for the Static Pointer Table
--
-- (c) 2014 I/O Tweag
--
-- Each module that uses 'static' keyword declares an initialization function of
-- the form hs_spt_init_<module>() which is emitted into the _stub.c file and
-- annotated with __attribute__((constructor)) so that it gets executed at
-- startup time.
--
-- The function's purpose is to call hs_spt_insert to insert the static
-- pointers of this module in the hashtable of the RTS, and it looks something
-- like this:
--
-- > static void hs_hpc_init_Main(void) __attribute__((constructor));
-- > static void hs_hpc_init_Main(void) {
-- >
-- >   static StgWord64 k0[2] = {16252233372134256ULL,7370534374096082ULL};
-- >   extern StgPtr Main_r2wb_closure;
-- >   hs_spt_insert(k0, &Main_r2wb_closure);
-- >
-- >   static StgWord64 k1[2] = {12545634534567898ULL,5409674567544151ULL};
-- >   extern StgPtr Main_r2wc_closure;
-- >   hs_spt_insert(k1, &Main_r2wc_closure);
-- >
-- > }
--
-- where the constants are fingerprints produced from the static forms.
--
-- The linker must find the definitions matching the @extern StgPtr <name>@
-- declarations. For this to work, the identifiers of static pointers need to be
-- exported. This is done in TidyPgm.chooseExternalIds.
--
-- There is also a finalization function for the time when the module is
-- unloaded.
--
-- > static void hs_hpc_fini_Main(void) __attribute__((destructor));
-- > static void hs_hpc_fini_Main(void) {
-- >
-- >   static StgWord64 k0[2] = {16252233372134256ULL,7370534374096082ULL};
-- >   hs_spt_remove(k0);
-- >
-- >   static StgWord64 k1[2] = {12545634534567898ULL,5409674567544151ULL};
-- >   hs_spt_remove(k1);
-- >
-- > }
--

{-# LANGUAGE ViewPatterns #-}
module StaticPtrTable (sptModuleInitCode) where

-- See SimplCore Note [Grand plan for static forms]

import CLabel
import CoreSyn
import DataCon
import Id
import Literal
import Module
import Outputable
import PrelNames

import Data.Maybe
import GHC.Fingerprint

-- | @sptModuleInitCode module binds@ is a C stub to insert the static entries
-- found in @binds@ of @module@ into the static pointer table.
--
-- A bind is considered a static entry if it is an application of the
-- data constructor @StaticPtr@.
--
sptModuleInitCode :: Module -> CoreProgram -> SDoc
sptModuleInitCode this_mod binds =
    sptInitCode $ catMaybes
                $ map (\(b, e) -> ((,) b) <$> staticPtrFp e)
                $ flattenBinds binds
  where
    staticPtrFp :: CoreExpr -> Maybe Fingerprint
    staticPtrFp (collectTyBinders -> (_, e))
      | (Var v, _ : Lit lit0 : Lit lit1 : _) <- collectArgs e
      , Just con <- isDataConId_maybe v
      , dataConName con == staticPtrDataConName
      , Just w0 <- fromPlatformWord64Rep lit0
      , Just w1 <- fromPlatformWord64Rep lit1
      = Just $ Fingerprint (fromInteger w0) (fromInteger w1)
      | ConApp dc (_ : Lit lit0 : Lit lit1 : _) <- e
      , dataConName dc == staticPtrDataConName
      , Just w0 <- fromPlatformWord64Rep lit0
      , Just w1 <- fromPlatformWord64Rep lit1
      = Just $ Fingerprint (fromInteger w0) (fromInteger w1)
    staticPtrFp _ = Nothing

    fromPlatformWord64Rep (MachWord w)   = Just w
    fromPlatformWord64Rep (MachWord64 w) = Just w
    fromPlatformWord64Rep _              = Nothing

    sptInitCode :: [(Id, Fingerprint)] -> SDoc
    sptInitCode [] = Outputable.empty
    sptInitCode entries = vcat
      [ text "static void hs_spt_init_" <> ppr this_mod
             <> text "(void) __attribute__((constructor));"
      , text "static void hs_spt_init_" <> ppr this_mod <> text "(void)"
      , braces $ vcat $
          [  text "static StgWord64 k" <> int i <> text "[2] = "
             <> pprFingerprint fp <> semi
          $$ text "extern StgPtr "
             <> (ppr $ mkClosureLabel (idName n) (idCafInfo n)) <> semi
          $$ text "hs_spt_insert" <> parens
               (hcat $ punctuate comma
                  [ char 'k' <> int i
                  , char '&' <> ppr (mkClosureLabel (idName n) (idCafInfo n))
                  ]
               )
          <> semi
          |  (i, (n, fp)) <- zip [0..] entries
          ]
      , text "static void hs_spt_fini_" <> ppr this_mod
             <> text "(void) __attribute__((destructor));"
      , text "static void hs_spt_fini_" <> ppr this_mod <> text "(void)"
      , braces $ vcat $
          [  text "StgWord64 k" <> int i <> text "[2] = "
             <> pprFingerprint fp <> semi
          $$ text "hs_spt_remove" <> parens (char 'k' <> int i) <> semi
          | (i, (_, fp)) <- zip [0..] entries
          ]
      ]

    pprFingerprint :: Fingerprint -> SDoc
    pprFingerprint (Fingerprint w1 w2) =
      braces $ hcat $ punctuate comma
                 [ integer (fromIntegral w1) <> text "ULL"
                 , integer (fromIntegral w2) <> text "ULL"
                 ]

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
-- >   extern StgPtr Main_sptEntryZC0_closure;
-- >   hs_spt_insert(k0, &Main_sptEntryZC0_closure);
-- >
-- >   static StgWord64 k1[2] = {12545634534567898ULL,5409674567544151ULL};
-- >   extern StgPtr Main_sptEntryZC1_closure;
-- >   hs_spt_insert(k1, &Main_sptEntryZC1_closure);
-- >
-- > }
--
-- where the constants are fingerprints produced from the static forms.
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
module StaticPtrTable (sptInitCode) where

import CoreSyn
import Module
import Outputable
import Id
import CLabel
import GHC.Fingerprint


-- | @sptInitCode module statics@ is a C stub to insert the static entries
-- @statics@ of @module@ into the static pointer table.
--
-- Each entry contains the fingerprint used to locate the entry and the
-- top-level binding for the entry.
--
sptInitCode :: Module -> [(Fingerprint, (Id,CoreExpr))] -> SDoc
sptInitCode _ [] = Outputable.empty
sptInitCode this_mod entries = vcat
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
        |  (i, (fp, (n, _))) <- zip [0..] entries
        ]
    , text "static void hs_spt_fini_" <> ppr this_mod
           <> text "(void) __attribute__((destructor));"
    , text "static void hs_spt_fini_" <> ppr this_mod <> text "(void)"
    , braces $ vcat $
        [  text "StgWord64 k" <> int i <> text "[2] = "
           <> pprFingerprint fp <> semi
        $$ text "hs_spt_remove" <> parens (char 'k' <> int i) <> semi
        | (i, (fp, _)) <- zip [0..] entries
        ]
    ]

  where

    pprFingerprint :: Fingerprint -> SDoc
    pprFingerprint (Fingerprint w1 w2) =
      braces $ hcat $ punctuate comma
                 [ integer (fromIntegral w1) <> text "ULL"
                 , integer (fromIntegral w2) <> text "ULL"
                 ]

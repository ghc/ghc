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
module StaticPtrTable (sptCreateStaticBinds) where

-- See SimplCore Note [Grand plan for static forms] for an overview.

import CLabel
import CoreSyn
import CoreUtils (collectMakeStaticArgs)
import DataCon
import DynFlags
import HscTypes
import Id
import Module
import Name
import Outputable
import Platform
import PrelNames
import Type

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import GHC.Fingerprint

-- | Replaces all bindings of the form
--
-- > b = /\ ... -> makeStatic info value
--
--  with
--
-- > b = /\ ... -> StaticPtr key info value
--
--  where a distinct key is generated for each binding.
--
-- It also yields the C stub that inserts these bindings into the static
-- pointer table.
sptCreateStaticBinds :: HscEnv -> Module -> CoreProgram
                     -> IO (SDoc, CoreProgram)
sptCreateStaticBinds hsc_env this_mod binds = do
    (fps, binds') <- evalStateT (go [] [] binds) 0
    return (sptModuleInitCode this_mod fps, binds')
  where
    go fps bs xs = case xs of
      []        -> return (reverse fps, reverse bs)
      bnd : xs' -> do
        (fps', bnd') <- replaceStaticBind bnd
        go (reverse fps' ++ fps) (bnd' : bs) xs'

    -- Generates keys and replaces 'makeStatic' with 'StaticPtr'.
    --
    -- The 'Int' state is used to produce a different key for each binding.
    replaceStaticBind :: CoreBind
                      -> StateT Int IO ([(Id, Fingerprint)], CoreBind)
    replaceStaticBind (NonRec b e) = do (mfp, (b', e')) <- replaceStatic b e
                                        return (maybeToList mfp, NonRec b' e')
    replaceStaticBind (Rec rbs) = do
      (mfps, rbs') <- unzip <$> mapM (uncurry replaceStatic) rbs
      return (catMaybes mfps, Rec rbs')

    replaceStatic :: Id -> CoreExpr
                  -> StateT Int IO (Maybe (Id, Fingerprint), (Id, CoreExpr))
    replaceStatic b e@(collectTyBinders -> (tvs, e0)) =
      case collectMakeStaticArgs e0 of
        Nothing      -> return (Nothing, (b, e))
        Just (_, t, info, arg) -> do
          (fp, e') <- mkStaticBind t info arg
          return (Just (b, fp), (b, foldr Lam e' tvs))

    mkStaticBind :: Type -> CoreExpr -> CoreExpr
                 -> StateT Int IO (Fingerprint, CoreExpr)
    mkStaticBind t info e = do
      i <- get
      put (i + 1)
      let fp@(Fingerprint w0 w1) = mkStaticPtrFingerprint i
          dflags = hsc_dflags hsc_env

      staticPtrDataCon <- lift $ lookupDataCon staticPtrDataConName
      return (fp, mkConApp staticPtrDataCon
                               [ Type t
                               , mkWord64LitWordRep dflags w0
                               , mkWord64LitWordRep dflags w1
                               , info
                               , e ])

    mkStaticPtrFingerprint :: Int -> Fingerprint
    mkStaticPtrFingerprint n = fingerprintString $ intercalate ":"
        [ unitIdString $ moduleUnitId this_mod
        , moduleNameString $ moduleName this_mod
        , show n
        ]

    -- Choose either 'Word64#' or 'Word#' to represent the arguments of the
    -- 'Fingerprint' data constructor.
    mkWord64LitWordRep dflags
      | platformWordSize (targetPlatform dflags) < 8 = mkWord64LitWord64
      | otherwise = mkWordLit dflags . toInteger

    lookupDataCon :: Name -> IO DataCon
    lookupDataCon n = lookupTypeHscEnv hsc_env n >>=
                        maybe (getError n) (return . tyThingDataCon)

    getError n = pprPanic "sptCreateStaticBinds.get: not found" $
      text "Couldn't find" <+> ppr n

-- | @sptModuleInitCode module fps@ is a C stub to insert the static entries
-- of @module@ into the static pointer table.
--
-- @fps@ is a list associating each binding corresponding to a static entry with
-- its fingerprint.
sptModuleInitCode :: Module -> [(Id, Fingerprint)] -> SDoc
sptModuleInitCode _ [] = Outputable.empty
sptModuleInitCode this_mod entries = vcat
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
  where
    pprFingerprint :: Fingerprint -> SDoc
    pprFingerprint (Fingerprint w1 w2) =
      braces $ hcat $ punctuate comma
                 [ integer (fromIntegral w1) <> text "ULL"
                 , integer (fromIntegral w2) <> text "ULL"
                 ]

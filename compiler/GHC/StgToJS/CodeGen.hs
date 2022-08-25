{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

-- | JavaScript code generator
module GHC.StgToJS.CodeGen
  ( stgToJS
  )
where

import GHC.Prelude

import GHC.Driver.Flags (DumpFlag (Opt_D_dump_js))

import GHC.JS.Ppr
import GHC.JS.Syntax
import GHC.JS.Make
import GHC.JS.Transform

import GHC.StgToJS.Arg
import GHC.StgToJS.Sinker
import GHC.StgToJS.Types
import qualified GHC.StgToJS.Object as Object
import GHC.StgToJS.StgUtils
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Deps
import GHC.StgToJS.Expr
import GHC.StgToJS.ExprCtx
import GHC.StgToJS.Monad
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.StaticPtr
import GHC.StgToJS.UnitUtils
import GHC.StgToJS.Stack
import GHC.StgToJS.Ids

import GHC.Stg.Syntax
import GHC.Core.DataCon
import GHC.Core.TyCo.Rep (scaledThing)

import GHC.Unit.Module
import GHC.Linker.Types (SptEntry (..))

import GHC.Types.CostCentre
import GHC.Types.ForeignStubs (ForeignStubs (..), getCHeader, getCStub)
import GHC.Types.RepType
import GHC.Types.Id
import GHC.Types.Unique
import GHC.Types.TyThing

import GHC.Data.FastString
import GHC.Utils.Encoding
import GHC.Utils.Logger
import GHC.Utils.Panic
import GHC.Utils.Misc
import qualified Control.Monad.Trans.State.Strict as State
import GHC.Utils.Outputable hiding ((<>))

import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Class
import Data.Bifunctor

-- | Code generator for JavaScript
stgToJS
  :: Logger
  -> StgToJSConfig
  -> [CgStgTopBinding]
  -> Module
  -> [SptEntry]
  -> ForeignStubs
  -> CollectedCCs
  -> FilePath -- ^ Output file name
  -> IO ()
stgToJS logger config stg_binds0 this_mod spt_entries foreign_stubs cccs output_fn = do

  let (unfloated_binds, stg_binds) = sinkPgm this_mod stg_binds0
    -- TODO: avoid top level lifting in core-2-core when the JS backend is
    -- enabled instead of undoing it here

    -- TODO: add dump pass for optimized STG ast for JS

  obj <- runG config this_mod unfloated_binds $ do
        ifProfilingM $ initCostCentres cccs
        (sym_table, lus) <- genUnits this_mod stg_binds spt_entries foreign_stubs

        -- (exported symbol names, javascript statements) for each linkable unit
        p <- forM lus \u -> do
           ts <- mapM (fmap (\(TxtI i) -> i) . identForId) (luIdExports u)
           return (ts ++ luOtherExports u, luStat u)

        deps <- genDependencyData this_mod lus
        lift $ Object.object' (moduleName this_mod) sym_table deps (map (second BL.fromStrict) p)

  -- Doc to dump when -ddump-js is enabled
  let mod_name = renderWithContext defaultSDocContext (ppr this_mod)
  o <- Object.readObject mod_name obj
  putDumpFileMaybe logger Opt_D_dump_js "JavaScript code" FormatJS
    $ vcat (fmap (docToSDoc . jsToDoc . Object.oiStat) o)

  BL.writeFile output_fn obj



-- | Generate the ingredients for the linkable units for this module
genUnits :: HasDebugCallStack
         => Module
         -> [CgStgTopBinding]
         -> [SptEntry]
         -> ForeignStubs
         -> G (Object.SymbolTable, [LinkableUnit]) -- ^ the final symbol table and the linkable units
genUnits m ss spt_entries foreign_stubs
                                 = generateGlobalBlock =<<
                                   generateExportsBlock =<<
                                   go 2 Object.emptySymbolTable ss
    where
      go :: HasDebugCallStack
         => Int                 -- the block we're generating (block 0 is the global unit for the module)
         -> Object.SymbolTable  -- the shared symbol table
         -> [CgStgTopBinding]
         -> G (Object.SymbolTable, [LinkableUnit])
      go !n st (x:xs) = do
        (st', mlu) <- generateBlock st x n
        (st'', lus)  <- go (n+1) st' xs
        return (st'', maybe lus (:lus) mlu)
      go _ st []     = return (st, [])

      --   Generate the global unit that all other blocks in the module depend on
      --   used for cost centres and static initializers
      --   the global unit has no dependencies, exports the moduleGlobalSymbol
      generateGlobalBlock :: HasDebugCallStack
                          => (Object.SymbolTable, [LinkableUnit])
                          -> G (Object.SymbolTable, [LinkableUnit])
      generateGlobalBlock (st, lus) = do
        glbl <- State.gets gsGlobal
        staticInit <-
          initStaticPtrs spt_entries
        (st', _, bs) <- serializeLinkableUnit m st [] [] []
                         ( -- O.optimize .
                           jsSaturate (Just $ modulePrefix m 1)
                         $ mconcat (reverse glbl) <> staticInit) "" [] []
        return ( st'
               , LinkableUnit bs
                              []
                              [moduleGlobalSymbol m]
                              []
                              []
                              []
                              False
                              []
                 : lus
               )

      generateExportsBlock :: HasDebugCallStack
                          => (Object.SymbolTable, [LinkableUnit])
                          -> G (Object.SymbolTable, [LinkableUnit])
      generateExportsBlock (st, lus) = do
        let (f_hdr, f_c) = case foreign_stubs of
                                  NoStubs            -> (empty, empty)
                                  ForeignStubs hdr c -> (getCHeader hdr, getCStub c)
            unique_deps = map mkUniqueDep (lines $ renderWithContext defaultSDocContext f_hdr)
            mkUniqueDep (tag:xs) = mkUnique tag (read xs)
            mkUniqueDep []       = panic "mkUniqueDep"

        (st', _, bs) <- serializeLinkableUnit m
                                              st
                                              []
                                              []
                                              []
                                              mempty
                                              (mkFastString $ renderWithContext defaultSDocContext f_c)
                                              []
                                              []
        return ( st'
               , LinkableUnit bs
                              []
                              [moduleExportsSymbol m]
                              [] -- id deps
                              unique_deps -- pseudo id deps
                              []
                              True
                              []
                 : lus
               )

      --   Generate the linkable unit for one binding or group of
      --   mutually recursive bindings
      generateBlock :: HasDebugCallStack
                    => Object.SymbolTable
                    -> CgStgTopBinding
                    -> Int
                    -> G (Object.SymbolTable, Maybe LinkableUnit)
      generateBlock st (StgTopStringLit bnd str) n = do
        bids <- identsForId bnd
        case bids of
          [(TxtI b1t),(TxtI b2t)] -> do
            -- [e1,e2] <- genLit (MachStr str)
            emitStatic b1t (StaticUnboxed (StaticUnboxedString str)) Nothing
            emitStatic b2t (StaticUnboxed (StaticUnboxedStringOffset str)) Nothing
            _extraTl   <- State.gets (ggsToplevelStats . gsGroup)
            si        <- State.gets (ggsStatic . gsGroup)
            let stat = mempty -- mconcat (reverse extraTl) <> b1 ||= e1 <> b2 ||= e2
            (st', _ss, bs) <- serializeLinkableUnit m st [bnd] [] si
                              (jsSaturate (Just $ modulePrefix m n) stat) "" [] []
            pure (st', Just $ LinkableUnit bs [bnd] [] [] [] [] False [])
          _ -> panic "generateBlock: invalid size"
      generateBlock st (StgTopLifted decl) n = do
        tl        <- genToplevel decl
        extraTl   <- State.gets (ggsToplevelStats . gsGroup)
        ci        <- State.gets (ggsClosureInfo . gsGroup)
        si        <- State.gets (ggsStatic . gsGroup)
        unf       <- State.gets gsUnfloated
        extraDeps <- State.gets (ggsExtraDeps . gsGroup)
        fRefs     <- State.gets (ggsForeignRefs . gsGroup)
        resetGroup
        let allDeps  = collectIds unf decl
            topDeps  = collectTopIds decl
            required = hasExport decl
            stat     = -- Opt.optimize .
                       jsSaturate (Just $ modulePrefix m n)
                     $ mconcat (reverse extraTl) <> tl
        (st', _ss, bs) <- serializeLinkableUnit m st topDeps ci si stat mempty [] fRefs
        return $! seqList topDeps `seq` seqList allDeps `seq` st' `seq`
          (st', Just $ LinkableUnit bs topDeps [] allDeps [] (S.toList extraDeps) required fRefs)

-- | serialize the payload of a linkable unit in the object file, adding strings
-- to the SymbolTable where necessary
serializeLinkableUnit :: HasDebugCallStack
                      => Module
                      -> Object.SymbolTable  -- symbol table to start with
                      -> [Id]                -- id's exported by unit
                      -> [ClosureInfo]
                      -> [StaticInfo]
                      -> JStat               -- generated code for the unit
                      -> FastString
                      -> [Object.ExpFun]
                      -> [ForeignJSRef]
                      -> G (Object.SymbolTable, [FastString], BS.ByteString)
serializeLinkableUnit _m st i ci si stat rawStat fe fi = do
  !i' <- mapM idStr i
  !(!st', !o) <- lift $ Object.serializeStat st ci si stat rawStat fe fi
  return (st', i', o) -- deepseq results?
    where
      idStr i = itxt <$> identForId i

-- | variable prefix for the nth block in module
modulePrefix :: Module -> Int -> FastString
modulePrefix m n =
  let encMod = zEncodeString . moduleNameString . moduleName $ m
  in  mkFastString $ "h$" ++ encMod ++ "_id_" ++ show n

genToplevel :: CgStgBinding -> G JStat
genToplevel (StgNonRec bndr rhs) = genToplevelDecl bndr rhs
genToplevel (StgRec bs)          =
  mconcat <$> mapM (\(bndr, rhs) -> genToplevelDecl bndr rhs) bs

genToplevelDecl :: Id -> CgStgRhs -> G JStat
genToplevelDecl i rhs = do
  s1 <- resetSlots (genToplevelConEntry i rhs)
  s2 <- resetSlots (genToplevelRhs i rhs)
  return (s1 <> s2)

genToplevelConEntry :: Id -> CgStgRhs -> G JStat
genToplevelConEntry i rhs = case rhs of
  StgRhsCon _cc con _mu _ts _args
    | i `elem` [ i' | AnId i' <- dataConImplicitTyThings con ]
    -> genSetConInfo i con (stgRhsLive rhs) -- NoSRT
  StgRhsClosure _ _cc _upd_flag _args body
    | StgConApp dc _n _cargs _tys <- removeTick body
    , i `elem` [ i' | AnId i' <- dataConImplicitTyThings dc ]
    -> genSetConInfo i dc (stgRhsLive rhs) -- srt
  _ -> pure mempty

genSetConInfo :: HasDebugCallStack => Id -> DataCon -> LiveVars -> G JStat
genSetConInfo i d l {- srt -} = do
  ei@(TxtI eii) <- identForDataConEntryId i
  sr <- genStaticRefs l
  emitClosureInfo $ ClosureInfo eii
                                (CIRegs 0 [PtrV])
                                (mkFastString $ renderWithContext defaultSDocContext (ppr d))
                                (fixedLayout $ map uTypeVt fields)
                                (CICon $ dataConTag d)
                                sr
  return (ei ||= mkDataEntry)
    where
      -- dataConRepArgTys sometimes returns unboxed tuples. is that a bug?
      fields = concatMap (map primRepToType . typePrimRep . unwrapType . scaledThing)
                         (dataConRepArgTys d)
        -- concatMap (map slotTyToType . repTypeSlots . repType) (dataConRepArgTys d)

mkDataEntry :: JExpr
mkDataEntry = ValExpr $ JFunc [] returnStack

genToplevelRhs :: Id -> CgStgRhs -> G JStat
-- general cases:
genToplevelRhs i rhs = case rhs of
  StgRhsCon cc con _mu _tys args -> do
    ii <- identForId i
    allocConStatic ii cc con args
    return mempty
  StgRhsClosure _ext cc _upd_flag {- srt -} args body -> do
    {-
      algorithm:
       - collect all Id refs that are in the global id cache
       - count usage in body for each ref
       - order by increasing use
       - prepend loading lives var to body: body can stay the same
    -}
    eid@(TxtI eidt) <- identForEntryId i
    (TxtI idt)   <- identForId i
    body <- genBody (initExprCtx i) i R2 args body
    (lidents, lids) <- unzip <$> liftToGlobal (jsSaturate (Just "ghcjs_tmp_sat_") body)
    let lidents' = map (\(TxtI t) -> t) lidents
    CIStaticRefs sr0 <- genStaticRefsRhs rhs
    let sri = filter (`notElem` lidents') sr0
        sr   = CIStaticRefs sri
    et <- genEntryType args
    ll <- loadLiveFun lids
    (static, regs, upd) <-
      if et == CIThunk
        then do
          r <- updateThunk
          pure (StaticThunk (Just (eidt, map StaticObjArg lidents')), CIRegs 0 [PtrV],r)
        else return (StaticFun eidt (map StaticObjArg lidents'),
                    (if null lidents then CIRegs 1 (concatMap idVt args)
                                     else CIRegs 0 (PtrV : concatMap idVt args))
                      , mempty)
    setcc <- ifProfiling $
               if et == CIThunk
                 then enterCostCentreThunk
                 else enterCostCentreFun cc
    emitClosureInfo (ClosureInfo eidt
                                 regs
                                 idt
                                 (fixedLayout $ map (uTypeVt . idType) lids)
                                 et
                                 sr)
    ccId <- costCentreStackLbl cc
    emitStatic idt static ccId
    return $ (eid ||= toJExpr (JFunc [] (ll <> upd <> setcc <> body)))

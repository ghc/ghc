
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998
-}

-- | Desugaring foreign declarations
module GHC.HsToCore.Foreign.Decl
  ( dsForeigns
  )
where

import GHC.Prelude
import GHC.Data.FastString

import GHC.Tc.Utils.Monad        -- temp

import GHC.HsToCore.Foreign.C
import GHC.HsToCore.Foreign.JavaScript
import GHC.HsToCore.Foreign.Wasm
import GHC.HsToCore.Foreign.Utils
import GHC.HsToCore.Monad

import GHC.Hs
import GHC.Types.Id
import GHC.Types.ForeignStubs
import GHC.Unit.Module
import GHC.Core.Coercion

import GHC.Cmm.CLabel
import GHC.Types.ForeignCall
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Driver.DynFlags
import GHC.Platform
import GHC.Data.OrdList
import GHC.Driver.Hooks

import Data.List (unzip4)

{-
Desugaring of @foreign@ declarations is naturally split up into
parts, an @import@ and an @export@  part. A @foreign import@
declaration
\begin{verbatim}
  foreign import cc nm f :: prim_args -> IO prim_res
\end{verbatim}
is the same as
\begin{verbatim}
  f :: prim_args -> IO prim_res
  f a1 ... an = _ccall_ nm cc a1 ... an
\end{verbatim}
so we reuse the desugaring code in @GHC.HsToCore.Foreign.Call@ to deal with these.
-}

dsForeigns :: [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList Binding)
dsForeigns fos = do
    hooks <- getHooks
    case dsForeignsHook hooks of
        Nothing -> dsForeigns' fos
        Just h  -> h fos

dsForeigns' :: [LForeignDecl GhcTc]
            -> DsM (ForeignStubs, OrdList Binding)
dsForeigns' []
  = return (NoStubs, nilOL)
dsForeigns' fos = do
    mod <- getModule
    platform <- targetPlatform <$> getDynFlags
    fives <- mapM do_ldecl fos
    let
        (hs, cs, idss, bindss) = unzip4 fives
        fe_ids = concat idss
        fe_init_code = foreignExportsInitialiser platform mod fe_ids
    --
    return (ForeignStubs
             (mconcat hs)
             (mconcat cs `mappend` fe_init_code),
            foldr (appOL . toOL) nilOL bindss)
  where
   do_ldecl (L loc decl) = putSrcSpanDs (locA loc) (do_decl decl)

   do_decl :: ForeignDecl GhcTc -> DsM (CHeader, CStub, [Id], [Binding])
   do_decl (ForeignImport { fd_name = id, fd_i_ext = co, fd_fi = spec }) = do
      traceIf (text "fi start" <+> ppr id)
      let id' = unLoc id
      (bs, h, c, ids) <- dsFImport id' co spec
      traceIf (text "fi end" <+> ppr id)
      return (h, c, ids, bs)

   do_decl (ForeignExport { fd_name = L _ id
                          , fd_e_ext = co
                          , fd_fe = CExport _
                              (L _ (CExportStatic _ ext_nm cconv)) }) = do
      (h, c, _, ids, bs) <- dsFExport id co ext_nm cconv False
      return (h, c, ids, bs)

{-
************************************************************************
*                                                                      *
\subsection{Foreign import}
*                                                                      *
************************************************************************

Desugaring foreign imports is just the matter of creating a binding
that on its RHS unboxes its arguments, performs the external call
(using the @CCallOp@ primop), before boxing the result up and returning it.

However, we create a worker/wrapper pair, thus:

        foreign import f :: Int -> IO Int
==>
        f x = IO ( \s -> case x of { I# x# ->
                         case fw s x# of { (# s1, y# #) ->
                         (# s1, I# y# #)}})

        fw s x# = ccall f s x#

The strictness/CPR analyser won't do this automatically because it doesn't look
inside returned tuples; but inlining this wrapper is a Really Good Idea
because it exposes the boxing to the call site.
-}

dsFImport :: Id
          -> Coercion
          -> ForeignImport (GhcPass p)
          -> DsM ([Binding], CHeader, CStub, [Id])
dsFImport id co (CImport _ cconv safety mHeader spec) = do
  platform <- getPlatform
  let cconv' = unLoc cconv
      safety' = unLoc safety
  case (platformArch platform, cconv') of
    (ArchJavaScript, _) -> do
      (bs, h, c) <- dsJsImport id co spec cconv' safety' mHeader
      pure (bs, h, c, [])
    (ArchWasm32, JavaScriptCallConv) ->
      dsWasmJSImport id co spec safety'
    _ -> do
      (bs, h, c) <- dsCImport id co spec cconv' safety' mHeader
      pure (bs, h, c, [])

{-
************************************************************************
*                                                                      *
\subsection{Foreign export}
*                                                                      *
************************************************************************

The function that does most of the work for `@foreign export@' declarations.
(see below for the boilerplate code a `@foreign export@' declaration expands
 into.)

For each `@foreign export foo@' in a module M we generate:
\begin{itemize}
\item a C function `@foo@', which calls
\item a Haskell stub `@M.\$ffoo@', which calls
\end{itemize}
the user-written Haskell function `@M.foo@'.
-}

dsFExport :: Id                 -- Either the exported Id,
                                -- or the foreign-export-dynamic constructor
          -> Coercion           -- Coercion between the Haskell type callable
                                -- from C, and its representation type
          -> CLabelString       -- The name to export to C land
          -> CCallConv
          -> Bool               -- True => foreign export dynamic
                                --         so invoke IO action that's hanging off
                                --         the first argument's stable pointer
          -> DsM ( CHeader      -- contents of Module_stub.h
                 , CStub        -- contents of Module_stub.c
                 , String       -- string describing type to pass to createAdj.
                 , [Id]         -- function closures to be registered as GC roots
                 , [Binding]    -- additional bindings used by desugared foreign export
                 )
dsFExport fn_id co ext_name cconv is_dyn = do
  platform <- getPlatform
  case (platformArch platform, cconv) of
    (ArchJavaScript, _) -> do
      (h, c, ts) <- dsJsFExport fn_id co ext_name cconv is_dyn
      pure (h, c, ts, [fn_id], [])
    (ArchWasm32, JavaScriptCallConv) ->
      dsWasmJSExport fn_id co ext_name
    _ -> do
      (h, c, ts) <- dsCFExport fn_id co ext_name cconv is_dyn
      pure (h, c, ts, [fn_id], [])


foreignExportsInitialiser :: Platform -> Module -> [Id] -> CStub
foreignExportsInitialiser _        _   []     = mempty
foreignExportsInitialiser platform mod hs_fns =
   -- Initialise foreign exports by registering a stable pointer from an
   -- __attribute__((constructor)) function.
   -- The alternative is to do this from stginit functions generated in
   -- codeGen/CodeGen.hs; however, stginit functions have a negative impact
   -- on binary sizes and link times because the static linker will think that
   -- all modules that are imported directly or indirectly are actually used by
   -- the program.
   -- (this is bad for big umbrella modules like Graphics.Rendering.OpenGL)
   --
   -- See Note [Tracking foreign exports] in rts/ForeignExports.c
   initializerCStub platform fn_nm list_decl fn_body
  where
    fn_nm       = mkInitializerStubLabel mod (fsLit "fexports")
    mod_str     = pprModuleName (moduleName mod)
    fn_body     = text "registerForeignExports" <> parens (char '&' <> list_symbol) <> semi
    list_symbol = text "stg_exports_" <> mod_str
    list_decl   = text "static struct ForeignExportsList" <+> list_symbol <+> equals
         <+> braces (
           text ".exports = " <+> export_list <> comma <+>
           text ".n_entries = " <+> ppr (length hs_fns))
         <> semi

    export_list = braces $ pprWithCommas closure_ptr hs_fns

    closure_ptr :: Id -> SDoc
    closure_ptr fn = text "(StgPtr) &" <> ppr fn <> text "_closure"

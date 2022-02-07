{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section{Code output phase}
-}



module GHC.Driver.CodeOutput
   ( codeOutput
   , outputForeignStubs
   , profilingInitCode
   , ipInitCode
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.ForeignSrcLang

import GHC.Cmm.Lint         ( cmmLint )
import GHC.Cmm              ( RawCmmGroup )
import GHC.Cmm.CLabel

import GHC.Driver.Session
import GHC.Driver.Config.Finder    (initFinderOpts)
import GHC.Driver.Ppr
import GHC.Driver.Backend
import GHC.Driver.Backend.Refunctionalize

import qualified GHC.Data.ShortText as ST
import GHC.Data.Stream           ( Stream )
import qualified GHC.Data.Stream as Stream

import GHC.Utils.TmpFs


import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Logger

import GHC.Unit
import GHC.Unit.Finder      ( mkStubPaths )

import GHC.Types.SrcLoc
import GHC.Types.CostCentre
import GHC.Types.ForeignStubs

import System.Directory
import System.FilePath
import Data.Set (Set)

{-
************************************************************************
*                                                                      *
\subsection{Steering}
*                                                                      *
************************************************************************
-}

codeOutput
    :: Logger
    -> TmpFs
    -> DynFlags
    -> UnitState
    -> Module
    -> FilePath
    -> ModLocation
    -> (a -> ForeignStubs)
    -> [(ForeignSrcLang, FilePath)]
    -- ^ additional files to be compiled with the C compiler
    -> Set UnitId -- ^ Dependencies
    -> Stream IO RawCmmGroup a                       -- Compiled C--
    -> IO (FilePath,
           (Bool{-stub_h_exists-}, Maybe FilePath{-stub_c_exists-}),
           [(ForeignSrcLang, FilePath)]{-foreign_fps-},
           a)
codeOutput logger tmpfs dflags unit_state this_mod filenm location genForeignStubs foreign_fps pkg_deps
  cmm_stream
  =
    do  {
        -- Lint each CmmGroup as it goes past
        ; let linted_cmm_stream =
                 if gopt Opt_DoCmmLinting dflags
                    then Stream.mapM do_lint cmm_stream
                    else cmm_stream

              do_lint cmm = withTimingSilent logger
                  (text "CmmLint"<+>brackets (ppr this_mod))
                  (const ()) $ do
                { case cmmLint (targetPlatform dflags) cmm of
                        Just err -> do { logMsg logger
                                                   MCDump
                                                   noSrcSpan
                                                   $ withPprStyle defaultDumpStyle err
                                       ; ghcExit logger 1
                                       }
                        Nothing  -> return ()
                ; return cmm
                }

        ; a <- applyCodeOutput (backendCodeOutput $ backend dflags)
               logger dflags this_mod location filenm pkg_deps linted_cmm_stream
        ; let stubs = genForeignStubs a
        ; stubs_exist <- outputForeignStubs logger tmpfs dflags unit_state this_mod location stubs
        ; return (filenm, stubs_exist, foreign_fps, a)
        }

{-
************************************************************************
*                                                                      *
\subsection{Foreign import/export}
*                                                                      *
************************************************************************
-}

outputForeignStubs
    :: Logger
    -> TmpFs
    -> DynFlags
    -> UnitState
    -> Module
    -> ModLocation
    -> ForeignStubs
    -> IO (Bool,         -- Header file created
           Maybe FilePath) -- C file created
outputForeignStubs logger tmpfs dflags unit_state mod location stubs
 = do
   let stub_h = mkStubPaths (initFinderOpts dflags) (moduleName mod) location
   stub_c <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "c"

   case stubs of
     NoStubs ->
        return (False, Nothing)

     ForeignStubs (CHeader h_code) (CStub c_code) -> do
        let
            stub_c_output_d = pprCode CStyle c_code
            stub_c_output_w = showSDoc dflags stub_c_output_d

            -- Header file protos for "foreign export"ed functions.
            stub_h_output_d = pprCode CStyle h_code
            stub_h_output_w = showSDoc dflags stub_h_output_d

        createDirectoryIfMissing True (takeDirectory stub_h)

        putDumpFileMaybe logger Opt_D_dump_foreign
                      "Foreign export header file"
                      FormatC
                      stub_h_output_d

        -- we need the #includes from the rts package for the stub files
        let rts_includes =
               let mrts_pkg = lookupUnitId unit_state rtsUnitId
                   mk_include i = "#include \"" ++ ST.unpack i ++ "\"\n"
               in case mrts_pkg of
                    Just rts_pkg -> concatMap mk_include (unitIncludes rts_pkg)
                    -- This case only happens when compiling foreign stub for the rts
                    -- library itself. The only time we do this at the moment is for
                    -- IPE information for the RTS info tables
                    Nothing -> ""

            -- wrapper code mentions the ffi_arg type, which comes from ffi.h
            ffi_includes
              | platformMisc_libFFI $ platformMisc dflags = "#include <ffi.h>\n"
              | otherwise = ""

        stub_h_file_exists
           <- outputForeignStubs_help stub_h stub_h_output_w
                ("#include <HsFFI.h>\n" ++ cplusplus_hdr) cplusplus_ftr

        putDumpFileMaybe logger Opt_D_dump_foreign
                      "Foreign export stubs" FormatC stub_c_output_d

        stub_c_file_exists
           <- outputForeignStubs_help stub_c stub_c_output_w
                ("#define IN_STG_CODE 0\n" ++
                 "#include <Rts.h>\n" ++
                 rts_includes ++
                 ffi_includes ++
                 cplusplus_hdr)
                 cplusplus_ftr
           -- We're adding the default hc_header to the stub file, but this
           -- isn't really HC code, so we need to define IN_STG_CODE==0 to
           -- avoid the register variables etc. being enabled.

        return (stub_h_file_exists, if stub_c_file_exists
                                       then Just stub_c
                                       else Nothing )
 where
   cplusplus_hdr = "#if defined(__cplusplus)\nextern \"C\" {\n#endif\n"
   cplusplus_ftr = "#if defined(__cplusplus)\n}\n#endif\n"


-- It is more than likely that the stubs file will
-- turn out to be empty, in which case no file should be created.
outputForeignStubs_help :: FilePath -> String -> String -> String -> IO Bool
outputForeignStubs_help _fname ""      _header _footer = return False
outputForeignStubs_help fname doc_str header footer
   = do writeFile fname (header ++ doc_str ++ '\n':footer ++ "\n")
        return True

-- -----------------------------------------------------------------------------
-- Initialising cost centres

-- We must produce declarations for the cost-centres defined in this
-- module;

-- | Generate code to initialise cost centres
profilingInitCode :: Platform -> Module -> CollectedCCs -> CStub
profilingInitCode platform this_mod (local_CCs, singleton_CCSs)
 = CStub $ vcat
    $  map emit_cc_decl local_CCs
    ++ map emit_ccs_decl singleton_CCSs
    ++ [emit_cc_list local_CCs]
    ++ [emit_ccs_list singleton_CCSs]
    ++ [ text "static void prof_init_" <> ppr this_mod
            <> text "(void) __attribute__((constructor));"
       , text "static void prof_init_" <> ppr this_mod <> text "(void)"
       , braces (vcat
                 [ text "registerCcList" <> parens local_cc_list_label <> semi
                 , text "registerCcsList" <> parens singleton_cc_list_label <> semi
                 ])
       ]
 where
   emit_cc_decl cc =
       text "extern CostCentre" <+> cc_lbl <> text "[];"
     where cc_lbl = pdoc platform (mkCCLabel cc)
   local_cc_list_label = text "local_cc_" <> ppr this_mod
   emit_cc_list ccs =
      text "static CostCentre *" <> local_cc_list_label <> text "[] ="
      <+> braces (vcat $ [ pdoc platform (mkCCLabel cc) <> comma
                         | cc <- ccs
                         ] ++ [text "NULL"])
      <> semi

   emit_ccs_decl ccs =
       text "extern CostCentreStack" <+> ccs_lbl <> text "[];"
     where ccs_lbl = pdoc platform (mkCCSLabel ccs)
   singleton_cc_list_label = text "singleton_cc_" <> ppr this_mod
   emit_ccs_list ccs =
      text "static CostCentreStack *" <> singleton_cc_list_label <> text "[] ="
      <+> braces (vcat $ [ pdoc platform (mkCCSLabel cc) <> comma
                         | cc <- ccs
                         ] ++ [text "NULL"])
      <> semi

-- | Generate code to initialise info pointer origin
-- See Note [Mapping Info Tables to Source Positions]
ipInitCode
  :: Bool            -- is Opt_InfoTableMap enabled or not
  -> Platform
  -> Module
  -> [InfoProvEnt]
  -> CStub
ipInitCode do_info_table platform this_mod ents
 = if not do_info_table
    then mempty
    else CStub $ vcat
    $  map emit_ipe_decl ents
    ++ [emit_ipe_list ents]
    ++ [ text "static void ip_init_" <> ppr this_mod
            <> text "(void) __attribute__((constructor));"
       , text "static void ip_init_" <> ppr this_mod <> text "(void)"
       , braces (vcat
                 [ text "registerInfoProvList" <> parens local_ipe_list_label <> semi
                 ])
       ]
 where
   emit_ipe_decl ipe =
       text "extern InfoProvEnt" <+> ipe_lbl <> text "[];"
     where ipe_lbl = pprCLabel platform CStyle (mkIPELabel ipe)
   local_ipe_list_label = text "local_ipe_" <> ppr this_mod
   emit_ipe_list ipes =
      text "static InfoProvEnt *" <> local_ipe_list_label <> text "[] ="
      <+> braces (vcat $ [ pprCLabel platform CStyle (mkIPELabel ipe) <> comma
                         | ipe <- ipes
                         ] ++ [text "NULL"])
      <> semi



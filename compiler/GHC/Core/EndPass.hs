{-# LANGUAGE ScopedTypeVariables #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

Routine diagnostics/debugging aides to do at the end of every
compilation pass that returns Core. Heavily leverages `GHC.Core.Lint`.
-}

module GHC.Core.EndPass (
    EndPassConfig (..),
    endPassIO,
    dumpPassResult
 ) where

import GHC.Prelude

import GHC.Driver.Flags ( DumpFlag (..) )

import GHC.Core
import GHC.Core.Lint
import GHC.Core.Stats ( coreBindsStats )
import GHC.Core.Ppr

import GHC.Utils.Outputable as Outputable
import qualified GHC.Utils.Error as Err
import GHC.Utils.Logger

import Control.Monad
import Data.Foldable      ( for_ )

{-
************************************************************************
*                                                                      *
                 Beginning and ending passes
*                                                                      *
************************************************************************
-}

-- | Configuration for boilerplate operations at the end of a
-- compilation pass producing Core.
data EndPassConfig = EndPassConfig
  { ep_dumpCoreSizes :: !Bool
  -- ^ Whether core bindings should be dumped with the size of what they
  -- are binding (i.e. the size of the RHS of the binding).

  , ep_lintPassResult :: !(Maybe LintPassResultConfig)
  -- ^ Whether we should lint the result of this pass.

  , ep_printUnqual :: !PrintUnqualified

  , ep_dumpFlag :: !(Maybe DumpFlag)

  , ep_prettyPass :: !SDoc

  , ep_passDetails :: !SDoc
  }

endPassIO :: Logger
          -> EndPassConfig
          -> CoreProgram -> [CoreRule]
          -> IO ()
-- Used by the IO-is CorePrep too
endPassIO logger cfg binds rules
  = do { dumpPassResult logger (ep_dumpCoreSizes cfg) (ep_printUnqual cfg) mb_flag
                        (renderWithContext defaultSDocContext (ep_prettyPass cfg))
                        (ep_passDetails cfg) binds rules
       ; for_ (ep_lintPassResult cfg) $ \lp_cfg ->
           lintPassResult logger lp_cfg binds
       }
  where
    mb_flag = case ep_dumpFlag cfg of
                Just flag | logHasDumpFlag logger flag                    -> Just flag
                          | logHasDumpFlag logger Opt_D_verbose_core2core -> Just flag
                _ -> Nothing

dumpPassResult :: Logger
               -> Bool                  -- dump core sizes?
               -> PrintUnqualified
               -> Maybe DumpFlag        -- Just df => show details in a file whose
                                        --            name is specified by df
               -> String                -- Header
               -> SDoc                  -- Extra info to appear after header
               -> CoreProgram -> [CoreRule]
               -> IO ()
dumpPassResult logger dump_core_sizes unqual mb_flag hdr extra_info binds rules
  = do { forM_ mb_flag $ \flag -> do
           logDumpFile logger (mkDumpStyle unqual) flag hdr FormatCore dump_doc

         -- Report result size
         -- This has the side effect of forcing the intermediate to be evaluated
         -- if it's not already forced by a -ddump flag.
       ; Err.debugTraceMsg logger 2 size_doc
       }

  where
    size_doc = sep [text "Result size of" <+> text hdr, nest 2 (equals <+> ppr (coreBindsStats binds))]

    dump_doc  = vcat [ nest 2 extra_info
                     , size_doc
                     , blankLine
                     , if dump_core_sizes
                        then pprCoreBindingsWithSize binds
                        else pprCoreBindings         binds
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , text "------ Local rules for imported ids --------"
                    , pprRules rules ]

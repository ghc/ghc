{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Simple.RemovePlugin where

import Control.Monad.IO.Class
import Data.List (intercalate)
import GHC.Driver.Plugins
import GHC.Plugins
import GHC.Tc.Types
import Language.Haskell.Syntax.Extension
import GHC.Hs.Expr
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.SrcLoc
import GHC.Hs
import GHC.Hs.Binds
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Avail
import GHC.Hs.Dump

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = parsedPlugin
                       , typeCheckResultAction = typecheckPlugin
                       , spliceRunAction = metaPlugin'
                       , interfaceLoadAction = interfaceLoadPlugin'
                       }

parsedPlugin :: [CommandLineOption] -> ModSummary
             -> ParsedResult -> Hsc ParsedResult
parsedPlugin [name, "parse"] _ (ParsedResult pm msgs)
  = return (ParsedResult pm { hpm_module = removeParsedBinding name (hpm_module pm) } msgs)
parsedPlugin _ _ parsed = return parsed

removeParsedBinding :: String -> Located (HsModule GhcPs)
                         -> Located (HsModule GhcPs)
removeParsedBinding name (L l m)
  = (L l (m { hsmodDecls = filter (notNamedAs name) (hsmodDecls m) } ))
  where notNamedAs name (L _ (ValD _ (FunBind { fun_id = L _ fid })))
          = occNameString (rdrNameOcc fid) /= name
        notNamedAs _ _ = True

typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckPlugin [name, "typecheck"] _ tc
  = return $ tc { tcg_exports = filter (availNotNamedAs name) (tcg_exports tc)
                , tcg_binds = filter (notNamedAs name) (tcg_binds tc)
                }
  where notNamedAs name (L _ FunBind { fun_id = L _ fid })
          = occNameString (getOccName fid) /= name
        notNamedAs name (L _ (XHsBindsLR (AbsBinds { abs_binds = bnds })))
          = all (notNamedAs name) bnds
        notNamedAs _ (L _ b) = True
typecheckPlugin _ _ tc = return tc

metaPlugin' :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
metaPlugin' [name, "meta"] (L l (XExpr (WrapExpr w (HsPar _ (L _ (HsApp noExt (L _ (HsVar _ (L _ id))) e))))))
  | occNameString (getOccName id) == name
  = return (L l (XExpr (WrapExpr w (unLoc e))))
-- The test should always match this first case. If the desugaring changes
-- again in the future then the panic is more useful than the previous
-- inscrutable failure.
metaPlugin' _ meta = pprPanic "meta" (showAstData BlankSrcSpan BlankEpAnnotations meta)

interfaceLoadPlugin' :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
interfaceLoadPlugin' [name, "interface"] iface
  = return $ set_mi_exports (filter (availNotNamedAs name)
                                    (mi_exports iface))
                            iface

interfaceLoadPlugin' _ iface = return iface

availNotNamedAs :: String -> AvailInfo -> Bool
availNotNamedAs name avail
  = occNameString (getOccName (availName avail)) /= name

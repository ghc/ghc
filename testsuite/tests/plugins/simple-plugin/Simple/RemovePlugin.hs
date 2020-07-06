{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Simple.RemovePlugin where

import Control.Monad.IO.Class
import Data.List (intercalate)
import GHC.Driver.Plugins
import GHC.Plugins
import GHC.Data.Bag
import GHC.Tc.Types
import GHC.Hs.Extension
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

parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule
                  -> Hsc HsParsedModule
parsedPlugin [name, "parse"] _ pm
  = return $ pm { hpm_module = removeParsedBinding name (hpm_module pm) }
parsedPlugin _ _ pm = return pm

removeParsedBinding :: String -> Located HsModule
                         -> Located HsModule
removeParsedBinding name (L l m)
  = (L l (m { hsmodDecls = filter (notNamedAs name) (hsmodDecls m) } ))
  where notNamedAs name (L _ (ValD _ (FunBind { fun_id = L _ fid })))
          = occNameString (rdrNameOcc fid) /= name
        notNamedAs _ _ = True

typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckPlugin [name, "typecheck"] _ tc
  = return $ tc { tcg_exports = filter (availNotNamedAs name) (tcg_exports tc)
                , tcg_binds = filterBag (notNamedAs name) (tcg_binds tc)
                }
  where notNamedAs name (L _ FunBind { fun_id = L _ fid })
          = occNameString (getOccName fid) /= name
        notNamedAs name (L _ AbsBinds { abs_binds = bnds })
          = all (notNamedAs name) bnds
        notNamedAs _ (L _ b) = True
typecheckPlugin _ _ tc = return tc

metaPlugin' :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
metaPlugin' [name, "meta"] (L l (HsPar x (L _ (XExpr (WrapExpr (HsWrap w (HsApp noExt (L _ (HsVar _ (L _ id))) e)))))))
  | occNameString (getOccName id) == name
  = return (L l (XExpr (WrapExpr (HsWrap w (unLoc e)))))
-- The test should always match this first case. If the desugaring changes
-- again in the future then the panic is more useful than the previous
-- inscrutable failure.
metaPlugin' _ meta = pprPanic "meta" (showAstData BlankSrcSpan BlankApiAnnotations meta)

interfaceLoadPlugin' :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
interfaceLoadPlugin' [name, "interface"] iface
  = return $ iface { mi_exports = filter (availNotNamedAs name)
                                         (mi_exports iface)
                   }
interfaceLoadPlugin' _ iface = return iface

availNotNamedAs :: String -> AvailInfo -> Bool
availNotNamedAs name avail
  = occNameString (getOccName (availName avail)) /= name

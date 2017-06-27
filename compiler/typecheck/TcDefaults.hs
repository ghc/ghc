{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

\section[TcDefaults]{Typechecking \tr{default} declarations}
-}
{-# LANGUAGE TypeFamilies #-}

module TcDefaults ( tcDefaults ) where

import HsSyn
import Class
import TcRnMonad
import TcEnv
import TcHsType
import TcHsSyn
import TcSimplify
import TcValidity
import TcType
import PrelNames
import SrcLoc
import Outputable
import FastString
import qualified GHC.LanguageExtensions as LangExt

tcDefaults :: [LDefaultDecl GhcRn]
           -> TcM (Maybe [Type])    -- Defaulting types to heave
                                    -- into Tc monad for later use
                                    -- in Disambig.

tcDefaults []
  = getDeclaredDefaultTys       -- No default declaration, so get the
                                -- default types from the envt;
                                -- i.e. use the current ones
                                -- (the caller will put them back there)
        -- It's important not to return defaultDefaultTys here (which
        -- we used to do) because in a TH program, tcDefaults [] is called
        -- repeatedly, once for each group of declarations between top-level
        -- splices.  We don't want to carefully set the default types in
        -- one group, only for the next group to ignore them and install
        -- defaultDefaultTys

tcDefaults [L _ (DefaultDecl [])]
  = return (Just [])            -- Default declaration specifying no types

tcDefaults [L locn (DefaultDecl mono_tys)]
  = setSrcSpan locn                     $
    addErrCtxt defaultDeclCtxt          $
    do  { ovl_str   <- xoptM LangExt.OverloadedStrings
        ; ext_deflt <- xoptM LangExt.ExtendedDefaultRules
        ; num_class    <- tcLookupClass numClassName
        ; deflt_str <- if ovl_str
                       then mapM tcLookupClass [isStringClassName]
                       else return []
        ; deflt_interactive <- if ext_deflt
                               then mapM tcLookupClass interactiveClassNames
                               else return []
        ; let deflt_clss = num_class : deflt_str ++ deflt_interactive

        ; tau_tys <- mapAndReportM (tc_default_ty deflt_clss) mono_tys

        ; return (Just tau_tys) }

tcDefaults decls@(L locn (DefaultDecl _) : _)
  = setSrcSpan locn $
    failWithTc (dupDefaultDeclErr decls)


tc_default_ty :: [Class] -> LHsType GhcRn -> TcM Type
tc_default_ty deflt_clss hs_ty
 = do   { (ty, _kind) <- solveEqualities $
                         tcLHsType hs_ty
        ; ty <- zonkTcTypeToType emptyZonkEnv ty   -- establish Type invariants
        ; checkValidType DefaultDeclCtxt ty

        -- Check that the type is an instance of at least one of the deflt_clss
        ; oks <- mapM (check_instance ty) deflt_clss
        ; checkTc (or oks) (badDefaultTy ty deflt_clss)
        ; return ty }

check_instance :: Type -> Class -> TcM Bool
  -- Check that ty is an instance of cls
  -- We only care about whether it worked or not; return a boolean
check_instance ty cls
  = do  { (_, success) <- discardErrs $
                          askNoErrs $
                          simplifyDefault [mkClassPred cls [ty]]
        ; return success }

defaultDeclCtxt :: SDoc
defaultDeclCtxt = text "When checking the types in a default declaration"

dupDefaultDeclErr :: [Located (DefaultDecl GhcRn)] -> SDoc
dupDefaultDeclErr (L _ (DefaultDecl _) : dup_things)
  = hang (text "Multiple default declarations")
       2 (vcat (map pp dup_things))
  where
    pp (L locn (DefaultDecl _)) = text "here was another default declaration" <+> ppr locn
dupDefaultDeclErr [] = panic "dupDefaultDeclErr []"

badDefaultTy :: Type -> [Class] -> SDoc
badDefaultTy ty deflt_clss
  = hang (text "The default type" <+> quotes (ppr ty) <+> ptext (sLit "is not an instance of"))
       2 (foldr1 (\a b -> a <+> text "or" <+> b) (map (quotes. ppr) deflt_clss))

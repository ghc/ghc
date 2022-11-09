{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

-}
{-# LANGUAGE TypeFamilies #-}

-- | Typechecking @default@ declarations
module GHC.Tc.Gen.Default ( tcDefaults ) where

import GHC.Prelude

import GHC.Hs
import GHC.Core.Class
import GHC.Core.Type( typeKind )

import GHC.Types.Var( tyVarKind )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.Zonk
import GHC.Tc.Solver
import GHC.Tc.Validity
import GHC.Tc.Utils.TcType
import GHC.Builtin.Names
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import qualified GHC.LanguageExtensions as LangExt

import Data.List.NonEmpty ( NonEmpty (..) )

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

tcDefaults [L _ (DefaultDecl _ [])]
  = return (Just [])            -- Default declaration specifying no types

tcDefaults [L locn (DefaultDecl _ mono_tys)]
  = setSrcSpan (locA locn)              $
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

tcDefaults (decl@(L locn (DefaultDecl _ _)) : decls)
  = setSrcSpan (locA locn) $
    failWithTc (dupDefaultDeclErr (decl:|decls))


tc_default_ty :: [Class] -> LHsType GhcRn -> TcM Type
tc_default_ty deflt_clss hs_ty
 = do   { ty <- solveEqualities "tc_default_ty" $
                tcInferLHsType hs_ty
        ; ty <- zonkTcTypeToType ty   -- establish Type invariants
        ; checkValidType DefaultDeclCtxt ty

        -- Check that the type is an instance of at least one of the deflt_clss
        ; oks <- mapM (check_instance ty) deflt_clss
        ; checkTc (or oks) (TcRnBadDefaultType ty deflt_clss)
        ; return ty }

check_instance :: Type -> Class -> TcM Bool
-- Check that ty is an instance of cls
-- We only care about whether it worked or not; return a boolean
-- This checks that  cls :: k -> Constraint
-- with just one argument and no polymorphism; if we need to add
-- polymorphism we can make it more complicated.  For now we are
-- concerned with classes like
--    Num      :: Type -> Constraint
--    Foldable :: (Type->Type) -> Constraint
check_instance ty cls
  | [cls_tv] <- classTyVars cls
  , tyVarKind cls_tv `tcEqType` typeKind ty
  = simplifyDefault [mkClassPred cls [ty]]
  | otherwise
  = return False

defaultDeclCtxt :: SDoc
defaultDeclCtxt = text "When checking the types in a default declaration"

dupDefaultDeclErr :: NonEmpty (LDefaultDecl GhcRn) -> TcRnMessage
dupDefaultDeclErr (L _ (DefaultDecl _ _) :| dup_things)
  = TcRnMultipleDefaultDeclarations dup_things

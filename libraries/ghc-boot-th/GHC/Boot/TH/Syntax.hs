{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Boot.TH.Syntax
  (module GHC.Internal.TH.Syntax) where

import GHC.Internal.TH.Syntax

import Control.DeepSeq
import Prelude (seq)

instance NFData AnnLookup
instance NFData PkgName
instance NFData Name
instance NFData OccName
instance NFData Type
instance NFData DocLoc
instance NFData ForeignSrcLang
instance NFData Module
instance NFData ModName
instance NFData Dec
instance NFData a => NFData (TyVarBndr a)
instance NFData Clause
instance NFData Specificity
instance NFData NameFlavour
instance NFData Pat
instance NFData NameSpace
instance NFData Body
instance NFData Lit
instance NFData Guard
instance NFData Bytes where
  rnf (Bytes a b c) = a `seq` rnf b `seq` rnf c
instance NFData Exp
instance NFData TyLit
instance NFData BndrVis
instance NFData Con
instance NFData Match
instance NFData Stmt
instance NFData Bang
instance NFData DerivClause
instance NFData DerivStrategy
instance NFData SourceUnpackedness
instance NFData Range
instance NFData FunDep
instance NFData Overlap
instance NFData SourceStrictness
instance NFData Foreign
instance NFData Fixity
instance NFData Callconv
instance NFData Safety
instance NFData FixityDirection
instance NFData NamespaceSpecifier
instance NFData Pragma
instance NFData TySynEqn
instance NFData Inline
instance NFData TypeFamilyHead
instance NFData RuleMatch
instance NFData Phases
instance NFData FamilyResultSig
instance NFData RuleBndr
instance NFData InjectivityAnn
instance NFData Role
instance NFData AnnTarget
instance NFData PatSynArgs
instance NFData PatSynDir
instance NFData Extension

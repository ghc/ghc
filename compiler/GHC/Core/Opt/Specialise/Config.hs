

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[Specialise]{Stamping out overloading, and (optionally) polymorphism}
-}

module GHC.Core.Opt.Specialise.Config
  ( SpecialiseOpts(..)
  ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.SimpleOpt ( SimpleOpts )

import GHC.Types.Error

import GHC.Utils.Outputable

data SpecialiseOpts = SpecialiseOpts
  { so_uniq_mask :: !Char
  , so_unqual :: !PrintUnqualified
  , so_cross_module_specialise :: !Bool
  , so_specialise_aggressively :: !Bool
  , so_warn_missed_specs :: !(Maybe MessageClass)
  , so_warn_all_missed_specs :: !(Maybe MessageClass)
  , so_sdoc_context :: !SDocContext
  , so_simpl_opts :: !SimpleOpts
  , so_rule_opts :: !RuleOpts
  }

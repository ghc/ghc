{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE -fno-warn-orphans #-}

module OccName.Ppr
  ( pprOccName
  )

import Util
import Unique
--import DynFlags
import UniqFM
import UniqSet
import FastString
import FastStringEnv
import Outputable
import Lexeme
import Binary
import Control.DeepSeq
import Data.Char
import Data.Data

{-
************************************************************************
*                                                                      *
\subsection{Printing}
*                                                                      *
************************************************************************
-}

instance Outputable OccName where
    ppr = pprOccName

instance OutputableBndr OccName where
    pprBndr _ = ppr
    pprInfixOcc n = pprInfixVar (isSymOcc n) (ppr n)
    pprPrefixOcc n = pprPrefixVar (isSymOcc n) (ppr n)

pprOccName :: OccName -> SDoc
pprOccName (OccName sp occ)
  = getPprStyle $ \ sty ->
    if codeStyle sty
    then ztext (zEncodeFS occ)
    else pp_occ <> pp_debug sty
  where
    pp_debug sty | debugStyle sty = braces (pprNameSpaceBrief sp)
                 | otherwise      = empty

    pp_occ = sdocWithDynFlags $ \dflags ->
             if gopt Opt_SuppressUniques dflags
             then text (strip_th_unique (unpackFS occ))
             else ftext occ

        -- See Note [Suppressing uniques in OccNames]
    strip_th_unique ('[' : c : _) | isAlphaNum c = []
    strip_th_unique (c : cs) = c : strip_th_unique cs
    strip_th_unique []       = []

{-
Note [Suppressing uniques in OccNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a hack to de-wobblify the OccNames that contain uniques from
Template Haskell that have been turned into a string in the OccName.
See Note [Unique OccNames from Template Haskell] in Convert.hs
-}

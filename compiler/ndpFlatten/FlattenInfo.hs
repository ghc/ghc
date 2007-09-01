--  $Id$
--
--  Copyright (c) 2002 Manuel M T Chakravarty & Gabriele Keller
--
--  Information for modules outside of the flattening module collection.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module contains information that is needed, and thus imported, by
--  modules that are otherwise independent of flattening and may in fact be
--  directly or indirectly imported by some of the flattening-related
--  modules.  This is to avoid cyclic module dependencies.
-- 
--- DOCU ----------------------------------------------------------------------
--
--  Language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

{-# OPTIONS_GHC -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
-- for details

module FlattenInfo (
  namesNeededForFlattening
) where

import StaticFlags (opt_Flatten)
import NameSet     (FreeVars, emptyFVs, mkFVs)
import PrelNames   (fstName, andName, orName, lengthPName, replicatePName,
		    mapPName, bpermutePName, bpermuteDftPName, indexOfPName)


-- this is a list of names that need to be available if flattening is
-- performed (EXPORTED)
--
--  * needs to be kept in sync with the names used in Core generation in
--   `FlattenMonad' and `NDPCoreUtils'
--
namesNeededForFlattening :: FreeVars
namesNeededForFlattening
  | not opt_Flatten = emptyFVs		-- none without -fflatten
  | otherwise
  = mkFVs [fstName, andName, orName, lengthPName, replicatePName, mapPName,
	   bpermutePName, bpermuteDftPName, indexOfPName]
    -- stuff from PrelGHC doesn't have to go here

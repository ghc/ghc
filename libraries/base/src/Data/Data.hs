{-# LANGUAGE Safe #-}

-- |
-- Module      :  Data.Data
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (local universal quantification)
--
-- This module provides the 'Data' class with its primitives for
-- generic programming, along with instances for many datatypes. It
-- corresponds to a merge between the previous "Data.Generics.Basics"
-- and almost all of "Data.Generics.Instances". The instances that are
-- not present in this module were moved to the
-- @Data.Generics.Instances@ module in the @syb@ package.
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell.  See
-- <https://wiki.haskell.org/Research_papers/Generics#Scrap_your_boilerplate.21>.
--

module Data.Data (

        -- * Module Data.Typeable re-exported for convenience
        module Data.Typeable,

        -- * The Data class for processing constructor applications
        Data(
                gfoldl,
                gunfold,
                toConstr,
                dataTypeOf,
                dataCast1,      -- mediate types and unary type constructors
                dataCast2,      -- mediate types and binary type constructors
                -- Generic maps defined in terms of gfoldl
                gmapT,
                gmapQ,
                gmapQl,
                gmapQr,
                gmapQi,
                gmapM,
                gmapMp,
                gmapMo
            ),

        -- * Datatype representations
        DataType,       -- abstract
        -- ** Constructors
        mkDataType,
        mkIntType,
        mkFloatType,
        mkCharType,
        mkNoRepType,
        -- ** Observers
        dataTypeName,
        DataRep(..),
        dataTypeRep,
        -- ** Convenience functions
        repConstr,
        isAlgType,
        dataTypeConstrs,
        indexConstr,
        maxConstrIndex,
        isNorepType,

        -- * Data constructor representations
        Constr,         -- abstract
        ConIndex,       -- alias for Int, start at 1
        Fixity(..),
        -- ** Constructors
        mkConstr,
        mkConstrTag,
        mkIntegralConstr,
        mkRealConstr,
        mkCharConstr,
        -- ** Observers
        constrType,
        ConstrRep(..),
        constrRep,
        constrFields,
        constrFixity,
        -- ** Convenience function: algebraic data types
        constrIndex,
        -- ** From strings to constructors and vice versa: all data types
        showConstr,
        readConstr,

        -- * Convenience functions: take type constructors apart
        tyconUQname,
        tyconModule,

        -- * Generic operations defined in terms of 'gunfold'
        fromConstr,
        fromConstrB,
        fromConstrM

  ) where

import GHC.Internal.Data.Data
import Data.Typeable

import GHC.Real (toRational)
import GHC.Float (Double)
import Data.Eq ((==))
import Data.Function ((.))
import Data.Maybe (Maybe (Nothing, Just))
import Data.List (filter)
import Data.String (String)
import Text.Read (Read, reads)

-- | Lookup a constructor via a string
readConstr :: DataType -> String -> Maybe Constr
readConstr dt str =
      case dataTypeRep dt of
        AlgRep cons -> idx cons
        IntRep      -> mkReadCon (\i -> (mkPrimCon dt str (IntConstr i)))
        FloatRep    -> mkReadCon ffloat
        CharRep     -> mkReadCon (\c -> (mkPrimCon dt str (CharConstr c)))
        NoRep       -> Nothing
  where

    -- Read a value and build a constructor
    mkReadCon :: Read t => (t -> Constr) -> Maybe Constr
    mkReadCon f = case (reads str) of
                    [(t,"")] -> Just (f t)
                    _ -> Nothing

    -- Traverse list of algebraic datatype constructors
    idx :: [Constr] -> Maybe Constr
    idx cons = case filter ((==) str . showConstr) cons of
                [] -> Nothing
                hd : _ -> Just hd

    ffloat :: Double -> Constr
    ffloat =  mkPrimCon dt str . FloatConstr . toRational

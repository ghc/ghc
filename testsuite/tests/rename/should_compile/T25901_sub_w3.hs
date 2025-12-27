{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module T25901_sub_w3 where

import T25901_sub_w3_nc (NameSpace(..), NameScope(..), NameCheck, expect)

import T25901_sub_w3_helper qualified as T1  (T (.., ..))                 -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T2  (T (.., type ..))            -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T3  (T (.., data ..))            -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T4  (T (type .., ..))            -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T5  (T (type .., data ..))       -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T6  (T (data .., ..))            -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T7  (T (data .., type ..))       -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T8  (T (type .., type ..))       -- equivalent to T(type ..)
import T25901_sub_w3_helper qualified as T9  (T (data .., data ..))       -- equivalent to T(data ..)

import T25901_sub_w3_helper qualified as T10 (T (.., .., ..))                -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T11 (T (.., .., type ..))           -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T12 (T (.., .., data ..))           -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T13 (T (.., type .., ..))           -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T14 (T (.., type .., type ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T15 (T (.., type .., data ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T16 (T (.., data .., ..))           -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T17 (T (.., data .., type ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T18 (T (.., data .., data ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T19 (T (type .., .., ..))           -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T20 (T (type .., .., type ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T21 (T (type .., .., data ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T22 (T (type .., type .., ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T23 (T (type .., type .., data ..)) -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T24 (T (type .., data .., ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T25 (T (type .., data .., type ..)) -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T26 (T (type .., data .., data ..)) -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T27 (T (data .., .., ..))           -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T28 (T (data .., .., type ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T29 (T (data .., .., data ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T30 (T (data .., type .., ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T31 (T (data .., type .., type ..)) -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T32 (T (data .., type .., data ..)) -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T33 (T (data .., data .., ..))      -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T34 (T (data .., data .., type ..)) -- equivalent to T(..)
import T25901_sub_w3_helper qualified as T35 (T (type .., type .., type ..)) -- equivalent to T(type ..)
import T25901_sub_w3_helper qualified as T36 (T (data .., data .., data ..)) -- equivalent to T(data ..)

import T25901_sub_w3_helper qualified as C1  (C (.., ..))                    -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C2  (C (.., type ..))               -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C3  (C (.., data ..))               -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C4  (C (type .., ..))               -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C5  (C (type .., data ..))          -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C6  (C (data .., ..))               -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C7  (C (data .., type ..))          -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C8  (C (type .., type ..))          -- equivalent to C(type ..)
import T25901_sub_w3_helper qualified as C9  (C (data .., data ..))          -- equivalent to C(data ..)

import T25901_sub_w3_helper qualified as C10 (C (.., .., ..))                -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C11 (C (.., .., type ..))           -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C12 (C (.., .., data ..))           -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C13 (C (.., type .., ..))           -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C14 (C (.., type .., type ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C15 (C (.., type .., data ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C16 (C (.., data .., ..))           -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C17 (C (.., data .., type ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C18 (C (.., data .., data ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C19 (C (type .., .., ..))           -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C20 (C (type .., .., type ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C21 (C (type .., .., data ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C22 (C (type .., type .., ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C23 (C (type .., type .., data ..)) -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C24 (C (type .., data .., ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C25 (C (type .., data .., type ..)) -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C26 (C (type .., data .., data ..)) -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C27 (C (data .., .., ..))           -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C28 (C (data .., .., type ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C29 (C (data .., .., data ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C30 (C (data .., type .., ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C31 (C (data .., type .., type ..)) -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C32 (C (data .., type .., data ..)) -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C33 (C (data .., data .., ..))      -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C34 (C (data .., data .., type ..)) -- equivalent to C(..)
import T25901_sub_w3_helper qualified as C35 (C (type .., type .., type ..)) -- equivalent to C(type ..)
import T25901_sub_w3_helper qualified as C36 (C (data .., data .., data ..)) -- equivalent to C(data ..)

import T25901_sub_w3_helper qualified as K1  (K (.., ..))                 -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K2  (K (.., type ..))            -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K3  (K (.., data ..))            -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K4  (K (type .., ..))            -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K5  (K (type .., data ..))       -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K6  (K (data .., ..))            -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K7  (K (data .., type ..))       -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K8  (K (type .., type ..))       -- equivalent to K(type ..)
import T25901_sub_w3_helper qualified as K9  (K (data .., data ..))       -- equivalent to K(data ..)

import T25901_sub_w3_helper qualified as K10 (K (.., .., ..))                -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K11 (K (.., .., type ..))           -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K12 (K (.., .., data ..))           -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K13 (K (.., type .., ..))           -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K14 (K (.., type .., type ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K15 (K (.., type .., data ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K16 (K (.., data .., ..))           -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K17 (K (.., data .., type ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K18 (K (.., data .., data ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K19 (K (type .., .., ..))           -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K20 (K (type .., .., type ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K21 (K (type .., .., data ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K22 (K (type .., type .., ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K23 (K (type .., type .., data ..)) -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K24 (K (type .., data .., ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K25 (K (type .., data .., type ..)) -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K26 (K (type .., data .., data ..)) -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K27 (K (data .., .., ..))           -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K28 (K (data .., .., type ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K29 (K (data .., .., data ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K30 (K (data .., type .., ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K31 (K (data .., type .., type ..)) -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K32 (K (data .., type .., data ..)) -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K33 (K (data .., data .., ..))      -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K34 (K (data .., data .., type ..)) -- equivalent to K(..)
import T25901_sub_w3_helper qualified as K35 (K (type .., type .., type ..)) -- equivalent to K(type ..)
import T25901_sub_w3_helper qualified as K36 (K (data .., data .., data ..)) -- equivalent to K(data ..)

import T25901_sub_w3_helper qualified as X1 (T (MkT, data ..), C (F, data ..))
import T25901_sub_w3_helper qualified as X2 (C (type .., f))
import T25901_sub_w3_helper qualified as X3 (K (data .., type TNum))

-- Verify the "equivalent to" claims using Template Haskell name lookup.
-- A 'fail' in the splice means the actual semantics disagree with the comment.
$(do
  let checkT dataScope n = do
        expect (DataName, dataScope, "T" ++ shows n ".MkT")
        expect (DataName, dataScope, "T" ++ shows n ".unT")
      checkC typeScope dataScope n = do
        expect (TypeName, typeScope,  "C" ++ shows n ".F")
        expect (DataName, dataScope,  "C" ++ shows n ".f")
        expect (DataName, dataScope,  "C" ++ shows n ".#")
        expect (TypeName, NotInScope, "C" ++ shows n ".#")  -- type (#) never exported
      checkK typeScope n = do
        expect (TypeName, typeScope,  "K" ++ shows n ".TNum")
        expect (TypeName, typeScope,  "K" ++ shows n ".TStr")
        expect (TypeName, NotInScope, "K" ++ shows n ".TBool")  -- TBool never exported

  mapM_ (checkT InScope)    [1..7]        -- T(..)
  mapM_ (checkT NotInScope) [8]           -- T(type ..)
  mapM_ (checkT InScope)    [9]           -- T(data ..)

  mapM_ (checkT InScope)    [10..34]      -- T(..)
  mapM_ (checkT NotInScope) [35]          -- T(type ..)
  mapM_ (checkT InScope)    [36]          -- T(data ..)

  mapM_ (checkC InScope InScope)    [1..7]      -- C(..)
  mapM_ (checkC InScope NotInScope) [8]         -- C(type ..)
  mapM_ (checkC NotInScope InScope) [9]         -- C(data ..)

  mapM_ (checkC InScope InScope)    [10..34]    -- C(..)
  mapM_ (checkC InScope NotInScope) [35]        -- C(type ..)
  mapM_ (checkC NotInScope InScope) [36]        -- C(data ..)

  -- K(..) = K(type ..) for type data (constructors in type namespace)
  mapM_ (checkK InScope)    [1..8]        -- K(..) or K(type ..): TNum in type ns
  mapM_ (checkK NotInScope) [9]           -- K(data ..): no type-ns subordinates

  mapM_ (checkK InScope)    [10..35]      -- K(..) or K(type ..): TNum in type ns
  mapM_ (checkK NotInScope) [36]          -- K(data ..): no type-ns subordinates

  -- X1: T (MkT, data ..), C (F, data ..)
  expect (DataName, InScope,    "X1.MkT")
  expect (DataName, InScope,    "X1.unT")
  expect (TypeName, InScope,    "X1.F")
  expect (DataName, InScope,    "X1.f")
  expect (DataName, InScope,    "X1.#")
  expect (TypeName, NotInScope, "X1.#")

  -- X2: C (type .., f)
  expect (TypeName, InScope,    "X2.F")
  expect (DataName, InScope,    "X2.f")
  expect (DataName, NotInScope, "X2.#")
  expect (TypeName, NotInScope, "X2.#")

  -- X3: K (data .., type TNum)
  expect (TypeName, InScope,    "X3.TNum")
  expect (TypeName, NotInScope, "X3.TStr")
  expect (TypeName, NotInScope, "X3.TBool")

  return []
 )

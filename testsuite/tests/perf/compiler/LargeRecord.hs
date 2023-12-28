{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -freduction-depth=0 #-}

{- Notes on LargeRecord
~~~~~~~~~~~~~~~~~~~~~~~
Subject to Note [Sensitivity to unique increment] in T12545.hs with spread of 2.2%


I noticed that in GHC of July 2022, when compiling this
module I got lots of "SPEC" rules

      SuperRecord.$fRecCopy:ltsrts_$crecCopyInto @"f2"
                                                 @'["f1" := Int, "f2" := Int, "f3" := Int,
                                                    "f4" := Int]
                                                 @Int
                                                 @'["f2" := Int, "f3" := Int, "f4" := Int]
                                                 @'["f3" := Int, "f4" := Int]
                                                 $d(%,,%)_X1 $d(%,,%)1_X2 $dRecCopy_X3
      SuperRecord.$fRecCopy:ltsrts_$crecCopyInto @"f3"
                                                 @'["f1" := Int, "f2" := Int, "f3" := Int,
                                                    "f4" := Int]
                                                 @Int
                                                 @'["f2" := Int, "f3" := Int, "f4" := Int]
                                                 @'["f4" := Int]
                                                 $d(%,,%)_X1 $d(%,,%)1_s6yK $dRecCopy_X2

      SuperRecord.$fRecCopy:ltsrts_$crecCopyInto @"f3"
                                                 @'["f2" := Int, "f3" := Int, "f4" := Int]
                                                 @Int
                                                 @'["f3" := Int, "f4" := Int]
                                                 @'["f4" := Int]
                                                 $d(%,,%)_s6yr $d(%,,%)1_X1 $dRecCopy_X2
      SuperRecord.$fRecCopy:ltsrts_$crecCopyInto @"f4"
                                                 @(SortInsert'
                                                     (GHC.TypeLits.Internal.CmpSymbol "f3" "f4")
                                                     ("f3" := Int)
                                                     ("f4" := Int)
                                                     '[])
                                                 @Int
                                                 @'["f4" := Int]
                                                 @'[]
                                                 $d(%,,%)_X1 $d(%,,%)1_X2 $dRecCopy_s6yb

(This was with BigFieldList having only four elements.)

The relevant function SuperRecord.$fRecCopy:ltsrts_$crecCopyInto is
only a wrapper that we were specialising -- little or no benefit.  We
don't want to specialise wrappers!  -}

module DCo_Record where

import SuperRecord

type BigFieldList =
  '[ "f1" := Int
   , "f2" := Int
   , "f3" := Int
   , "f4" := Int
   , "f5" := Int
   , "f6" := Int
   , "f7" := Int
   , "f8" := Int
   , "f9" := Int
   , "f10" := Int
   , "f11" := Int
   , "f12" := Int
   , "f13" := Int
   , "f14" := Int
   , "f15" := Int
   ]

bigRec :: Record BigFieldList
bigRec =
    #f1   := 1
  & #f2   := 2
  & #f3   := 3
  & #f4   := 4
  & #f5   := 5
  & #f6   := 6
  & #f7   := 7
  & #f8   := 8
  & #f9   := 9
  & #f10  := 10
  & #f11  := 11
  & #f12  := 12
  & #f13  := 13
  & #f14  := 14
  & #f15  := 15
  & rnil

main :: IO ()
main = print "ok"

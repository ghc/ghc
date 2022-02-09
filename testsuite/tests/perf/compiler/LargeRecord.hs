{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -freduction-depth=0 #-}

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
   , "f16" := Int
   , "f17" := Int
   , "f18" := Int
   , "f19" := Int
   , "f20" := Int
   , "f21" := Int
   , "f22" := Int
   , "f23" := Int
   , "f24" := Int
   , "f25" := Int
   , "f26" := Int
   , "f27" := Int
   , "f28" := Int
   , "f29" := Int
   , "f30" := Int
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
  & #f16  := 16
  & #f17  := 17
  & #f18  := 18
  & #f19  := 19
  & #f20  := 20
  & #f21  := 21
  & #f22  := 22
  & #f23  := 23
  & #f24  := 24
  & #f25  := 25
  & #f26  := 26
  & #f27  := 27
  & #f28  := 28
  & #f29  := 29
  & #f30  := 30
  & rnil

main :: IO ()
main = print "ok"

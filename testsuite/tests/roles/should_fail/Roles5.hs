module Roles5 where

data T a
class C a
type S a = Int

type role T nominal
type role C representational
type role S phantom
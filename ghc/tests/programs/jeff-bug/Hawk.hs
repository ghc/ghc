module Hawk
   (
      module Arithmetic
     ,module Cell
     ,module Devices
     ,module HawkIO
     ,module Memory
     ,module Register
     ,module STEx
     ,module Signal
     ,module Utilities
     ,module VRegister
     ,module Words
     ,module Instruction
     ,module Probe
     ,module PipeReg
     ,module Init
   ) where

import Arithmetic
import Cell
import Devices
import HawkIO
import Memory
import Register
import STEx
import Signal
import Utilities
import VRegister
import Words
import Instruction
import Probe
import PipeReg
import Init


-- This serves as the top-level module, with perhaps some instance
-- declarations:

instance (Show a, Show b, Show c, Show d, Show e,Show f) => 
              Show (a,b,c,d,e,f) where
       showsPrec n (a,b,c,d,e,f) s = ('(' : ) $
                                     showsPrec n a $  (',' :) $
                                     showsPrec n b $  (',' :) $
                                     showsPrec n c $  (',' :) $
                                     showsPrec n d $  (',' :) $
                                     showsPrec n e $  (',' :) $
                                     showsPrec n f $ ')' : s

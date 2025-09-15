module BootFldReexport where

import {-# SOURCE #-} BootFldReexport_N
  ( fld {- variable -} )
import BootFldReexport_O
  ( fld {- record field -} )

test3 = fld

{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Prelude.Compat (
  module Base
#if !(MIN_VERSION_base(4,9,0))
, Fail.MonadFail(Fail.fail)
, Semi.Semigroup((Semi.<>))
#endif
) where

import "base-compat" Prelude.Compat as Base
#if !(MIN_VERSION_base(4,9,0))
  hiding (fail)
#endif

#if !(MIN_VERSION_base(4,9,0))
import "fail"       Control.Monad.Fail as Fail
import "semigroups" Data.Semigroup as Semi
#endif

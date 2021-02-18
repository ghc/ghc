-- | Types that govern unboxing decisisions of the worker/wrapper transformation.
-- Concrete 'UnboxingStrategy's are defined in "GHC.Core.Opt.WorkWrap.Utils".
module GHC.Types.Unbox (
    DataConPatContext(..), UnboxingDecision(..), UnboxingStrategy
  ) where

import GHC.Core.Coercion
import GHC.Core.DataCon
import GHC.Core.Type

-- | The information needed to build a pattern for a DataCon to be unboxed.
-- The pattern can be generated from 'dcpc_dc' and 'dcpc_tc_args' via
-- 'GHC.Core.Utils.dataConRepInstPat'. The coercion 'dcpc_co' is for newtype
-- wrappers.
--
-- If we get `DataConPatContext dc tys co` for some type `ty`
-- and `dataConRepInstPat ... dc tys = (exs, flds)`, then
--
--   * `dc @exs flds :: T tys`
--   * `co :: T tys ~ ty`
data DataConPatContext
  = DataConPatContext
  { dcpc_dc      :: !DataCon
  , dcpc_tc_args :: ![Type]
  , dcpc_co      :: !Coercion
  }

-- | Describes the outer shape of an argument to be unboxed or left as-is
-- Depending on how `s` is instantiated (e.g., 'Demand').
data UnboxingDecision s
  = StopUnboxing
  -- ^ We ran out of strictness info. Leave untouched.
  | Unbox !DataConPatContext [s]
  -- ^ The argument is used strictly or the returned product was constructed, so
  -- unbox it.
  -- The 'DataConPatContext' carries the bits necessary for
  -- instantiation with 'dataConRepInstPat'.
  -- The `[s]` carries the bits of information with which we can continue
  -- unboxing, e.g. `s` will be 'Demand'.

-- | Encapsulates whether and how to unbox an argument or field of the given
-- type by looking at an `s` (e.g. 'Demand'). Concrete implementation in
-- "GHC.Core.Opt.WorkWrap.Utils".
type UnboxingStrategy s = Type -> s -> UnboxingDecision s

module HsUtils where
import HsBinds
import SrcLoc


-- | We have to be careful to normalise @SrcSpanLess (LHsBind)@ to
-- @LHsBindLR l r@ before passing the representative of @unLoc bind@ on to
-- @mkOneConFull@, otherwise this triggers a panic in @zipTvSubst@.
addPatSynSelector:: LHsBind p -> [a]
addPatSynSelector bind
  | PatSynBind _ _ <- unLoc bind
  = []

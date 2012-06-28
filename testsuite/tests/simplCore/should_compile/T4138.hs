
module T4138 where

import T4138_A

-- We NOINLINE f because we want to count the number of F#s in the
-- -ddump-simpl output, so we don't want to be confused by F#s
-- appearing in the inlining
{-# NOINLINE f #-}
f :: (Float, Float) -> ()
f = rnf

{-
We're hoping that the output will include something like:

  \ (ds_dvm :: (GHC.Types.Float, GHC.Types.Float)) ->
    case ds_dvm of _ { (x_aux, y_auy) ->
    case x_aux of _ { GHC.Types.F# ipv_svw ->
    case y_auy of _ { GHC.Types.F# ipv_svx -> GHC.Unit.() } } }
-}

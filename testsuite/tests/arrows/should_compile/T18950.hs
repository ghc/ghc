{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
-- NOTE: use ExistentialQuantification for non-GADT 'data Point' declaration
-- {-# LANGUAGE ExistentialQuantification #-}
{--
Issue: Compiling `Example.hs` causes ghc to panic (happens in 8.8.x, 8.10.x, and 9.0.x)

Reproduce:
```bash
nix-shell -I nixpkgs=https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz -p haskell.compiler.ghc901
ghc Example.hs
```

Results:

ghc: panic! (the 'impossible' happened)
  (GHC version 9.0.0.20200925:
    GHC.StgToCmm.Env: variable not found
  $dRealFloat_a1tn
  local binds for:
  pos
  Point
  Obj
  $dArrow_s1xr
  ds_s1xs
  ds1_s1xt
  ds2_s1ya
  ds3_s1yb
  ds4_s1yc
  ds6_s1ye
  sat_s1yf
  sat_s1yg
  sat_s1yh
  sat_s1yi
  sat_s1yj
  sat_s1yk
  sat_s1yl
  sat_s1ym
  sat_s1yn
  Call stack:
      CallStack (from HasCallStack):
        callStackDoc, called at compiler/GHC/Utils/Outputable.hs:1230:37 in ghc:GHC.Utils.Outputable
        pprPanic, called at compiler/GHC/StgToCmm/Env.hs:152:9 in ghc:GHC.StgToCmm.Env

Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
--}
{--
```bash
ghc -dcore-lint Example.hs
```
--}
module Example (
-- NOTE: no error if `step` not exported
  step,
) where

import Control.Arrow

-- NOTE: existentially qualified declaration of Point,
--       mirrors definition of 'Point2' in package 'simple-affine-space'
-- import Data.Point2

-- NOTE: no error without 'RealFloat a` context; both examples fail
--data Point a = RealFloat a => Point !a
data Point a where Point :: RealFloat a => a -> Point a

type Position = Point Float

ptrPos :: Arrow a => a Obj Position
ptrPos = arr pos

data Obj = Obj { pos :: !Position }

step :: Arrow a => a Obj Obj
step = proc gi -> do
  -- NOTE: no error without this arrow line; no error if not deconstructed.
  (Point _) <- ptrPos -< gi
  {--
  -- NOTE: this code does work (in place of `(Point _) <- ptrPos -< gi` above)
  pt <- ptrPos -< gi
  let (Point _) = pt
  --}
  returnA -< Obj {
               -- NOTE: no error without this 'pos' field
               pos = (Point 0)
             }


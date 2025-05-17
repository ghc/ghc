{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Bug where

import Data.Kind

-- This nasty example fails with quick-look
-- (which here is switched on by ($))
-- because of a very subtle issue where we instantiate an
-- instantiation variable with (F alpha), where F is a type function
--
-- It's a stripped-down version of T5490

register :: forall rs op_ty.
           (HDrop rs ~ HSingle (WaitOpResult op_ty))
        => rs -> op_ty
        -> Maybe (WaitOpResult (WaitOps rs))
        -> Int
register _ _ ev
      = ($) -- (px -> qx) -> px -> qx   px=a_a2iT   qx=b_a2iU
                (foo ev)   -- Instantiate at ax=a2iW bx=a2iX;
                                  --    (ax,bx) -> Int
                                  -- ql on arg ev   bx := WaitOpResult (WaitOps rs) = [rs]
                   -- px := (ax,bx)
                   -- qx := Int
                (inj 3)        -- instantiate lx=l_a2iZ;
                               -- res_ty    (HNth n lx, [lx])
                   -- res_ty px = (ax,bx) ~ (HNth n lx, [lx])
                   -- ax := HNth n lx
                   -- bx := [lx]
          -- Result ql: WaitOpResult op_ty ~ ax = HNth n lx

{-
    ($) @(HNth l0, WR (WO rs))
        @Int
        (foo ev)
        (inj 3)
-}


foo :: forall a b . Maybe b -> (a,b) -> Int
foo = error "urk"

inj :: Int -> (HNth l, [l])
inj = error "urk"

data HSingle (a :: Type)
type family HHead l
type instance HHead (HSingle h) = h

data WaitOps (rs :: Type)
type family WaitOpResult op
type instance WaitOpResult (WaitOps rs) = [rs]

type family HDrop l

type HNth l = HHead (HDrop l)




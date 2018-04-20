{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module T12135 where


import Data.Kind (Type)


class sub :<: sup
  where
    -- | Injection from @sub@ to @sup@
    inj ::  sub sig -> sup sig

instance (sub :<: sup) => (sub :<: AST sup)
  where
    inj = Sym . inj


data Sig a = Full a | (:->) a (Sig a)

infixr :->

type Sym t = (Sig t -> Type)

data AST :: Sym t -> Sym t
  where
    Sym  :: sym sig -> AST sym sig
    (:$) :: AST sym (a :-> sig) -> AST sym (Full a) -> AST sym sig

-- | Generic N-ary syntactic construct
--
-- 'Construct' gives a quick way to introduce a syntactic construct by giving its name and semantic
-- function.
data Constr sig = Constr String

smartSym :: (sub :<: sup) => sub sig -> SmartFun sup sig
smartSym = error "urk"

type family   SmartFun (sym :: Sig t -> Type) sig = f | f -> sym sig  where
-- In full glory:
-- type family SmartFun {t} (sym :: Sig t -> Type) (sig :: Sig t) :: *
--            = f | f -> {t} sym sig  where
  SmartFun sym (Full a)    = AST sym (Full a)
  SmartFun sym (a :-> sig) = AST sym (Full a) -> SmartFun sym sig

-- | Get the length of an array
arrLen ::  AST Constr (Full [a]) -> AST Constr (Full Int)
arrLen = smartSym sym where
  sym = Constr "arrLen"



{- The original bug was a failure to substitute
   properly during type-function improvement.

--------------------------------------
doTopReact
  [WD] hole{a1y1} {0}:: (SmartFun
                           t_a1x9[tau:2] sup_a1xb[tau:2] sig_a1xc[tau:2] :: *)
                        GHC.Prim.~#
                        (s_a1y0[fuv:0] :: *) (CFunEqCan)
matchFamTcM
  Matching: SmartFun t_a1x9[tau:2] sup_a1xb[tau:2] sig_a1xc[tau:2]
  Match failed
improveTopFunEqs
  SmartFun [t_a1x9[tau:2], sup_a1xb[tau:2],
            sig_a1xc[tau:2]] s_a1y0[fuv:0]
  [* ~ t_a1x9[tau:2], Constr (Sig *) ~ sup_a1xb[tau:2],
   (':->) * [a_a1wT[sk:2]] sig_a1y3[tau:2] ~ sig_a1xc[tau:2]]

Emitting new derived equality
  [D] _ {0}:: (* :: *) GHC.Prim.~# (t_a1x9[tau:2] :: *)
  [D] _ {0}:: (Constr (Sig *) :: (Sig * -> *))
                GHC.Prim.~#
              (sup_a1xb[tau:2] :: (Sig t_a1x9[tau:2] -> *))
  [D] _ {0}:: ((':->) * [a_a1wT[sk:2]] sig_a1y3[tau:2] :: Sig *)
              GHC.Prim.~#
              (sig_a1xc[tau:2] :: Sig t_a1x9[tau:2])

but sig_a1y3 :: Sig t   BAD
--------------------------------------

Instance
  forall t (sig :: Sig t) (a :: t) (sym :: Sig t -> *).
    SmartFun t sym ((:->) t a sig) = AST t sym (Full t a) -> SmartFun t sym sig

Wanted:
  SmartFun t_a1x9[tau:2] sup_a1xb[tau:2] sig_a1xc[tau:2] ~ s_a1y0[fuv:0]
  s_a1y0[fuv:0]  ~  AST * (Constr (Sig *))
                          ('Full * [a_a1wT[sk:2]])
                    -> AST * (Constr (Sig *)) ('Full * Int)

Substitution after matching RHSs
  [ t -> *
  , (sym :: Sig t -> *) -> Constr (Sig *)
  , a -> a_a1wT ]

add sig -> sig0 :: Sig t   -- Or rather Sig *

Apply to LHS

   SmartFun * (Constr (Sig *)) ((:->) * a_a1wT sig0)


improveTopFunEqs
  SmartFun [t_a1x9[tau:2], sup_a1xb[tau:2],
            sig_a1xc[tau:2]] s_a1y0[fuv:0]
  [* ~ t_a1x9[tau:2], Constr (Sig *) ~ sup_a1xb[tau:2],
   (':->) * [a_a1wT[sk:2]] sig_a1y3[tau:2] ~ sig_a1xc[tau:2]]
-}

{-# LANGUAGE TypeOperators, StarIsType
           , DataKinds
           , PolyKinds
           , TypeFamilies
           , GADTs
           , UndecidableInstances
           , RankNTypes
           , ScopedTypeVariables
  #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -O1 -fspec-constr #-}

{-

With reversed order of allocated uniques the type variables would be in
wrong order:

*** Core Lint errors : in result of SpecConstr ***
determ004.hs:88:12: warning:
    [in body of lambda with binder m_azbFg :: a_afdP_azbON]
    @ (a_afdP_azbON :: BOX) is out of scope
*** Offending Program ***

...

Rec {
$s$wsFoldr1_szbtK
  :: forall (m_azbFg :: a_afdP_azbON)
            (x_azbOM :: TyFun
                          a_afdP_azbON (TyFun a_afdP_azbON a_afdP_azbON -> *)
                        -> *)
            (a_afdP_azbON :: BOX)
            (ipv_szbwN :: a_afdP_azbON)
            (ipv_szbwO :: [a_afdP_azbON]).
     R:Sing[]z (ipv_szbwN : ipv_szbwO)
     ~R# Sing (Apply (Apply (:$) ipv_szbwN) ipv_szbwO)
     -> Sing ipv_szbwO
     -> Sing ipv_szbwN
     -> (forall (t_azbNM :: a_afdP_azbON).
         Sing t_azbNM -> Sing (Apply x_azbOM t_azbNM))
     -> Sing
          (Apply
             (Apply Foldr1Sym0 x_azbOM)
             (Let1627448493XsSym4 x_azbOM m_azbFg ipv_szbwN ipv_szbwO))
[LclId,
 Arity=4,
 Str=<L,U><L,U><L,U><C(S(C(S))),C(U(1*C1(U)))>]
$s$wsFoldr1_szbtK =
  \ (@ (m_azbFg :: a_afdP_azbON))
    (@ (x_azbOM :: TyFun
                     a_afdP_azbON (TyFun a_afdP_azbON a_afdP_azbON -> *)
                   -> *))
    (@ (a_afdP_azbON :: BOX))
    (@ (ipv_szbwN :: a_afdP_azbON))
    (@ (ipv_szbwO :: [a_afdP_azbON]))
    (sg_szbtL
       :: R:Sing[]z (ipv_szbwN : ipv_szbwO)
          ~R# Sing (Apply (Apply (:$) ipv_szbwN) ipv_szbwO))
    (sc_szbtM :: Sing ipv_szbwO)
    (sc_szbtN :: Sing ipv_szbwN)
    (sc_szbtP
       :: forall (t_azbNM :: a_afdP_azbON).
          Sing t_azbNM -> Sing (Apply x_azbOM t_azbNM)) ->
    case (SCons
            @ a_afdP_azbON
            @ (ipv_szbwN : ipv_szbwO)
            @ ipv_szbwO
            @ ipv_szbwN
            @~ (<ipv_szbwN : ipv_szbwO>_N
                :: (ipv_szbwN : ipv_szbwO) ~# (ipv_szbwN : ipv_szbwO))
            sc_szbtN
            sc_szbtM)
         `cast` (sg_szbtL
                 ; TFCo:R:Sing[]z[0] <a_afdP_azbON>_N <Let1627448493XsSym4
                                                         x_azbOM m_azbFg ipv_szbwN ipv_szbwO>_N
                 :: R:Sing[]z (ipv_szbwN : ipv_szbwO)
                    ~R# R:Sing[]z
                          (Let1627448493XsSym4 x_azbOM m_azbFg ipv_szbwN ipv_szbwO))
    of wild_XD {
      SNil dt_dzbxX ->
        (lvl_szbwi @ a_afdP_azbON)
        `cast` ((Sing
                   (Sym (TFCo:R:Foldr1[2] <a_afdP_azbON>_N <x_azbOM>_N)
                    ; Sym
                        (TFCo:R:Apply[]kFoldr1Sym1l_afe2[0]
                           <a_afdP_azbON>_N <'[]>_N <x_azbOM>_N)
                    ; (Apply
                         (Sym
                            (TFCo:R:Apply(->)(->)Foldr1Sym0l[0] <a_afdP_azbON>_N <x_azbOM>_N))
                         (Sym dt_dzbxX))_N))_R
                :: Sing (Apply ErrorSym0 "Data.Singletons.List.foldr1: empty list")
                   ~R# Sing
                         (Apply
                            (Apply Foldr1Sym0 x_azbOM)
                            (Let1627448493XsSym4 x_azbOM m_azbFg ipv_szbwN ipv_szbwO)));
      SCons @ n_azbFh @ m_XzbGe dt_dzbxK _sX_azbOH
            ds_dzbyu [Dmd=<S,1*U>] ->
        case ds_dzbyu
             `cast` (TFCo:R:Sing[]z[0] <a_afdP_azbON>_N <n_azbFh>_N
                     :: Sing n_azbFh ~R# R:Sing[]z n_azbFh)
        of wild_Xo {
          SNil dt_dzbxk ->
            (lvl_szbw1 @ a_afdP_azbON @ m_XzbGe)
            `cast` ((Sing
                       (Sym (TFCo:R:Foldr1[0] <a_afdP_azbON>_N <m_XzbGe>_N <x_azbOM>_N)
                        ; Sym
                            (TFCo:R:Apply[]kFoldr1Sym1l_afe2[0]
                               <a_afdP_azbON>_N <'[m_XzbGe]>_N <x_azbOM>_N)
                        ; (Apply
                             (Sym
                                (TFCo:R:Apply(->)(->)Foldr1Sym0l[0] <a_afdP_azbON>_N <x_azbOM>_N))
                             ((<m_XzbGe>_N ': Sym dt_dzbxk)_N ; Sym dt_dzbxK))_N))_R
                    :: Sing m_XzbGe
                       ~R# Sing
                             (Apply
                                (Apply Foldr1Sym0 x_azbOM)
                                (Let1627448493XsSym4 x_azbOM m_azbFg ipv_szbwN ipv_szbwO)));
          SCons @ ipv_XzbxR @ ipv_XzbyV ipv_szbwM ipv_szbwL ipv_szbwK ->
            case (sc_szbtP @ m_XzbGe _sX_azbOH)
                 `cast` (TFCo:R:Sing(->)f[0]
                           <a_afdP_azbON>_N <a_afdP_azbON>_N <Apply x_azbOM m_XzbGe>_N
                         :: Sing (Apply x_azbOM m_XzbGe)
                            ~R# R:Sing(->)f (Apply x_azbOM m_XzbGe))
            of wild_X3X { SLambda ds_XzbBr [Dmd=<C(S),1*C1(U)>] ->
            (ds_XzbBr
               @ (Foldr1 x_azbOM (ipv_XzbyV : ipv_XzbxR))
               (($wsFoldr1_szbuc
                   @ a_afdP_azbON
                   @ x_azbOM
                   @ (Let1627448493XsSym4 x_azbOM m_XzbGe ipv_XzbyV ipv_XzbxR)
                   sc_szbtP
                   ((SCons
                       @ a_afdP_azbON
                       @ (ipv_XzbyV : ipv_XzbxR)
                       @ ipv_XzbxR
                       @ ipv_XzbyV
                       @~ (<ipv_XzbyV : ipv_XzbxR>_N
                           :: (ipv_XzbyV : ipv_XzbxR) ~# (ipv_XzbyV : ipv_XzbxR))
                       ipv_szbwL
                       ipv_szbwK)
                    `cast` (Sym (TFCo:R:Sing[]z[0] <a_afdP_azbON>_N) (Sym
                                                                        (TFCo:R:Apply[][]:$$i[0]
                                                                           <a_afdP_azbON>_N
                                                                           <ipv_XzbxR>_N
                                                                           <ipv_XzbyV>_N)
                                                                      ; (Apply
                                                                           (Sym
                                                                              (TFCo:R:Applyk(->):$l[0]
                                                                                 <a_afdP_azbON>_N
                                                                                 <ipv_XzbyV>_N))
                                                                           <ipv_XzbxR>_N)_N)
                            :: R:Sing[]z (ipv_XzbyV : ipv_XzbxR)
                               ~R# Sing (Apply (Apply (:$) ipv_XzbyV) ipv_XzbxR))))
                `cast` ((Sing
                           ((Apply
                               (TFCo:R:Apply(->)(->)Foldr1Sym0l[0] <a_afdP_azbON>_N <x_azbOM>_N)
                               <Let1627448493XsSym4 x_azbOM m_XzbGe ipv_XzbyV ipv_XzbxR>_N)_N
                            ; TFCo:R:Apply[]kFoldr1Sym1l_afe2[0]
                                <a_afdP_azbON>_N
                                ((Apply
                                    (TFCo:R:Applyk(->):$l[0] <a_afdP_azbON>_N <ipv_XzbyV>_N)
                                    <ipv_XzbxR>_N)_N
                                 ; TFCo:R:Apply[][]:$$i[0]
                                     <a_afdP_azbON>_N <ipv_XzbxR>_N <ipv_XzbyV>_N)
                                <x_azbOM>_N))_R
                        :: Sing
                             (Apply
                                (Apply Foldr1Sym0 x_azbOM)
                                (Let1627448493XsSym4 x_azbOM m_XzbGe ipv_XzbyV ipv_XzbxR))
                           ~R# Sing (Foldr1Sym2 x_azbOM (ipv_XzbyV : ipv_XzbxR)))))
            `cast` ((Sing
                       ((Apply
                           <Apply x_azbOM m_XzbGe>_N
                           (Sym
                              (TFCo:R:Apply[]kFoldr1Sym1l_afe2[0]
                                 <a_afdP_azbON>_N <ipv_XzbyV : ipv_XzbxR>_N <x_azbOM>_N)
                            ; (Apply
                                 (Sym
                                    (TFCo:R:Apply(->)(->)Foldr1Sym0l[0]
                                       <a_afdP_azbON>_N <x_azbOM>_N))
                                 (Sym
                                    (TFCo:R:Apply[][]:$$i[0]
                                       <a_afdP_azbON>_N <ipv_XzbxR>_N <ipv_XzbyV>_N)
                                  ; (Apply
                                       (Sym
                                          (TFCo:R:Applyk(->):$l[0] <a_afdP_azbON>_N <ipv_XzbyV>_N))
                                       <ipv_XzbxR>_N)_N))_N))_N
                        ; Sym
                            (TFCo:R:Foldr1[1]
                               <a_afdP_azbON>_N
                               <ipv_XzbxR>_N
                               <ipv_XzbyV>_N
                               <m_XzbGe>_N
                               <x_azbOM>_N)
                        ; Sym
                            (TFCo:R:Apply[]kFoldr1Sym1l_afe2[0]
                               <a_afdP_azbON>_N <m_XzbGe : ipv_XzbyV : ipv_XzbxR>_N <x_azbOM>_N)
                        ; (Apply
                             (Sym
                                (TFCo:R:Apply(->)(->)Foldr1Sym0l[0] <a_afdP_azbON>_N <x_azbOM>_N))
                             ((<m_XzbGe>_N ': Sym ipv_szbwM)_N ; Sym dt_dzbxK))_N))_R
                    :: Sing
                         (Apply
                            (Apply x_azbOM m_XzbGe)
                            (Foldr1Sym2 x_azbOM (ipv_XzbyV : ipv_XzbxR)))
                       ~R# Sing
                             (Apply
                                (Apply Foldr1Sym0 x_azbOM)
                                (Let1627448493XsSym4 x_azbOM m_azbFg ipv_szbwN ipv_szbwO)))
            }
        }
    }
...
-}

module List (sFoldr1) where

import Data.Kind (Type)

data Proxy t

data family Sing (a :: k)

data TyFun (a :: Type) (b :: Type)

type family Apply (f :: TyFun k1 k2 -> Type) (x :: k1) :: k2

data instance Sing (f :: TyFun k1 k2 -> Type) =
  SLambda { applySing :: forall t. Sing t -> Sing (Apply f t) }

type SingFunction1 f = forall t. Sing t -> Sing (Apply f t)

type SingFunction2 f = forall t. Sing t -> SingFunction1 (Apply f t)
singFun2 :: Proxy f -> SingFunction2 f -> Sing f
singFun2 _ f = SLambda (\x -> SLambda (f x))

data (:$$) (j :: a) (i :: TyFun [a] [a])
type instance Apply ((:$$) j) i = (:) j i

data (:$) (l :: TyFun a (TyFun [a] [a] -> Type))
type instance Apply (:$) l = (:$$) l
data instance Sing (z :: [a])
  = z ~ '[] =>
    SNil
  | forall (m :: a)
           (n :: [a]). z ~ (:) m n =>
    SCons (Sing m) (Sing n)

data ErrorSym0 (t1 :: TyFun k1 k2)

type Let1627448493XsSym4 t_afee t_afef t_afeg t_afeh = Let1627448493Xs t_afee t_afef t_afeg t_afeh

type Let1627448493Xs f_afe9
                     x_afea
                     wild_1627448474_afeb
                     wild_1627448476_afec =
    Apply (Apply (:$) wild_1627448474_afeb) wild_1627448476_afec
type Foldr1Sym2 (t_afdY :: TyFun a_afdP (TyFun a_afdP a_afdP -> Type)
                           -> Type)
                (t_afdZ :: [a_afdP]) =
    Foldr1 t_afdY t_afdZ
data Foldr1Sym1 (l_afe3 :: TyFun a_afdP (TyFun a_afdP a_afdP -> Type)
                           -> Type)
                (l_afe2 :: TyFun [a_afdP] a_afdP)
type instance Apply (Foldr1Sym1 l_afe3) l_afe2 = Foldr1Sym2 l_afe3 l_afe2

data Foldr1Sym0 (l_afe0 :: TyFun (TyFun a_afdP (TyFun a_afdP a_afdP
                                                -> Type)
                                  -> Type) (TyFun [a_afdP] a_afdP -> Type))
type instance Apply Foldr1Sym0 l = Foldr1Sym1 l

type family Foldr1 (a_afe5 :: TyFun a_afdP (TyFun a_afdP a_afdP
                                            -> Type)
                              -> Type)
                   (a_afe6 :: [a_afdP]) :: a_afdP where
  Foldr1 z_afe7 '[x_afe8] = x_afe8
  Foldr1 f_afe9 ((:) x_afea ((:) wild_1627448474_afeb wild_1627448476_afec)) = Apply (Apply f_afe9 x_afea) (Apply (Apply Foldr1Sym0 f_afe9) (Let1627448493XsSym4 f_afe9 x_afea wild_1627448474_afeb wild_1627448476_afec))
  Foldr1 z_afew '[] = Apply ErrorSym0 "Data.Singletons.List.foldr1: empty list"

sFoldr1 ::
  forall a_afdP.
  forall (x :: TyFun a_afdP (TyFun a_afdP a_afdP -> Type) -> Type)
         (y :: [a_afdP]).
  Sing x
  -> Sing y -> Sing (Apply (Apply Foldr1Sym0 x) y)
sFoldr1 _ (SCons _sX SNil) = undefined
sFoldr1 sF (SCons sX (SCons sWild_1627448474 sWild_1627448476))
  = let
      lambda_afeC ::
        forall f_afe9 x_afea wild_1627448474_afeb wild_1627448476_afec.
        Sing f_afe9
        -> Sing x_afea
           -> Sing wild_1627448474_afeb
              -> Sing wild_1627448476_afec
                 -> Sing (Apply (Apply Foldr1Sym0 f_afe9) (Apply (Apply (:$) x_afea) (Apply (Apply (:$) wild_1627448474_afeb) wild_1627448476_afec)))
      lambda_afeC f_afeD x_afeE wild_1627448474_afeF wild_1627448476_afeG
        = let
            sXs ::
              Sing (Let1627448493XsSym4 f_afe9 x_afea wild_1627448474_afeb wild_1627448476_afec)
            sXs
              = applySing
                  (applySing
                     (singFun2 (undefined :: Proxy (:$)) SCons) wild_1627448474_afeF)
                  wild_1627448476_afeG
          in
            applySing
              (applySing f_afeD x_afeE)
              (applySing
                 (applySing (singFun2 (undefined :: Proxy Foldr1Sym0) sFoldr1) f_afeD)
                 sXs)
    in lambda_afeC sF sX sWild_1627448474 sWild_1627448476
sFoldr1 _ SNil = undefined

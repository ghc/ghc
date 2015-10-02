{-# LANGUAGE TypeOperators
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
ghc-stage2: panic! (the 'impossible' happened)
  (GHC version 7.11.20150723 for x86_64-unknown-linux):
        Template variable unbound in rewrite rule
-}

module List (sFoldr1) where

data Proxy t

data family Sing (a :: k)

data TyFun (a :: *) (b :: *)

type family Apply (f :: TyFun k1 k2 -> *) (x :: k1) :: k2

data instance Sing (f :: TyFun k1 k2 -> *) =
  SLambda { applySing :: forall t. Sing t -> Sing (Apply f t) }

type SingFunction1 f = forall t. Sing t -> Sing (Apply f t)

type SingFunction2 f = forall t. Sing t -> SingFunction1 (Apply f t)
singFun2 :: Proxy f -> SingFunction2 f -> Sing f
singFun2 _ f = SLambda (\x -> SLambda (f x))

data (:$$) (j :: a) (i :: TyFun [a] [a])
type instance Apply ((:$$) j) i = (:) j i

data (:$) (l :: TyFun a (TyFun [a] [a] -> *))
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
type Foldr1Sym2 (t_afdY :: TyFun a_afdP (TyFun a_afdP a_afdP -> *)
                           -> *)
                (t_afdZ :: [a_afdP]) =
    Foldr1 t_afdY t_afdZ
data Foldr1Sym1 (l_afe3 :: TyFun a_afdP (TyFun a_afdP a_afdP -> *)
                           -> *)
                (l_afe2 :: TyFun [a_afdP] a_afdP)
type instance Apply (Foldr1Sym1 l_afe3) l_afe2 = Foldr1Sym2 l_afe3 l_afe2

data Foldr1Sym0 (l_afe0 :: TyFun (TyFun a_afdP (TyFun a_afdP a_afdP
                                                -> *)
                                  -> *) (TyFun [a_afdP] a_afdP -> *))
type instance Apply Foldr1Sym0 l = Foldr1Sym1 l

type family Foldr1 (a_afe5 :: TyFun a_afdP (TyFun a_afdP a_afdP
                                            -> *)
                              -> *)
                   (a_afe6 :: [a_afdP]) :: a_afdP where
  Foldr1 z_afe7 '[x_afe8] = x_afe8
  Foldr1 f_afe9 ((:) x_afea ((:) wild_1627448474_afeb wild_1627448476_afec)) = Apply (Apply f_afe9 x_afea) (Apply (Apply Foldr1Sym0 f_afe9) (Let1627448493XsSym4 f_afe9 x_afea wild_1627448474_afeb wild_1627448476_afec))
  Foldr1 z_afew '[] = Apply ErrorSym0 "Data.Singletons.List.foldr1: empty list"

sFoldr1 ::
  forall (x :: TyFun a_afdP (TyFun a_afdP a_afdP -> *) -> *)
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

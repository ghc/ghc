{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CoOpt_Singletons where

-- base
import Data.Kind
  ( Type, Constraint )

{-------------------------------------------------------------------------------
This module extracts out everything needed to compile the following code
from Data.Foldable.Singletons in singletons-base:

$(singletonsOnly [d|
  deriving instance Foldable ((,) a)
  |])

Except that we replace (,) with the custom Tuple2 datatype defined below.
-}

type Apply :: (k1 ~> k2) -> k1 -> k2
type family Apply (f :: k1 ~> k2) (x :: k1) :: k2

type (@@) :: (k1 ~> k2) -> k1 -> k2
type a @@ b = Apply a b
infixl 9 @@

type TyFun :: Type -> Type -> Type
data TyFun :: Type -> Type -> Type

type (~>) :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type SameKind :: k -> k -> Constraint
type SameKind (a :: k) (b :: k) = (() :: Constraint)

type Sing :: k -> Type
type family Sing :: k -> Type

type SLambda :: (k1 ~> k2) -> Type
newtype SLambda (f :: k1 ~> k2) =
  SLambda { applySing :: forall t. Sing t -> Sing (f @@ t) }
type instance Sing = SLambda

type SingFunction1 :: (a1 ~> b) -> Type
type SingFunction2 :: (a1 ~> a2 ~> b) -> Type
type SingFunction3 :: (a1 ~> a2 ~> a3 ~> b) -> Type

type SingFunction1 (f :: a1 ~> b) =
  forall t. Sing t -> Sing (f @@ t)
singFun1 :: forall f. SingFunction1 f -> Sing f
singFun1 f = SLambda f

type SingFunction2 (f :: a1 ~> a2 ~> b) =
  forall t1 t2. Sing t1 -> Sing t2 -> Sing (f @@ t1 @@ t2)
singFun2 :: forall f. SingFunction2 f -> Sing f
singFun2 f = SLambda (\x -> singFun1 (f x))

type SingFunction3 (f :: a1 ~> a2 ~> a3 ~> b) =
     forall t1 t2 t3.
     Sing t1 -> Sing t2 -> Sing t3
  -> Sing (f @@ t1 @@ t2 @@ t3)
singFun3 :: forall f. SingFunction3 f -> Sing f
singFun3 f = SLambda (\x -> singFun2 (f x))

------------------------------------------------------------

data Tuple2 a b = Tuple2 a b

type STuple2 :: Tuple2 a b -> Type
data STuple2 tup where
  STuple2 :: Sing a -> Sing b -> STuple2 ('Tuple2 a b)

type instance Sing @(Tuple2 a b) = STuple2

type Endo :: Type -> Type
newtype Endo a = Endo (a ~> a)
type SEndo :: Endo a -> Type
data SEndo e where
  SEndo :: Sing x -> SEndo ('Endo x)
type instance Sing = SEndo
type EndoSym0 :: (a ~> a) ~> Endo a
data EndoSym0 tf
type instance Apply EndoSym0 x = 'Endo x

{-
$(singletonsOnly [d|

  id                      :: a -> a
  id x                    =  x

  flip                    :: (a -> b -> c) -> b -> a -> c
  flip f x y              =  f y x

  (.)    :: (b -> c) -> (a -> b) -> a -> c
  (.) f g = \x -> f (g x)
  infixr 9 .

  ($!)                    :: (a -> b) -> a -> b
  f $! x                  = let {-!-}vx = x in f vx
  infixr 0 $!

 |])
-}

data Let6989586621679038864VxSym0 f6989586621679038862
  where
    Let6989586621679038864VxSym0KindInference :: SameKind (Apply Let6989586621679038864VxSym0 arg_a7yV) (Let6989586621679038864VxSym1 arg_a7yV) =>
                                                 Let6989586621679038864VxSym0 f6989586621679038862
type instance Apply Let6989586621679038864VxSym0 f6989586621679038862 = Let6989586621679038864VxSym1 f6989586621679038862
data Let6989586621679038864VxSym1 f6989586621679038862 x6989586621679038863
  where
    Let6989586621679038864VxSym1KindInference :: SameKind (Apply (Let6989586621679038864VxSym1 f6989586621679038862) arg_a7yV) (Let6989586621679038864VxSym2 f6989586621679038862 arg_a7yV) =>
                                                 Let6989586621679038864VxSym1 f6989586621679038862 x6989586621679038863
type instance Apply (Let6989586621679038864VxSym1 f6989586621679038862) x6989586621679038863 = Let6989586621679038864Vx f6989586621679038862 x6989586621679038863
type family Let6989586621679038864VxSym2 f6989586621679038862 x6989586621679038863 where
  Let6989586621679038864VxSym2 f6989586621679038862 x6989586621679038863 = Let6989586621679038864Vx f6989586621679038862 x6989586621679038863
type family Let6989586621679038864Vx f_a7yS x_a7yT where
  Let6989586621679038864Vx f_a7yS x_a7yT = x_a7yT
type family Lambda_6989586621679038878_a7z9 f_a7z5 g_a7z6 a_6989586621679038866_a7z7 x_a7za where
  Lambda_6989586621679038878_a7z9 f_a7z5 g_a7z6 a_6989586621679038866_a7z7 x_a7za = Apply f_a7z5 (Apply g_a7z6 x_a7za)
data Lambda_6989586621679038878Sym0 f6989586621679038875
  where
    Lambda_6989586621679038878Sym0KindInference :: SameKind (Apply Lambda_6989586621679038878Sym0 arg_a7zb) (Lambda_6989586621679038878Sym1 arg_a7zb) =>
                                                   Lambda_6989586621679038878Sym0 f6989586621679038875
type instance Apply Lambda_6989586621679038878Sym0 f6989586621679038875 = Lambda_6989586621679038878Sym1 f6989586621679038875
data Lambda_6989586621679038878Sym1 f6989586621679038875 g6989586621679038876
  where
    Lambda_6989586621679038878Sym1KindInference :: SameKind (Apply (Lambda_6989586621679038878Sym1 f6989586621679038875) arg_a7zb) (Lambda_6989586621679038878Sym2 f6989586621679038875 arg_a7zb) =>
                                                   Lambda_6989586621679038878Sym1 f6989586621679038875 g6989586621679038876
type instance Apply (Lambda_6989586621679038878Sym1 f6989586621679038875) g6989586621679038876 = Lambda_6989586621679038878Sym2 f6989586621679038875 g6989586621679038876
data Lambda_6989586621679038878Sym2 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877
  where
    Lambda_6989586621679038878Sym2KindInference :: SameKind (Apply (Lambda_6989586621679038878Sym2 f6989586621679038875 g6989586621679038876) arg_a7zb) (Lambda_6989586621679038878Sym3 f6989586621679038875 g6989586621679038876 arg_a7zb) =>
                                                   Lambda_6989586621679038878Sym2 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877
type instance Apply (Lambda_6989586621679038878Sym2 f6989586621679038875 g6989586621679038876) a_69895866216790388666989586621679038877 = Lambda_6989586621679038878Sym3 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877
data Lambda_6989586621679038878Sym3 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877 x6989586621679038880
  where
    Lambda_6989586621679038878Sym3KindInference :: SameKind (Apply (Lambda_6989586621679038878Sym3 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877) arg_a7zb) (Lambda_6989586621679038878Sym4 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877 arg_a7zb) =>
                                                   Lambda_6989586621679038878Sym3 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877 x6989586621679038880
type instance Apply (Lambda_6989586621679038878Sym3 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877) x6989586621679038880 = Lambda_6989586621679038878_a7z9 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877 x6989586621679038880
type family Lambda_6989586621679038878Sym4 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877 x6989586621679038880 where
  Lambda_6989586621679038878Sym4 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877 x6989586621679038880 = Lambda_6989586621679038878_a7z9 f6989586621679038875 g6989586621679038876 a_69895866216790388666989586621679038877 x6989586621679038880
type ($!@#@$) :: (~>) ((~>) a_a7yq b_a7yr) ((~>) a_a7yq b_a7yr)
data ($!@#@$) :: (~>) ((~>) a_a7yq b_a7yr) ((~>) a_a7yq b_a7yr)
  where
    (:$!@#@$###) :: SameKind (Apply ($!@#@$) arg_a7yP) (($!@#@$$) arg_a7yP) =>
                    ($!@#@$) a6989586621679038860
type instance Apply ($!@#@$) a6989586621679038860 = ($!@#@$$) a6989586621679038860
infixr 0 $!@#@$
type ($!@#@$$) :: (~>) a_a7yq b_a7yr -> (~>) a_a7yq b_a7yr
data ($!@#@$$) (a6989586621679038860 :: (~>) a_a7yq b_a7yr) :: (~>) a_a7yq b_a7yr
  where
    (:$!@#@$$###) :: SameKind (Apply (($!@#@$$) a6989586621679038860) arg_a7yP) (($!@#@$$$) a6989586621679038860 arg_a7yP) =>
                     ($!@#@$$) a6989586621679038860 a6989586621679038861
type instance Apply (($!@#@$$) a6989586621679038860) a6989586621679038861 = ($!) a6989586621679038860 a6989586621679038861
infixr 0 $!@#@$$
type ($!@#@$$$) :: (~>) a_a7yq b_a7yr -> a_a7yq -> b_a7yr
type family ($!@#@$$$) (a6989586621679038860 :: (~>) a_a7yq b_a7yr) (a6989586621679038861 :: a_a7yq) :: b_a7yr where
  ($!@#@$$$) a6989586621679038860 a6989586621679038861 = ($!) a6989586621679038860 a6989586621679038861
infixr 0 $!@#@$$$
type (.@#@$) :: (~>) ((~>) b_a7ys c_a7yt) ((~>) ((~>) a_a7yu b_a7ys) ((~>) a_a7yu c_a7yt))
data (.@#@$) :: (~>) ((~>) b_a7ys c_a7yt) ((~>) ((~>) a_a7yu b_a7ys) ((~>) a_a7yu c_a7yt))
  where
    (:.@#@$###) :: SameKind (Apply (.@#@$) arg_a7z1) ((.@#@$$) arg_a7z1) =>
                   (.@#@$) a6989586621679038872
type instance Apply (.@#@$) a6989586621679038872 = (.@#@$$) a6989586621679038872
infixr 9 .@#@$
type (.@#@$$) :: (~>) b_a7ys c_a7yt
                 -> (~>) ((~>) a_a7yu b_a7ys) ((~>) a_a7yu c_a7yt)
data (.@#@$$) (a6989586621679038872 :: (~>) b_a7ys c_a7yt) :: (~>) ((~>) a_a7yu b_a7ys) ((~>) a_a7yu c_a7yt)
  where
    (:.@#@$$###) :: SameKind (Apply ((.@#@$$) a6989586621679038872) arg_a7z1) ((.@#@$$$) a6989586621679038872 arg_a7z1) =>
                    (.@#@$$) a6989586621679038872 a6989586621679038873
type instance Apply ((.@#@$$) a6989586621679038872) a6989586621679038873 = (.@#@$$$) a6989586621679038872 a6989586621679038873
infixr 9 .@#@$$
type (.@#@$$$) :: (~>) b_a7ys c_a7yt
                  -> (~>) a_a7yu b_a7ys -> (~>) a_a7yu c_a7yt
data (.@#@$$$) (a6989586621679038872 :: (~>) b_a7ys c_a7yt) (a6989586621679038873 :: (~>) a_a7yu b_a7ys) :: (~>) a_a7yu c_a7yt
  where
    (:.@#@$$$###) :: SameKind (Apply ((.@#@$$$) a6989586621679038872 a6989586621679038873) arg_a7z1) ((.@#@$$$$) a6989586621679038872 a6989586621679038873 arg_a7z1) =>
                     (.@#@$$$) a6989586621679038872 a6989586621679038873 a6989586621679038874
type instance Apply ((.@#@$$$) a6989586621679038872 a6989586621679038873) a6989586621679038874 = (.) a6989586621679038872 a6989586621679038873 a6989586621679038874
infixr 9 .@#@$$$
type (.@#@$$$$) :: (~>) b_a7ys c_a7yt
                   -> (~>) a_a7yu b_a7ys -> a_a7yu -> c_a7yt
type family (.@#@$$$$) (a6989586621679038872 :: (~>) b_a7ys c_a7yt) (a6989586621679038873 :: (~>) a_a7yu b_a7ys) (a6989586621679038874 :: a_a7yu) :: c_a7yt where
  (.@#@$$$$) a6989586621679038872 a6989586621679038873 a6989586621679038874 = (.) a6989586621679038872 a6989586621679038873 a6989586621679038874
infixr 9 .@#@$$$$
type FlipSym0 :: (~>) ((~>) a_a7yv ((~>) b_a7yw c_a7yx)) ((~>) b_a7yw ((~>) a_a7yv c_a7yx))
data FlipSym0 :: (~>) ((~>) a_a7yv ((~>) b_a7yw c_a7yx)) ((~>) b_a7yw ((~>) a_a7yv c_a7yx))
  where
    FlipSym0KindInference :: SameKind (Apply FlipSym0 arg_a7zf) (FlipSym1 arg_a7zf) =>
                             FlipSym0 a6989586621679038886
type instance Apply FlipSym0 a6989586621679038886 = FlipSym1 a6989586621679038886
type FlipSym1 :: (~>) a_a7yv ((~>) b_a7yw c_a7yx)
                 -> (~>) b_a7yw ((~>) a_a7yv c_a7yx)
data FlipSym1 (a6989586621679038886 :: (~>) a_a7yv ((~>) b_a7yw c_a7yx)) :: (~>) b_a7yw ((~>) a_a7yv c_a7yx)
  where
    FlipSym1KindInference :: SameKind (Apply (FlipSym1 a6989586621679038886) arg_a7zf) (FlipSym2 a6989586621679038886 arg_a7zf) =>
                             FlipSym1 a6989586621679038886 a6989586621679038887
type instance Apply (FlipSym1 a6989586621679038886) a6989586621679038887 = FlipSym2 a6989586621679038886 a6989586621679038887
type FlipSym2 :: (~>) a_a7yv ((~>) b_a7yw c_a7yx)
                 -> b_a7yw -> (~>) a_a7yv c_a7yx
data FlipSym2 (a6989586621679038886 :: (~>) a_a7yv ((~>) b_a7yw c_a7yx)) (a6989586621679038887 :: b_a7yw) :: (~>) a_a7yv c_a7yx
  where
    FlipSym2KindInference :: SameKind (Apply (FlipSym2 a6989586621679038886 a6989586621679038887) arg_a7zf) (FlipSym3 a6989586621679038886 a6989586621679038887 arg_a7zf) =>
                             FlipSym2 a6989586621679038886 a6989586621679038887 a6989586621679038888
type instance Apply (FlipSym2 a6989586621679038886 a6989586621679038887) a6989586621679038888 = Flip a6989586621679038886 a6989586621679038887 a6989586621679038888
type FlipSym3 :: (~>) a_a7yv ((~>) b_a7yw c_a7yx)
                 -> b_a7yw -> a_a7yv -> c_a7yx
type family FlipSym3 (a6989586621679038886 :: (~>) a_a7yv ((~>) b_a7yw c_a7yx)) (a6989586621679038887 :: b_a7yw) (a6989586621679038888 :: a_a7yv) :: c_a7yx where
  FlipSym3 a6989586621679038886 a6989586621679038887 a6989586621679038888 = Flip a6989586621679038886 a6989586621679038887 a6989586621679038888
type IdSym0 :: (~>) a_a7yy a_a7yy
data IdSym0 :: (~>) a_a7yy a_a7yy
  where
    IdSym0KindInference :: SameKind (Apply IdSym0 arg_a7zn) (IdSym1 arg_a7zn) =>
                           IdSym0 a6989586621679038894
type instance Apply IdSym0 a6989586621679038894 = Id a6989586621679038894
type IdSym1 :: a_a7yy -> a_a7yy
type family IdSym1 (a6989586621679038894 :: a_a7yy) :: a_a7yy where
  IdSym1 a6989586621679038894 = Id a6989586621679038894
type ($!) :: (~>) a_a7yq b_a7yr -> a_a7yq -> b_a7yr
type family ($!) (a_a7yN :: (~>) a_a7yq b_a7yr) (a_a7yO :: a_a7yq) :: b_a7yr where
  ($!) f_a7yS x_a7yT = Apply f_a7yS (Let6989586621679038864VxSym2 f_a7yS x_a7yT)
type (.) :: (~>) b_a7ys c_a7yt
            -> (~>) a_a7yu b_a7ys -> a_a7yu -> c_a7yt
type family (.) (a_a7yY :: (~>) b_a7ys c_a7yt) (a_a7yZ :: (~>) a_a7yu b_a7ys) (a_a7z0 :: a_a7yu) :: c_a7yt where
  (.) f_a7z5 g_a7z6 a_6989586621679038866_a7z7 = Apply (Apply (Apply (Apply Lambda_6989586621679038878Sym0 f_a7z5) g_a7z6) a_6989586621679038866_a7z7) a_6989586621679038866_a7z7
type Flip :: (~>) a_a7yv ((~>) b_a7yw c_a7yx)
             -> b_a7yw -> a_a7yv -> c_a7yx
type family Flip (a_a7zc :: (~>) a_a7yv ((~>) b_a7yw c_a7yx)) (a_a7zd :: b_a7yw) (a_a7ze :: a_a7yv) :: c_a7yx where
  Flip f_a7zj x_a7zk y_a7zl = Apply (Apply f_a7zj y_a7zl) x_a7zk
type Id :: a_a7yy -> a_a7yy
type family Id (a_a7zm :: a_a7yy) :: a_a7yy where
  Id x_a7zp = x_a7zp
infixr 0 $!
infixr 9 .
infixr 0 %$!
infixr 9 %.
(%$!) ::
  forall a_a7yq
         b_a7yr
         (t_a7zq :: (~>) a_a7yq b_a7yr)
         (t_a7zr :: a_a7yq). Sing t_a7zq
                             -> Sing t_a7zr
                                -> Sing (Apply (Apply ($!@#@$) t_a7zq) t_a7zr :: b_a7yr)
(%.) ::
  forall b_a7ys
         c_a7yt
         a_a7yu
         (t_a7zv :: (~>) b_a7ys c_a7yt)
         (t_a7zw :: (~>) a_a7yu b_a7ys)
         (t_a7zx :: a_a7yu). Sing t_a7zv
                             -> Sing t_a7zw
                                -> Sing t_a7zx
                                   -> Sing (Apply (Apply (Apply (.@#@$) t_a7zv) t_a7zw) t_a7zx :: c_a7yt)
sFlip ::
  forall a_a7yv
         b_a7yw
         c_a7yx
         (t_a7zF :: (~>) a_a7yv ((~>) b_a7yw c_a7yx))
         (t_a7zG :: b_a7yw)
         (t_a7zH :: a_a7yv). Sing t_a7zF
                             -> Sing t_a7zG
                                -> Sing t_a7zH
                                   -> Sing (Apply (Apply (Apply FlipSym0 t_a7zF) t_a7zG) t_a7zH :: c_a7yx)
sId ::
  forall a_a7yy (t_a7zP :: a_a7yy). Sing t_a7zP
                                    -> Sing (Apply IdSym0 t_a7zP :: a_a7yy)
(%$!) (sF :: Sing f_a7yS) (sX :: Sing x_a7yT)
  = let
      sVx :: Sing @_ (Let6989586621679038864VxSym2 f_a7yS x_a7yT)
      sVx = sX
    in (applySing sF) sVx
(%.)
  (sF :: Sing f_a7z5)
  (sG :: Sing g_a7z6)
  (sA_6989586621679038866 :: Sing a_6989586621679038866_a7z7)
  = (applySing
       ((singFun1
           @(Apply (Apply (Apply Lambda_6989586621679038878Sym0 f_a7z5) g_a7z6) a_6989586621679038866_a7z7))
          (\ sX
             -> case sX of
                  (_ :: Sing x_a7za) -> (applySing sF) ((applySing sG) sX))))
      sA_6989586621679038866
sFlip (sF :: Sing f_a7zj) (sX :: Sing x_a7zk) (sY :: Sing y_a7zl)
  = (applySing ((applySing sF) sY)) sX
sId (sX :: Sing x_a7zp) = sX

------------------------------------------------------------

newtype Dual a = Dual { getDual :: a }

{-
$(genSingletons [''Dual])
-}

type DualSym0 :: forall (a_a5fi :: Type). (~>) a_a5fi (Dual (a_a5fi :: Type))
data DualSym0 :: (~>) a_a5fi (Dual (a_a5fi :: Type))
  where
    DualSym0KindInference :: SameKind (Apply DualSym0 arg_a9sh) (DualSym1 arg_a9sh) =>
                             DualSym0 a6989586621679046142
type instance Apply DualSym0 a6989586621679046142 = 'Dual a6989586621679046142
type DualSym1 :: forall (a_a5fi :: Type). a_a5fi
                                          -> Dual (a_a5fi :: Type)
type family DualSym1 (a6989586621679046142 :: a_a5fi) :: Dual (a_a5fi :: Type) where
  DualSym1 a6989586621679046142 = 'Dual a6989586621679046142
type GetDualSym0 :: forall (a_a5fi :: Type). (~>) (Dual (a_a5fi :: Type)) a_a5fi
data GetDualSym0 :: (~>) (Dual (a_a5fi :: Type)) a_a5fi
  where
    GetDualSym0KindInference :: SameKind (Apply GetDualSym0 arg_a9sk) (GetDualSym1 arg_a9sk) =>
                                GetDualSym0 a6989586621679046145
type instance Apply GetDualSym0 a6989586621679046145 = GetDual a6989586621679046145
type GetDualSym1 :: forall (a_a5fi :: Type). Dual (a_a5fi :: Type)
                                             -> a_a5fi
type family GetDualSym1 (a6989586621679046145 :: Dual (a_a5fi :: Type)) :: a_a5fi where
  GetDualSym1 a6989586621679046145 = GetDual a6989586621679046145
type GetDual :: forall (a_a5fi :: Type). Dual (a_a5fi :: Type)
                                         -> a_a5fi
type family GetDual (a_a9sj :: Dual (a_a5fi :: Type)) :: a_a5fi where
  GetDual ('Dual field_a9sm) = field_a9sm
sGetDual ::
  forall (a_a5fi :: Type)
         (t_a9sn :: Dual (a_a5fi :: Type)). Sing t_a9sn
                                            -> Sing (Apply GetDualSym0 t_a9sn :: a_a5fi)
sGetDual (SDual (sField :: Sing field_a9sm)) = sField
type SDual :: forall (a_a5fi :: Type). Dual a_a5fi -> Type
data SDual :: forall (a_a5fi :: Type). Dual a_a5fi -> Type
  where
    SDual :: forall (a_a5fi :: Type) (n_a9sq :: a_a5fi).
             (Sing n_a9sq) -> SDual ('Dual n_a9sq :: Dual (a_a5fi :: Type))
type instance Sing @(Dual a_a5fi) = SDual

{-
$(singletonsOnly [d|
  class Semigroup a where
    (<>) :: a -> a -> a

  class Semigroup a => Monoid a where
        mempty  :: a

        mappend :: a -> a -> a
        mappend = (<>)

  instance Semigroup a => Semigroup (Dual a) where
    Dual a <> Dual b = Dual (b <> a)

  instance Monoid a => Monoid (Dual a) where
          mempty = Dual mempty
  |])
-}

type (<>@#@$) :: forall a_a9GJ. (~>) a_a9GJ ((~>) a_a9GJ a_a9GJ)
data (<>@#@$) :: (~>) a_a9GJ ((~>) a_a9GJ a_a9GJ)
  where
    (:<>@#@$###) :: SameKind (Apply (<>@#@$) arg_a9GZ) ((<>@#@$$) arg_a9GZ) =>
                    (<>@#@$) a6989586621679047054
type instance Apply (<>@#@$) a6989586621679047054 = (<>@#@$$) a6989586621679047054
type (<>@#@$$) :: forall a_a9GJ. a_a9GJ -> (~>) a_a9GJ a_a9GJ
data (<>@#@$$) (a6989586621679047054 :: a_a9GJ) :: (~>) a_a9GJ a_a9GJ
  where
    (:<>@#@$$###) :: SameKind (Apply ((<>@#@$$) a6989586621679047054) arg_a9GZ) ((<>@#@$$$) a6989586621679047054 arg_a9GZ) =>
                     (<>@#@$$) a6989586621679047054 a6989586621679047055
type instance Apply ((<>@#@$$) a6989586621679047054) a6989586621679047055 = (<>) a6989586621679047054 a6989586621679047055
type (<>@#@$$$) :: forall a_a9GJ. a_a9GJ -> a_a9GJ -> a_a9GJ
type family (<>@#@$$$) (a6989586621679047054 :: a_a9GJ) (a6989586621679047055 :: a_a9GJ) :: a_a9GJ where
  (<>@#@$$$) a6989586621679047054 a6989586621679047055 = (<>) a6989586621679047054 a6989586621679047055
class PSemigroup a_a9GJ where
  type family (<>) (arg_a9GX :: a_a9GJ) (arg_a9GY :: a_a9GJ) :: a_a9GJ
type MemptySym0 :: forall a_a9GK. a_a9GK
type family MemptySym0 :: a_a9GK where
  MemptySym0 = Mempty
type MappendSym0 :: forall a_a9GK. (~>) a_a9GK ((~>) a_a9GK a_a9GK)
data MappendSym0 :: (~>) a_a9GK ((~>) a_a9GK a_a9GK)
  where
    MappendSym0KindInference :: SameKind (Apply MappendSym0 arg_a9H5) (MappendSym1 arg_a9H5) =>
                                MappendSym0 a6989586621679047060
type instance Apply MappendSym0 a6989586621679047060 = MappendSym1 a6989586621679047060
type MappendSym1 :: forall a_a9GK. a_a9GK -> (~>) a_a9GK a_a9GK
data MappendSym1 (a6989586621679047060 :: a_a9GK) :: (~>) a_a9GK a_a9GK
  where
    MappendSym1KindInference :: SameKind (Apply (MappendSym1 a6989586621679047060) arg_a9H5) (MappendSym2 a6989586621679047060 arg_a9H5) =>
                                MappendSym1 a6989586621679047060 a6989586621679047061
type instance Apply (MappendSym1 a6989586621679047060) a6989586621679047061 = Mappend a6989586621679047060 a6989586621679047061
type MappendSym2 :: forall a_a9GK. a_a9GK -> a_a9GK -> a_a9GK
type family MappendSym2 (a6989586621679047060 :: a_a9GK) (a6989586621679047061 :: a_a9GK) :: a_a9GK where
  MappendSym2 a6989586621679047060 a6989586621679047061 = Mappend a6989586621679047060 a6989586621679047061
type Mappend_6989586621679047064 :: a_a9GK -> a_a9GK -> a_a9GK
type family Mappend_6989586621679047064 (a_a9Hg :: a_a9GK) (a_a9Hh :: a_a9GK) :: a_a9GK where
  Mappend_6989586621679047064 a_6989586621679047066_a9Hl a_6989586621679047068_a9Hm = Apply (Apply (<>@#@$) a_6989586621679047066_a9Hl) a_6989586621679047068_a9Hm
type Mappend_6989586621679047064Sym0 :: (~>) a_a9GK ((~>) a_a9GK a_a9GK)
data Mappend_6989586621679047064Sym0 :: (~>) a_a9GK ((~>) a_a9GK a_a9GK)
  where
    Mappend_6989586621679047064Sym0KindInference :: SameKind (Apply Mappend_6989586621679047064Sym0 arg_a9Hi) (Mappend_6989586621679047064Sym1 arg_a9Hi) =>
                                                    Mappend_6989586621679047064Sym0 a6989586621679047073
type instance Apply Mappend_6989586621679047064Sym0 a6989586621679047073 = Mappend_6989586621679047064Sym1 a6989586621679047073
type Mappend_6989586621679047064Sym1 :: a_a9GK
                                        -> (~>) a_a9GK a_a9GK
data Mappend_6989586621679047064Sym1 (a6989586621679047073 :: a_a9GK) :: (~>) a_a9GK a_a9GK
  where
    Mappend_6989586621679047064Sym1KindInference :: SameKind (Apply (Mappend_6989586621679047064Sym1 a6989586621679047073) arg_a9Hi) (Mappend_6989586621679047064Sym2 a6989586621679047073 arg_a9Hi) =>
                                                    Mappend_6989586621679047064Sym1 a6989586621679047073 a6989586621679047074
type instance Apply (Mappend_6989586621679047064Sym1 a6989586621679047073) a6989586621679047074 = Mappend_6989586621679047064 a6989586621679047073 a6989586621679047074
type Mappend_6989586621679047064Sym2 :: a_a9GK -> a_a9GK -> a_a9GK
type family Mappend_6989586621679047064Sym2 (a6989586621679047073 :: a_a9GK) (a6989586621679047074 :: a_a9GK) :: a_a9GK where
  Mappend_6989586621679047064Sym2 a6989586621679047073 a6989586621679047074 = Mappend_6989586621679047064 a6989586621679047073 a6989586621679047074
class PMonoid a_a9GK where
  type family Mempty :: a_a9GK
  type family Mappend (arg_a9H3 :: a_a9GK) (arg_a9H4 :: a_a9GK) :: a_a9GK
  type Mappend a_a9H8 a_a9H9 = Apply (Apply Mappend_6989586621679047064Sym0 a_a9H8) a_a9H9
type TFHelper_6989586621679047079 :: Dual a_a9GL
                                     -> Dual a_a9GL -> Dual a_a9GL
type family TFHelper_6989586621679047079 (a_a9Hr :: Dual a_a9GL) (a_a9Hs :: Dual a_a9GL) :: Dual a_a9GL where
  TFHelper_6989586621679047079 ('Dual a_a9Hw) ('Dual b_a9Hx) = Apply DualSym0 (Apply (Apply (<>@#@$) b_a9Hx) a_a9Hw)
type TFHelper_6989586621679047079Sym0 :: (~>) (Dual a_a9GL) ((~>) (Dual a_a9GL) (Dual a_a9GL))
data TFHelper_6989586621679047079Sym0 :: (~>) (Dual a_a9GL) ((~>) (Dual a_a9GL) (Dual a_a9GL))
  where
    TFHelper_6989586621679047079Sym0KindInference :: SameKind (Apply TFHelper_6989586621679047079Sym0 arg_a9Ht) (TFHelper_6989586621679047079Sym1 arg_a9Ht) =>
                                                     TFHelper_6989586621679047079Sym0 a6989586621679047084
type instance Apply TFHelper_6989586621679047079Sym0 a6989586621679047084 = TFHelper_6989586621679047079Sym1 a6989586621679047084
type TFHelper_6989586621679047079Sym1 :: Dual a_a9GL
                                         -> (~>) (Dual a_a9GL) (Dual a_a9GL)
data TFHelper_6989586621679047079Sym1 (a6989586621679047084 :: Dual a_a9GL) :: (~>) (Dual a_a9GL) (Dual a_a9GL)
  where
    TFHelper_6989586621679047079Sym1KindInference :: SameKind (Apply (TFHelper_6989586621679047079Sym1 a6989586621679047084) arg_a9Ht) (TFHelper_6989586621679047079Sym2 a6989586621679047084 arg_a9Ht) =>
                                                     TFHelper_6989586621679047079Sym1 a6989586621679047084 a6989586621679047085
type instance Apply (TFHelper_6989586621679047079Sym1 a6989586621679047084) a6989586621679047085 = TFHelper_6989586621679047079 a6989586621679047084 a6989586621679047085
type TFHelper_6989586621679047079Sym2 :: Dual a_a9GL
                                         -> Dual a_a9GL -> Dual a_a9GL
type family TFHelper_6989586621679047079Sym2 (a6989586621679047084 :: Dual a_a9GL) (a6989586621679047085 :: Dual a_a9GL) :: Dual a_a9GL where
  TFHelper_6989586621679047079Sym2 a6989586621679047084 a6989586621679047085 = TFHelper_6989586621679047079 a6989586621679047084 a6989586621679047085
instance PSemigroup (Dual a_a9GL) where
  type (<>) a_a9Hn a_a9Ho = Apply (Apply TFHelper_6989586621679047079Sym0 a_a9Hn) a_a9Ho
type Mempty_6989586621679047088 :: Dual a_a9GO
type family Mempty_6989586621679047088 :: Dual a_a9GO where
  Mempty_6989586621679047088 = Apply DualSym0 MemptySym0
type Mempty_6989586621679047088Sym0 :: Dual a_a9GO
type family Mempty_6989586621679047088Sym0 :: Dual a_a9GO where
  Mempty_6989586621679047088Sym0 = Mempty_6989586621679047088
instance PMonoid (Dual a_a9GO) where
  type Mempty = Mempty_6989586621679047088Sym0
class SSemigroup a_a9GJ where
  (%<>) ::
    forall (t_a9HB :: a_a9GJ) (t_a9HC :: a_a9GJ). Sing t_a9HB
                                                  -> Sing t_a9HC
                                                     -> Sing (Apply (Apply (<>@#@$) t_a9HB) t_a9HC :: a_a9GJ)
class SSemigroup a_a9GK => SMonoid a_a9GK where
  sMempty :: Sing (MemptySym0 :: a_a9GK)
  sMappend ::
    forall (t_a9HG :: a_a9GK) (t_a9HH :: a_a9GK). Sing t_a9HG
                                                  -> Sing t_a9HH
                                                     -> Sing (Apply (Apply MappendSym0 t_a9HG) t_a9HH :: a_a9GK)
  default sMappend ::
            forall (t_a9HG :: a_a9GK)
                   (t_a9HH :: a_a9GK). ((Apply (Apply MappendSym0 t_a9HG) t_a9HH :: a_a9GK)
                                        ~
                                        Apply (Apply Mappend_6989586621679047064Sym0 t_a9HG) t_a9HH) =>
                                       Sing t_a9HG
                                       -> Sing t_a9HH
                                          -> Sing (Apply (Apply MappendSym0 t_a9HG) t_a9HH :: a_a9GK)
  sMappend
    (sA_6989586621679047066 :: Sing a_6989586621679047066_a9Hl)
    (sA_6989586621679047068 :: Sing a_6989586621679047068_a9Hm)
    = (applySing
         ((applySing ((singFun2 @(<>@#@$)) (%<>))) sA_6989586621679047066))
        sA_6989586621679047068
instance SSemigroup a_a9GL => SSemigroup (Dual a_a9GL) where
  (%<>) ::
    forall (t_a9HN :: Dual a_a9GL) (t_a9HO :: Dual a_a9GL). Sing t_a9HN
                                                            -> Sing t_a9HO
                                                               -> Sing (Apply (Apply (<>@#@$) t_a9HN) t_a9HO :: Dual a_a9GL)
  (%<>) (SDual (sA :: Sing a_a9Hw)) (SDual (sB :: Sing b_a9Hx))
    = (applySing ((singFun1 @DualSym0) SDual))
        ((applySing ((applySing ((singFun2 @(<>@#@$)) (%<>))) sB)) sA)
instance SMonoid a_a9GO => SMonoid (Dual a_a9GO) where
  sMempty :: Sing (MemptySym0 :: Dual a_a9GO)
  sMempty = (applySing ((singFun1 @DualSym0) SDual)) sMempty

{-
$(singletonsOnly [d|
  appEndo :: Endo a -> (a -> a)
  appEndo (Endo x) = x

  instance Semigroup (Endo a) where
          Endo x <> Endo y = Endo (x . y)

  instance Monoid (Endo a) where
          mempty = Endo id
  |])
-}

type AppEndoSym0 :: (~>) (Endo a_agCh) ((~>) a_agCh a_agCh)
data AppEndoSym0 :: (~>) (Endo a_agCh) ((~>) a_agCh a_agCh)
  where
    AppEndoSym0KindInference :: SameKind (Apply AppEndoSym0 arg_agUE) (AppEndoSym1 arg_agUE) =>
                                AppEndoSym0 a6989586621679074809
type instance Apply AppEndoSym0 a6989586621679074809 = AppEndoSym1 a6989586621679074809
type AppEndoSym1 :: Endo a_agCh -> (~>) a_agCh a_agCh
data AppEndoSym1 (a6989586621679074809 :: Endo a_agCh) :: (~>) a_agCh a_agCh
  where
    AppEndoSym1KindInference :: SameKind (Apply (AppEndoSym1 a6989586621679074809) arg_agUE) (AppEndoSym2 a6989586621679074809 arg_agUE) =>
                                AppEndoSym1 a6989586621679074809 a6989586621679074810
type instance Apply (AppEndoSym1 a6989586621679074809) a6989586621679074810 = AppEndo a6989586621679074809 a6989586621679074810
type AppEndoSym2 :: Endo a_agCh -> a_agCh -> a_agCh
type family AppEndoSym2 (a6989586621679074809 :: Endo a_agCh) (a6989586621679074810 :: a_agCh) :: a_agCh where
  AppEndoSym2 a6989586621679074809 a6989586621679074810 = AppEndo a6989586621679074809 a6989586621679074810
type AppEndo :: Endo a_agCh -> a_agCh -> a_agCh
type family AppEndo (a_agUC :: Endo a_agCh) (a_agUD :: a_agCh) :: a_agCh where
  AppEndo ('Endo x_agUH) a_6989586621679074804_agUI = Apply x_agUH a_6989586621679074804_agUI
type TFHelper_6989586621679075091 :: Endo a_agCk
                                     -> Endo a_agCk -> Endo a_agCk
type family TFHelper_6989586621679075091 (a_agZf :: Endo a_agCk) (a_agZg :: Endo a_agCk) :: Endo a_agCk where
  TFHelper_6989586621679075091 ('Endo x_agZk) ('Endo y_agZl) = Apply EndoSym0 (Apply (Apply (.@#@$) x_agZk) y_agZl)
type TFHelper_6989586621679075091Sym0 :: (~>) (Endo a_agCk) ((~>) (Endo a_agCk) (Endo a_agCk))
data TFHelper_6989586621679075091Sym0 :: (~>) (Endo a_agCk) ((~>) (Endo a_agCk) (Endo a_agCk))
  where
    TFHelper_6989586621679075091Sym0KindInference :: SameKind (Apply TFHelper_6989586621679075091Sym0 arg_agZh) (TFHelper_6989586621679075091Sym1 arg_agZh) =>
                                                     TFHelper_6989586621679075091Sym0 a6989586621679075096
type instance Apply TFHelper_6989586621679075091Sym0 a6989586621679075096 = TFHelper_6989586621679075091Sym1 a6989586621679075096
type TFHelper_6989586621679075091Sym1 :: Endo a_agCk
                                         -> (~>) (Endo a_agCk) (Endo a_agCk)
data TFHelper_6989586621679075091Sym1 (a6989586621679075096 :: Endo a_agCk) :: (~>) (Endo a_agCk) (Endo a_agCk)
  where
    TFHelper_6989586621679075091Sym1KindInference :: SameKind (Apply (TFHelper_6989586621679075091Sym1 a6989586621679075096) arg_agZh) (TFHelper_6989586621679075091Sym2 a6989586621679075096 arg_agZh) =>
                                                     TFHelper_6989586621679075091Sym1 a6989586621679075096 a6989586621679075097
type instance Apply (TFHelper_6989586621679075091Sym1 a6989586621679075096) a6989586621679075097 = TFHelper_6989586621679075091 a6989586621679075096 a6989586621679075097
type TFHelper_6989586621679075091Sym2 :: Endo a_agCk
                                         -> Endo a_agCk -> Endo a_agCk
type family TFHelper_6989586621679075091Sym2 (a6989586621679075096 :: Endo a_agCk) (a6989586621679075097 :: Endo a_agCk) :: Endo a_agCk where
  TFHelper_6989586621679075091Sym2 a6989586621679075096 a6989586621679075097 = TFHelper_6989586621679075091 a6989586621679075096 a6989586621679075097
instance PSemigroup (Endo a_agCk) where
  type (<>) a_agZb a_agZc = Apply (Apply TFHelper_6989586621679075091Sym0 a_agZb) a_agZc
type Mempty_6989586621679075313 :: Endo a_agCn
type family Mempty_6989586621679075313 :: Endo a_agCn where
  Mempty_6989586621679075313 = Apply EndoSym0 IdSym0
type Mempty_6989586621679075313Sym0 :: Endo a_agCn
type family Mempty_6989586621679075313Sym0 :: Endo a_agCn where
  Mempty_6989586621679075313Sym0 = Mempty_6989586621679075313
instance PMonoid (Endo a_agCn) where
  type Mempty = Mempty_6989586621679075313Sym0
sAppEndo ::
  forall a_agCh
         (t_ah2Q :: Endo a_agCh)
         (t_ah2R :: a_agCh). Sing t_ah2Q
                             -> Sing t_ah2R
                                -> Sing (Apply (Apply AppEndoSym0 t_ah2Q) t_ah2R :: a_agCh)
sAppEndo
  (SEndo (sX :: Sing x_agUH))
  (sA_6989586621679074804 :: Sing a_6989586621679074804_agUI)
  = (applySing sX) sA_6989586621679074804
instance SSemigroup (Endo a_agCk) where
  (%<>) ::
    forall (t1_ih30 :: Endo a_agCk)
           (t2_ih31 :: Endo a_agCk). Sing t1_ih30
                                     -> Sing t2_ih31
                                        -> Sing (Apply (Apply ((<>@#@$) :: TyFun (Endo a_agCk) ((~>) (Endo a_agCk) (Endo a_agCk))
                                          -> Type) t1_ih30) t2_ih31)
  (%<>) (SEndo (sX :: Sing x_agZk)) (SEndo (sY :: Sing y_agZl))
    = (applySing ((singFun1 @EndoSym0) SEndo))
        ((applySing ((applySing ((singFun3 @(.@#@$)) (%.))) sX)) sY)
instance SMonoid (Endo a_agCn) where
  sMempty :: Sing (MemptySym0 :: Endo a_agCn)
  sMempty
    = (applySing ((singFun1 @EndoSym0) SEndo)) ((singFun1 @IdSym0) sId)

{-
$(singletonsOnly [d|

  class Foldable t where

      foldMap :: Monoid m => (a -> m) -> t a -> m
      foldr   :: (a -> b -> b) -> b -> t a -> b

      foldr' :: (a -> b -> b) -> b -> t a -> b
      foldr' f z0 xs = foldl f' id xs z0
        where f' k x z = k $! f x z

      foldl :: (b -> a -> b) -> b -> t a -> b
      foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

  |])
-}

type FoldMapSym0 :: forall a_ahBz
                           m_ahBy
                           t_ahBx. (~>) ((~>) a_ahBz m_ahBy) ((~>) (t_ahBx a_ahBz) m_ahBy)
data FoldMapSym0 :: (~>) ((~>) a_ahBz m_ahBy) ((~>) (t_ahBx a_ahBz) m_ahBy)
  where
    FoldMapSym0KindInference :: SameKind (Apply FoldMapSym0 arg_ahBS) (FoldMapSym1 arg_ahBS) =>
                                FoldMapSym0 a6989586621679077489
type instance Apply FoldMapSym0 a6989586621679077489 = FoldMapSym1 a6989586621679077489
type FoldMapSym1 :: forall a_ahBz m_ahBy t_ahBx. (~>) a_ahBz m_ahBy
                                                 -> (~>) (t_ahBx a_ahBz) m_ahBy
data FoldMapSym1 (a6989586621679077489 :: (~>) a_ahBz m_ahBy) :: (~>) (t_ahBx a_ahBz) m_ahBy
  where
    FoldMapSym1KindInference :: SameKind (Apply (FoldMapSym1 a6989586621679077489) arg_ahBS) (FoldMapSym2 a6989586621679077489 arg_ahBS) =>
                                FoldMapSym1 a6989586621679077489 a6989586621679077490
type instance Apply (FoldMapSym1 a6989586621679077489) a6989586621679077490 = FoldMap a6989586621679077489 a6989586621679077490
type FoldMapSym2 :: forall a_ahBz m_ahBy t_ahBx. (~>) a_ahBz m_ahBy
                                                 -> t_ahBx a_ahBz -> m_ahBy
type family FoldMapSym2 (a6989586621679077489 :: (~>) a_ahBz m_ahBy) (a6989586621679077490 :: t_ahBx a_ahBz) :: m_ahBy where
  FoldMapSym2 a6989586621679077489 a6989586621679077490 = FoldMap a6989586621679077489 a6989586621679077490
type FoldrSym0 :: forall a_ahBA
                         b_ahBB
                         t_ahBx. (~>) ((~>) a_ahBA ((~>) b_ahBB b_ahBB)) ((~>) b_ahBB ((~>) (t_ahBx a_ahBA) b_ahBB))
data FoldrSym0 :: (~>) ((~>) a_ahBA ((~>) b_ahBB b_ahBB)) ((~>) b_ahBB ((~>) (t_ahBx a_ahBA) b_ahBB))
  where
    FoldrSym0KindInference :: SameKind (Apply FoldrSym0 arg_ahBY) (FoldrSym1 arg_ahBY) =>
                              FoldrSym0 a6989586621679077495
type instance Apply FoldrSym0 a6989586621679077495 = FoldrSym1 a6989586621679077495
type FoldrSym1 :: forall a_ahBA
                         b_ahBB
                         t_ahBx. (~>) a_ahBA ((~>) b_ahBB b_ahBB)
                                 -> (~>) b_ahBB ((~>) (t_ahBx a_ahBA) b_ahBB)
data FoldrSym1 (a6989586621679077495 :: (~>) a_ahBA ((~>) b_ahBB b_ahBB)) :: (~>) b_ahBB ((~>) (t_ahBx a_ahBA) b_ahBB)
  where
    FoldrSym1KindInference :: SameKind (Apply (FoldrSym1 a6989586621679077495) arg_ahBY) (FoldrSym2 a6989586621679077495 arg_ahBY) =>
                              FoldrSym1 a6989586621679077495 a6989586621679077496
type instance Apply (FoldrSym1 a6989586621679077495) a6989586621679077496 = FoldrSym2 a6989586621679077495 a6989586621679077496
type FoldrSym2 :: forall a_ahBA
                         b_ahBB
                         t_ahBx. (~>) a_ahBA ((~>) b_ahBB b_ahBB)
                                 -> b_ahBB -> (~>) (t_ahBx a_ahBA) b_ahBB
data FoldrSym2 (a6989586621679077495 :: (~>) a_ahBA ((~>) b_ahBB b_ahBB)) (a6989586621679077496 :: b_ahBB) :: (~>) (t_ahBx a_ahBA) b_ahBB
  where
    FoldrSym2KindInference :: SameKind (Apply (FoldrSym2 a6989586621679077495 a6989586621679077496) arg_ahBY) (FoldrSym3 a6989586621679077495 a6989586621679077496 arg_ahBY) =>
                              FoldrSym2 a6989586621679077495 a6989586621679077496 a6989586621679077497
type instance Apply (FoldrSym2 a6989586621679077495 a6989586621679077496) a6989586621679077497 = Foldr a6989586621679077495 a6989586621679077496 a6989586621679077497
type FoldrSym3 :: forall a_ahBA
                         b_ahBB
                         t_ahBx. (~>) a_ahBA ((~>) b_ahBB b_ahBB)
                                 -> b_ahBB -> t_ahBx a_ahBA -> b_ahBB
type family FoldrSym3 (a6989586621679077495 :: (~>) a_ahBA ((~>) b_ahBB b_ahBB)) (a6989586621679077496 :: b_ahBB) (a6989586621679077497 :: t_ahBx a_ahBA) :: b_ahBB where
  FoldrSym3 a6989586621679077495 a6989586621679077496 a6989586621679077497 = Foldr a6989586621679077495 a6989586621679077496 a6989586621679077497
type Foldr'Sym0 :: forall a_ahBC
                          b_ahBD
                          t_ahBx. (~>) ((~>) a_ahBC ((~>) b_ahBD b_ahBD)) ((~>) b_ahBD ((~>) (t_ahBx a_ahBC) b_ahBD))
data Foldr'Sym0 :: (~>) ((~>) a_ahBC ((~>) b_ahBD b_ahBD)) ((~>) b_ahBD ((~>) (t_ahBx a_ahBC) b_ahBD))
  where
    Foldr'Sym0KindInference :: SameKind (Apply Foldr'Sym0 arg_ahC5) (Foldr'Sym1 arg_ahC5) =>
                               Foldr'Sym0 a6989586621679077502
type instance Apply Foldr'Sym0 a6989586621679077502 = Foldr'Sym1 a6989586621679077502
type Foldr'Sym1 :: forall a_ahBC
                          b_ahBD
                          t_ahBx. (~>) a_ahBC ((~>) b_ahBD b_ahBD)
                                  -> (~>) b_ahBD ((~>) (t_ahBx a_ahBC) b_ahBD)
data Foldr'Sym1 (a6989586621679077502 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)) :: (~>) b_ahBD ((~>) (t_ahBx a_ahBC) b_ahBD)
  where
    Foldr'Sym1KindInference :: SameKind (Apply (Foldr'Sym1 a6989586621679077502) arg_ahC5) (Foldr'Sym2 a6989586621679077502 arg_ahC5) =>
                               Foldr'Sym1 a6989586621679077502 a6989586621679077503
type instance Apply (Foldr'Sym1 a6989586621679077502) a6989586621679077503 = Foldr'Sym2 a6989586621679077502 a6989586621679077503
type Foldr'Sym2 :: forall a_ahBC
                          b_ahBD
                          t_ahBx. (~>) a_ahBC ((~>) b_ahBD b_ahBD)
                                  -> b_ahBD -> (~>) (t_ahBx a_ahBC) b_ahBD
data Foldr'Sym2 (a6989586621679077502 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)) (a6989586621679077503 :: b_ahBD) :: (~>) (t_ahBx a_ahBC) b_ahBD
  where
    Foldr'Sym2KindInference :: SameKind (Apply (Foldr'Sym2 a6989586621679077502 a6989586621679077503) arg_ahC5) (Foldr'Sym3 a6989586621679077502 a6989586621679077503 arg_ahC5) =>
                               Foldr'Sym2 a6989586621679077502 a6989586621679077503 a6989586621679077504
type instance Apply (Foldr'Sym2 a6989586621679077502 a6989586621679077503) a6989586621679077504 = Foldr' a6989586621679077502 a6989586621679077503 a6989586621679077504
type Foldr'Sym3 :: forall a_ahBC
                          b_ahBD
                          t_ahBx. (~>) a_ahBC ((~>) b_ahBD b_ahBD)
                                  -> b_ahBD -> t_ahBx a_ahBC -> b_ahBD
type family Foldr'Sym3 (a6989586621679077502 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)) (a6989586621679077503 :: b_ahBD) (a6989586621679077504 :: t_ahBx a_ahBC) :: b_ahBD where
  Foldr'Sym3 a6989586621679077502 a6989586621679077503 a6989586621679077504 = Foldr' a6989586621679077502 a6989586621679077503 a6989586621679077504
type FoldlSym0 :: forall b_ahBE
                         a_ahBF
                         t_ahBx. (~>) ((~>) b_ahBE ((~>) a_ahBF b_ahBE)) ((~>) b_ahBE ((~>) (t_ahBx a_ahBF) b_ahBE))
data FoldlSym0 :: (~>) ((~>) b_ahBE ((~>) a_ahBF b_ahBE)) ((~>) b_ahBE ((~>) (t_ahBx a_ahBF) b_ahBE))
  where
    FoldlSym0KindInference :: SameKind (Apply FoldlSym0 arg_ahCc) (FoldlSym1 arg_ahCc) =>
                              FoldlSym0 a6989586621679077509
type instance Apply FoldlSym0 a6989586621679077509 = FoldlSym1 a6989586621679077509
type FoldlSym1 :: forall b_ahBE
                         a_ahBF
                         t_ahBx. (~>) b_ahBE ((~>) a_ahBF b_ahBE)
                                 -> (~>) b_ahBE ((~>) (t_ahBx a_ahBF) b_ahBE)
data FoldlSym1 (a6989586621679077509 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)) :: (~>) b_ahBE ((~>) (t_ahBx a_ahBF) b_ahBE)
  where
    FoldlSym1KindInference :: SameKind (Apply (FoldlSym1 a6989586621679077509) arg_ahCc) (FoldlSym2 a6989586621679077509 arg_ahCc) =>
                              FoldlSym1 a6989586621679077509 a6989586621679077510
type instance Apply (FoldlSym1 a6989586621679077509) a6989586621679077510 = FoldlSym2 a6989586621679077509 a6989586621679077510
type FoldlSym2 :: forall b_ahBE
                         a_ahBF
                         t_ahBx. (~>) b_ahBE ((~>) a_ahBF b_ahBE)
                                 -> b_ahBE -> (~>) (t_ahBx a_ahBF) b_ahBE
data FoldlSym2 (a6989586621679077509 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)) (a6989586621679077510 :: b_ahBE) :: (~>) (t_ahBx a_ahBF) b_ahBE
  where
    FoldlSym2KindInference :: SameKind (Apply (FoldlSym2 a6989586621679077509 a6989586621679077510) arg_ahCc) (FoldlSym3 a6989586621679077509 a6989586621679077510 arg_ahCc) =>
                              FoldlSym2 a6989586621679077509 a6989586621679077510 a6989586621679077511
type instance Apply (FoldlSym2 a6989586621679077509 a6989586621679077510) a6989586621679077511 = Foldl a6989586621679077509 a6989586621679077510 a6989586621679077511
type FoldlSym3 :: forall b_ahBE
                         a_ahBF
                         t_ahBx. (~>) b_ahBE ((~>) a_ahBF b_ahBE)
                                 -> b_ahBE -> t_ahBx a_ahBF -> b_ahBE
type family FoldlSym3 (a6989586621679077509 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)) (a6989586621679077510 :: b_ahBE) (a6989586621679077511 :: t_ahBx a_ahBF) :: b_ahBE where
  FoldlSym3 a6989586621679077509 a6989586621679077510 a6989586621679077511 = Foldl a6989586621679077509 a6989586621679077510 a6989586621679077511
data Let6989586621679077527F'Sym0 f6989586621679077524
  where
    Let6989586621679077527F'Sym0KindInference :: SameKind (Apply Let6989586621679077527F'Sym0 arg_ahCz) (Let6989586621679077527F'Sym1 arg_ahCz) =>
                                                 Let6989586621679077527F'Sym0 f6989586621679077524
type instance Apply Let6989586621679077527F'Sym0 f6989586621679077524 = Let6989586621679077527F'Sym1 f6989586621679077524
data Let6989586621679077527F'Sym1 f6989586621679077524 z06989586621679077525
  where
    Let6989586621679077527F'Sym1KindInference :: SameKind (Apply (Let6989586621679077527F'Sym1 f6989586621679077524) arg_ahCz) (Let6989586621679077527F'Sym2 f6989586621679077524 arg_ahCz) =>
                                                 Let6989586621679077527F'Sym1 f6989586621679077524 z06989586621679077525
type instance Apply (Let6989586621679077527F'Sym1 f6989586621679077524) z06989586621679077525 = Let6989586621679077527F'Sym2 f6989586621679077524 z06989586621679077525
data Let6989586621679077527F'Sym2 f6989586621679077524 z06989586621679077525 xs6989586621679077526
  where
    Let6989586621679077527F'Sym2KindInference :: SameKind (Apply (Let6989586621679077527F'Sym2 f6989586621679077524 z06989586621679077525) arg_ahCz) (Let6989586621679077527F'Sym3 f6989586621679077524 z06989586621679077525 arg_ahCz) =>
                                                 Let6989586621679077527F'Sym2 f6989586621679077524 z06989586621679077525 xs6989586621679077526
type instance Apply (Let6989586621679077527F'Sym2 f6989586621679077524 z06989586621679077525) xs6989586621679077526 = Let6989586621679077527F'Sym3 f6989586621679077524 z06989586621679077525 xs6989586621679077526
data Let6989586621679077527F'Sym3 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528
  where
    Let6989586621679077527F'Sym3KindInference :: SameKind (Apply (Let6989586621679077527F'Sym3 f6989586621679077524 z06989586621679077525 xs6989586621679077526) arg_ahCz) (Let6989586621679077527F'Sym4 f6989586621679077524 z06989586621679077525 xs6989586621679077526 arg_ahCz) =>
                                                 Let6989586621679077527F'Sym3 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528
type instance Apply (Let6989586621679077527F'Sym3 f6989586621679077524 z06989586621679077525 xs6989586621679077526) a6989586621679077528 = Let6989586621679077527F'Sym4 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528
data Let6989586621679077527F'Sym4 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529
  where
    Let6989586621679077527F'Sym4KindInference :: SameKind (Apply (Let6989586621679077527F'Sym4 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528) arg_ahCz) (Let6989586621679077527F'Sym5 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 arg_ahCz) =>
                                                 Let6989586621679077527F'Sym4 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529
type instance Apply (Let6989586621679077527F'Sym4 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528) a6989586621679077529 = Let6989586621679077527F'Sym5 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529
data Let6989586621679077527F'Sym5 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529 a6989586621679077530
  where
    Let6989586621679077527F'Sym5KindInference :: SameKind (Apply (Let6989586621679077527F'Sym5 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529) arg_ahCz) (Let6989586621679077527F'Sym6 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529 arg_ahCz) =>
                                                 Let6989586621679077527F'Sym5 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529 a6989586621679077530
type instance Apply (Let6989586621679077527F'Sym5 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529) a6989586621679077530 = Let6989586621679077527F' f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529 a6989586621679077530
type family Let6989586621679077527F'Sym6 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529 a6989586621679077530 where
  Let6989586621679077527F'Sym6 f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529 a6989586621679077530 = Let6989586621679077527F' f6989586621679077524 z06989586621679077525 xs6989586621679077526 a6989586621679077528 a6989586621679077529 a6989586621679077530
type family Let6989586621679077527F' f_ahCs z0_ahCt xs_ahCu a_ahCw a_ahCx a_ahCy where
  Let6989586621679077527F' f_ahCs z0_ahCt xs_ahCu k_ahCA x_ahCB z_ahCC = Apply (Apply ($!@#@$) k_ahCA) (Apply (Apply f_ahCs x_ahCB) z_ahCC)
type Foldr'_6989586621679077515 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)
                                   -> b_ahBD -> t_ahBx a_ahBC -> b_ahBD
type family Foldr'_6989586621679077515 (a_ahCl :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)) (a_ahCm :: b_ahBD) (a_ahCn :: t_ahBx a_ahBC) :: b_ahBD where
  Foldr'_6989586621679077515 f_ahCs z0_ahCt xs_ahCu = Apply (Apply (Apply (Apply FoldlSym0 (Let6989586621679077527F'Sym3 f_ahCs z0_ahCt xs_ahCu)) IdSym0) xs_ahCu) z0_ahCt
type Foldr'_6989586621679077515Sym0 :: (~>) ((~>) a_ahBC ((~>) b_ahBD b_ahBD)) ((~>) b_ahBD ((~>) (t_ahBx a_ahBC) b_ahBD))
data Foldr'_6989586621679077515Sym0 :: (~>) ((~>) a_ahBC ((~>) b_ahBD b_ahBD)) ((~>) b_ahBD ((~>) (t_ahBx a_ahBC) b_ahBD))
  where
    Foldr'_6989586621679077515Sym0KindInference :: SameKind (Apply Foldr'_6989586621679077515Sym0 arg_ahCo) (Foldr'_6989586621679077515Sym1 arg_ahCo) =>
                                                   Foldr'_6989586621679077515Sym0 a6989586621679077521
type instance Apply Foldr'_6989586621679077515Sym0 a6989586621679077521 = Foldr'_6989586621679077515Sym1 a6989586621679077521
type Foldr'_6989586621679077515Sym1 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)
                                       -> (~>) b_ahBD ((~>) (t_ahBx a_ahBC) b_ahBD)
data Foldr'_6989586621679077515Sym1 (a6989586621679077521 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)) :: (~>) b_ahBD ((~>) (t_ahBx a_ahBC) b_ahBD)
  where
    Foldr'_6989586621679077515Sym1KindInference :: SameKind (Apply (Foldr'_6989586621679077515Sym1 a6989586621679077521) arg_ahCo) (Foldr'_6989586621679077515Sym2 a6989586621679077521 arg_ahCo) =>
                                                   Foldr'_6989586621679077515Sym1 a6989586621679077521 a6989586621679077522
type instance Apply (Foldr'_6989586621679077515Sym1 a6989586621679077521) a6989586621679077522 = Foldr'_6989586621679077515Sym2 a6989586621679077521 a6989586621679077522
type Foldr'_6989586621679077515Sym2 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)
                                       -> b_ahBD -> (~>) (t_ahBx a_ahBC) b_ahBD
data Foldr'_6989586621679077515Sym2 (a6989586621679077521 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)) (a6989586621679077522 :: b_ahBD) :: (~>) (t_ahBx a_ahBC) b_ahBD
  where
    Foldr'_6989586621679077515Sym2KindInference :: SameKind (Apply (Foldr'_6989586621679077515Sym2 a6989586621679077521 a6989586621679077522) arg_ahCo) (Foldr'_6989586621679077515Sym3 a6989586621679077521 a6989586621679077522 arg_ahCo) =>
                                                   Foldr'_6989586621679077515Sym2 a6989586621679077521 a6989586621679077522 a6989586621679077523
type instance Apply (Foldr'_6989586621679077515Sym2 a6989586621679077521 a6989586621679077522) a6989586621679077523 = Foldr'_6989586621679077515 a6989586621679077521 a6989586621679077522 a6989586621679077523
type Foldr'_6989586621679077515Sym3 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)
                                       -> b_ahBD -> t_ahBx a_ahBC -> b_ahBD
type family Foldr'_6989586621679077515Sym3 (a6989586621679077521 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)) (a6989586621679077522 :: b_ahBD) (a6989586621679077523 :: t_ahBx a_ahBC) :: b_ahBD where
  Foldr'_6989586621679077515Sym3 a6989586621679077521 a6989586621679077522 a6989586621679077523 = Foldr'_6989586621679077515 a6989586621679077521 a6989586621679077522 a6989586621679077523
type Foldl_6989586621679077538 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)
                                  -> b_ahBE -> t_ahBx a_ahBF -> b_ahBE
type family Foldl_6989586621679077538 (a_ahCI :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)) (a_ahCJ :: b_ahBE) (a_ahCK :: t_ahBx a_ahBF) :: b_ahBE where
  Foldl_6989586621679077538 f_ahCP z_ahCQ t_ahCR = Apply (Apply AppEndoSym0 (Apply GetDualSym0 (Apply (Apply FoldMapSym0 (Apply (Apply (.@#@$) DualSym0) (Apply (Apply (.@#@$) EndoSym0) (Apply FlipSym0 f_ahCP)))) t_ahCR))) z_ahCQ
type Foldl_6989586621679077538Sym0 :: (~>) ((~>) b_ahBE ((~>) a_ahBF b_ahBE)) ((~>) b_ahBE ((~>) (t_ahBx a_ahBF) b_ahBE))
data Foldl_6989586621679077538Sym0 :: (~>) ((~>) b_ahBE ((~>) a_ahBF b_ahBE)) ((~>) b_ahBE ((~>) (t_ahBx a_ahBF) b_ahBE))
  where
    Foldl_6989586621679077538Sym0KindInference :: SameKind (Apply Foldl_6989586621679077538Sym0 arg_ahCL) (Foldl_6989586621679077538Sym1 arg_ahCL) =>
                                                  Foldl_6989586621679077538Sym0 a6989586621679077544
type instance Apply Foldl_6989586621679077538Sym0 a6989586621679077544 = Foldl_6989586621679077538Sym1 a6989586621679077544
type Foldl_6989586621679077538Sym1 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)
                                      -> (~>) b_ahBE ((~>) (t_ahBx a_ahBF) b_ahBE)
data Foldl_6989586621679077538Sym1 (a6989586621679077544 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)) :: (~>) b_ahBE ((~>) (t_ahBx a_ahBF) b_ahBE)
  where
    Foldl_6989586621679077538Sym1KindInference :: SameKind (Apply (Foldl_6989586621679077538Sym1 a6989586621679077544) arg_ahCL) (Foldl_6989586621679077538Sym2 a6989586621679077544 arg_ahCL) =>
                                                  Foldl_6989586621679077538Sym1 a6989586621679077544 a6989586621679077545
type instance Apply (Foldl_6989586621679077538Sym1 a6989586621679077544) a6989586621679077545 = Foldl_6989586621679077538Sym2 a6989586621679077544 a6989586621679077545
type Foldl_6989586621679077538Sym2 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)
                                      -> b_ahBE -> (~>) (t_ahBx a_ahBF) b_ahBE
data Foldl_6989586621679077538Sym2 (a6989586621679077544 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)) (a6989586621679077545 :: b_ahBE) :: (~>) (t_ahBx a_ahBF) b_ahBE
  where
    Foldl_6989586621679077538Sym2KindInference :: SameKind (Apply (Foldl_6989586621679077538Sym2 a6989586621679077544 a6989586621679077545) arg_ahCL) (Foldl_6989586621679077538Sym3 a6989586621679077544 a6989586621679077545 arg_ahCL) =>
                                                  Foldl_6989586621679077538Sym2 a6989586621679077544 a6989586621679077545 a6989586621679077546
type instance Apply (Foldl_6989586621679077538Sym2 a6989586621679077544 a6989586621679077545) a6989586621679077546 = Foldl_6989586621679077538 a6989586621679077544 a6989586621679077545 a6989586621679077546
type Foldl_6989586621679077538Sym3 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)
                                      -> b_ahBE -> t_ahBx a_ahBF -> b_ahBE
type family Foldl_6989586621679077538Sym3 (a6989586621679077544 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)) (a6989586621679077545 :: b_ahBE) (a6989586621679077546 :: t_ahBx a_ahBF) :: b_ahBE where
  Foldl_6989586621679077538Sym3 a6989586621679077544 a6989586621679077545 a6989586621679077546 = Foldl_6989586621679077538 a6989586621679077544 a6989586621679077545 a6989586621679077546
class PFoldable t_ahBx where
  type family FoldMap (arg_ahBQ :: (~>) a_ahBz m_ahBy) (arg_ahBR :: t_ahBx a_ahBz) :: m_ahBy
  type family Foldr (arg_ahBV :: (~>) a_ahBA ((~>) b_ahBB b_ahBB)) (arg_ahBW :: b_ahBB) (arg_ahBX :: t_ahBx a_ahBA) :: b_ahBB
  type family Foldr' (arg_ahC2 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD)) (arg_ahC3 :: b_ahBD) (arg_ahC4 :: t_ahBx a_ahBC) :: b_ahBD
  type family Foldl (arg_ahC9 :: (~>) b_ahBE ((~>) a_ahBF b_ahBE)) (arg_ahCa :: b_ahBE) (arg_ahCb :: t_ahBx a_ahBF) :: b_ahBE
  type Foldr' a_ahCg a_ahCh a_ahCi = Apply (Apply (Apply Foldr'_6989586621679077515Sym0 a_ahCg) a_ahCh) a_ahCi
  type Foldl a_ahCD a_ahCE a_ahCF = Apply (Apply (Apply Foldl_6989586621679077538Sym0 a_ahCD) a_ahCE) a_ahCF
class SFoldable t_ahBx where
  sFoldMap ::
    forall a_ahBz
           m_ahBy
           (t_ahCS :: (~>) a_ahBz m_ahBy)
           (t_ahCT :: t_ahBx a_ahBz). SMonoid m_ahBy =>
                                      Sing t_ahCS
                                      -> Sing t_ahCT
                                         -> Sing (Apply (Apply FoldMapSym0 t_ahCS) t_ahCT :: m_ahBy)
  sFoldr ::
    forall a_ahBA
           b_ahBB
           (t_ahCX :: (~>) a_ahBA ((~>) b_ahBB b_ahBB))
           (t_ahCY :: b_ahBB)
           (t_ahCZ :: t_ahBx a_ahBA). Sing t_ahCX
                                      -> Sing t_ahCY
                                         -> Sing t_ahCZ
                                            -> Sing (Apply (Apply (Apply FoldrSym0 t_ahCX) t_ahCY) t_ahCZ :: b_ahBB)
  sFoldr' ::
    forall a_ahBC
           b_ahBD
           (t_ahD7 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD))
           (t_ahD8 :: b_ahBD)
           (t_ahD9 :: t_ahBx a_ahBC). Sing t_ahD7
                                      -> Sing t_ahD8
                                         -> Sing t_ahD9
                                            -> Sing (Apply (Apply (Apply Foldr'Sym0 t_ahD7) t_ahD8) t_ahD9 :: b_ahBD)
  sFoldl ::
    forall b_ahBE
           a_ahBF
           (t_ahDh :: (~>) b_ahBE ((~>) a_ahBF b_ahBE))
           (t_ahDi :: b_ahBE)
           (t_ahDj :: t_ahBx a_ahBF). Sing t_ahDh
                                      -> Sing t_ahDi
                                         -> Sing t_ahDj
                                            -> Sing (Apply (Apply (Apply FoldlSym0 t_ahDh) t_ahDi) t_ahDj :: b_ahBE)
  default sFoldr' ::
            forall a_ahBC
                   b_ahBD
                   (t_ahD7 :: (~>) a_ahBC ((~>) b_ahBD b_ahBD))
                   (t_ahD8 :: b_ahBD)
                   (t_ahD9 :: t_ahBx a_ahBC). ((Apply (Apply (Apply Foldr'Sym0 t_ahD7) t_ahD8) t_ahD9 :: b_ahBD)
                                               ~
                                               Apply (Apply (Apply Foldr'_6989586621679077515Sym0 t_ahD7) t_ahD8) t_ahD9) =>
                                              Sing t_ahD7
                                              -> Sing t_ahD8
                                                 -> Sing t_ahD9
                                                    -> Sing (Apply (Apply (Apply Foldr'Sym0 t_ahD7) t_ahD8) t_ahD9 :: b_ahBD)
  default sFoldl ::
            forall b_ahBE
                   a_ahBF
                   (t_ahDh :: (~>) b_ahBE ((~>) a_ahBF b_ahBE))
                   (t_ahDi :: b_ahBE)
                   (t_ahDj :: t_ahBx a_ahBF). ((Apply (Apply (Apply FoldlSym0 t_ahDh) t_ahDi) t_ahDj :: b_ahBE)
                                               ~
                                               Apply (Apply (Apply Foldl_6989586621679077538Sym0 t_ahDh) t_ahDi) t_ahDj) =>
                                              Sing t_ahDh
                                              -> Sing t_ahDi
                                                 -> Sing t_ahDj
                                                    -> Sing (Apply (Apply (Apply FoldlSym0 t_ahDh) t_ahDi) t_ahDj :: b_ahBE)
  sFoldr'
    (sF :: Sing f_ahCs)
    (sZ0 :: Sing z0_ahCt)
    (sXs :: Sing xs_ahCu)
    = let
        sF' ::
          forall arg_ahDr arg_ahDs arg_ahDt. Sing arg_ahDr
                                             -> Sing arg_ahDs
                                                -> Sing arg_ahDt
                                                   -> Sing (Apply (Apply (Apply (Let6989586621679077527F'Sym3 f_ahCs z0_ahCt xs_ahCu) arg_ahDr) arg_ahDs) arg_ahDt)
        sF' (sK :: Sing k_ahCA) (sX :: Sing x_ahCB) (sZ :: Sing z_ahCC)
          = (applySing ((applySing ((singFun2 @($!@#@$)) (%$!))) sK))
              ((applySing ((applySing sF) sX)) sZ)
      in
        (applySing
           ((applySing
               ((applySing
                   ((applySing ((singFun3 @FoldlSym0) sFoldl))
                      ((singFun3
                          @(Let6989586621679077527F'Sym3 f_ahCs z0_ahCt xs_ahCu))
                         sF')))
                  ((singFun1 @IdSym0) sId)))
              sXs))
          sZ0
  sFoldl (sF :: Sing f_ahCP) (sZ :: Sing z_ahCQ) (sT :: Sing t_ahCR)
    = (applySing
         ((applySing ((singFun2 @AppEndoSym0) sAppEndo))
            ((applySing ((singFun1 @GetDualSym0) sGetDual))
               ((applySing
                   ((applySing ((singFun2 @FoldMapSym0) sFoldMap))
                      ((applySing
                          ((applySing ((singFun3 @(.@#@$)) (%.)))
                             ((singFun1 @DualSym0) SDual)))
                         ((applySing
                             ((applySing ((singFun3 @(.@#@$)) (%.)))
                                ((singFun1 @EndoSym0) SEndo)))
                            ((applySing ((singFun3 @FlipSym0) sFlip)) sF)))))
                  sT))))
        sZ

{-
$(singletonsOnly [d|
  deriving instance Foldable (Tuple2 a)
  |])
-}

type family Lambda_6989586621679081147_aiyU _f_6989586621679081117_aiyQ a_6989586621679081123_aiyR a_6989586621679081125_aiyS n_6989586621679081121_aiyV where
  Lambda_6989586621679081147_aiyU _f_6989586621679081117_aiyQ a_6989586621679081123_aiyR a_6989586621679081125_aiyS n_6989586621679081121_aiyV = MemptySym0
data Lambda_6989586621679081147Sym0 _f_69895866216790811176989586621679081144
  where
    Lambda_6989586621679081147Sym0KindInference :: SameKind (Apply Lambda_6989586621679081147Sym0 arg_aiyW) (Lambda_6989586621679081147Sym1 arg_aiyW) =>
                                                   Lambda_6989586621679081147Sym0 _f_69895866216790811176989586621679081144
type instance Apply Lambda_6989586621679081147Sym0 _f_69895866216790811176989586621679081144 = Lambda_6989586621679081147Sym1 _f_69895866216790811176989586621679081144
data Lambda_6989586621679081147Sym1 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145
  where
    Lambda_6989586621679081147Sym1KindInference :: SameKind (Apply (Lambda_6989586621679081147Sym1 _f_69895866216790811176989586621679081144) arg_aiyW) (Lambda_6989586621679081147Sym2 _f_69895866216790811176989586621679081144 arg_aiyW) =>
                                                   Lambda_6989586621679081147Sym1 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145
type instance Apply (Lambda_6989586621679081147Sym1 _f_69895866216790811176989586621679081144) a_69895866216790811236989586621679081145 = Lambda_6989586621679081147Sym2 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145
data Lambda_6989586621679081147Sym2 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146
  where
    Lambda_6989586621679081147Sym2KindInference :: SameKind (Apply (Lambda_6989586621679081147Sym2 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145) arg_aiyW) (Lambda_6989586621679081147Sym3 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 arg_aiyW) =>
                                                   Lambda_6989586621679081147Sym2 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146
type instance Apply (Lambda_6989586621679081147Sym2 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145) a_69895866216790811256989586621679081146 = Lambda_6989586621679081147Sym3 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146
data Lambda_6989586621679081147Sym3 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146 n_69895866216790811216989586621679081149
  where
    Lambda_6989586621679081147Sym3KindInference :: SameKind (Apply (Lambda_6989586621679081147Sym3 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146) arg_aiyW) (Lambda_6989586621679081147Sym4 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146 arg_aiyW) =>
                                                   Lambda_6989586621679081147Sym3 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146 n_69895866216790811216989586621679081149
type instance Apply (Lambda_6989586621679081147Sym3 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146) n_69895866216790811216989586621679081149 = Lambda_6989586621679081147_aiyU _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146 n_69895866216790811216989586621679081149
type family Lambda_6989586621679081147Sym4 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146 n_69895866216790811216989586621679081149 where
  Lambda_6989586621679081147Sym4 _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146 n_69895866216790811216989586621679081149 = Lambda_6989586621679081147_aiyU _f_69895866216790811176989586621679081144 a_69895866216790811236989586621679081145 a_69895866216790811256989586621679081146 n_69895866216790811216989586621679081149
type FoldMap_6989586621679081137 :: (~>) a_ahBz_acmX m_ahBy_acmY
                                    -> Tuple2 a_aikY a_ahBz_acmX -> m_ahBy_acmY
type family FoldMap_6989586621679081137 (a_aiyL :: (~>) a_ahBz_acmX m_ahBy_acmY) (a_aiyM :: Tuple2 a_aikY a_ahBz_acmX) :: m_ahBy_acmY where
  FoldMap_6989586621679081137 _f_6989586621679081117_aiyQ ('Tuple2 a_6989586621679081123_aiyR a_6989586621679081125_aiyS) = Apply (Apply MappendSym0 (Apply (Apply (Apply (Apply Lambda_6989586621679081147Sym0 _f_6989586621679081117_aiyQ) a_6989586621679081123_aiyR) a_6989586621679081125_aiyS) a_6989586621679081123_aiyR)) (Apply _f_6989586621679081117_aiyQ a_6989586621679081125_aiyS)
type FoldMap_6989586621679081137Sym0 :: (~>) ((~>) a_ahBz_acmX m_ahBy_acmY) ((~>) (Tuple2 a_aikY a_ahBz_acmX) m_ahBy_acmY)
data FoldMap_6989586621679081137Sym0 :: (~>) ((~>) a_ahBz_acmX m_ahBy_acmY) ((~>) (Tuple2 a_aikY a_ahBz_acmX) m_ahBy_acmY)
  where
    FoldMap_6989586621679081137Sym0KindInference :: SameKind (Apply FoldMap_6989586621679081137Sym0 arg_aiyN) (FoldMap_6989586621679081137Sym1 arg_aiyN) =>
                                                    FoldMap_6989586621679081137Sym0 a6989586621679081142
type instance Apply FoldMap_6989586621679081137Sym0 a6989586621679081142 = FoldMap_6989586621679081137Sym1 a6989586621679081142
type FoldMap_6989586621679081137Sym1 :: (~>) a_ahBz_acmX m_ahBy_acmY
                                        -> (~>) (Tuple2 a_aikY a_ahBz_acmX) m_ahBy_acmY
data FoldMap_6989586621679081137Sym1 (a6989586621679081142 :: (~>) a_ahBz_acmX m_ahBy_acmY) :: (~>) (Tuple2 a_aikY a_ahBz_acmX) m_ahBy_acmY
  where
    FoldMap_6989586621679081137Sym1KindInference :: SameKind (Apply (FoldMap_6989586621679081137Sym1 a6989586621679081142) arg_aiyN) (FoldMap_6989586621679081137Sym2 a6989586621679081142 arg_aiyN) =>
                                                    FoldMap_6989586621679081137Sym1 a6989586621679081142 a6989586621679081143
type instance Apply (FoldMap_6989586621679081137Sym1 a6989586621679081142) a6989586621679081143 = FoldMap_6989586621679081137 a6989586621679081142 a6989586621679081143
type FoldMap_6989586621679081137Sym2 :: (~>) a_ahBz_acmX m_ahBy_acmY
                                        -> Tuple2 a_aikY a_ahBz_acmX -> m_ahBy_acmY
type family FoldMap_6989586621679081137Sym2 (a6989586621679081142 :: (~>) a_ahBz_acmX m_ahBy_acmY) (a6989586621679081143 :: Tuple2 a_aikY a_ahBz_acmX) :: m_ahBy_acmY where
  FoldMap_6989586621679081137Sym2 a6989586621679081142 a6989586621679081143 = FoldMap_6989586621679081137 a6989586621679081142 a6989586621679081143
type family Lambda_6989586621679081167_aize _f_6989586621679081117_aiz9 _z_6989586621679081119_aiza a_6989586621679081131_aizb a_6989586621679081133_aizc n1_6989586621679081127_aizf n2_6989586621679081129_aizg where
  Lambda_6989586621679081167_aize _f_6989586621679081117_aiz9 _z_6989586621679081119_aiza a_6989586621679081131_aizb a_6989586621679081133_aizc n1_6989586621679081127_aizf n2_6989586621679081129_aizg = n2_6989586621679081129_aizg
data Lambda_6989586621679081167Sym0 _f_69895866216790811176989586621679081163
  where
    Lambda_6989586621679081167Sym0KindInference :: SameKind (Apply Lambda_6989586621679081167Sym0 arg_aizh) (Lambda_6989586621679081167Sym1 arg_aizh) =>
                                                   Lambda_6989586621679081167Sym0 _f_69895866216790811176989586621679081163
type instance Apply Lambda_6989586621679081167Sym0 _f_69895866216790811176989586621679081163 = Lambda_6989586621679081167Sym1 _f_69895866216790811176989586621679081163
data Lambda_6989586621679081167Sym1 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164
  where
    Lambda_6989586621679081167Sym1KindInference :: SameKind (Apply (Lambda_6989586621679081167Sym1 _f_69895866216790811176989586621679081163) arg_aizh) (Lambda_6989586621679081167Sym2 _f_69895866216790811176989586621679081163 arg_aizh) =>
                                                   Lambda_6989586621679081167Sym1 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164
type instance Apply (Lambda_6989586621679081167Sym1 _f_69895866216790811176989586621679081163) _z_69895866216790811196989586621679081164 = Lambda_6989586621679081167Sym2 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164
data Lambda_6989586621679081167Sym2 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165
  where
    Lambda_6989586621679081167Sym2KindInference :: SameKind (Apply (Lambda_6989586621679081167Sym2 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164) arg_aizh) (Lambda_6989586621679081167Sym3 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 arg_aizh) =>
                                                   Lambda_6989586621679081167Sym2 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165
type instance Apply (Lambda_6989586621679081167Sym2 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164) a_69895866216790811316989586621679081165 = Lambda_6989586621679081167Sym3 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165
data Lambda_6989586621679081167Sym3 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166
  where
    Lambda_6989586621679081167Sym3KindInference :: SameKind (Apply (Lambda_6989586621679081167Sym3 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165) arg_aizh) (Lambda_6989586621679081167Sym4 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 arg_aizh) =>
                                                   Lambda_6989586621679081167Sym3 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166
type instance Apply (Lambda_6989586621679081167Sym3 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165) a_69895866216790811336989586621679081166 = Lambda_6989586621679081167Sym4 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166
data Lambda_6989586621679081167Sym4 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169
  where
    Lambda_6989586621679081167Sym4KindInference :: SameKind (Apply (Lambda_6989586621679081167Sym4 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166) arg_aizh) (Lambda_6989586621679081167Sym5 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 arg_aizh) =>
                                                   Lambda_6989586621679081167Sym4 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169
type instance Apply (Lambda_6989586621679081167Sym4 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166) n1_69895866216790811276989586621679081169 = Lambda_6989586621679081167Sym5 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169
data Lambda_6989586621679081167Sym5 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169 n2_69895866216790811296989586621679081170
  where
    Lambda_6989586621679081167Sym5KindInference :: SameKind (Apply (Lambda_6989586621679081167Sym5 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169) arg_aizh) (Lambda_6989586621679081167Sym6 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169 arg_aizh) =>
                                                   Lambda_6989586621679081167Sym5 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169 n2_69895866216790811296989586621679081170
type instance Apply (Lambda_6989586621679081167Sym5 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169) n2_69895866216790811296989586621679081170 = Lambda_6989586621679081167_aize _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169 n2_69895866216790811296989586621679081170
type family Lambda_6989586621679081167Sym6 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169 n2_69895866216790811296989586621679081170 where
  Lambda_6989586621679081167Sym6 _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169 n2_69895866216790811296989586621679081170 = Lambda_6989586621679081167_aize _f_69895866216790811176989586621679081163 _z_69895866216790811196989586621679081164 a_69895866216790811316989586621679081165 a_69895866216790811336989586621679081166 n1_69895866216790811276989586621679081169 n2_69895866216790811296989586621679081170
type Foldr_6989586621679081154 :: (~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)
                                  -> b_ahBB_acn2 -> Tuple2 a_aikY a_ahBA_acn1 -> b_ahBB_acn2
type family Foldr_6989586621679081154 (a_aiz2 :: (~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)) (a_aiz3 :: b_ahBB_acn2) (a_aiz4 :: Tuple2 a_aikY a_ahBA_acn1) :: b_ahBB_acn2 where
  Foldr_6989586621679081154 _f_6989586621679081117_aiz9 _z_6989586621679081119_aiza ('Tuple2 a_6989586621679081131_aizb a_6989586621679081133_aizc) = Apply (Apply (Apply (Apply (Apply (Apply Lambda_6989586621679081167Sym0 _f_6989586621679081117_aiz9) _z_6989586621679081119_aiza) a_6989586621679081131_aizb) a_6989586621679081133_aizc) a_6989586621679081131_aizb) (Apply (Apply _f_6989586621679081117_aiz9 a_6989586621679081133_aizc) _z_6989586621679081119_aiza)
type Foldr_6989586621679081154Sym0 :: (~>) ((~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)) ((~>) b_ahBB_acn2 ((~>) (Tuple2 a_aikY a_ahBA_acn1) b_ahBB_acn2))
data Foldr_6989586621679081154Sym0 :: (~>) ((~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)) ((~>) b_ahBB_acn2 ((~>) (Tuple2 a_aikY a_ahBA_acn1) b_ahBB_acn2))
  where
    Foldr_6989586621679081154Sym0KindInference :: SameKind (Apply Foldr_6989586621679081154Sym0 arg_aiz5) (Foldr_6989586621679081154Sym1 arg_aiz5) =>
                                                  Foldr_6989586621679081154Sym0 a6989586621679081160
type instance Apply Foldr_6989586621679081154Sym0 a6989586621679081160 = Foldr_6989586621679081154Sym1 a6989586621679081160
type Foldr_6989586621679081154Sym1 :: (~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)
                                      -> (~>) b_ahBB_acn2 ((~>) (Tuple2 a_aikY a_ahBA_acn1) b_ahBB_acn2)
data Foldr_6989586621679081154Sym1 (a6989586621679081160 :: (~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)) :: (~>) b_ahBB_acn2 ((~>) (Tuple2 a_aikY a_ahBA_acn1) b_ahBB_acn2)
  where
    Foldr_6989586621679081154Sym1KindInference :: SameKind (Apply (Foldr_6989586621679081154Sym1 a6989586621679081160) arg_aiz5) (Foldr_6989586621679081154Sym2 a6989586621679081160 arg_aiz5) =>
                                                  Foldr_6989586621679081154Sym1 a6989586621679081160 a6989586621679081161
type instance Apply (Foldr_6989586621679081154Sym1 a6989586621679081160) a6989586621679081161 = Foldr_6989586621679081154Sym2 a6989586621679081160 a6989586621679081161
type Foldr_6989586621679081154Sym2 :: (~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)
                                      -> b_ahBB_acn2
                                         -> (~>) (Tuple2 a_aikY a_ahBA_acn1) b_ahBB_acn2
data Foldr_6989586621679081154Sym2 (a6989586621679081160 :: (~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)) (a6989586621679081161 :: b_ahBB_acn2) :: (~>) (Tuple2 a_aikY a_ahBA_acn1) b_ahBB_acn2
  where
    Foldr_6989586621679081154Sym2KindInference :: SameKind (Apply (Foldr_6989586621679081154Sym2 a6989586621679081160 a6989586621679081161) arg_aiz5) (Foldr_6989586621679081154Sym3 a6989586621679081160 a6989586621679081161 arg_aiz5) =>
                                                  Foldr_6989586621679081154Sym2 a6989586621679081160 a6989586621679081161 a6989586621679081162
type instance Apply (Foldr_6989586621679081154Sym2 a6989586621679081160 a6989586621679081161) a6989586621679081162 = Foldr_6989586621679081154 a6989586621679081160 a6989586621679081161 a6989586621679081162
type Foldr_6989586621679081154Sym3 :: (~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)
                                      -> b_ahBB_acn2 -> Tuple2 a_aikY a_ahBA_acn1 -> b_ahBB_acn2
type family Foldr_6989586621679081154Sym3 (a6989586621679081160 :: (~>) a_ahBA_acn1 ((~>) b_ahBB_acn2 b_ahBB_acn2)) (a6989586621679081161 :: b_ahBB_acn2) (a6989586621679081162 :: Tuple2 a_aikY a_ahBA_acn1) :: b_ahBB_acn2 where
  Foldr_6989586621679081154Sym3 a6989586621679081160 a6989586621679081161 a6989586621679081162 = Foldr_6989586621679081154 a6989586621679081160 a6989586621679081161 a6989586621679081162
instance PFoldable (Tuple2 a_aikY) where
  type FoldMap a_aiyH a_aiyI = Apply (Apply FoldMap_6989586621679081137Sym0 a_aiyH) a_aiyI
  type Foldr a_aiyX a_aiyY a_aiyZ = Apply (Apply (Apply Foldr_6989586621679081154Sym0 a_aiyX) a_aiyY) a_aiyZ
instance SFoldable (Tuple2 a_aikY) where
  sFoldMap ::
    forall (a_ahBz_acm7 :: Type)
           (m_ahBy_acm8 :: Type)
           (t_ahCS_acm9 :: (~>) a_ahBz_acm7 m_ahBy_acm8)
           (t_ahCT_acma :: Tuple2 a_aikY a_ahBz_acm7). SMonoid m_ahBy_acm8 =>
                                                       Sing t_ahCS_acm9
                                                       -> Sing t_ahCT_acma
                                                          -> Sing (Apply (Apply (FoldMapSym0 :: TyFun ((~>) a_ahBz_acm7 m_ahBy_acm8) ((~>) (Tuple2 a_aikY a_ahBz_acm7) m_ahBy_acm8)
                                                                                                -> Type) t_ahCS_acm9) t_ahCT_acma)
  sFoldr ::
    forall (a_ahBA_acmb :: Type)
           (b_ahBB_acmc :: Type)
           (t_ahCX_acmd :: (~>) a_ahBA_acmb ((~>) b_ahBB_acmc b_ahBB_acmc))
           (t_ahCY_acme :: b_ahBB_acmc)
           (t_ahCZ_acmf :: Tuple2 a_aikY a_ahBA_acmb). Sing t_ahCX_acmd
                                                       -> Sing t_ahCY_acme
                                                          -> Sing t_ahCZ_acmf
                                                             -> Sing (Apply (Apply (Apply (FoldrSym0 :: TyFun ((~>) a_ahBA_acmb ((~>) b_ahBB_acmc b_ahBB_acmc)) ((~>) b_ahBB_acmc ((~>) (Tuple2 a_aikY a_ahBA_acmb) b_ahBB_acmc))
                                                                                                        -> Type) t_ahCX_acmd) t_ahCY_acme) t_ahCZ_acmf)
  sFoldMap
    (_sf_6989586621679081117 :: Sing _f_6989586621679081117_aiyQ)
    (STuple2 (sA_6989586621679081123 :: Sing a_6989586621679081123_aiyR)
             (sA_6989586621679081125 :: Sing a_6989586621679081125_aiyS))
    = (applySing
         ((applySing ((singFun2 @MappendSym0) sMappend))
            ((applySing
                ((singFun1
                    @(Apply (Apply (Apply Lambda_6989586621679081147Sym0 _f_6989586621679081117_aiyQ) a_6989586621679081123_aiyR) a_6989586621679081125_aiyS))
                   (\ sN_6989586621679081121
                      -> case sN_6989586621679081121 of
                           (_ :: Sing n_6989586621679081121_aiyV) -> sMempty)))
               sA_6989586621679081123)))
        ((applySing _sf_6989586621679081117) sA_6989586621679081125)
  sFoldr
    (_sf_6989586621679081117 :: Sing _f_6989586621679081117_aiz9)
    (_sz_6989586621679081119 :: Sing _z_6989586621679081119_aiza)
    (STuple2 (sA_6989586621679081131 :: Sing a_6989586621679081131_aizb)
             (sA_6989586621679081133 :: Sing a_6989586621679081133_aizc))
    = (applySing
         ((applySing
             ((singFun2
                 @(Apply (Apply (Apply (Apply Lambda_6989586621679081167Sym0 _f_6989586621679081117_aiz9) _z_6989586621679081119_aiza) a_6989586621679081131_aizb) a_6989586621679081133_aizc))
                (\ sN1_6989586621679081127 sN2_6989586621679081129
                   -> case ((,) sN1_6989586621679081127) sN2_6989586621679081129 of
                        (,) (_ :: Sing n1_6989586621679081127_aizf)
                            (_ :: Sing n2_6989586621679081129_aizg)
                          -> sN2_6989586621679081129)))
            sA_6989586621679081131))
        ((applySing
            ((applySing _sf_6989586621679081117) sA_6989586621679081133))
           _sz_6989586621679081119)

{-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module SlowComp where

import Control.Monad

-------------------------------------------------------------------------------
-- Usual Peano integers.


class NatInt a where
    natInt :: a -> Int

data D0 n = D0 {d0Arg :: n}
data D1 n = D1 {d1Arg :: n}

data C0
data C1

class DPosInt n where posInt :: n -> (Int,Int)
instance DPosInt () where posInt _ = (0,1)
instance DPosInt n => DPosInt (D0 n) where
        posInt a = (dsum,w*2)
                where
                        (dsum,w) = posInt $ d0Arg a
instance DPosInt n => DPosInt (D1 n) where
        posInt a = (dsum+w,w*2)
                where
                        (dsum,w) = posInt $ d1Arg a

instance NatInt () where natInt _ = 0
instance DPosInt n => NatInt (D0 n) where natInt a = fst $ posInt a
instance DPosInt n => NatInt (D1 n) where natInt a = fst $ posInt a

type family DRev a
type instance DRev a = DRev' a ()

type family DRev' x acc
type instance DRev' () acc = acc
type instance DRev' (D0 a) acc = DRev' a (D0 acc)
type instance DRev' (D1 a) acc = DRev' a (D1 acc)

type family DAddC c a b
type instance DAddC C0 (D0 a) (D0 b) = D0 (DAddC C0 a b)
type instance DAddC C0 (D1 a) (D0 b) = D1 (DAddC C0 a b)
type instance DAddC C0 (D0 a) (D1 b) = D1 (DAddC C0 a b)
type instance DAddC C0 (D1 a) (D1 b) = D0 (DAddC C1 a b)
type instance DAddC C1 (D0 a) (D0 b) = D1 (DAddC C0 a b)
type instance DAddC C1 (D1 a) (D0 b) = D0 (DAddC C1 a b)
type instance DAddC C1 (D0 a) (D1 b) = D0 (DAddC C1 a b)
type instance DAddC C1 (D1 a) (D1 b) = D1 (DAddC C1 a b)
type instance DAddC C0 ()     ()     = ()
type instance DAddC C1 ()     ()     = D1 ()
type instance DAddC c  (D0 a) ()     = DAddC c (D0 a) (D0 ())
type instance DAddC c  (D1 a) ()     = DAddC c (D1 a) (D0 ())
type instance DAddC c  ()     (D0 b) = DAddC c (D0 b) (D0 ())
type instance DAddC c  ()     (D1 b) = DAddC c (D1 b) (D0 ())

type family DNorm a
type instance DNorm () = D0 ()
type instance DNorm (D0 ()) = D0 ()
type instance DNorm (D0 (D1 a)) = D1 a
type instance DNorm (D0 (D0 a)) = DNorm a
type instance DNorm (D1 a) = D1 a

type family DPlus a b
type instance DPlus a b = DNorm (DRev (DAddC C0 (DRev a) (DRev b)))

type family DDepth a
type instance DDepth () = D0 ()
type instance DDepth (D0 ()) = D0 ()
type instance DDepth (D1 ()) = D1 ()
type instance DDepth (D1 (D0 n)) = DPlus ONE (DDepth (D1 n))
type instance DDepth (D1 (D1 n)) = DPlus ONE (DDepth (D1 n))

type family DLog2 a
type instance DLog2 a = DDepth a

type ZERO = D0 ()

type ONE = D1 ()
type TWO = DPlus ONE ONE
type THREE = DPlus ONE TWO
type FOUR = DPlus TWO TWO
type FIVE = DPlus ONE FOUR
type SIX = DPlus TWO FOUR
type SEVEN = DPlus ONE SIX
type EIGHT = DPlus FOUR FOUR
type NINE = DPlus FOUR FIVE
type TEN = DPlus EIGHT TWO
type SIZE8  = EIGHT
type SIZE9  = NINE
type SIZE10 = TEN
type SIZE12 = DPlus SIX SIX
type SIZE15 = DPlus EIGHT SEVEN
type SIZE16 = DPlus EIGHT EIGHT
type SIZE17 = DPlus ONE SIZE16
type SIZE24 = DPlus SIZE8 SIZE16
type SIZE32 = DPlus SIZE8 SIZE24
type SIZE30 = DPlus SIZE24 SIX

-------------------------------------------------------------------------------
-- Description of CPU.

class CPU cpu where
        -- register address.
        type RegAddrSize cpu
        -- register width
        type RegDataSize cpu
        -- immediate width.
        type ImmSize cpu
        -- variables in CPU - register indices, command format variables, etc.
        type CPUVars cpu :: * -> *

data Const size = Const Integer

data Var cpu size where
        Temp :: Int -> Var cpu size
        Var :: CPUVars cpu size -> Var cpu size

-------------------------------------------------------------------------------
-- Command description monad.

data Command cpu where
        Command :: (Var cpu size) -> (Operation cpu size) -> Command cpu

type RegVar cpu = Var cpu (RegDataSize cpu)
type Immediate cpu = Const (ImmSize cpu)

data Operation cpu resultSize where
        Add :: RegVar cpu -> Either (Immediate cpu) (RegVar cpu) -> Operation cpu (RegDataSize cpu)
        Sub :: RegVar cpu -> Either (Immediate cpu) (RegVar cpu) -> Operation cpu (RegDataSize cpu)

type CDM cpu a = IO a

($=) :: CPU cpu => Var cpu size -> Operation cpu size -> CDM cpu ()
var $= op = undefined

tempVar :: CPU cpu => CDM cpu (Var cpu size)
tempVar = do
        cnt <- liftM fst undefined
        return $ Temp cnt

op :: CPU cpu => Operation cpu size -> CDM cpu (Var cpu size)
op operation = do
        v <- tempVar
        v $= operation
        return v

-------------------------------------------------------------------------------
-- Dummy CPU.

data DummyCPU = DummyCPU

data DummyVar size where
        DummyFlag :: Flag -> DummyVar ONE
        DummyReg :: Int -> DummyVar SIZE16
        DummyZero :: DummyVar SIZE16

data Flag = C | Z | N | V

instance CPU DummyCPU where
        type RegAddrSize DummyCPU = FIVE
        type RegDataSize DummyCPU = SIZE16
        type ImmSize DummyCPU = SIZE12
        type CPUVars DummyCPU = DummyVar

-------------------------------------------------------------------------------
-- Long compiling program.

cnst :: Integer -> Either (Immediate DummyCPU) (RegVar DummyCPU)
cnst x = Left (Const x)

longCompilingProgram :: CDM DummyCPU ()
longCompilingProgram = do
-- the number of lines below greatly affects compilation time.
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        x10 <- op $ Add (Var DummyZero) (cnst 10)
        return ()

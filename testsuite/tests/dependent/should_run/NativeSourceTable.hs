{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, FlexibleContexts, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# LANGUAGE NoStarIsType, RankNTypes, TypeFamilies #-}

module Main where

import ClosedTableLibrary
import Control.Exception (ArithException(DivideByZero), evaluate, throw, try)
import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import System.Mem.StableName (makeStableName)
import PackedVecLibrary (type (+), type (*), Vec(..), Nat(..), appendVec, boolVector, flattenRows, isSuccessor, listCount, natToInt, plus, previous, times,
                         replicateAt, replicateNative, sameNat, sourceLength, sourceVector, toList)

type Flipped = '[flipBit 'False, flipBit 'True]

flipProof :: Flipped :~: '[ 'True, 'False]
flipProof = Refl

type Signals = '[signal 'Red, signal 'Amber, signal 'Green]

signalProof :: Signals :~: '[ 'Stop, 'Stop, 'Go]
signalProof = Refl

-- The local table and its input type both follow the type that uses them.
type FirstSignal = atPhase 'Start

phaseProof :: FirstSignal :~: 'Stop
phaseProof = Refl

data Phase = Start | Proceed

atPhase :: Phase -> Signal
atPhase Start   = Stop
atPhase Proceed = Go

-- A known constructor is enough to project a field whose value is unknown.
fieldProof :: Proxy n -> previous ('S n) :~: n
fieldProof _ = Refl

type LastSignal = after ('Change 'Stop 'Go)

afterProof :: LastSignal :~: 'Go
afterProof = Refl

data Change = Change Signal Signal

after :: Change -> Signal
after (Change _ new) = new

observeSignal :: foreach (s :: Signal) -> (Proxy s, Signal)
observeSignal s = (Proxy, s)

-- The constructor field consumes a Signal, while the nested call takes a Lamp.
afterLamp :: foreach (l :: Lamp) -> (Proxy (after ('Change 'Stop (signal l))), Signal)
afterLamp l = observeSignal (after (Change Stop (signal l)))

lengthProof :: listCount '[ 'True, 'False] :~: 'S ('S 'Z)
lengthProof = Refl

tailProof :: Proxy (xs :: [Bool]) -> listCount ('True ': xs) :~: 'S (listCount xs)
tailProof _ = Refl

type EmptyInts = sourceLength @Int '[]

emptyProof :: EmptyInts :~: 'Z
emptyProof = Refl

polyStep :: forall a (x :: a) (xs :: [a]). Proxy x -> Proxy xs
         -> sourceLength @a (x ': xs) :~: 'S (sourceLength xs)
polyStep _ _ = Refl

-- Both the type and the recursive term can supply the specified argument.
type Singleton = count @Bool '[ 'False]

count :: forall a. [a] -> Nat
count []       = Z
count (_ : xs) = S (count @a xs)

localSingleton :: Singleton :~: 'S 'Z
localSingleton = Refl

-- The recursive equation can precede its input datatype in this group.
bitCount :: Bits -> Nat
bitCount End = Z
bitCount (Bit _ bs) = S (bitCount bs)

data Bits = End | Bit Bool Bits

bitsProof :: bitCount ('Bit 'False 'End) :~: 'S 'Z
bitsProof = Refl

literalThree :: Vec Bool 3
literalThree = VCons True (VCons False (VCons True VNil))

filledThree :: Vec Bool 3
filledThree = replicateNative 3 True

oneMore :: foreach (n :: Nat) -> Vec Bool (n + 1)
oneMore n = replicateNative (n + 1) False

headOfNonempty :: Vec a (1 + n) -> a
headOfNonempty (VCons x _) = x

-- The list cell determines both the element type and nonemptiness.
firstCell :: a -> [a] -> a
firstCell x xs = headOfNonempty (sourceVector (x : xs))

-- The inner constructor supplies both parameters of the outer constructor.
data Parcel a b = Leaf a b | Layer (Parcel a b)

parcelTag :: forall a b. foreach (p :: Parcel a b) -> Proxy p
parcelTag _ = Proxy

-- The later tag fixes the first addend before the vector determines the tail.
withPrefix :: Vec a (p + k) -> Proxy p -> [a]
withPrefix xs _ = toList xs

appendOne :: Vec a n -> a -> Vec a (n + 1)
appendOne xs x = appendVec xs (VCons x VNil)

literalSum :: (1 + 2) :~: (3 :: Nat)
literalSum = Refl

literalProduct :: (1 + 2 * 3) :~: (7 :: Nat)
literalProduct = Refl

-- A change to left association would make this expression equal to three.
infixr 5 <.>
(<.>) :: Nat -> Nat -> Nat
Z   <.> m = S m
S n <.> m = S (S (n <.> m))

-- | Use the completed equations of an infix function in this module.
localInfix :: foreach (n :: Nat) -> Vec Bool (n <.> Z)
localInfix n = let cells = n <.> Z in replicateNative cells True

-- | Use the local operator's equations together with a selected Num method.
localFormula :: foreach (n :: Nat) -> Vec Bool (n <.> (n + 1))
localFormula n = let cells = (n <.> ((+) @Nat n 1) :: Nat) in replicateNative cells True

fixityProof :: (0 <.> 0 <.> 0) :~: (2 :: Nat)
fixityProof = Refl

baseSum :: Proxy (m :: Nat) -> plus 'Z m :~: m
baseSum _ = Refl

stepSum :: Proxy (n :: Nat) -> Proxy (m :: Nat)
        -> plus ('S n) m :~: 'S (plus n m)
stepSum _ _ = Refl

replicateSum :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> a -> Vec a (n + m)
replicateSum n m x = replicateNative @a (n + m) x

-- | Use a local product as a retained length.
replicateProductPrefix :: foreach (rows :: Nat) -> foreach (cols :: Nat)
                       -> Vec Bool (1 + rows * cols)
replicateProductPrefix rows cols =
  let cells = rows * cols
  in replicateNative (1 + cells) True

-- | Keep both references to the local product.
{-# NOINLINE sharedProduct #-}
sharedProduct :: foreach (rows :: Nat) -> foreach (cols :: Nat)
              -> (Nat, Nat, Vec Bool (1 + rows * cols))
sharedProduct rows cols =
  let cells = rows * cols
  in (cells, cells, replicateNative (1 + cells) True)

-- | Keep both references below separate constructors.
{-# NOINLINE sharedInnerProduct #-}
sharedInnerProduct :: foreach (rows :: Nat) -> foreach (cols :: Nat)
                   -> (Nat, Nat, Vec Bool (1 + rows * cols))
sharedInnerProduct rows cols =
  let total, cells :: Nat
      total = 1 + cells
      cells = rows * cols
  in (S cells, S cells, replicateNative total True)

-- | Use the caller's multiplication independently of the vector length.
ordinaryProduct :: Num Nat => foreach (rows :: Nat) -> foreach (cols :: Nat)
                -> (Nat, Vec Bool (times rows cols))
ordinaryProduct rows cols =
  let cells = rows * cols
  in (cells, replicateNative (times rows cols) True)

-- The call is the only use of this source function. Its definition follows it.
localOne :: Vec Bool 1
localOne = replicateNative @Bool (countFlag True) True

countFlag :: Bool -> Nat
countFlag False = Z
countFlag True  = S Z

{-# NOINLINE tagLength #-}
tagLength :: foreach (n :: Nat) -> Proxy n
tagLength _n = Proxy

main :: IO ()
main = case (flipProof, signalProof, phaseProof) of
  (Refl, Refl, Refl) -> do
    print (map flipBit [False, True])
    print (map signal [Red, Amber, Green])
    forced <- try (evaluate (flipBit (throw DivideByZero)))
    print (forced == Left DivideByZero)
    print (natToInt (previous (S (S Z))), isSuccessor (S (throw DivideByZero)))
    demanded <- try (evaluate (natToInt (previous (S (throw DivideByZero)))))
    print (demanded == Left DivideByZero)
    print (toList (boolVector [False, True]), natToInt (listCount [throw DivideByZero]))
    print (isSuccessor (listCount (True : throw DivideByZero)))
    spine <- try (evaluate (natToInt (listCount (True : throw DivideByZero))))
    print (spine == Left DivideByZero)
    let ints = [11, 22] :: [Int]
    print (toList (sourceVector @Int ints))
    tailRef <- newIORef (throw DivideByZero :: [Int])
    tailCells <- readIORef tailRef
    print (firstCell 37 tailCells)
    case parcelTag (Layer (Leaf True tailCells)) of Proxy -> print True
    print (natToInt (plus (S Z) (S Z)), isSuccessor (plus (S Z) (throw DivideByZero)))
    print (toList literalThree, headOfNonempty literalThree, natToInt (2 + 3))
    print (toList (replicateAt 3 True), length (toList (replicateAt 3 (undefined :: Int))))
    (n,m) <- newIORef ((1, 2) :: (Nat, Nat)) >>= readIORef
    print (toList (replicateSum n m False))
    print (toList filledThree, toList (oneMore n))
    bottom <- newIORef (throw DivideByZero :: Nat) >>= readIORef
    -- Both lengths expose one constructor without evaluating the tail.
    print (headOfNonempty (replicateNative (1 + bottom) 'a'),
           headOfNonempty (replicateNative (S bottom) 'b'))
    print (take 1 (withPrefix (replicateNative (1 + bottom) True) (Proxy @('S 'Z))))
    let tag :: Proxy (bottom + m)
        tag = tagLength (plus bottom m)
        cells = toList (replicateSum n bottom True)
    case tag of Proxy -> print True
    case cells of
        x : rest -> do
            print x
            forcedTail <- try (evaluate (null rest))
            print (forcedTail == Left DivideByZero)
        [] -> fail "Expected one cell before the undefined tail"
    print (toList localOne)
    let row :: Vec Bool n
        row = replicateNative n True
        rectangle = replicateNative m row
        flattened :: Vec Bool (m * n)
        flattened = flattenRows rectangle
        filled :: Vec Bool (m * n)
        filled = replicateNative (m * n) False
    case literalProduct of Refl -> print (toList flattened, toList filled)
    let prefix :: Vec Bool (1 + n * m)
        prefix = replicateNative (1 + n * m) False
        grouped :: Vec Bool ((1 + n) * m)
        grouped = replicateNative ((1 + n) * m) True
    print (toList prefix, toList grouped)
    rightSuccessor <- try (evaluate (isSuccessor (bottom + 1)))
    case ordinaryProduct n m of
        (ordinaryCells, checkedVector) ->
            print (toList (replicateProductPrefix n m),
                   headOfNonempty (replicateProductPrefix bottom m),
                   natToInt ordinaryCells, length (toList checkedVector),
                   rightSuccessor == Left DivideByZero)
    case sharedProduct n m of
        (left, right, _) -> do
            leftName <- makeStableName left
            rightName <- makeStableName right
            print (leftName == rightName)
    case sharedInnerProduct n m of
        (S left, S right, _) -> do
            leftName <- makeStableName left
            rightName <- makeStableName right
            print (leftName == rightName)
    lamp <- newIORef Green >>= readIORef
    print (snd (afterLamp lamp))
    let partial = toList (replicateNative (S (times bottom m)) 'x')
        emptyCells = toList (replicateNative (times 0 (plus bottom n)) True)
    case partial of
        x : rest -> do
            tailResult <- try (evaluate (null rest))
            print (x, tailResult == Left DivideByZero, null emptyCells)
        [] -> fail "Expected a cell before the nested product"
    print (length (toList (localInfix n)), length (toList (localFormula n)))
    print (toList (signedPrefix n m), take 1 (toList (signedPrefix bottom m)))
    print (snd (checkedProduct atZero 0 m), snd (checkedProduct atZero n m))
    print (headOfProduct n bottom, whereHead n bottom, toList localThree)
    print (validatedHead Z, validatedHead (S Z), nestedHead bottom, guardedFallback True)

-- The call infers the erased tail through a checked constructor.
-- Its tail can use a caller-supplied multiplication dictionary.
headOfProduct :: Num Nat => foreach (r :: Nat) -> foreach (c :: Nat) -> Char
headOfProduct r c =
  let total, area :: Nat
      total = S area
      area = r * c
  in headOfNonempty (replicateNative total 's')

localThree :: Vec Bool 3
localThree = let count :: Nat; count = 3 in replicateNative count True

whereHead :: foreach (r :: Nat) -> foreach (c :: Nat) -> Char
whereHead r c = headOfNonempty (replicateNative total 'w')
  where
    total, area :: Nat
    total = 1 + area
    area = r * c

-- The test supplies evidence even if the caller supplied the addition method.
validatedHead :: Num Nat => foreach (extent :: Nat) -> Maybe Char
validatedHead extent =
  let size :: Nat; size = 1 + extent
  in case sameNat size (S Z) of
       Just Refl -> Just (headOfNonempty (replicateNative size 'g'))
       Nothing -> Nothing

nestedHead :: foreach (n :: Nat) -> Char
nestedHead n =
  let total, one :: Nat
      total = plus one n
      one = S Z
  in headOfNonempty (replicateNative total 'n')

-- A failed guard goes on to the next equation.
guardedFallback :: Bool -> Int
guardedFallback b | b && reject = value
  where
    reject = False
    value = error "forced a value from a failed guard"
guardedFallback _ = 42

-- Check references to a later declaration in a shared local signature.
signedPrefix :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Bool (1 + rows * cols)
signedPrefix rows cols =
  let total, cells :: Nat
      total = 1 + cells
      cells = rows * cols
  in replicateNative total True

-- A GADT assumption about a local name must still solve the corresponding Wanted.
type family Flag (x :: Nat) :: Bool
type instance Flag Z = True
data On (x :: Nat) where
  On :: Flag x ~ True => On x

atZero :: foreach (x :: Nat) -> Maybe (On x)
atZero Z = Just On
atZero (S _) = Nothing

requireOn :: foreach (x :: Nat) -> (Flag x :~: True) -> Bool
requireOn _ Refl = True

checkedProduct :: (foreach (x :: Nat) -> Maybe (On x))
               -> foreach (r :: Nat) -> foreach (c :: Nat) -> (Vec Bool (times r c), Bool)
checkedProduct witness r c =
  let area :: Nat
      area = r * c
  in case witness area of
       Just On -> (replicateNative area True, requireOn area Refl)
       Nothing -> (replicateNative area True, False)

-- The selector rule applies to Int. Local vector lengths select Num Nat.
{-# RULES "Int multiply by one" forall (n :: Int). n * 1 = n #-}

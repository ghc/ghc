{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module T26910_Input where

-- base
import Prelude
  hiding ( negate, fromInteger )
import qualified Prelude
import Control.Arrow
  ( (>>>), arr, first, returnA )
import Data.Kind
  ( Type )
import Data.Tuple
  ( Solo(..) )
import GHC.Exts
  ( TYPE, RuntimeRep(..), LiftedRep
  , IsList (..)
  , Int#, Word#
  )
import GHC.StaticPtr
  ( StaticPtr )

-- template-haskell
import qualified Language.Haskell.TH as TH

--------------------------------------------------------------------------------

ifThenElse :: Bool -> a -> a -> a
ifThenElse c t f =
  case c of
    True  -> t
    False -> f

data MyRec = MyRec { recInt :: Int, recBool :: Bool }

-- Used to test ArithSeq with overloaded fromList (OverloadedLists)
newtype MyList a = MyList [a]
instance IsList (MyList a) where
  type Item (MyList a) = a
  fromList = MyList
  toList (MyList xs) = xs

-- RecordCon
e_reccon :: MyRec
e_reccon = MyRec { recInt = 1, recBool = True }

-- NegApp
e_negapp :: T Word
e_negapp = -(1 :: Int)

type R :: Type -> RuntimeRep
type family R a where
  R Word = LiftedRep

type T :: forall (a :: Type) -> TYPE (R a)
type family T a where
  T Word = Int

-- Weird RebindableSyntax negation that involves casts
negate :: T Word -> T Word
negate = Prelude.negate

-- HsProc
e_proc :: Int -> Int
e_proc = proc x -> returnA -< x

-- ArithSeq (with OverloadedLists)
e_arith_ol :: MyList Int
e_arith_ol = [1..10 :: Int]

-- XExpr (ConLikeTc)
e_var :: Either Int Bool
e_var = Left 3

-- HsLit
e_lit :: Char
e_lit = 'x'

-- HsOverLit
e_overlit :: T Word
e_overlit = 42

fromInteger :: Integer -> T Word
fromInteger = Prelude.fromInteger

-- HsLam
e_lam :: Bool -> Bool
e_lam = \ x -> not x

-- HsApp
e_app :: Bool
e_app = not True

-- HsAppType
e_apptype :: Bool -> Bool
e_apptype = id @Bool

-- HsPar
e_par :: Bool
e_par = (True)

-- ExplicitTuple
e_tuple1 :: (Int, Bool)
e_tuple1 = (1 :: Int, True)

-- ExplicitTuple + TupleSections
e_tuple2 :: Int -> (Int, Bool)
e_tuple2 = (, True)

-- ExplicitTuple one-tuple
e_tuple3 :: Solo Char
e_tuple3 =
  $( return $ TH.TupE [ Just $ TH.LitE ( TH.CharL 'x' ) ] )

-- Unboxed tuple
e_utuple1 :: () -> (# Int, Int# #)
e_utuple1 _ = (# 1, 1# #)

-- Unboxed tuple + TupleSections
e_utuple2 :: Int -> Int# -> (# Int, Int# #)
e_utuple2 = (# , #)

-- Unboxed sums
e_usum :: () -> (# Int# | Word# #)
e_usum _ = (# 1# | #)

-- HsCase
e_case :: Bool
e_case = case id True of { True -> False; False -> True }

-- HsIf
e_if :: Char
e_if = if id True then 'x' else 'y'

-- HsMultiIf
e_multiif :: Int
e_multiif = if | id True   -> (1 :: Int)
               | otherwise -> (2 :: Int)

-- HsLet
e_let :: Int
e_let = let x = 1 :: Int in x

-- ExplicitList
e_list :: [Int]
e_list = [1 :: Int, 2, 3]

-- ArithSeq with overloaded fromList
e_arith :: MyList Int
e_arith = [1 :: Int ..]

-- ExprWithTySig
e_tysig :: Bool
e_tysig = (True :: Bool)

-- HsDo ListComp
e_listcomp :: [Int]
e_listcomp = [x | x <- [1 :: Int, 2, 3]]

-- HsRecSelTc
e_recsel :: MyRec -> Int
e_recsel = recInt

-- HsUntypedBracket
e_ubracket :: TH.Q TH.Exp
e_ubracket = [| 'y' |]

-- HsTypedBracket
e_tbracket :: TH.Code TH.Q Char
e_tbracket = [|| 'z' ||]

-- HsStatic
e_static :: StaticPtr Bool
e_static = static True

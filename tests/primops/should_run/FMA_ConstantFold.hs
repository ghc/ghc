{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE LexicalNegation  #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
  ( unless )
import Data.IORef
  ( newIORef, readIORef, writeIORef )
import GHC.Exts
  ( Float(..), Float#, Double(..), Double#
  , fmaddFloat# , fmsubFloat# , fnmaddFloat# , fnmsubFloat#
  , fmaddDouble#, fmsubDouble#, fnmaddDouble#, fnmsubDouble#
  )
import GHC.Float
  ( castFloatToWord32, castDoubleToWord64 )
import System.Exit
  ( exitFailure, exitSuccess )

--------------------------------------------------------------------------------

-- NB: This test tests constant folding for fused multiply-add operations.
-- See FMA_Primops for the test of the primops.

-- Want "on-the-nose" equality to make sure we properly distinguish 0.0 and -0.0.
class StrictEq a where
  strictlyEqual :: a -> a -> Bool
instance StrictEq Float where
  strictlyEqual x y = castFloatToWord32 x == castFloatToWord32 y
instance StrictEq Double where
  strictlyEqual x y = castDoubleToWord64 x == castDoubleToWord64 y

class FMA a where
  fmadd, fmsub, fnmadd, fnmsub :: a -> a -> a -> a
instance FMA Float where
  fmadd  (F# x) (F# y) (F# z) = F# ( fmaddFloat# x y z)
  fmsub  (F# x) (F# y) (F# z) = F# ( fmsubFloat# x y z)
  fnmadd (F# x) (F# y) (F# z) = F# (fnmaddFloat# x y z)
  fnmsub (F# x) (F# y) (F# z) = F# (fnmsubFloat# x y z)
  {-# INLINE fmadd  #-}
  {-# INLINE fnmadd #-}
  {-# INLINE fmsub  #-}
  {-# INLINE fnmsub #-}
instance FMA Double where
  fmadd  (D# x) (D# y) (D# z) = D# ( fmaddDouble# x y z)
  fmsub  (D# x) (D# y) (D# z) = D# ( fmsubDouble# x y z)
  fnmadd (D# x) (D# y) (D# z) = D# (fnmaddDouble# x y z)
  fnmsub (D# x) (D# y) (D# z) = D# (fnmsubDouble# x y z)
  {-# INLINE fmadd  #-}
  {-# INLINE fnmadd #-}
  {-# INLINE fmsub  #-}
  {-# INLINE fnmsub #-}

main :: IO ()
main = do

    exit_ref <- newIORef False

    let
      it :: forall a. ( StrictEq a, Show a ) => String -> a -> a -> IO ()
      it desc actual expected =
        unless (actual `strictlyEqual` expected) do
          writeIORef exit_ref True
          putStrLn $ unlines
            [ "FAIL " ++ desc
            , "  expected: " ++ show expected
            , "    actual: " ++ show actual ]

    -- NB: throughout this test, we are using "round to nearest".

    -- fmadd: x * y + z

    -- Float
    it "fmaddFloat#: sniff test"
      ( fmadd @Float 2 3 1 ) 7

    it "fmaddFloat#: excess precision"
      ( fmadd @Float 0.99999994 1.00000012 -1 ) 5.96046377e-08

    it "fmaddFloat#: +0 + +0 rounds properly"
      ( fmadd @Float 1 0 0 ) 0

    it "fmaddFloat#: +0 + -0 rounds properly"
      ( fmadd @Float 1 0 -0 ) 0

    it "fmaddFloat#: -0 + +0 rounds properly"
      ( fmadd @Float 1 -0 0 ) 0

    it "fmaddFloat#: -0 + -0 rounds properly"
      ( fmadd @Float 1 -0 -0 ) -0

    -- Double
    it "fmaddDouble#: sniff test"
      ( fmadd @Double 2 3 1 ) 7

    it "fmaddDouble#: excess precision"
      ( fmadd @Double 0.99999999999999989 1.0000000000000002 -1 ) 1.1102230246251563e-16

    it "fmaddDouble#: +0 + +0 rounds properly"
      ( fmadd @Double 1 0 0 ) 0

    it "fmaddDouble#: +0 + -0 rounds properly"
      ( fmadd @Double 1 0 -0 ) 0

    it "fmaddDouble#: -0 + +0 rounds properly"
      ( fmadd @Double 1 -0 0 ) 0

    it "fmaddDouble#: -0 + -0 rounds properly"
      ( fmadd @Double 1 -0 -0 ) -0

    -- fmsub: x * y - z

    -- Float
    it "fmsubFloat#: sniff test"
      ( fmsub @Float 2 3 1 ) 5.0

    it "fmsubFloat#: excess precision"
      ( fmsub @Float 0.99999994 1.00000012 1 ) 5.96046377e-08

    it "fmsubFloat#: +0 + +0 rounds properly"
      ( fmsub @Float 1 0 0 ) 0

    it "fmsubFloat#: +0 + -0 rounds properly"
      ( fmsub @Float 1 0 -0 ) 0

    it "fmsubFloat#: -0 + +0 rounds properly"
      ( fmsub @Float 1 -0 0 ) -0

    it "fmsubFloat#: -0 + -0 rounds properly"
      ( fmsub @Float 1 -0 -0 ) 0

    -- Double
    it "fmsubDouble#: sniff test"
      ( fmsub @Double 2 3 1 ) 5.0

    it "fmsubDouble#: excess precision"
      ( fmsub @Double 0.99999999999999989 1.0000000000000002 1 ) 1.1102230246251563e-16

    it "fmsubDouble#: +0 + +0 rounds properly"
      ( fmsub @Double 1 0 0 ) 0

    it "fmsubDouble#: +0 + -0 rounds properly"
      ( fmsub @Double 1 0 -0 ) 0

    it "fmsubDouble#: -0 + +0 rounds properly"
      ( fmsub @Double 1 -0 0 ) -0

    it "fmsubDouble#: -0 + -0 rounds properly"
      ( fmsub @Double 1 -0 -0 ) 0

    -- fnmadd: - x * y + z

    -- Float
    it "fnmaddFloat#: sniff test"
      ( fnmadd @Float 2 3 1 ) -5.0

    it "fnmaddFloat#: excess precision"
      ( fnmadd @Float 0.99999994 1.00000012 1 ) -5.96046377e-08

    it "fnmaddFloat#: +0 + +0 rounds properly"
      ( fnmadd @Float 1 0 0 ) 0

    it "fnmaddFloat#: +0 + -0 rounds properly"
      ( fnmadd @Float 1 0 -0 ) -0

    it "fnmaddFloat#: -0 + +0 rounds properly"
      ( fnmadd @Float 1 -0 0 ) 0

    it "fnmaddFloat#: -0 + -0 rounds properly"
      ( fnmadd @Float 1 -0 -0 ) 0

    -- Double
    it "fnmaddDouble#: sniff test"
      ( fnmadd @Double 2 3 1 ) -5.0

    it "fnmaddDouble#: excess precision"
      ( fnmadd @Double 0.99999999999999989 1.0000000000000002 1 ) -1.1102230246251563e-16

    it "fnmaddDouble#: +0 + +0 rounds properly"
      ( fnmadd @Double 1 0 0 ) 0

    it "fnmaddDouble#: +0 + -0 rounds properly"
      ( fnmadd @Double 1 0 -0 ) -0

    it "fnmaddDouble#: -0 + +0 rounds properly"
      ( fnmadd @Double 1 -0 0 ) 0

    it "fnmaddDouble#: -0 + -0 rounds properly"
      ( fnmadd @Double 1 -0 -0 ) 0

    -- fnmsub: - x * y - z

    -- Float
    it "fnmsubFloat#: sniff test"
      ( fnmsub @Float 2 3 1 ) -7

    it "fnmsubFloat#: excess precision"
      ( fnmsub @Float 0.99999994 1.00000012 -1 ) -5.96046377e-08

    it "fnmsubFloat#: +0 + +0 rounds properly"
      ( fnmsub @Float 1 0 0 ) -0

    it "fnmsubFloat#: +0 + -0 rounds properly"
      ( fnmsub @Float 1 0 -0 ) 0

    it "fnmsubFloat#: -0 + +0 rounds properly"
      ( fnmsub @Float 1 -0 0 ) 0

    it "fnmsubFloat#: -0 + -0 rounds properly"
      ( fnmsub @Float 1 -0 -0 ) 0

    -- Double
    it "fnmsubDouble#: sniff test"
      ( fnmsub @Double 2 3 1 ) -7

    it "fnmsubDouble#: excess precision"
      ( fnmsub @Double 0.99999999999999989 1.0000000000000002 -1 ) -1.1102230246251563e-16

    it "fnmsubDouble#: +0 + +0 rounds properly"
      ( fnmsub @Double 1 0 0 ) -0

    it "fnmsubDouble#: +0 + -0 rounds properly"
      ( fnmsub @Double 1 0 -0 ) 0

    it "fnmsubDouble#: -0 + +0 rounds properly"
      ( fnmsub @Double 1 -0 0 ) 0

    it "fnmsubDouble#: -0 + -0 rounds properly"
      ( fnmsub @Double 1 -0 -0 ) 0

    failure <- readIORef exit_ref
    if failure
    then exitFailure
    else exitSuccess

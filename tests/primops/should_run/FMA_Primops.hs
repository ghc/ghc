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

f1, f2, f3 :: Float
f1  = 0.99999994     -- float before 1
f2  = 1.00000012     -- float after  1
f3  = 5.96046377e-08 -- float after  0
d1, d2, d3 :: Double
d1  = 0.99999999999999989    -- double before 1
d2  = 1.0000000000000002     -- double after  1
d3  = 1.1102230246251563e-16 -- double after  0
zero, one, two, three, five, seven :: Num a => a
zero  = 0
one   = 1
two   = 2
three = 3
five  = 5
seven = 7

-- NOINLINE to prevent constant folding
-- (The test FMA_ConstantFold tests constant folding.)
{-# NOINLINE f1 #-}
{-# NOINLINE f2 #-}
{-# NOINLINE f3 #-}
{-# NOINLINE d1 #-}
{-# NOINLINE d2 #-}
{-# NOINLINE d3 #-}
{-# NOINLINE zero  #-}
{-# NOINLINE one   #-}
{-# NOINLINE two   #-}
{-# NOINLINE three #-}
{-# NOINLINE five  #-}
{-# NOINLINE seven #-}

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
      ( fmadd @Float two three one ) seven

    it "fmaddFloat#: excess precision"
      ( fmadd @Float f1 f2 -one ) f3

    it "fmaddFloat#: +0 + +0 rounds properly"
      ( fmadd @Float one zero zero ) zero

    it "fmaddFloat#: +0 + -0 rounds properly"
      ( fmadd @Float one zero -zero ) zero

    it "fmaddFloat#: -0 + +0 rounds properly"
      ( fmadd @Float one -zero zero ) zero

    it "fmaddFloat#: -0 + -0 rounds properly"
      ( fmadd @Float one -zero -zero ) -zero

    -- Double
    it "fmaddDouble#: sniff test"
      ( fmadd @Double two three one ) seven

    it "fmaddDouble#: excess precision"
      ( fmadd @Double d1 d2 -one ) d3

    it "fmaddDouble#: +0 + +0 rounds properly"
      ( fmadd @Double one zero zero ) zero

    it "fmaddDouble#: +0 + -0 rounds properly"
      ( fmadd @Double one zero -zero ) zero

    it "fmaddDouble#: -0 + +0 rounds properly"
      ( fmadd @Double one -zero zero ) zero

    it "fmaddDouble#: -0 + -0 rounds properly"
      ( fmadd @Double one -zero -zero ) -zero

    -- fmsub: x * y - z

    -- Float
    it "fmsubFloat#: sniff test"
      ( fmsub @Float two three one ) five

    it "fmsubFloat#: excess precision"
      ( fmsub @Float f1 f2 one ) f3

    it "fmsubFloat#: +0 + +0 rounds properly"
      ( fmsub @Float one zero zero ) zero

    it "fmsubFloat#: +0 + -0 rounds properly"
      ( fmsub @Float one zero -zero ) zero

    it "fmsubFloat#: -0 + +0 rounds properly"
      ( fmsub @Float one -zero zero ) -zero

    it "fmsubFloat#: -0 + -0 rounds properly"
      ( fmsub @Float one -zero -zero ) zero

    -- Double
    it "fmsubDouble#: sniff test"
      ( fmsub @Double two three one ) five

    it "fmsubDouble#: excess precision"
      ( fmsub @Double d1 d2 one ) d3

    it "fmsubDouble#: +0 + +0 rounds properly"
      ( fmsub @Double one zero zero ) zero

    it "fmsubDouble#: +0 + -0 rounds properly"
      ( fmsub @Double one zero -zero ) zero

    it "fmsubDouble#: -0 + +0 rounds properly"
      ( fmsub @Double one -zero zero ) -zero

    it "fmsubDouble#: -0 + -0 rounds properly"
      ( fmsub @Double one -zero -zero ) zero

    -- fnmadd: - x * y + z

    -- Float
    it "fnmaddFloat#: sniff test"
      ( fnmadd @Float two three one ) -five

    it "fnmaddFloat#: excess precision"
      ( fnmadd @Float f1 f2 one ) -f3

    it "fnmaddFloat#: +0 + +0 rounds properly"
      ( fnmadd @Float one zero zero ) zero

    it "fnmaddFloat#: +0 + -0 rounds properly"
      ( fnmadd @Float one zero -zero ) -zero

    it "fnmaddFloat#: -0 + +0 rounds properly"
      ( fnmadd @Float one -zero zero ) zero

    it "fnmaddFloat#: -0 + -0 rounds properly"
      ( fnmadd @Float one -zero -zero ) zero

    -- Double
    it "fnmaddDouble#: sniff test"
      ( fnmadd @Double two three one ) -five

    it "fnmaddDouble#: excess precision"
      ( fnmadd @Double d1 d2 one ) -d3

    it "fnmaddDouble#: +0 + +0 rounds properly"
      ( fnmadd @Double one zero zero ) zero

    it "fnmaddDouble#: +0 + -0 rounds properly"
      ( fnmadd @Double one zero -zero ) -zero

    it "fnmaddDouble#: -0 + +0 rounds properly"
      ( fnmadd @Double one -zero zero ) zero

    it "fnmaddDouble#: -0 + -0 rounds properly"
      ( fnmadd @Double one -zero -zero ) zero

    -- fnmsub: - x * y - z

    -- Float
    it "fnmsubFloat#: sniff test"
      ( fnmsub @Float two three one ) -seven

    it "fnmsubFloat#: excess precision"
      ( fnmsub @Float f1 f2 -one ) -f3

    it "fnmsubFloat#: +0 + +0 rounds properly"
      ( fnmsub @Float one zero zero ) -zero

    it "fnmsubFloat#: +0 + -0 rounds properly"
      ( fnmsub @Float one zero -zero ) zero

    it "fnmsubFloat#: -0 + +0 rounds properly"
      ( fnmsub @Float one -zero zero ) zero

    it "fnmsubFloat#: -0 + -0 rounds properly"
      ( fnmsub @Float one -zero -zero ) zero

    -- Double
    it "fnmsubDouble#: sniff test"
      ( fnmsub @Double two three one ) -seven

    it "fnmsubDouble#: excess precision"
      ( fnmsub @Double d1 d2 -one ) -d3

    it "fnmsubDouble#: +0 + +0 rounds properly"
      ( fnmsub @Double one zero zero ) -zero

    it "fnmsubDouble#: +0 + -0 rounds properly"
      ( fnmsub @Double one zero -zero ) zero

    it "fnmsubDouble#: -0 + +0 rounds properly"
      ( fnmsub @Double one -zero zero ) zero

    it "fnmsubDouble#: -0 + -0 rounds properly"
      ( fnmsub @Double one -zero -zero ) zero

    failure <- readIORef exit_ref
    if failure
    then exitFailure
    else exitSuccess

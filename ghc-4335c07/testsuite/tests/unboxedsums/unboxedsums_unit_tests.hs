module Main where

import BasicTypes
import GHC
import GhcMonad
import Outputable
import RepType
import TysPrim
import TysWiredIn
import UniqSet
import Unique

import qualified Control.Exception as E
import Control.Monad
import System.Environment (getArgs)
import System.IO

assert :: Bool -> String -> SDoc -> IO ()
assert False tn msg = pprPanic tn msg
assert True  _  _   = return ()

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ liftIO $ do
      -- need to initialize the monad to initialize static flags etc.
      sequence_ [ uniq_tests, layout_tests ]

-- Make sure sum datacon/tycon uniques are really uniq
uniq_tests :: IO ()
uniq_tests = do
    let tycons   = map sumTyCon [2 .. 20]
        datacons = [ sumDataCon alt arity | arity <- [ 2 .. 20 ]
                                          , alt   <- [ 1 .. arity ] ]

        us = mkUniqSet (map getUnique tycons)
               `unionUniqSets` mkUniqSet (map getUnique datacons)

    assert (sizeUniqSet us == length tycons + length datacons)
           "uniq_tests"
           (text "Sum cons/tycons have same uniques.")

layout_tests :: IO ()
layout_tests = sequence_
    [ layout1, layout2, layout3, layout4, layout5, enum_layout ]
  where
    assert_layout tn tys layout =
      let
        layout_ret = ubxSumRepType (map typePrimRep tys)
      in
        assert (layout_ret == layout)
               tn
               (text "Unexpected sum layout." $$
                text "Alts:           " <+> ppr tys $$
                text "Expected layout:" <+> ppr layout $$
                text "Actual layout:  " <+> ppr layout_ret)

    ubxtup = mkTupleTy Unboxed

    layout1 =
      assert_layout "layout1"
        [ ubxtup [ intTy, intPrimTy ]
        , ubxtup [ intPrimTy, intTy ] ]
        [ WordSlot, PtrSlot, WordSlot ]

    layout2 =
      assert_layout "layout2"
        [ ubxtup [ intTy ]
        , intTy ]
        [ WordSlot, PtrSlot ]

    layout3 =
      assert_layout "layout3"
        [ ubxtup [ intTy, intPrimTy, intTy, intPrimTy ]
        , ubxtup [ intPrimTy, intTy, intPrimTy, intTy ] ]
        [ WordSlot, PtrSlot, PtrSlot, WordSlot, WordSlot ]

    layout4 =
      assert_layout "layout4"
        [ ubxtup [ floatPrimTy, floatPrimTy ]
        , ubxtup [ intPrimTy, intPrimTy ] ]
        [ WordSlot, WordSlot, WordSlot, FloatSlot, FloatSlot ]

    layout5 =
      assert_layout "layout5"
        [ ubxtup [ intPrimTy, intPrimTy ]
        , ubxtup [ floatPrimTy, floatPrimTy ] ]
        [ WordSlot, WordSlot, WordSlot, FloatSlot, FloatSlot ]

    enum_layout =
      assert_layout "enum"
        (replicate 10 (ubxtup []))
        [ WordSlot ]

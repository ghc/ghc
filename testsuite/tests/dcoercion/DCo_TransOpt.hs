{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

-- base
import Data.Foldable
  ( for_ )

-- ghc
import GHC.Core.Coercion
  ( DCoercion(ReflDCo, StepsDCo, TransDCo, TyConAppDCo)
  , mkTransDCo
  )

-------------------------------------------------------------------------------

main :: IO ()
main =
  for_ test_dcos \ ( nm, dco ) ->
    case unreducedTopTransitivities dco of
      Nothing -> do
        putStrLn $ "OK:   " ++ nm
        putStrLn $ "      " ++ show_dco dco
      Just ( i, l, r ) -> do
        putStrLn $ "FAIL: " ++ nm
        putStrLn $ "      " ++ show_dco dco
        putStrLn $ "       unreduced pair at index " ++ show i
        putStrLn $ "       LHS:" ++ show_dco l
        putStrLn $ "       RHS:" ++ show_dco r

unreducedTopTransitivities
  :: DCoercion -> Maybe ( Int, DCoercion, DCoercion )
unreducedTopTransitivities
  = check_reductions . top_trans

top_trans :: DCoercion -> [ DCoercion ]
top_trans = \case
  ldco `TransDCo` rdco -> top_trans ldco ++ top_trans rdco
  dco                  -> [dco]

check_reductions :: [ DCoercion ] -> Maybe ( Int, DCoercion, DCoercion )
check_reductions = go 0
  where
    go i ( dco1 : dco2 : dcos )
      | should_cancel dco1 dco2
      = Just ( i, dco1, dco2 )
      | otherwise
      = go (i+1) ( dco2 : dcos )
    go _ _ = Nothing

should_cancel :: DCoercion -> DCoercion -> Bool
should_cancel ReflDCo _ = True
should_cancel _ ReflDCo = True
should_cancel (StepsDCo {}) (StepsDCo {}) = True
should_cancel _ _ = False

--------------------------------------------------------------------------------
-- Handwritten directed coercions used for testing...

-- Assume the LHS cannot be simplified further.
test_lhs_dco_1 = ReflDCo
test_lhs_dco_2 = StepsDCo 3
test_lhs_dco_3 = TyConAppDCo [] `TransDCo` StepsDCo 3
test_lhs_dco_4 = ( TyConAppDCo [] `TransDCo` TyConAppDCo [] ) `TransDCo` StepsDCo 3
test_lhs_dco_5 = TyConAppDCo [] `TransDCo` ( TyConAppDCo [] `TransDCo` StepsDCo 3 )

-- Don't make any such assumptions about the RHS.
test_rhs_dco_1 = ReflDCo
test_rhs_dco_2 = StepsDCo 3
test_rhs_dco_3 = StepsDCo 3 `mkTransDCo` StepsDCo 10
test_rhs_dco_4 = ReflDCo `mkTransDCo` TyConAppDCo []
test_rhs_dco_5 = StepsDCo 4 `mkTransDCo` TyConAppDCo []
test_rhs_dco_6 = ( ReflDCo `mkTransDCo` TyConAppDCo [] ) `mkTransDCo` TyConAppDCo []
test_rhs_dco_7 = ( StepsDCo 4 `mkTransDCo` TyConAppDCo [] ) `mkTransDCo` TyConAppDCo []
test_rhs_dco_8 = ReflDCo
    `mkTransDCo` ( ReflDCo `mkTransDCo` StepsDCo 100 `mkTransDCo` ReflDCo )
    `mkTransDCo` ReflDCo

test_lhs_dcos :: [ ( String, DCoercion ) ]
test_lhs_dcos = [ ( "lhs 1", test_lhs_dco_1 )
                , ( "lhs 2", test_lhs_dco_2 )
                , ( "lhs 3", test_lhs_dco_3 )
                , ( "lhs 4", test_lhs_dco_4 )
                , ( "lhs 5", test_lhs_dco_5 ) ]

test_rhs_dcos :: [ ( String, DCoercion ) ]
test_rhs_dcos = [ ( "rhs 1", test_rhs_dco_1 )
                , ( "rhs 2", test_rhs_dco_2 )
                , ( "rhs 3", test_rhs_dco_3 )
                , ( "rhs 4", test_rhs_dco_4 )
                , ( "rhs 5", test_rhs_dco_5 )
                , ( "rhs 6", test_rhs_dco_6 )
                , ( "rhs 7", test_rhs_dco_7 )
                , ( "rhs 8", test_rhs_dco_8 )]

test_dcos :: [ ( String, DCoercion ) ]
test_dcos = [ ( l_nm ++ ", " ++ r_nm, lhs `mkTransDCo` rhs )
            | (l_nm, lhs) <- test_lhs_dcos
            , (r_nm, rhs) <- test_rhs_dcos ]

show_dco :: DCoercion -> String
show_dco = \case
  ReflDCo        -> "Refl"
  StepsDCo n     -> show n
  TyConAppDCo {} -> "TC"
  l `TransDCo` r -> show_dco l ++ " ; " ++ show_dco r
  _              -> "???"

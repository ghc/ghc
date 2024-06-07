
import GHC.JS.Optimizer
import GHC.JS.Syntax
import GHC.JS.Ident

import GHC.Data.FastString

double_return :: JStat
double_return = BlockStat [ ReturnStat (Int 0)
                          , ReturnStat (Int 1)
                          ]

double_return_opt :: JStat
double_return_opt = (BlockStat [ReturnStat (Int 0)])

in_func :: JStat
in_func = AssignStat (var (fsLit "foo")) AssignOp (ValExpr (JFunc [] double_return))

in_func_opt :: JStat
in_func_opt = AssignStat (var (fsLit "foo")) AssignOp (ValExpr (JFunc [] double_return_opt))

nested_blocks :: JStat
nested_blocks = BlockStat [ double_return <> double_return
                          , double_return
                          ] <> double_return

nested_blocks_opt :: JStat
nested_blocks_opt = double_return_opt

global_func :: JStat
global_func = FuncStat (name (fsLit "bar")) [] double_return

global_func_opt :: JStat
global_func_opt = FuncStat (name (fsLit "bar")) [] double_return_opt

func_with_locals :: JStat
func_with_locals = AssignStat (var (fsLit "foo"))
                   AssignOp
                   (ValExpr (JFunc []
                            (BlockStat [ AssignStat (var (fsLit "one")) AssignOp (Int 2)
                                       , AssignStat (var (fsLit "two")) AssignOp (Int 3)
                                       , ApplStat (var (fsLit "f")) [(Int 100)]
                                       , ReturnStat (Int 0)
                                       , ReturnStat (Int 1)
                                       ])))

func_with_locals_opt :: JStat
func_with_locals_opt = AssignStat (var (fsLit "foo"))
                       AssignOp
                       (ValExpr (JFunc []
                                 (BlockStat [ AssignStat (var (fsLit "one")) AssignOp (Int 2)
                                            , AssignStat (var (fsLit "two")) AssignOp (Int 3)
                                            , ApplStat (var (fsLit "f")) [(Int 100)]
                                            , ReturnStat (Int 0)
                                            ])))

-- This one comes straight from MR10260 where we noticed the optimizer was not catching the redundant return
bignum_test :: JStat
bignum_test = DeclStat (name $ fsLit "h$ghczmbignumZCGHCziNumziIntegerziintegerToInt64zh_e")
              (Just (ValExpr $ JFunc [] $ BlockStat [ DeclStat (name $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e") (Just (var $ fsLit "h$r2"))
                                                    , ApplStat (var $ fsLit "h$p1") [var $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99"]
                                                    , ReturnStat (ApplExpr (var $ fsLit "h$e") [var $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e"])
                                                    , ReturnStat (ApplExpr (var $ fsLit "h$rs") [])]))

bignum_test_opt :: JStat
bignum_test_opt =
  DeclStat (name $ fsLit "h$ghczmbignumZCGHCziNumziIntegerziintegerToInt64zh_e")
              (Just (ValExpr $ JFunc [] $ BlockStat [ DeclStat (name $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e") (Just (var $ fsLit "h$r2"))
                                                    , ApplStat (var $ fsLit "h$p1") [var $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99"]
                                                    , ReturnStat (ApplExpr (var $ fsLit "h$e") [var $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e"])
                                                    ]))

bignum_test_2 :: JStat
bignum_test_2 = BlockStat [FuncStat (name $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99") [] (BlockStat [DeclStat (name $ fsLit "h$ghczmbignumZCGHCziNumziIntegerziintegerToInt64zh_e")
              (Just (ValExpr $ JFunc [] $ BlockStat [ DeclStat (name $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e") (Just (var $ fsLit "h$r2"))
                                                    , ApplStat (var $ fsLit "h$p1") [var $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99"]
                                                    , ReturnStat (ApplExpr (var $ fsLit "h$e") [var $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e"])
                                                    , ReturnStat (ApplExpr (var $ fsLit "h$rs") [])]))])]

bignum_test_opt_2 :: JStat
bignum_test_opt_2 = BlockStat [
  FuncStat (name $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99") [] (DeclStat (name $ fsLit "a")
              (Just (ValExpr $ JFunc [] $ BlockStat [ DeclStat (name $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e") (Just (var $ fsLit "h$r2"))
                                                    , ApplStat (var $ fsLit "h$p1") [var $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99"]
                                                    , ReturnStat (ApplExpr (var $ fsLit "h$e") [var $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e"])
                                                    ]))) ]


main :: IO ()
main = mapM_ print
       [ jsOptimize double_return == double_return_opt
       , jsOptimize in_func       == in_func_opt
       , jsOptimize nested_blocks == nested_blocks_opt
       , jsOptimize global_func   == global_func_opt
       , jsOptimize func_with_locals == func_with_locals_opt
       , jsOptimize bignum_test == bignum_test_opt
       , jsOptimize bignum_test_2 == bignum_test_opt_2
       ]

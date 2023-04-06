
import GHC.JS.Optimizer
import GHC.JS.Syntax
import GHC.JS.Unsat.Syntax (Ident (..))

import GHC.Data.FastString

double_return :: JStat
double_return = BlockStat [ ReturnStat (SatInt 0)
                          , ReturnStat (SatInt 1)
                          ]

double_return_opt :: JStat
double_return_opt = (BlockStat [ReturnStat (SatInt 0)])

in_func :: JStat
in_func = AssignStat (jvar (fsLit "foo")) AssignOp (ValExpr (JFunc [] double_return))

in_func_opt :: JStat
in_func_opt = AssignStat (jvar (fsLit "foo")) AssignOp (ValExpr (JFunc [] double_return_opt))

nested_blocks :: JStat
nested_blocks = BlockStat [ double_return <> double_return
                          , double_return
                          ] <> double_return

nested_blocks_opt :: JStat
nested_blocks_opt = double_return_opt

global_func :: JStat
global_func = FuncStat (TxtI (fsLit "bar")) [] double_return

global_func_opt :: JStat
global_func_opt = FuncStat (TxtI (fsLit "bar")) [] double_return_opt

func_with_locals :: JStat
func_with_locals = AssignStat (jvar (fsLit "foo"))
                   AssignOp
                   (ValExpr (JFunc []
                            (BlockStat [ AssignStat (jvar (fsLit "one")) AssignOp (SatInt 2)
                                       , AssignStat (jvar (fsLit "two")) AssignOp (SatInt 3)
                                       , ApplStat (jvar (fsLit "f")) [(SatInt 100)]
                                       , ReturnStat (SatInt 0)
                                       , ReturnStat (SatInt 1)
                                       ])))

func_with_locals_opt :: JStat
func_with_locals_opt = AssignStat (jvar (fsLit "foo"))
                       AssignOp
                       (ValExpr (JFunc []
                                 (BlockStat [ AssignStat (jvar (fsLit "one")) AssignOp (SatInt 2)
                                            , AssignStat (jvar (fsLit "two")) AssignOp (SatInt 3)
                                            , ApplStat (jvar (fsLit "f")) [(SatInt 100)]
                                            , ReturnStat (SatInt 0)
                                            ])))

-- This one comes straight from MR10260 where we noticed the optimizer was not catching the redundant return
bignum_test :: JStat
bignum_test = DeclStat (TxtI $ fsLit "h$ghczmbignumZCGHCziNumziIntegerziintegerToInt64zh_e")
              (Just (ValExpr $ JFunc [] $ BlockStat [ DeclStat (TxtI $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e") (Just (jvar $ fsLit "h$r2"))
                                                    , ApplStat (jvar $ fsLit "h$p1") [jvar $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99"]
                                                    , ReturnStat (ApplExpr (jvar $ fsLit "h$e") [jvar $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e"])
                                                    , ReturnStat (ApplExpr (jvar $ fsLit "h$rs") [])]))

bignum_test_opt :: JStat
bignum_test_opt =
  DeclStat (TxtI $ fsLit "h$ghczmbignumZCGHCziNumziIntegerziintegerToInt64zh_e")
              (Just (ValExpr $ JFunc [] $ BlockStat [ DeclStat (TxtI $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e") (Just (jvar $ fsLit "h$r2"))
                                                    , ApplStat (jvar $ fsLit "h$p1") [jvar $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99"]
                                                    , ReturnStat (ApplExpr (jvar $ fsLit "h$e") [jvar $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e"])
                                                    ]))

bignum_test_2 :: JStat
bignum_test_2 = BlockStat [FuncStat (TxtI $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99") [] (BlockStat [DeclStat (TxtI $ fsLit "h$ghczmbignumZCGHCziNumziIntegerziintegerToInt64zh_e")
              (Just (ValExpr $ JFunc [] $ BlockStat [ DeclStat (TxtI $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e") (Just (jvar $ fsLit "h$r2"))
                                                    , ApplStat (jvar $ fsLit "h$p1") [jvar $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99"]
                                                    , ReturnStat (ApplExpr (jvar $ fsLit "h$e") [jvar $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e"])
                                                    , ReturnStat (ApplExpr (jvar $ fsLit "h$rs") [])]))])]

bignum_test_opt_2 :: JStat
bignum_test_opt_2 = BlockStat [FuncStat (TxtI $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99") [] (BlockStat [DeclStat (TxtI $ fsLit "h$ghczmbignumZCGHCziNumziIntegerziintegerToInt64zh_e")
              (Just (ValExpr $ JFunc [] $ BlockStat [ DeclStat (TxtI $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e") (Just (jvar $ fsLit "h$r2"))
                                                    , ApplStat (jvar $ fsLit "h$p1") [jvar $ fsLit "h$$ghczmbignumZCGHCziNumziInteger_99"]
                                                    , ReturnStat (ApplExpr (jvar $ fsLit "h$e") [jvar $ fsLit "h$$ghczmbignumZCGHCziNumziIntegerzids_s_2f9e"])
                                                    ]))])]

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

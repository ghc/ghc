{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

{-
   Test for the JavaScript optimizer.

   This tests contains some hand-written JavaScript code to test the
   optimizer.

   The tests are run by executing the JavaScript code with node.js

   Run with -v to see the original and optimized JavaScript code.
 -}

import GHC.Utils.TmpFs
import System.Process
import System.Exit
import System.FilePath
import Control.Monad
import System.IO
import qualified GHC.Utils.Ppr as Ppr
import qualified GHC.JS.Syntax as JS
import qualified GHC.Types.Unique.Map as UM

import GHC.JS.Syntax

import GHC.Utils.BufHandle
import GHC.Utils.Outputable
import GHC.StgToJS.Linker.Opt
import GHC.Data.FastString
import System.Environment

import qualified GHC.JS.Optimizer as Opt

{-
  The location of the node.js program.

  This is used to run the tests and compare the output of the optimized
  and unoptimized code.
 -}
nodePgm :: IO FilePath
nodePgm = pure "node"

main :: IO ()
main = do
  args <- getArgs
  let trace = args == ["-v"]
  mapM_ (runTest trace) tests

data Test = Test
  { testName   :: String
  , optSize    :: Maybe Double  {- ^ expected ratio of optimized size to
                                     unoptimized size -}
  , optPred    :: Maybe (JStat -> Bool) {- ^ predicate expected to be False
                                             for unoptimized code and True
                                             for optimized code -}
  , testScript :: JStat
  }

printJS :: JStat -> IO ()
printJS x = do
  bh <- newBufHandle stdout
  bPutHDoc bh defaultJsContext (line $ pretty False x)
  bPutChar bh '\n'
  bFlush bh

runTest :: Bool -> Test -> IO ()
runTest trace test = do
  let script_unopt = testScript test
  let script_opt = Opt.jsOptimize script_unopt
  let out = if trace then putStrLn else const (pure ())
      outJS = if trace then printJS else const (pure ())
  out $ "###### " ++ testName test ++ " ######"
  out "### Unoptimized ###" >> outJS script_unopt
  out "### Optimized ###" >> outJS script_opt
  out "###"
  (size_u, code_u, out_u, err_u) <- runTestScript script_unopt
  (size_o, code_o, out_o, err_o) <- runTestScript script_opt
  out ("### result " ++ show code_u ++ " ###")
  out ("### stdout ###") >> out out_u
  out ("### stderr ###") >> out err_u
  let smallEnough = maybe True
                          (\r -> fromIntegral size_o < r * fromIntegral size_u)
                          (optSize test)
      (predUnopt, predOpt) = maybe (False, True)
                                   (\p -> (p script_unopt, p script_opt))
                                   (optPred test)
  when (predUnopt || not predOpt) $ failTest ("predicate failed: " ++ show (predUnopt, predOpt))
  when (code_u /= code_o) $ failTest ("different exit codes\n" ++ show (code_u, code_o))
  when (out_u /= out_o)   $ failTest ("different stdout\n" ++ show (out_u, out_o))
  when (err_u /= err_o)   $ failTest ("different stderr\n" ++ show (err_u, err_o))
  when (not smallEnough)  $ failTest ("not small enough: " ++ show (size_o, size_u, optSize test))
    where
      failTest msg = do
        error $ "Test " ++ testName test ++ " failed: " ++ msg

runTestScript :: JStat -> IO (Integer, ExitCode, String, String)
runTestScript script = withTempDirectory "." "test" $ \dir -> do
  let scriptFile = dir </> "test.js"
  scriptSize <- withFile scriptFile WriteMode (\h -> hPutJS True h script >> hFileSize h)
  (run_exit, run_out, run_err) <- readProcessWithExitCode "node" [scriptFile] ""
  pure (scriptSize, run_exit, run_out, run_err)

hPutJS :: Bool -> Handle -> JS.JStat -> IO Integer
hPutJS render_pretty h = \case
  JS.BlockStat [] -> pure 0
  x                -> do
    before <- hTell h
    if render_pretty
      then do
        printSDoc defaultJsContext (Ppr.PageMode True) h (pretty render_pretty x)
      else do
        bh <- newBufHandle h
        bPutHDoc bh defaultJsContext (line $ pretty render_pretty x)
        bFlush bh
    hPutChar h '\n'
    after <- hTell h
    pure $! (after - before)

defaultJsContext :: SDocContext
defaultJsContext = defaultSDocContext{sdocStyle = PprCode}


-- tests here
tests :: [Test]
tests =
  [ {-
      Test that all local variables are renamed to short names
     -}
    Test "rename_args" Nothing (Just (maxLongDecls 0)) $ BlockStat
      [ FuncStat (ii "testFunc") [ii "foo", ii "bar"] $
          BlockStat [ decl' "baz" (Int 1)
                    , clog (var (fsLit "foo"))
                    , clog (var (fsLit "bar"))
                    , clog (var (fsLit "baz"))
                    , clog (var (fsLit "baz"))
                    , ReturnStat (Int 0)
                    ]
      , ApplStat (var (fsLit "testFunc")) [Int 1, Int 2]
      ]
  {-
    Test that local variables are removed:
      foo: unused
      bar: constant propagation
   -}
  , Test "remove_unused"  Nothing    (Just (maxDecls 0)) $ BlockStat
      [ FuncStat (ii "testFunc") [] $
          BlockStat [ decl' "foo" (Int 1)
                    , decl' "bar" (Int 2)
                    , clog (iv "bar")
                    , ReturnStat (Int 0)
                    ]
      , ApplStat (var (fsLit "testFunc")) []
      ]
  {- test that second return is removed in:
       return 0;
       return 1;
    -}
  , Test "unreachable_return" Nothing (Just (maxReturns 1)) $
        testFuncPrint $
        [ DeclStat (ii "foo") (Just (Int 0))
        , ReturnStat (Int 0)
        , ReturnStat (Int 1)
        ]
  {- make sure we don't rename things around an eval -}
  , Test "eval_bailout"   Nothing    Nothing (
     testFuncRun $
  [ DeclStat (ii "foo") (Just (Int 0))
  , AssignStat (var (fsLit "foo")) AssignOp (ValExpr (JFunc [] (BlockStat [ ApplStat (var (fsLit "eval")) [(str "foo++;")]
                                                                            , ReturnStat (Int 0)
                                                                            ])))
  , ReturnStat (ApplExpr (var (fsLit "foo")) [])
             ]
  )
  {- make sure we remove operations for known constants -}
  , Test "constant_fold" Nothing  (Just (maxOp AddOp 0)) (testFuncRun $
  [ decl' "foo" (Int 1)
  , decl' "bar" (Int 2)
  , decl' "baz" (Int 3)
  , clog (InfixExpr AddOp (var "foo") (InfixExpr AddOp (var "bar") (var "baz")))
  ])
  {- nested function that closes over a local variable in outer function -}
  , Test "nested_function1" Nothing Nothing (testFuncRun $
  [ decl' "xyz" (Int 1)
  , clog (var "xyz")
  , FuncStat (ii "f") [] $
      BlockStat [ UOpStat PreIncOp (var "xyz")
                , clog (var "xyz")
                , ReturnStat (Int 0)
                ]
  , app "f" []
  ])
  {- nested function arguments -}
  , Test "nested_function2" Nothing Nothing (testFuncRun $
  [ decl' "xyz" (Int 1)
  , clog (var (fsLit "xyz"))
  , FuncStat (ii "f") [ii "xyz"] $
      BlockStat [ UOpStat PreIncOp (var "xyz")
                , clog (var "xyz")
                , ReturnStat (Int 0)
                ]
  , clog (var "xyz")
  , app "f" [Int 2]
  , clog (var "xyz")
  , app "f" [var "xyz"]
  , clog (var "xyz")
  ])
  ]

ii :: String -> Ident
ii = TxtI . fsLit

iv :: String -> JExpr
iv = ValExpr . JVar . TxtI . fsLit

str :: String -> JExpr
str = ValExpr . JStr . fsLit

decl :: String -> JStat
decl i = DeclStat (TxtI (fsLit i)) Nothing

decl' :: String -> JExpr -> JStat
decl' i e = DeclStat (TxtI (fsLit i)) (Just e)

clog :: JExpr -> JStat
clog e = ApplStat (SelExpr (var "console") (ii "log")) [e]

app :: String -> [JExpr] -> JStat
app f es = ApplStat (iv f) es

testFunc :: [JStat] -> JStat
testFunc body = FuncStat (ii "test") [] (BlockStat body)

testFuncRun :: [JStat] -> JStat
testFuncRun body = BlockStat [ testFunc body
                             , ApplStat (var (fsLit "test")) []
                             ]

testFuncPrint :: [JStat] -> JStat
testFuncPrint body = BlockStat [ testFunc body
                               , clog (ApplExpr (var (fsLit "test")) [])
                               ]



-- predicates
maxDecls, maxLongDecls, maxReturns :: Int -> JStat -> Bool

{-
  Test that the number of local variable declarations is at most n
 -}
maxDecls n s = countStats isDecl s <= n
  where
    isDecl (DeclStat _ _) = True
    isDecl _              = False

{-
  Test that the number of return statements is at most n
 -}
maxReturns n s = countStats isReturn s <= n
  where
    isReturn (ReturnStat _) = True
    isReturn _              = False

{-
   Test that the number of long (more than one character) declarations 
   is at most n

   Declarations are both explicit local variables ('var x') and
   function arguments ('function f(x) { ... }') or other implicitly
   declared variables ('catch(x) { }', 'for(var x in y) { }').
-}
maxLongDecls n s = countCode (CodeCount countLongStat countLongExpr) s <= n
  where
    isLong :: Ident -> Bool
    isLong (TxtI i) = length (unpackFS i) > 1
    countLongStat :: JStat -> Int
    countLongStat (DeclStat i _) = b2i isLong i
    countLongStat (FuncStat _ args _) = length (filter isLong args)
    countLongStat (ForInStat True i _ _) = b2i isLong i
    countLongStat _                   = 0
    countLongExpr :: JExpr -> Int
    countLongExpr (ValExpr (JFunc args _)) = length (filter isLong args)
    countLongExpr _                      = 0

{-
  Test that the number of operations of type op is at most n
 -}
maxOp :: Op -> Int -> JStat -> Bool
maxOp op n s = countExprs isOp s <= n
  where
    isOp (InfixExpr op' _ _) = op == op'
    isOp _                   = False

{-
  Test that the number of long (more than one character) variable references
  is at most n
 -}
maxLongVars :: Int -> JStat -> Bool
maxLongVars n s = countExprs isLongVar s <= n
  where
    isLongVar (ValExpr (JVar (TxtI i))) = length (unpackFS i) > 1
    isLongVar _                         = False

countStats :: (JStat -> Bool) -> JStat -> Int
countStats p stat = countCode (CodeCount (b2i p) (const 0)) stat

countExprs :: (JExpr -> Bool) -> JStat -> Int
countExprs p expr = countCode (CodeCount (const 0) (b2i p)) expr

b2i :: (a -> Bool) -> a -> Int
b2i f x = if f x then 1 else 0

data CodeCount = CodeCount
  { statCount :: JStat -> Int
  , exprCount :: JExpr -> Int
  }

countCode :: CodeCount -> JStat -> Int
countCode c s0 = goST s0
  where    
    goST :: JStat -> Int
    goST s = statCount c s + goS s

    goET :: JExpr -> Int
    goET e = exprCount c e + goE e
    
    goS (DeclStat _i mb_e) = maybe 0 goET mb_e
    goS (ReturnStat e) = goET e
    goS (IfStat e s1 s2) = goET e + goST s1 + goST s2
    goS (WhileStat _b e s) = goET e + goST s
    goS (ForStat s1 e s2 s3) = goST s1 + goET e + goST s2 + goST s3
    goS (ForInStat _b _i e s) = goET e + goST s
    goS (SwitchStat e xs s) = goET e +
                              sum (map (goET . fst) xs) +
                              sum (map (goST . snd) xs) +
                              goST s
    goS (TryStat s1 _i s2 s3) = goST s1 + goST s2 + goST s3
    goS (BlockStat xs) = sum (map goST xs)
    goS (ApplStat e es) = goET e + sum (map goET es)
    goS (UOpStat _ e) = goET e
    goS (AssignStat e1 _ e2) = goET e1 + goET e2
    goS (LabelStat _l s) = goST s
    goS (BreakStat _l) = 0
    goS (ContinueStat _l) = 0
    goS (FuncStat _i _is s) = goST s

    goE (ValExpr v) = goV v
    goE (SelExpr e _i) = goET e
    goE (IdxExpr e1 e2) = goET e1 + goET e2
    goE (InfixExpr _ e1 e2) = goET e1 + goET e2
    goE (UOpExpr _ e) = goET e
    goE (IfExpr e1 e2 e3) = goET e1 + goET e2 + goET e3
    goE (ApplExpr e es) = goET e + sum (map goET es)

    -- traverse JVal for JExpr or JStat inside
    goV (JList es) = sum (map goET es)
    goV (JFunc _is s) = goST s
    goV (JHash xs) = sum (map (goET . snd) (UM.nonDetUniqMapToList xs))
    goV _ = 0

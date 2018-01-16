{-# LANGUAGE NoMonomorphismRestriction #-}

module CrossCodegen where

{-
A special cross-compilation mode for hsc2hs, which generates a .hs
file without needing to run the executables that the C compiler
outputs.

Instead, it uses the output of compilations only -- specifically,
whether compilation fails.  This is the same trick that autoconf uses
when cross compiling; if you want to know if sizeof(int) <= 4, then try
compiling:

> int x() {
>   static int ary[1 - 2*(sizeof(int) <= 4)];
> }

and see if it fails. If you want to know sizeof(int), then
repeatedly apply this kind of test with differing values, using
binary search.
-}

import Prelude hiding (concatMap)
import System.IO (hPutStr, openFile, IOMode(..), hClose)
import System.Directory (removeFile)
import Data.Char (toLower,toUpper,isSpace)
import Control.Exception (assert, onException)
import Control.Monad (when, liftM, forM, ap)
import Control.Applicative as AP (Applicative(..))
import Data.Foldable (concatMap)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
import Data.Sequence ((|>),ViewL(..))
import System.Exit ( ExitCode(..) )
import System.Process

import C
import Common
import Flags
import HSCParser

-- A monad over IO for performing tests; keeps the commandline flags
-- and a state counter for unique filename generation.
-- equivalent to ErrorT String (StateT Int (ReaderT TestMonadEnv IO))
newtype TestMonad a = TestMonad { runTest :: TestMonadEnv -> Int -> IO (Either String a, Int) }

instance Functor TestMonad where
    fmap = liftM

instance Applicative TestMonad where
    pure a = TestMonad (\_ c -> pure (Right a, c))
    (<*>) = ap

instance Monad TestMonad where
    return = AP.pure
    x >>= fn = TestMonad (\e c -> (runTest x e c) >>=
                                      (\(a,c') -> either (\err -> return (Left err, c'))
                                                         (\result -> runTest (fn result) e c')
                                                         a))

data TestMonadEnv = TestMonadEnv {
    testIsVerbose_ :: Bool,
    testLogNestCount_ :: Int,
    testKeepFiles_ :: Bool,
    testGetBaseName_ :: FilePath,
    testGetFlags_ :: [Flag],
    testGetConfig_ :: Config,
    testGetCompiler_ :: FilePath
}

testAsk :: TestMonad TestMonadEnv
testAsk = TestMonad (\e c -> return (Right e, c))

testIsVerbose :: TestMonad Bool
testIsVerbose = testIsVerbose_ `fmap` testAsk

testGetCompiler :: TestMonad FilePath
testGetCompiler = testGetCompiler_ `fmap` testAsk

testKeepFiles :: TestMonad Bool
testKeepFiles = testKeepFiles_ `fmap` testAsk

testGetFlags :: TestMonad [Flag]
testGetFlags = testGetFlags_ `fmap` testAsk

testGetConfig :: TestMonad Config
testGetConfig = testGetConfig_ `fmap` testAsk

testGetBaseName :: TestMonad FilePath
testGetBaseName = testGetBaseName_ `fmap` testAsk

testIncCount :: TestMonad Int
testIncCount = TestMonad (\_ c -> let next=succ c
                                  in next `seq` return (Right c, next))
testFail' :: String -> TestMonad a
testFail' s = TestMonad (\_ c -> return (Left s, c))

testFail :: SourcePos -> String -> TestMonad a
testFail (SourcePos file line _) s = testFail' (file ++ ":" ++ show line ++ " " ++ s)

-- liftIO for TestMonad
liftTestIO :: IO a -> TestMonad a
liftTestIO x = TestMonad (\_ c -> x >>= \r -> return (Right r, c))

-- finally for TestMonad
testFinally :: TestMonad a -> TestMonad b -> TestMonad a
testFinally action cleanup = do r <- action `testOnException` cleanup
                                _ <- cleanup
                                return r

-- onException for TestMonad. This rolls back the state on an
-- IO exception, which isn't great but shouldn't matter for now
-- since only the test count is stored there.
testOnException :: TestMonad a -> TestMonad b -> TestMonad a
testOnException action cleanup = TestMonad (\e c -> runTest action e c
                                                        `onException` runTest cleanup e c >>= \(actionResult,c') ->
                                                        case actionResult of
                                                           Left _ -> do (_,c'') <- runTest cleanup e c'
                                                                        return (actionResult,c'')
                                                           Right _ -> return (actionResult,c'))

-- prints the string to stdout if verbose mode is enabled.
-- Maintains a nesting count and pads with spaces so that:
-- testLog "a" $
--    testLog "b" $ return ()
-- will print
-- a
--     b
testLog :: String -> TestMonad a -> TestMonad a
testLog s a = TestMonad (\e c -> do let verbose = testIsVerbose_ e
                                        nestCount = testLogNestCount_ e
                                    when verbose $ putStrLn $ (concat $ replicate nestCount "    ") ++ s
                                    runTest a (e { testLogNestCount_ = nestCount+1 }) c)

testLog' :: String -> TestMonad ()
testLog' s = testLog s (return ())

testLogAtPos :: SourcePos -> String -> TestMonad a -> TestMonad a
testLogAtPos (SourcePos file line _) s a = testLog (file ++ ":" ++ show line ++ " " ++ s) a

-- Given a list of file suffixes, will generate a list of filenames
-- which are all unique and have the given suffixes. On exit from this
-- action, all those files will be removed (unless keepFiles is active)
makeTest :: [String] -> ([String] -> TestMonad a) -> TestMonad a
makeTest fileSuffixes fn = do
    c <- testIncCount
    fileBase <- testGetBaseName
    keepFiles <- testKeepFiles
    let files = zipWith (++) (repeat (fileBase ++ show c)) fileSuffixes
    testFinally (fn files)
                (when (not keepFiles)
                      (mapM_ removeOrIgnore files))
    where
     removeOrIgnore f = liftTestIO (catchIO (removeFile f) (const $ return ()))
-- Convert from lists to tuples (to avoid "incomplete pattern" warnings in the callers)
makeTest2 :: (String,String) -> ((String,String) -> TestMonad a) -> TestMonad a
makeTest2 (a,b) fn = makeTest [a,b] helper
    where helper [a',b'] = fn (a',b')
          helper _ = error "makeTest: internal error"
makeTest3 :: (String,String,String) -> ((String,String,String) -> TestMonad a) -> TestMonad a
makeTest3 (a,b,c) fn = makeTest [a,b,c] helper
    where helper [a',b',c'] = fn (a',b',c')
          helper _ = error "makeTest: internal error"

-- A Zipper over lists. Unlike ListZipper, this separates at the type level
-- a list which may have a currently focused item (Zipper a) from
-- a list which _definitely_ has a focused item (ZCursor a), so
-- that zNext can be total.
data Zipper a = End { zEnd :: S.Seq a }
              | Zipper (ZCursor a)

data ZCursor a = ZCursor { zCursor :: a,
                           zAbove :: S.Seq a, -- elements prior to the cursor
                                              -- in regular order (not reversed!)
                           zBelow :: S.Seq a -- elements after the cursor
                         }

zipFromList :: [a] -> Zipper a
zipFromList [] = End S.empty
zipFromList (l:ls) = Zipper (ZCursor l S.empty (S.fromList ls))

zNext :: ZCursor a -> Zipper a
zNext (ZCursor c above below) =
    case S.viewl below of
      S.EmptyL -> End (above |> c)
      c' :< below' -> Zipper (ZCursor c' (above |> c) below')

-- Generates the .hs file from the .hsc file, by looping over each
-- Special element and calling outputSpecial to find out what it needs.
diagnose :: String -> (String -> TestMonad ()) -> [Token] -> TestMonad ()
diagnose inputFilename output input = do
    checkValidity input
    output ("{-# LINE 1 \"" ++ inputFilename ++ "\" #-}\n")
    loop (True, True) (zipFromList input)

    where
    loop _ (End _) = return ()
    loop state@(lineSync, colSync)
         (Zipper z@ZCursor {zCursor=Special _ key _}) =
        case key of
            _ | key `elem` ["if","ifdef","ifndef","elif","else"] -> do
                condHolds <- checkConditional z
                if condHolds
                    then loop state (zNext z)
                    else loop state =<< either testFail' return
                                               (skipFalseConditional (zNext z))
            "endif" -> loop state (zNext z)
            _ -> do
                sync <- outputSpecial output z
                loop (lineSync && sync, colSync && sync) (zNext z)
    loop state (Zipper z@ZCursor {zCursor=Text pos txt}) = do
        state' <- outputText state output pos txt
        loop state' (zNext z)

outputSpecial :: (String -> TestMonad ()) -> ZCursor Token -> TestMonad Bool
outputSpecial output (z@ZCursor {zCursor=Special pos@(SourcePos file line _)  key value}) =
    case key of
       "const" -> outputConst value show >> return False
       "offset" -> outputConst ("offsetof(" ++ value ++ ")") (\i -> "(" ++ show i ++ ")") >> return False
       "size" -> outputConst ("sizeof(" ++ value ++ ")") (\i -> "(" ++ show i ++ ")") >> return False
       "alignment" -> outputConst (alignment value)
                                  (\i -> "(" ++ show i ++ ")") >> return False
       "peek" -> outputConst ("offsetof(" ++ value ++ ")")
                             (\i -> "(\\hsc_ptr -> peekByteOff hsc_ptr " ++ show i ++ ")") >> return False
       "poke" -> outputConst ("offsetof(" ++ value ++ ")")
                             (\i -> "(\\hsc_ptr -> pokeByteOff hsc_ptr " ++ show i ++ ")") >> return False
       "ptr" -> outputConst ("offsetof(" ++ value ++ ")")
                            (\i -> "(\\hsc_ptr -> hsc_ptr `plusPtr` " ++ show i ++ ")") >> return False
       "type" -> computeType z >>= output >> return False
       "enum" -> computeEnum z >>= output >> return False
       "error" -> testFail pos ("#error " ++ value)
       "warning" -> liftTestIO $ putStrLn (file ++ ":" ++ show line ++ " warning: " ++ value) >> return True
       "include" -> return True
       "define" -> return True
       "undef" -> return True
       _ -> testFail pos ("directive " ++ key ++ " cannot be handled in cross-compilation mode")
    where outputConst value' formatter = computeConst z value' >>= (output . formatter)
outputSpecial _ _ = error "outputSpecial's argument isn't a Special"

outputText :: (Bool, Bool) -> (String -> TestMonad ()) -> SourcePos -> String
           -> TestMonad (Bool, Bool)
outputText state output pos txt = do
    enableCol <- fmap cColumn testGetConfig
    let outCol col | enableCol = "{-# COLUMN " ++ show col ++ " #-}"
                   | otherwise = ""
    let outLine (SourcePos file line _) = "{-# LINE " ++ show (line + 1) ++
                                          " \"" ++ file ++ "\" #-}\n"
    let (s, state') = outTextHs state pos txt id outLine outCol
    output s
    return state'

-- Bleh, messy. For each test we're compiling, we have a specific line of
-- code that may cause compiler errors -- that's the test we want to perform.
-- However, we *really* don't want any other kinds of compiler errors sneaking
-- in (which might be e.g. due to the user's syntax errors) or we'll make the
-- wrong conclusions on our tests.
--
-- So before we compile any of the tests, take a pass over the whole file and
-- generate a .c file which should fail if there are any syntax errors in what
-- the user gaves us. Hopefully, then the only reason our later compilations
-- might fail is the particular reason we want.
--
-- Another approach would be to try to parse the stdout of GCC and diagnose
-- whether the error is the one we want. That's tricky because of localization
-- etc. etc., though it would be less nerve-wracking. FYI it's not the approach
-- that autoconf went with.
checkValidity :: [Token] -> TestMonad ()
checkValidity input = do
    config <- testGetConfig
    flags <- testGetFlags
    let test = outTemplateHeaderCProg (cTemplate config) ++
               concatMap outFlagHeaderCProg flags ++
               concatMap (uncurry outValidityCheck) (zip input [0..])
    testLog ("checking for compilation errors") $ do
        success <- makeTest2 (".c",".o") $ \(cFile,oFile) -> do
            liftTestIO $ writeBinaryFile cFile test
            compiler <- testGetCompiler
            runCompiler compiler
                        (["-c",cFile,"-o",oFile]++[f | CompFlag f <- flags])
                        Nothing
        when (not success) $ testFail' "compilation failed"
    testLog' "compilation is error-free"

outValidityCheck :: Token -> Int -> String
outValidityCheck s@(Special pos key value) uniq =
    case key of
       "const" -> checkValidConst value
       "offset" -> checkValidConst ("offsetof(" ++ value ++ ")")
       "size" -> checkValidConst ("sizeof(" ++ value ++ ")")
       "alignment" -> checkValidConst (alignment value)
       "peek" -> checkValidConst ("offsetof(" ++ value ++ ")")
       "poke" -> checkValidConst ("offsetof(" ++ value ++ ")")
       "ptr" -> checkValidConst ("offsetof(" ++ value ++ ")")
       "type" -> checkValidType
       "enum" -> checkValidEnum
       _ -> outHeaderCProg' s
    where
    checkValidConst value' = "void _hsc2hs_test" ++ show uniq ++ "()\n{\n" ++ validConstTest value' ++ "}\n";
    checkValidType = "void _hsc2hs_test" ++ show uniq ++ "()\n{\n" ++ outCLine pos ++ "    (void)(" ++ value ++ ")1;\n}\n";
    checkValidEnum =
        case parseEnum value of
            Nothing -> ""
            Just (_,_,enums) ->
                "void _hsc2hs_test" ++ show uniq ++ "()\n{\n" ++
                concatMap (\(_,cName) -> validConstTest cName) enums ++
                "}\n"

    -- we want this to fail if the value is syntactically invalid or isn't a constant
    validConstTest value' = outCLine pos ++ "    {\n        static int test_array[(" ++ value' ++ ") > 0 ? 2 : 1];\n        (void)test_array;\n    }\n";

outValidityCheck (Text _ _) _ = ""

-- Skips over some #if or other conditional that we found to be false.
-- I.e. the argument should be a zipper whose cursor is one past the #if,
-- and returns a zipper whose cursor points at the next item which
-- could possibly be compiled.
skipFalseConditional :: Zipper Token -> Either String (Zipper Token)
skipFalseConditional (End _) = Left "unterminated endif"
skipFalseConditional (Zipper z@(ZCursor {zCursor=Special _ key _})) =
    case key of
      "if" -> either Left skipFalseConditional $ skipFullConditional 0 (zNext z)
      "ifdef" -> either Left skipFalseConditional $ skipFullConditional 0 (zNext z)
      "ifndef" -> either Left skipFalseConditional $ skipFullConditional 0 (zNext z)
      "elif" -> Right $ Zipper z
      "else" -> Right $ Zipper z
      "endif" -> Right $ zNext z
      _ -> skipFalseConditional (zNext z)
skipFalseConditional (Zipper z) = skipFalseConditional (zNext z)

-- Skips over an #if all the way to the #endif
skipFullConditional :: Int -> Zipper Token -> Either String (Zipper Token)
skipFullConditional _ (End _) = Left "unterminated endif"
skipFullConditional nest (Zipper z@(ZCursor {zCursor=Special _ key _})) =
    case key of
      "if" -> skipFullConditional (nest+1) (zNext z)
      "ifdef" -> skipFullConditional (nest+1) (zNext z)
      "ifndef" -> skipFullConditional (nest+1) (zNext z)
      "endif" | nest > 0 -> skipFullConditional (nest-1) (zNext z)
      "endif" | otherwise -> Right $ zNext z
      _ -> skipFullConditional nest (zNext z)
skipFullConditional nest (Zipper z) = skipFullConditional nest (zNext z)

data IntegerConstant = Signed Integer |
                       Unsigned Integer deriving (Show)
-- Prints an syntatically valid integer in C
cShowInteger :: IntegerConstant -> String
cShowInteger (Signed x) | x < 0 = "(" ++ show (x+1) ++ "-1)"
                                  -- Trick to avoid overflowing large integer constants
                                  -- http://www.hardtoc.com/archives/119
cShowInteger (Signed x) = show x
cShowInteger (Unsigned x) = show x ++ "u"

data IntegerComparison = GreaterOrEqual IntegerConstant |
                         LessOrEqual IntegerConstant
instance Show IntegerComparison where
    showsPrec _ (GreaterOrEqual c) = showString "`GreaterOrEqual` " . shows c
    showsPrec _ (LessOrEqual c) = showString "`LessOrEqual` " . shows c

cShowCmpTest :: IntegerComparison -> String
cShowCmpTest (GreaterOrEqual x) = ">=" ++ cShowInteger x
cShowCmpTest (LessOrEqual x) = "<=" ++ cShowInteger x

-- The cursor should point at #{const SOME_VALUE} or something like that.
-- Determines the value of SOME_VALUE using binary search; this
-- is a trick which is cribbed from autoconf's AC_COMPUTE_INT.
computeConst :: ZCursor Token -> String -> TestMonad Integer
computeConst zOrig@(ZCursor (Special pos _ _) _ _) value = do
    testLogAtPos pos ("computing " ++ value) $ do
        nonNegative <- compareConst z (GreaterOrEqual (Signed 0))
        integral <- checkValueIsIntegral z nonNegative
        when (not integral) $ testFail pos $ value ++ " is not an integer"
        (lower,upper) <- bracketBounds z nonNegative
        int <- binarySearch z nonNegative lower upper
        testLog' $ "result: " ++ show int
        return int
    where -- replace the Special's value with the provided value; e.g. the special
          -- is #{size SOMETHING} and we might replace value with "sizeof(SOMETHING)".
          z = zOrig {zCursor=specialSetValue value (zCursor zOrig)}
          specialSetValue v (Special p k _) = Special p k v
          specialSetValue _ _ = error "computeConst argument isn't a Special"
computeConst _ _ = error "computeConst argument isn't a Special"

-- Binary search, once we've bracketed the integer.
binarySearch :: ZCursor Token -> Bool -> Integer -> Integer -> TestMonad Integer
binarySearch _ _ l u | l == u = return l
binarySearch z nonNegative l u = do
    let mid :: Integer
        mid = (l+u+1) `div` 2
    inTopHalf <- compareConst z (GreaterOrEqual $ (if nonNegative then Unsigned else Signed) mid)
    let (l',u') = if inTopHalf then (mid,u) else (l,(mid-1))
    assert (l < mid && mid <= u &&             -- l < mid <= u
            l <= l' && l' <= u' && u' <= u &&  -- l <= l' <= u' <= u
            u'-l' < u-l)                       -- |u' - l'| < |u - l|
           (binarySearch z nonNegative l' u')

-- Establishes bounds on the unknown integer. By searching increasingly
-- large powers of 2, it'll bracket an integer x by lower & upper
-- such that lower <= x <= upper.
--
-- Assumes 2's complement integers.
bracketBounds :: ZCursor Token -> Bool -> TestMonad (Integer, Integer)
bracketBounds z nonNegative = do
    let -- test against integers 2**x-1 when positive, and 2**x when negative,
        -- to avoid generating constants that'd overflow the machine's integers.
        -- I.e. suppose we're searching for #{const INT_MAX} (e.g. 2^32-1).
        -- If we're comparing against all 2**x-1, we'll stop our search
        -- before we ever overflow int.
        powersOfTwo = iterate (\a -> 2*a) 1
        positiveBounds = map pred powersOfTwo
        negativeBounds = map negate powersOfTwo

        -- Test each element of the bounds list until we find one that exceeds
        -- the integer.
        loop cmp inner (maybeOuter:bounds') = do
          outerBounded <- compareConst z (cmp maybeOuter)
          if outerBounded
            then return (inner,maybeOuter)
            else loop cmp maybeOuter bounds'
        loop _ _ _ = error "bracketBounds: infinite list exhausted"

    if nonNegative
      then do (inner,outer) <- loop (LessOrEqual . Unsigned) (-1) positiveBounds
              return (inner+1,outer)
      else do (inner,outer) <- loop (GreaterOrEqual . Signed) 0 negativeBounds
              return (outer,inner-1)

-- For #{enum} codegen; mimics template-hsc.h's hsc_haskellize
haskellize :: String -> String
haskellize [] = []
haskellize (firstLetter:next) = toLower firstLetter : loop False next
    where loop _ [] = []
          loop _ ('_':as) = loop True as
          loop upper (a:as) = (if upper then toUpper a else toLower a) : loop False as

-- For #{enum} codegen; in normal hsc2hs, any whitespace in the enum types &
-- constructors will be mangled by the C preprocessor. This mimics the same
-- mangling.
stringify :: String -> String
-- Spec: stringify = unwords . words
stringify = go False . dropWhile isSpace
  where
    go _haveSpace [] = []
    go  haveSpace (x:xs)
      | isSpace x = go True xs
      | otherwise = if haveSpace
                    then ' ' : x : go False xs
                    else x : go False xs

-- For #{alignment} codegen; mimic's template-hsc.h's hsc_alignment
alignment :: String -> String
alignment t = "offsetof(struct {char x__; " ++ t ++ " (y__); }, y__)"

computeEnum :: ZCursor Token -> TestMonad String
computeEnum z@(ZCursor (Special _ _ enumText) _ _) =
    case parseEnum enumText of
        Nothing -> return ""
        Just (enumType,constructor,enums) ->
            concatM enums $ \(maybeHsName, cName) -> do
                constValue <- computeConst z cName
                let hsName = fromMaybe (haskellize cName) maybeHsName
                return $
                    hsName ++ " :: " ++ stringify enumType ++ "\n" ++
                    hsName ++ " = " ++ stringify constructor ++ " " ++ showsPrec 11 constValue "\n"
    where concatM l = liftM concat . forM l
computeEnum _ = error "computeEnum argument isn't a Special"

-- Implementation of #{type}, using computeConst
computeType :: ZCursor Token -> TestMonad String
computeType z@(ZCursor (Special pos _ value) _ _) = do
    testLogAtPos pos ("computing type of " ++ value) $ do
        integral <- testLog ("checking if type " ++ value ++ " is an integer") $ do
            success <- runCompileBooleanTest z $ "(" ++ value ++ ")(int)(" ++ value ++ ")1.4 == (" ++ value ++ ")1.4"
            testLog' $ "result: " ++ (if success then "integer" else "floating")
            return success
        typeRet <- if integral
         then do
            signed <- testLog ("checking if type " ++ value ++ " is signed") $ do
                success <- runCompileBooleanTest z $ "(" ++ value ++ ")(-1) < (" ++ value ++ ")0"
                testLog' $ "result: " ++ (if success then "signed" else "unsigned")
                return success
            size <- computeConst z ("sizeof(" ++ value ++ ")")
            return $ (if signed then "Int" else "Word") ++ (show (size * 8))
         else do
            let checkSize test = testLog ("checking if " ++ test) $ do
                    success <- runCompileBooleanTest z test
                    testLog' $ "result: " ++ show success
                    return success
            ldouble <- checkSize ("sizeof(" ++ value ++ ") > sizeof(double)")
            if ldouble
             then return "LDouble"
             else do
                double <- checkSize ("sizeof(" ++ value ++ ") == sizeof(double)")
                if double
                 then return "Double"
                 else return "Float"
        testLog' $ "result: " ++ typeRet
        return typeRet
computeType _ = error "computeType argument isn't a Special"

outHeaderCProg' :: Token -> String
outHeaderCProg' (Special pos key value) = outHeaderCProg (pos,key,value)
outHeaderCProg' _ = ""

-- Checks if an #if/#ifdef etc. etc. is true by inserting a #error
-- and seeing if the compile fails.
checkConditional :: ZCursor Token -> TestMonad Bool
checkConditional (ZCursor s@(Special pos key value) above below) = do
    config <- testGetConfig
    flags <- testGetFlags
    let test = outTemplateHeaderCProg (cTemplate config) ++
               (concatMap outFlagHeaderCProg flags) ++
               (concatMap outHeaderCProg' above) ++
               outHeaderCProg' s ++ "#error T\n" ++
               (concatMap outHeaderCProg' below)
    testLogAtPos pos ("checking #" ++ key ++ " " ++ value) $ do
        condTrue <- not `fmap` runCompileTest test
        testLog' $ "result: " ++ show condTrue
        return condTrue
checkConditional _ = error "checkConditional argument isn't a Special"

-- Make sure the value we're trying to binary search isn't floating point.
checkValueIsIntegral :: ZCursor Token -> Bool -> TestMonad Bool
checkValueIsIntegral z@(ZCursor (Special _ _ value) _ _) nonNegative = do
    let intType = if nonNegative then "unsigned long long" else "long long"
    testLog ("checking if " ++ value ++ " is an integer") $ do
        success <- runCompileBooleanTest z $ "(" ++ intType ++ ")(" ++ value ++ ") == (" ++ value ++ ")"
        testLog' $ "result: " ++ (if success then "integer" else "floating")
        return success
checkValueIsIntegral _ _ = error "checkConditional argument isn't a Special"

compareConst :: ZCursor Token -> IntegerComparison -> TestMonad Bool
compareConst z@(ZCursor (Special _ _ value) _ _) cmpTest = do
    testLog ("checking " ++ value ++ " " ++ show cmpTest) $ do
        success <- runCompileBooleanTest z $ "(" ++ value ++ ") " ++ cShowCmpTest cmpTest
        testLog' $ "result: " ++ show success
        return success
compareConst _ _ = error "compareConst argument isn't a Special"

-- Given a compile-time constant with boolean type, this extracts the
-- value of the constant by compiling a .c file only.
--
-- The trick comes from autoconf: use the fact that the compiler must
-- perform constant arithmetic for computation of array dimensions, and
-- will generate an error if the array has negative size.
runCompileBooleanTest :: ZCursor Token -> String -> TestMonad Bool
runCompileBooleanTest (ZCursor s above below) booleanTest = do
    config <- testGetConfig
    flags <- testGetFlags
    let test = -- all the surrounding code
               outTemplateHeaderCProg (cTemplate config) ++
               (concatMap outFlagHeaderCProg flags) ++
               (concatMap outHeaderCProg' above) ++
               outHeaderCProg' s ++
               -- the test
               "int _hsc2hs_test() {\n" ++
               "  static int test_array[1 - 2 * !(" ++ booleanTest ++ ")];\n" ++
               "  return test_array[0];\n" ++
               "}\n" ++
               (concatMap outHeaderCProg' below)
    runCompileTest test

runCompileTest :: String -> TestMonad Bool
runCompileTest testStr = do
    makeTest3 (".c", ".o",".txt") $ \(cFile,oFile,stdout) -> do
      liftTestIO $ writeBinaryFile cFile testStr
      flags <- testGetFlags
      compiler <- testGetCompiler
      runCompiler compiler
                  (["-c",cFile,"-o",oFile]++[f | CompFlag f <- flags])
                  (Just stdout)

runCompiler :: FilePath -> [String] -> Maybe FilePath -> TestMonad Bool
runCompiler prog args mStdoutFile = do
  let cmdLine = showCommandForUser prog args
  testLog ("executing: " ++ cmdLine) $ liftTestIO $ do
      mHOut <- case mStdoutFile of
               Nothing -> return Nothing
               Just stdoutFile -> liftM Just $ openFile stdoutFile WriteMode
      process <- runProcess prog args Nothing Nothing Nothing mHOut mHOut
      case mHOut of
          Just hOut -> hClose hOut
          Nothing -> return ()
      exitStatus <- waitForProcess process
      return $ case exitStatus of
                 ExitSuccess -> True
                 ExitFailure _ -> False

-- The main driver for cross-compilation mode
outputCross :: Config -> String -> String -> String -> String -> [Token] -> IO ()
outputCross config outName outDir outBase inName toks =
    runTestMonad $ do
        file <- liftTestIO $ openFile outName WriteMode
        (diagnose inName (liftTestIO . hPutStr file) toks
           `testFinally` (liftTestIO $ hClose file))
           `testOnException` (liftTestIO $ removeFile outName) -- cleanup on errors
    where
    tmenv = TestMonadEnv (cVerbose config) 0 (cKeepFiles config) (outDir++outBase++"_hsc_test") (cFlags config) config (cCompiler config)
    runTestMonad x = runTest x tmenv 0 >>= (handleError . fst)

    handleError (Left e) = die (e++"\n")
    handleError (Right ()) = return ()

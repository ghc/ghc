
module CmdSemantics ( parseOneTFile, processParsedTFile )
where

import CmdSyntax
import CmdLexer		( isVarChar )
import CmdParser	( parseScript )
import Monad		( when )
import Directory	( doesFileExist )
import System		( ExitCode(..) )

#ifdef __NHC__
import NonStdTrace(trace)
#else
import IOExts(trace)
#endif
---------------------------------------------------------------------
-- Hook into Meurig Sage's regexp library

import Regexp		( MatcherFlag(..), searchS, legalRegexp, matchedAny )

myMatchRegexp :: String -> String -> Maybe Bool
myMatchRegexp rx str
   -- | trace (show (rx, str)) True
   = let result = searchS rx [Multi_Line] str
     in  if   not (legalRegexp result)
         then Nothing
         else Just (matchedAny result)

---------------------------------------------------------------------
-- A monad to carry around the EvalEnv.

type IOE a  = EvalEnv -> IO (EvalEnv, a)

thenE :: IOE a -> (a -> IOE b) -> IOE b
thenE x y p
   = do (p2, xv) <- x p
        y xv p2

thenE_ :: IOE a -> IOE b -> IOE b
thenE_ x y p
   = do (p2, xv) <- x p
        y p2

returnE :: a -> IOE a
returnE x p = return (p, x)

getEvalEnv :: IOE EvalEnv
getEvalEnv p = return (p, p)

setEvalEnv :: EvalEnv -> IOE ()
setEvalEnv p pnew = return (pnew, ())


---------------------------------------------------------------------
-- Enhanced version of IOE, which propagates failure values immediately.

data EvalResult a
   = FrameFail  String			-- failure; act like "throw"
   | Results    (Result, Result)	-- final result (exp,act); ditto
   | Value      a			-- value; keep going


type IOEV a = IOE (EvalResult a)

returnEV :: a -> IOE (EvalResult a)
returnEV x p = return (p, Value x)

failEV :: String -> IOE (EvalResult a)
failEV str p = return (p, FrameFail str)

resultsEV :: (Result, Result) -> IOE (EvalResult a)
resultsEV (r1,r2) p = return (p, Results (r1,r2))

thenEV :: IOEV a -> (a -> IOEV b) -> IOEV b
thenEV x y
   = x 						`thenE` \ res_x ->
     case res_x of
        Value x_ok  -> y x_ok
        FrameFail s -> failEV s
        Results rs  -> resultsEV rs

thenEV_ :: IOEV a -> IOEV b -> IOEV b
thenEV_ x y
   = x 						`thenE` \ res_x ->
     case res_x of
        Value x_ok  -> y
        FrameFail s -> failEV s
        Results rs  -> resultsEV rs

mapEV :: (a -> IOEV b) -> [a] -> IOEV [b]
mapEV f []     = returnEV []
mapEV f (x:xs) = f x       			`thenEV` \ x_done ->
                mapEV f xs 			`thenEV` \ xs_done ->
                returnEV (x_done:xs_done)

whenEV :: Bool -> IOEV () -> IOEV ()
whenEV b act
   = if b then act else returnEV ()

ioToEV :: IO a -> IOEV a
ioToEV io p
   = do r <- io
        return (p, Value r)

bind x f = f x


---------------------------------------------------------------------
-- environment management stuff

data EvalEnv 
   = EvalEnv {
        -- THESE NEVER CHANGE
        globals :: [(Var, String)],		-- global var binds
        mdefs   :: [(MacroName, MacroDef)],	-- macro defs
        -- WRITABLE, DISCARDED AT PROCEDURE EXIT
	locals  :: [(Var, String)],		-- local var binds
        -- THREADED
        results :: (Maybe Result, Maybe Result)
				-- expected and actual results
     }

-- Record in the environment an expected or actual result.
-- Complain about duplicate assignments.
-- If the assignment now means that both an expected and actual
-- result is available, terminate computation and return these
-- results to the top level of the driver.
setResult :: Bool -> Result -> IOEV ()
setResult is_actual res
   = getEvalEnv					`thenE` \ p ->
     results p					`bind` \ (r_exp, r_act) ->
     (is_actual && isJust r_act)		`bind` \ dup_act ->
     ((not is_actual) && isJust r_exp)		`bind` \ dup_exp ->
     if dup_act || dup_exp
     then failEV "duplicate assignment of expected or actual outcome"
     else
     (if is_actual then (r_exp, Just res)
                   else (Just res, r_act))	`bind` \ (new_exp, new_act) ->
     if   isJust new_exp && isJust new_act
     then resultsEV (unJust new_exp, unJust new_act)
     else 
     setEvalEnv (p{results = (new_exp, new_act)})
						`thenE_`
     returnEV ()


addLocalVarBind :: Var -> String -> IOEV ()
addLocalVarBind v s
   = getEvalEnv					`thenE` \ p ->
     if   v `elem` map fst (globals p)
     then failEV (isGlobalVar v)
     else setEvalEnv (p{ globals = (v,s):(globals p) })
						`thenE_`
          returnEV ()

lookupVar_maybe :: Var -> IOE (Maybe String)
lookupVar_maybe v
   = getEvalEnv					`thenE` \ p ->
     returnE (lookup v (locals p ++ globals p))

lookupVar :: Var -> IOEV String
lookupVar v 
   = lookupVar_maybe v				`thenE` \ maybe_v ->
     case maybe_v of
        Just xx -> returnEV xx
        Nothing -> failEV (missingVar v)

lookupMacro :: MacroName -> IOEV MacroDef
lookupMacro mnm
   = getEvalEnv					`thenE` \ p ->
     case lookup mnm (mdefs p) of
        Just mdef -> returnEV mdef
        Nothing   -> failEV (missingMacro mnm)

initialEnv global_env macro_env
   = EvalEnv{ globals=global_env, mdefs=macro_env,
              locals=[], results=(Nothing,Nothing) }

getLocalEnv :: IOE [(Var,String)]
getLocalEnv
   = getEvalEnv					`thenE` \ p ->
     returnE (locals p)

setLocalEnv :: [(Var,String)] -> IOE ()
setLocalEnv l_env
   = getEvalEnv 				`thenE` \ p ->
     setEvalEnv (p{locals=l_env})


---------------------------------------------------------------------
-- Run all the tests defined in a parsed .T file.

processParsedTFile :: [(Var,String)]
                   -> [TopDef]
                   -> IO [(TestName,
                             (Either String{-framefail-} 
                                     (Result, Result){-outcomes-})
                         )]
processParsedTFile global_env topdefs
   = do let tests = filter isTTest     topdefs
        let macs  = filter isTMacroDef topdefs
        let incls = filter isTInclude  topdefs
        let macro_env = map (\(TMacroDef mnm mrhs) -> (mnm,mrhs)) macs
        let doOne (TTest tname stmts)
               = do r <- doOneTest (("testname", tname):global_env)
                                   macro_env stmts
                    return (tname, r)
        all_done <- mapM doOne tests
        return all_done

        
---------------------------------------------------------------------
-- Parsing a complete .T file and the transitive closure of its includes.

parseOneTFile :: [(Var,String)]		-- global var env
              -> FilePath		-- the T file to parse
              -> IO (Either String{-complaint of some sort-}
                            (FilePath, [TopDef]))

parseOneTFile global_env tfile
   = do { have_f <- doesFileExist tfile
        ; if not have_f
           then return (Left ("can't open script file `" ++ tfile ++ "'"))
           else 
     do { f_cts <- readFile tfile
        ; let p_result = parseScript tfile f_cts
        ; case p_result of {
              Left errmsg -> return (Left errmsg) ;
              Right topdefs -> 
     do { -- filter out the includes and recurse on them
          let here_topdefs  = filter isTInclude topdefs
        ; let here_includes = filter (not.isTInclude) topdefs
        ; incl_paths
             <- mapM ( \i -> case i of 
                                TInclude expr -> evalIncludeExpr global_env expr
                     ) here_includes
        ; let bad_incl_exprs = filter isLeft incl_paths
        ; if not (null bad_incl_exprs)
            then case head bad_incl_exprs of
                    Left moanage -> return (Left moanage)
            else 
     do { let names_to_include = map unRight incl_paths
        ; incl_topdefss <- mapM (parseOneTFile global_env) names_to_include
        ; let failed_includes = filter isLeft incl_topdefss
        ; if not (null failed_includes)
            then return (head failed_includes)
            else 
     do { let more_topdefs = concatMap (snd.unRight) incl_topdefss
        ; return (Right (tfile, here_topdefs ++ more_topdefs))
     }}}}}}


-- Simplistically evaluate an expression, using just the global
-- value env.  Used for evaluating the args of include statements.
evalIncludeExpr :: [(Var,String)] 
                -> Expr 
                -> IO (Either String{-errmsg-} String{-result-})
evalIncludeExpr global_env expr
   = do let initial_env = initialEnv global_env []
        (final_env, res) <- evalExpr expr initial_env
        case res of
           FrameFail msg -> return (Left ("invalid include expr: " ++ msg))
           Value v       -> return (Right v)
           Results ress  -> panic "evalIncludeExpr"
        
          

---------------------------------------------------------------------
-- Running a single test.

-- Run the whole show for a given test, stopping when:
-- * A framework failure occurs
-- * Both expected and actual results are determined
-- * We run out of statements and neither of the above two
--   apply.  This also counts as a framework failure.

doOneTest :: [(Var,String)]		-- global var env
          -> [(MacroName, MacroDef)]	-- macro env
          -> [Stmt]			-- stmts for this test
          -> IO (Either String{-framefail-} 
                        (Result, Result){-outcomes-})

doOneTest global_env code_env stmts
   = do let initial_env = initialEnv global_env code_env
        res <- doStmts stmts initial_env
        case snd res of
           FrameFail msg   -> return (Left msg)
           Value _         -> inconclusive
           Results ress    -> return (Right ress)
     where
        inconclusive 
           = return (Left ("test completed but actual/expected " ++ 
                           "results not determined"))


-- Run a bunch of statements, and return either Nothing if 
-- there was no return statement, or the value computed by said.
doStmts :: [Stmt] -> IOEV (Maybe String)
doStmts []     = returnEV Nothing
doStmts (s:ss) = doStmt s `thenEV` \ maybe_v ->
                 case maybe_v of 
                    Just xx -> returnEV (Just xx)
                    Nothing -> doStmts ss


doStmt :: Stmt -> IOEV (Maybe String)
doStmt (SAssign v expr)
   = evalExpr expr				`thenEV`  \ str ->
     addLocalVarBind v str			`thenEV_`
     returnEV Nothing
doStmt (SPrint expr)
   = evalExpr expr				`thenEV` \ str ->
     ioToEV (putStrLn str)			`thenEV_`
     returnEV Nothing
doStmt (SCond c t maybe_f)
   = evalExprToBool c				`thenEV` \ c_bool ->
     if   c_bool
     then doStmts t
     else case maybe_f of
             Nothing -> returnEV Nothing
             Just f  -> doStmts f
doStmt (SRun var expr)
   = evalExpr expr				`thenEV` \ cmd_to_run ->
     systemEV cmd_to_run			`thenEV` \ exit_code ->
     addLocalVarBind var (show exit_code)	`thenEV_`
     returnEV Nothing

doStmt (SFFail expr)
   = evalExpr expr				`thenEV` \ res ->
     failEV ("user-frame-fail: " ++ res)
doStmt (SResult res expr)
   = evalExprToBool expr			`thenEV` \ b ->
     whenEV b (setResult True{-actual-} res)	`thenEV_`
     returnEV Nothing
doStmt (SExpect res)
   = setResult False{-expected-} res		`thenEV_`
     returnEV Nothing

doStmt (SMacro mnm args)
   = runMacro mnm args				`thenEV` \ maybe_v ->
     case maybe_v of
        Nothing -> returnEV Nothing
        Just _  -> failEV (hasValue mnm)

doStmt (SReturn expr)
   = evalExpr expr				`thenEV` \ res ->
     returnEV (Just res)

runMacro :: MacroName -> [Expr] -> IOEV (Maybe String)
runMacro mnm args
   = lookupMacro mnm				`thenEV` \ mdef ->
     case mdef of { MacroDef formals stmts ->
     length formals				`bind` \ n_formals ->
     length args				`bind` \ n_args ->
     if   n_formals /= n_args
     then failEV (arityErr mnm n_formals n_args)
     else mapEV evalExpr args			`thenEV` \ arg_vals ->
          zip formals arg_vals			`bind`  \ new_local_env ->
          getLocalEnv				`thenE` \ our_local_env ->
          setLocalEnv new_local_env		`thenE_`
          doStmts stmts				`thenEV` \ res ->
          setLocalEnv our_local_env		`thenE_`
          returnEV res
     }


---------------------------------------------------------------------
-- The expression evaluator.

fromBool b
   = if b then "True" else "False"

cantOpen f 
   = "Can't open file `" ++ f ++ "'"
regExpErr rx
   = "Invalid regexp `" ++ rx ++ "'"
missingVar v
   = "No binding for variable `$" ++ v ++ "'"
missingMacro mnm
   = "No binding for macro `" ++ mnm ++ "'"
notABool str
   = "String `" ++ str ++ "' is neither `True' nor `False'"
arityErr mnm n_formals n_actuals
   = "Macro `" ++ mnm ++ "' expects " ++ show n_formals 
     ++ " args, but was given " ++ show n_actuals
macroArg mnm arg
   = "No binding for formal param `$" ++ arg 
     ++ "' whilst expanding macro `" ++ mnm ++ "'"
isGlobalVar v
   = "Assigments to global variable `$" ++ v ++ "' are not allowed"
hasValue mnm
   = "Macro `" ++ mnm ++ "' used in context not expecting a value"
noValue mnm
   = "Macro `" ++ mnm ++ "' used in context expecting a value"


evalOpExpr :: Op -> String -> String -> IOEV String

evalOpExpr OpAppend s1 s2 = returnEV (s1 ++ s2)
evalOpExpr OpEq     s1 s2 = returnEV (fromBool (s1 == s2))
evalOpExpr OpNEq    s1 s2 = returnEV (fromBool (s1 /= s2))
evalOpExpr OpContains s rx 
   = case myMatchRegexp rx s of
        Nothing -> failEV (regExpErr rx)
        Just bb -> returnEV (fromBool bb)
evalOpExpr OpLacks s rx 
   = case myMatchRegexp rx s of
        Nothing -> failEV (regExpErr rx)
        Just bb -> returnEV (fromBool (not bb))


evalExpr :: Expr -> IOEV String
evalExpr (EOp op e1 e2)
   | op `elem` [OpEq, OpNEq, OpAppend, OpContains, OpLacks]
   = evalExpr e1 				`thenEV` \ e1s ->
     evalExpr e2 				`thenEV` \ e2s ->
     evalOpExpr op e1s e2s
evalExpr (EOp OpOr e1 e2)
   = evalExprToBool e1				`thenEV` \ b1 ->
     if b1 then returnEV (fromBool True)
           else evalExprToBool e2		`thenEV` \ b2 ->
                returnEV (fromBool b2)
evalExpr (EOp OpAnd e1 e2)
   = evalExprToBool e1				`thenEV` \ b1 ->
     if not b1 then returnEV (fromBool False)
               else evalExprToBool e2		`thenEV` \ b2 ->
                    returnEV (fromBool b2)
evalExpr (EString str)
   = returnEV str
evalExpr (EBool b)
   = returnEV (fromBool b)
evalExpr (EContents expr)
   = evalExpr expr 				`thenEV` \ filename ->
     readFileEV filename
evalExpr (EExists expr)
   = evalExpr expr 				`thenEV` \ filename ->
     doesFileExistEV filename			`thenEV` \ b ->
     returnEV (fromBool b)
evalExpr (EDefined v)
   | null v || head v /= '$'
   = panic "evalExpr(EDefined): not a var"
	-- This is a panic because the lexer+parser should have
	-- conspired to ensure this
   | otherwise
   = lookupVar_maybe v				`thenE` \ maybe_v ->
     returnEV (fromBool (isJust maybe_v))
evalExpr EOtherwise
   = returnEV (fromBool True)
evalExpr (ECond c t maybe_f)
   = evalExprToBool c				`thenEV` \ c_bool ->
     if   c_bool
     then evalExpr t
     else case maybe_f of
             Nothing -> returnEV ""
             Just f  -> evalExpr f
evalExpr (EVar v)
   = lookupVar v
evalExpr (EFFail expr)
   = evalExpr expr				`thenEV` \ res ->
     failEV ("user-frame-fail: " ++ res)

evalExpr (EMacro mnm args)
   = runMacro mnm args				`thenEV` \ maybe_v ->
     case maybe_v of
        Nothing -> failEV (noValue mnm)
        Just xx -> returnEV xx

-------------------------

-- Does filename exist?
doesFileExistEV :: String -> IOEV Bool
doesFileExistEV filename
   = ioToEV (doesFileExist filename)		`thenEV` \ b ->
     returnEV b


-- If filename doesn't contain any slashes, stick $testdir/ on
-- the front of it.
readFileEV :: String -> IOEV String
readFileEV filename
   = --qualify filename0 			`thenE` \ filename ->
     ioToEV (doesFileExist filename) 		`thenEV` \ exists ->
     if   not exists 
     then failEV (cantOpen filename)
     else ioToEV (readFile filename) 		`thenEV` \ contents ->
     returnEV contents
--     where
--        qualify fn 
--           | '/' `elem` fn 
--           = returnE fn
--           | otherwise 
--           = lookupVar p "testdir"	`thenE` \ testdir ->
--             returnE (testdir ++ "/" ++ fn)



systemEV :: String -> IOEV Int
systemEV str
   = ioToEV (my_system str) 			`thenEV` \ ret_code ->
     case ret_code of
        ExitSuccess   -> returnEV 0
        ExitFailure m -> returnEV m

---------------------------

evalExprToBool :: Expr -> IOEV Bool
evalExprToBool e
   = evalExpr e					`thenEV` \ e_eval ->
     case e_eval of
        "True"  -> returnEV True
        "False" -> returnEV False
        other   -> failEV (notABool other)

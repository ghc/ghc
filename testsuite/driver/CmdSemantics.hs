
module CmdSemantics ( doEval )
where

import CmdSyntax
import CmdLexer		( isVarChar )
import CmdParser	( Parser, pExpr, pStmt, pFile, parseStringWith )
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
-- A simple monad to propagate failure inside the evaluator.

type IOE a = IO (Either String a)

returnE :: a -> IOE a
returnE x = return (Right x)

failE :: String -> IOE a
failE str = return (Left str)

thenE_ :: IOE a -> IOE b -> IOE b
thenE_ x y
   = do xv <- x
        case xv of
           Left  xerr -> return (Left xerr)
           Right xok  -> y

thenE :: IOE a -> (a -> IOE b) -> IOE b
thenE x y
   = do xv <- x
        case xv of
           Left  xerr -> return (Left xerr)
           Right xok  -> y xok

mapE :: (a -> IOE b) -> [a] -> IOE [b]
mapE f [] = returnE []
mapE f (x:xs) = f x        `thenE` \ x_done ->
                mapE f xs `thenE` \ xs_done ->
                returnE (x_done:xs_done)

ioToE io 
   = do r <- io
        return (Right r)

bind x f = f x


---------------------------------------------------------------------
-- environment management stuff

data EvalEnv 
   = EvalEnv {
        vars :: [(Var, String)],	-- var binds
        macs :: [(MacroName, MacroDef)],
        dir  :: FilePath		-- 'cos evalCond might need
					-- to read files in test dir
     }

addVarBindToEnv :: EvalEnv -> Var -> String -> IOE EvalEnv
addVarBindToEnv env_in v s
   | v `elem` ["tool", "testdir", "confdir", "conffile"]
   = failE (readOnlyVar v)
   | otherwise
   = returnE (env_in { vars = (v,s):vars env_in })

addMacroBindToEnv :: EvalEnv -> MacroName -> MacroDef -> EvalEnv
addMacroBindToEnv env_in mnm mdef
   = env_in { macs = (mnm,mdef):macs env_in }

lookupVar :: EvalEnv -> Var -> IOE String
lookupVar p v 
   = case lookup v (vars p) of
        Just xx -> returnE xx
        Nothing -> failE (missingVar v)

lookupMacro :: EvalEnv -> MacroName -> IOE MacroDef
lookupMacro p mnm
   = case lookup mnm (macs p) of
        Just mdef -> returnE mdef
        Nothing   -> failE (missingMacro mnm)

initEvalEnv test_dir init_var_binds
   = EvalEnv { vars = init_var_binds, macs = [], dir = test_dir }


---------------------------------------------------------------------
-- Top-level stuff.

data TopRes
   = TopRes EvalEnv		-- accumulated so far
            (Maybe Result)	-- expected
            (Maybe Result)	-- actual
            [TopDef]		-- topdefs from include clauses?

doTopDef :: EvalEnv -> TopDef -> IOE TopRes
doTopDef p (TStmt stmt)
   = doStmt p stmt			`thenE` \ (p_new, _) ->
     returnE (TopRes p_new Nothing Nothing [])
doTopDef p (TSkip expr)
   = evalExprToBool p expr		`thenE` \ do_skip ->
     (if do_skip then Just Skipped else Nothing)
					`bind`  \ maybe_skipped ->
     returnE (TopRes p Nothing maybe_skipped [])
doTopDef p (TResult res when_expr)
   = evalExprToBool p when_expr		`thenE` \ expr_bool ->
     (if expr_bool then Just res else Nothing)
					`bind` \ maybe_result ->
     returnE (TopRes p Nothing maybe_result [])
doTopDef p (TExpect res)
   = returnE (TopRes p (Just res) Nothing [])
doTopDef p (TInclude expr)
   = evalExpr p expr			`thenE` \ filename ->
     readFileE p filename		`thenE` \ contents ->
     case parseStringWith ("file `" ++ filename ++ "'")
                          contents pFile of
             Left errmsg -> failE errmsg
             Right more_topdefs 
                -> returnE (TopRes p Nothing Nothing more_topdefs)
doTopDef p (TMacroDef mnm mdef)
   = addMacroBindToEnv p mnm mdef	`bind`  \ p_new ->
     returnE (TopRes p_new Nothing Nothing [])

-- Process top defs until either 
-- * One expected and one actual result are available
-- * We run out of topdefs
-- With the additional complication that we should stop as
-- soon as a `skip' actual result appears, regardless of 
-- whether we have an actual result.

doTopDefs :: EvalEnv -> [TopDef] -> ([Result], [Result]) 
          -> IOE (Result, Result)

doTopDefs p tds (_, (Skipped:_))
   = returnE (Skipped, Skipped)
doTopDefs p tds (e:exs, a:acts)
   = returnE (e, a)
doTopDefs p [] (exs, acts)
   | null exs
   = failE "No `expect' clauses found"
   | null acts
   = failE "Evaluation completed, but no actual result determined"
doTopDefs p (td:tds) (exs, acts)
   = doTopDef p td 			`thenE` \ td_result ->
     case td_result of
        TopRes p_new maybe_exp maybe_act new_tds
           -> doTopDefs p_new (new_tds ++ tds)
                              (exs  ++ listify maybe_exp,
                               acts ++ listify maybe_act)
              where listify (Just x) = [x]
                    listify Nothing  = []


-- Run the whole show, given some initial topdefs
doEval :: FilePath -> [(Var,String)] -> [TopDef] 
       -> IO (Maybe (Result, Result))
doEval test_dir init_var_binds tds
   = do outcome <- doTopDefs (initEvalEnv test_dir init_var_binds) tds ([],[])
        case outcome of
           Left err       -> do officialMsg err
                                return Nothing
           Right res_pair -> return (Just res_pair)

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
readOnlyVar v
   = "Assigments to variable `$" ++ v ++ "' are not allowed"
hasValue mnm
   = "Macro `" ++ mnm ++ "' used in context not expecting a value"
noValue mnm
   = "Macro `" ++ mnm ++ "' used in context expecting a value"

evalOpExpr :: Op -> String -> String -> IOE String

evalOpExpr OpAppend s1 s2 = returnE (s1 ++ s2)
evalOpExpr OpEq     s1 s2 = returnE (fromBool (s1 == s2))
evalOpExpr OpNEq    s1 s2 = returnE (fromBool (s1 /= s2))
evalOpExpr OpContains s rx 
   = case myMatchRegexp rx s of
        Nothing -> failE (regExpErr rx)
        Just bb -> returnE (fromBool bb)
evalOpExpr OpLacks s rx 
   = case myMatchRegexp rx s of
        Nothing -> failE (regExpErr rx)
        Just bb -> returnE (fromBool (not bb))


doStmt :: EvalEnv -> Stmt -> IOE (EvalEnv, Maybe String)

doStmt p (SAssign v expr)
   = evalExpr p expr			`thenE` \ str ->
     addVarBindToEnv p v str		`thenE` \ p_new ->
     returnE (p_new, Nothing)
doStmt p (SPrint expr)
   = evalExpr p expr			`thenE` \ str ->
     ioToE (putStrLn str)		`thenE_`
     returnE (p, Nothing)
doStmt p (SCond c t maybe_f)
   = evalExprToBool p c			`thenE` \ c_bool ->
     if   c_bool
     then doStmts p t
     else case maybe_f of
             Nothing -> returnE (p, Nothing)
             Just f  -> doStmts p f
doStmt p (SMacro mnm args)
   = evalMacroUse p mnm args		`thenE` \ (p_new, maybe_res) ->
     case maybe_res of
        Nothing -> returnE (p_new, Nothing)
        Just vv -> failE (hasValue mnm)
doStmt p (SRun var expr)
   = evalExpr p expr			`thenE` \ cmd_to_run ->
     systemE cmd_to_run			`thenE` \ exit_code ->
     addVarBindToEnv p var 
                     (show exit_code)	`thenE` \ p_new ->
     returnE (p_new, Nothing)
doStmt p (SReturn expr)
   = evalExpr p expr			`thenE` \ res ->
     returnE (p, Just res)
doStmt p (SFFail expr)
   = evalExpr p expr			`thenE` \ res ->
     failE ("user-frame-fail: " ++ res)


doStmts p []
   = returnE (p, Nothing)
doStmts p (s:ss)
   = doStmt p s `thenE` \ (p_s, maybe_ret) -> 
     case maybe_ret of
        Just xx -> returnE (p_s, maybe_ret)
        Nothing -> doStmts p_s ss


evalExpr :: EvalEnv -> Expr -> IOE String
evalExpr p (EOp op e1 e2)
   | op `elem` [OpEq, OpNEq, OpAppend, OpContains, OpLacks]
   = evalExpr p e1 			`thenE` \ e1s ->
     evalExpr p e2 			`thenE` \ e2s ->
     evalOpExpr op e1s e2s
evalExpr p (EOp OpOr e1 e2)
   = evalExprToBool p e1		`thenE` \ b1 ->
     if b1 then returnE (fromBool True)
           else evalExprToBool p e2	`thenE` \ b2 ->
                returnE (fromBool b2)
evalExpr p (EOp OpAnd e1 e2)
   = evalExprToBool p e1		`thenE` \ b1 ->
     if not b1 then returnE (fromBool False)
               else evalExprToBool p e2	`thenE` \ b2 ->
                    returnE (fromBool b2)
evalExpr p (EString str)
   = returnE str
evalExpr p (EBool b)
   = returnE (fromBool b)
evalExpr p (EContents expr)
   = evalExpr p expr 			`thenE` \ filename ->
     readFileE p filename
evalExpr p (EExists expr)
   = evalExpr p expr 			`thenE` \ filename ->
     doesFileExistE p filename		`thenE` \ b ->
     returnE (fromBool b)
evalExpr p (EHasValue expr)
   = evalExpr p expr			`thenE` \ str ->
     returnE (fromBool (not (null str)))
evalExpr p EOtherwise
   = returnE (fromBool True)
evalExpr p (ECond c t maybe_f)
   = evalExprToBool p c			`thenE` \ c_bool ->
     if   c_bool
     then evalExpr p t
     else case maybe_f of
             Nothing -> returnE ""
             Just f  -> evalExpr p f
evalExpr p (EVar v)
   = lookupVar p v
evalExpr p (EMacro mnm args)
   = evalMacroUse p mnm args		`thenE` \ (p_new, maybe_res) ->
     case maybe_res of
        Nothing -> failE (noValue mnm)
        Just vv -> returnE vv
evalExpr p (EFFail expr)
   = evalExpr p expr			`thenE` \ res ->
     failE ("user-frame-fail: " ++ res)


evalMacroUse :: EvalEnv -> MacroName -> [Expr] 
             -> IOE (EvalEnv, Maybe String)
evalMacroUse p mnm args
   = lookupMacro p mnm 			`thenE` \ macro ->
     case macro of { MacroDef formals stmts ->
     if   length formals /= length args
     then failE (arityErr mnm (length formals) (length args))
     else 
     mapE (evalExpr p) args		`thenE` \ arg_ress ->
     zip formals arg_ress		`bind`  \ subst_env ->
     map (substStmt subst_env) stmts	`bind`  \ stmts2 ->
     doStmts p stmts2			`thenE` \ pair ->
     returnE pair
     }

substStmt :: [(Var,String)] -> Stmt -> Stmt
substStmt env stmt
   = case stmt of
        SAssign v e -> SAssign v (se e)
        SPrint e    -> SPrint (se e)
        SCond c ts Nothing -> SCond (se c) (map ss ts) Nothing
        SCond c ts (Just fs) -> SCond (se c) (map ss ts) (Just (map ss fs))
        SRun v e -> SRun v (se e)
        SReturn e -> SReturn (se e)
        SMacro mnm es -> SMacro mnm (map se es)
        SFFail e -> SFFail (se e)
     where
        se = substExpr env
        ss = substStmt env

substExpr env expr
   = case expr of
        EOp op a1 a2 -> EOp op (se a1) (se a2)
        EVar v -> case lookup v env of
                     Just str -> EString str
                     Nothing -> EVar v
        EString str -> EString str
        EBool b -> EBool b
        EContents e -> EContents (se e)
        EExists e -> EExists (se e)
        EMacro mnm es -> EMacro mnm (map se es)
        ECond c t Nothing  -> ECond (se c) (se t) Nothing
        ECond c t (Just f) -> ECond (se c) (se t) (Just (se f))
        EOtherwise -> EOtherwise
        EHasValue e -> EHasValue (se e)
        EFFail e -> EFFail (se e)
     where
        se = substExpr env
        ss = substStmt env


-------------------------

-- Does filename exist?
doesFileExistE :: EvalEnv -> String -> IOE Bool
doesFileExistE p filename
   = ioToE (doesFileExist filename)	`thenE` \ b ->
     returnE b


-- If filename doesn't contain any slashes, stick $testdir/ on
-- the front of it.
readFileE :: EvalEnv -> String -> IOE String
readFileE p filename0
   = qualify filename0 			`thenE` \ filename ->
     ioToE (doesFileExist filename) 	`thenE` \ exists ->
     if   not exists 
     then failE (cantOpen filename)
     else ioToE (readFile filename) 	`thenE` \ contents ->
     returnE contents
     where
        qualify fn 
           | '/' `elem` fn 
           = returnE fn
           | otherwise 
           = lookupVar p "testdir"	`thenE` \ testdir ->
             returnE (testdir ++ "/" ++ fn)



systemE :: String -> IOE Int
systemE str
   = ioToE (my_system str) 		`thenE` \ ret_code ->
     case ret_code of
        ExitSuccess   -> returnE 0
        ExitFailure m -> returnE m

---------------------------

evalExprToBool :: EvalEnv -> Expr -> IOE Bool
evalExprToBool p e
   = evalExpr p e			`thenE` \ e_eval ->
     case e_eval of
        "True"  -> returnE True
        "False" -> returnE False
        other   -> failE (notABool other)


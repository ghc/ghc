
module CmdParser ( parseScript )
where

import CmdSyntax
import CmdLexer

import Char		( isAlpha, isDigit, isSpace )
import Directory	( doesFileExist )

#ifdef __NHC__
import NonStdTrace(trace)
#else
import IOExts(trace)
#endif


---------------------------------------------------------------------
-- Top level parser

parseScript :: String -> String -> Either String [TopDef]
parseScript fname fcontents 
   = parseStringWith fname fcontents pFile

---------------------------------------------------------------------
-- Parse a string with an arbitrary parser.

parseStringWith :: String -> String -> Parser a -> Either String a
parseStringWith err_src string p
   = let tokens = tokenise 1 string
         presult
            = case p tokens of
                 POk bads res [] -> Right res
                 POk bads res uu -> Left (further bads uu)
                 PFail bads      -> Left bads
     in  case presult of
            Right res  -> Right res
            Left  bads -> Left (err_src ++ ":" ++ show (getLineNo bads) 
                                ++ ": parse error on: "
                                ++ if   null bads 
                                   then "EOF" 
                                   else show (getLex (head bads))
                               )


---------------------------------------------------------------------
-- parsers
pFile 
   = pStar pTopDef

pTopDef
   = pAlts [
        p2 (\i expr -> TInclude expr) (pKW L_Include) pExpr,
        p4 (\d mnm formals stmts -> TMacroDef mnm (MacroDef formals stmts))
           (pKW L_Def) pText pFormals pStmtBlock,
        p3 (\t testname stmts -> TTest testname stmts)
           (pKW L_Test) pString pStmtBlock
     ]
     where
        pFormals
           = pInParens (pZeroOrMoreWithSep (pKW L_Comma) pFormalVar)
        pStmtBlock
           = pInBraces (pStar pStmt)
pStmt 
   = pAlts [
        p3 (\var eq expr -> SAssign var expr) pVar (pKW L_Assign) pExpr,
        p2 (\p expr -> SPrint expr) (pKW L_Print) pExpr,
        p5 (\_ c _ t e -> SCond c t e)
           (pKW L_If) pExpr (pKW L_Then) pStmts (pMaybeElse pStmts),
        p2 (\mnm args -> SMacro mnm args) pText pMacroArgs,
        p4 (\var eq run expr -> SRun var expr)
           pVar (pKW L_Assign) (pKW L_Run) pExpr,
        p2 (\ret expr -> SReturn expr) (pKW L_Return) pExpr,
        p3 (\s w expr -> SSkip expr) (pKW L_Skip) (pKW L_When) pExpr,
        p3 (\res w expr -> SResult res expr) pResult (pKW L_When) pExpr,
        p2 (\e res -> SExpect res) (pKW L_Expect) pResult,
        p2 (\ret expr -> SFFail expr) (pKW L_Framefail) pExpr
     ]
     where
        pStmts
           = pStar pStmt

pExpr
   = pExpr9
pExpr9
   = pApply (foldr1 (EOp OpOr)) (pOneOrMoreWithSep (pKW L_Or) pExpr8)
pExpr8
   = pApply (foldr1 (EOp OpAnd)) (pOneOrMoreWithSep (pKW L_And) pExpr7)

pExpr7
   = pAlts [ p3 (\e0 op e6 -> EOp op e0 e6) pExpr0 pOp pExpr7,
             pExpr0 ]
     where
        pOp = pAlts [pConstKW L_Eq OpEq,
                     pConstKW L_NEq OpNEq,
                     pConstKW L_Contains OpContains,
                     pConstKW L_Lacks OpLacks,
                     pConstKW L_Append OpAppend]

pExpr0
   = pAlts [
        pApply EVar pVar,
        pApply EString pString,
        pApply EBool pBool,
        p2 (\c expr -> EContents expr) (pKW L_Contents) (pInParens pExpr),
        p2 (\c expr -> EExists expr) (pKW L_Exists) (pInParens pExpr),
        p2 (\mnm args -> EMacro mnm args) pText pMacroArgs,
        p5 (\_ c _ t e -> ECond c t e)
           (pKW L_If) pExpr (pKW L_Then) pExpr (pMaybeElse pExpr),
        pConstKW L_Otherwise EOtherwise,
        p2 (\d var -> EDefined var) (pKW L_Defined) pVar,
        p2 (\ff expr -> EFFail expr) (pKW L_Framefail) pExpr,
        pInParens pExpr
     ]

----------------------------

pMacroArgs
   = pInParens (pZeroOrMoreWithSep (pKW L_Comma) pExpr)

pMaybeElse :: Parser a -> Parser (Maybe a)
pMaybeElse p
   = pAlts [ p3 (\_ e _ -> Just e) (pKW L_Else) p (pKW L_Fi),
             pConstKW L_Fi Nothing ]

pResult
   = pAlts [pConstKW L_Pass Pass,
            pConstKW L_Fail Fail,
            pConstKW L_Unknown Unknown
           ]

pBool
   = pSatMap f where f (LBool b) = Just b ; f _ = Nothing
pString
   = pSatMap f where f (LString str) = Just str ; f _ = Nothing
pVar
   = pSatMap f where f (LVar var) = Just var ; f _ = Nothing
pText
   = pSatMap f where f (LText txt) = Just txt; f _ = Nothing
pTextOrString
   = pAlts [pString, pText]
pInParens p
   = p3 (\l x r -> x) (pKW L_Open) p (pKW L_Close)
pInBraces p
   = p3 (\l x r -> x) (pKW L_LBrace) p (pKW L_RBrace)

pFormalVar
   = pSatMap f where f (LVar var) 
                        | take 1 var == "_" = Just var
                        | otherwise = Nothing
                     f _ = Nothing

---------------------------------------------------------------------
-- parser generics
data PResult a
   = PFail [Token] -- failure; [Token] is furthest failure
   | POk   [Token] -- succeed; 1st [Token] is furthest failure
                 a -- whatever
           [Token] -- unused
     deriving Show

type Parser a = [Token] -> PResult a

pEmpty :: a -> Parser a
pEmpty x ts = POk ts x ts

--pSat :: (Token -> Bool) -> Parser Token
--pSat p []     = PFail (getLineNo [])
--pSat p (t:ts) = if p t then POk t ts else PFail (getLineNo t)

pApply :: (a -> b) -> Parser a -> Parser b
pApply f p ts
   = case p ts of
        PFail bad      -> PFail bad
        POk   bad x uu -> POk   bad (f x) uu

pKW :: Lexeme -> Parser ()
pKW lexeme (t:ts) | lexeme == getLex t  = POk (t:ts) () ts
pKW lexeme ts                           = PFail ts

pConstKW :: Lexeme -> a -> Parser a
pConstKW lexeme x = pApply (const x) (pKW lexeme)


pSatMap :: (Lexeme -> Maybe a) -> Parser a
pSatMap fn (t:ts) 
   = case fn (getLex t) of Just x  -> POk (t:ts) x ts
                           Nothing -> PFail (t:ts)
pSatMap fn []
   = PFail []

p2 :: (a -> b -> c) 
      -> Parser a -> Parser b -> Parser c
p2 f a1 a2 ts1
   = case a1 ts1 of { PFail b1 -> PFail b1; POk ba1 x1 uu1 ->
     case a2 uu1 of { PFail b2 -> PFail b2; POk ba2 x2 uu2 ->
     POk (further ba1 ba2) (f x1 x2) uu2
     }}

p3 :: (a -> b -> c -> d) 
      -> Parser a -> Parser b -> Parser c -> Parser d
p3 f a1 a2 a3 ts1
   = case a1 ts1 of { PFail b1 -> PFail b1 ; POk ba1 x1 uu1 ->
     case a2 uu1 of { PFail b2 -> PFail b2 ; POk ba2 x2 uu2 ->
     case a3 uu2 of { PFail b3 -> PFail b3 ; POk ba3 x3 uu3 ->
     POk (further3 ba1 ba2 ba3) (f x1 x2 x3) uu3
     }}}

p4 :: (a -> b -> c -> d -> e) 
      -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
p4 f a1 a2 a3 a4 ts1
   = case a1 ts1 of { PFail b1 -> PFail b1 ; POk ba1 x1 uu1 ->
     case a2 uu1 of { PFail b2 -> PFail b2 ; POk ba2 x2 uu2 ->
     case a3 uu2 of { PFail b3 -> PFail b3 ; POk ba3 x3 uu3 ->
     case a4 uu3 of { PFail b4 -> PFail b4 ; POk ba4 x4 uu4 ->
     POk (further4 ba1 ba2 ba3 ba4) (f x1 x2 x3 x4) uu4
     }}}}

p5 :: (a -> b -> c -> d -> e -> f) 
      -> Parser a -> Parser b -> Parser c 
      -> Parser d -> Parser e -> Parser f
p5 f a1 a2 a3 a4 a5
   = p2 ( \ (r1,r2,r3) (r4,r5) -> f r1 r2 r3 r4 r5 )
        (p3 ( \x1 x2 x3 -> (x1,x2,x3) ) a1 a2 a3)
        (p2 ( \x4 x5    -> (x4,x5)    ) a4 a5)

pStar :: Parser a -> Parser [a]
pStar p ts
   = case p ts of
        PFail bad -> POk bad [] ts
        POk   bad x uu1 
           -> case pStar p uu1 of
                 POk bad2 xs uu2 -> POk (further bad bad2) (x:xs) uu2
                 PFail bad       -> panic "pStar failed"

pPlus :: Parser a -> Parser [a]
pPlus p = p2 (:) p (pStar p)

pOneOrMoreWithSep psep p
   = p2 (:) p (pStar (p2 (\x y -> y) psep p))
pZeroOrMoreWithSep psep p
   = pAlts [pOneOrMoreWithSep psep p, pEmpty [] ]


pAlts :: [Parser a] -> Parser a
pAlts ps ts 
   = loop ts ps
     where
        loop best_err_toks [] = PFail best_err_toks
        loop best_err_toks (p:ps)
           = case p ts of
                POk bad x uu  -> POk (further bad best_err_toks) x uu
                PFail other_err_toks 
                   -> loop (further best_err_toks other_err_toks) ps

further :: [Token] -> [Token] -> [Token]
further bads1 bads2
   = if getTokNo bads1 > getTokNo bads2 then bads1 else bads2
further3 bads1 bads2 bads3
   = further bads1 (further bads2 bads3)
further4 bads1 bads2 bads3 bads4
   = further (further bads4 bads1) (further bads2 bads3)

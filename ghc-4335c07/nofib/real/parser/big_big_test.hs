
--==========================================================--
--=== The parser.                                        ===--
--===                                          Parser.hs ===--
--==========================================================--

module Parser where

{- FIX THESE UP -}
--utLookupDef env k def
--   = head ( [ vv | (kk,vv) <- env, kk == k] ++ [def] )
panic = error
{- END FIXUPS -}

--paLiteral :: Parser Literal
paLiteral
   = pgAlts 
     [
        pgApply (LiteralInt.leStringToInt) (pgItem Lintlit),
        pgApply (LiteralChar.head)         (pgItem Lcharlit),
        pgApply LiteralString              (pgItem Lstringlit)
     ]

paExpr
   = pgAlts 
     [
        paCaseExpr, 
        paLetExpr, 
        paLamExpr,
        paIfExpr,
        paUnaryMinusExpr,
        hsDoExpr []
     ]

paUnaryMinusExpr
   = pgThen2
        (\minus (_, aexpr, _) -> 
             ExprApp (ExprApp (ExprVar "-") (ExprLiteral (LiteralInt 0))) aexpr)
        paMinus
        paAExpr

paCaseExpr
   = pgThen4
        (\casee expr off alts -> ExprCase expr alts)
        (pgItem Lcase)
        paExpr
        (pgItem Lof)
        (pgDeclList paAlt)

paAlt
   = pgAlts
     [
        pgThen4
           (\pat arrow expr wheres 
                -> MkExprCaseAlt pat (pa_MakeWhereExpr expr wheres))
           paPat
           (pgItem Larrow)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\pat agrdrhss wheres
                -> MkExprCaseAlt pat
                      (pa_MakeWhereExpr (ExprGuards agrdrhss) wheres))
           paPat
           (pgOneOrMore paGalt)
           (pgOptional paWhereClause)
     ]

paGalt
   = pgThen4
        (\bar guard arrow expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Larrow)
        paExpr

paLamExpr
   = pgThen4
        (\lam patterns arrow rhs -> ExprLam patterns rhs)
        (pgItem Lslash)
        (pgZeroOrMore paAPat)
        (pgItem Larrow)
        paExpr

paLetExpr
   = pgThen4
        (\lett decls inn rhs -> ExprLetrec decls rhs)
        (pgItem Llet)
        paValdefs
        (pgItem Lin)
        paExpr

paValdefs 
   = pgApply pa_MergeValdefs (pgDeclList paValdef)

pa_MergeValdefs 
   = id

paLhs
   = pgAlts
     [
        pgThen2 (\v ps -> LhsVar v ps) paVar (pgOneOrMore paPat),
        pgApply LhsPat paPat
     ]

paValdef
   = pgAlts
     [
        pgThen4
           (\(line, lhs) eq rhs wheres 
                -> MkValBind line lhs (pa_MakeWhereExpr rhs wheres))
           (pgGetLineNumber paLhs)
           (pgItem Lequals)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\(line, lhs) grdrhss wheres 
                -> MkValBind line lhs 
                      (pa_MakeWhereExpr (ExprGuards grdrhss) wheres))
           (pgGetLineNumber paLhs)
           (pgOneOrMore paGrhs)
           (pgOptional paWhereClause)
     ]

pa_MakeWhereExpr expr Nothing 
   = expr
pa_MakeWhereExpr expr (Just whereClauses) 
   = ExprWhere expr whereClauses

paWhereClause
   = pgThen2 (\x y -> y) (pgItem Lwhere) paValdefs
paGrhs
   = pgThen4
        (\bar guard equals expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Lequals)
        paExpr
        

paAPat
   = pgAlts
     [
        pgApply PatVar paVar,
        pgApply (\id -> PatCon id []) paCon,
        pgApply (const PatWild) (pgItem Lunder),
        pgApply PatTuple
                (pgThen3 (\l es r -> es)
                         (pgItem Llparen) 
                         (pgTwoOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrparen)),
        pgApply PatList
                (pgThen3 (\l es r -> es)
                         (pgItem Llbrack) 
                         (pgZeroOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrbrack)),
        pgThen3 (\l p r -> p)
                (pgItem Llparen)
                paPat
                (pgItem Lrparen)
     ]

paPat
   = pgAlts
     [
        pgThen2 (\c ps -> PatCon c ps)
                paCon
                (pgOneOrMore paAPat),
        pgThen3 (\ap c pa -> PatCon c [ap,pa])
                paAPat
                paConop
                paPat,
        paAPat
     ]


paIfExpr
 = pgThen4
      (\iff c thenn (t,f) -> ExprIf c t f)
      (pgItem Lif)
      paExpr
      (pgItem Lthen)
      (pgThen3
         (\t elsee f -> (t,f))
         paExpr
         (pgItem Lelse)
         paExpr
      )

paAExpr
 = pgApply (\x -> (False, x, []))
   (pgAlts 
    [
       pgApply ExprVar paVar,
       pgApply ExprCon paCon,
       pgApply ExprLiteral paLiteral,
       pgApply ExprList paListExpr,
       pgApply ExprTuple paTupleExpr,
       pgThen3 (\l e r -> e) (pgItem Llparen) paExpr (pgItem Lrparen)
    ]
   )

paListExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llbrack) 
             (pgZeroOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrbrack)

paTupleExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llparen) 
             (pgTwoOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrparen)

paVar = pgItem Lvar
paCon = pgItem Lcon
paVarop = pgItem Lvarop
paConop = pgItem Lconop
paMinus = pgItem Lminus

paOp
 = pgAlts [
            pgApply (\x -> (True, ExprVar x, x)) paVarop,
            pgApply (\x -> (True, ExprCon x, x)) paConop,
            pgApply (\x -> (True, ExprVar x, x)) paMinus
          ]

paDataDecl
   = pgThen2
        (\dataa useful -> useful)
        (pgItem Ldata)
        paDataDecl_main

paDataDecl_main
   = pgThen4
        (\name params eq drhs -> MkDataDecl name (params, drhs))
        paCon
        (pgZeroOrMore paVar)
        (pgItem Lequals)
        (pgOneOrMoreWithSep paConstrs (pgItem Lbar))

paConstrs
   = pgThen2
        (\con texprs -> (con, texprs))
        paCon
        (pgZeroOrMore paAType)

paType 
   = pgAlts
     [
        pgThen3 
           (\atype arrow typee -> TypeArr atype typee)
           paAType
           (pgItem Larrow)
           paType,
        pgThen2
           TypeCon
           paCon
           (pgOneOrMore paAType),
        paAType
     ]

paAType
   = pgAlts
     [
        pgApply TypeVar paVar,
        pgApply (\tycon -> TypeCon tycon []) paCon,
        pgThen3
           (\l t r -> t)
           (pgItem Llparen)
           paType
           (pgItem Lrparen),
        pgThen3
           (\l t r -> TypeList t)
           (pgItem Llbrack)
           paType
           (pgItem Lrbrack),
        pgThen3
           (\l t r -> TypeTuple t)
           (pgItem Llparen)
           (pgTwoOrMoreWithSep paType (pgItem Lcomma))
           (pgItem Lrparen)
     ]

paInfixDecl env toks
  = let dump (ExprVar v) = v
        dump (ExprCon c) = c
    in
    pa_UpdateFixityEnv 
       (pgThen3
          (\assoc prio name -> MkFixDecl name (assoc, prio))
          paInfixWord
          (pgApply leStringToInt (pgItem Lintlit)) 
          (pgApply (\(_, op, _) -> dump op) paOp)
          env 
          toks 
       )

paInfixWord
  = pgAlts
    [
       pgApply (const InfixL) (pgItem Linfixl),
       pgApply (const InfixR) (pgItem Linfixr),
       pgApply (const InfixN) (pgItem Linfix)
    ]

pa_UpdateFixityEnv (PFail tok) 
   = PFail tok

pa_UpdateFixityEnv (POk env toks (MkFixDecl name assoc_prio))
   = let 
         new_env = (name, assoc_prio) : env
     in
         POk new_env toks (MkFixDecl name assoc_prio)

paTopDecl
   = pgAlts
     [
        pgApply MkTopF paInfixDecl,
        pgApply MkTopD paDataDecl,
        pgApply MkTopV paValdef
     ]

paModule
   = pgThen4
        (\modyule name wheree topdecls -> MkModule name topdecls)
        (pgItem Lmodule)
        paCon
        (pgItem Lwhere)
        (pgDeclList paTopDecl)
   
parser_test toks
   = let parser_to_test
            = --paPat
              --paExpr
              --paValdef
              --pgZeroOrMore paInfixDecl
              --paDataDecl
              --paType
              paModule
              --pgTwoOrMoreWithSep (pgItem Lsemi) (pgItem Lcomma)
              
     in
         parser_to_test hsPrecTable toks

--==============================================--
--=== The Operator-Precedence parser (yuck!) ===--
--==============================================--

--
--==========================================================--
--
hsAExprOrOp 
 = pgAlts [paAExpr, paOp]

--hsDoExpr :: [PEntry] -> Parser Expr
-- [PaEntry] is a stack of operators and atomic expressions
-- hsDoExpr uses a parser (hsAexpOrOp :: Parsr PaEntry) for atomic
-- expressions or operators

hsDoExpr stack env toks = 
  let
     (validIn, restIn, parseIn, err)
        = case hsAExprOrOp env toks of
             POk env1 toks1 item1
                -> (True, toks1, item1, panic "hsDoExpr(1)")
             PFail err
                -> (False, panic "hsDoExpr(2)", panic "hsDoExpr(3)", err)
     (opIn, valueIn, nameIn)
        = parseIn
     (assocIn, priorIn)
        = utLookupDef env nameIn (InfixL, 9)
     shift
        = hsDoExpr (parseIn:stack) env restIn
  in 
     case stack of
        s1:s2:s3:ss
           | validIn && opS2 && opIn && priorS2 > priorIn
              -> reduce
           | validIn && opS2 && opIn && priorS2 == priorIn
              -> if assocS2 == InfixL && 
                    assocIn == InfixL 
                 then reduce
	         else 
                 if assocS2 == InfixR && 
                    assocIn == InfixR 
                 then shift
	         else PFail (head toks) -- Because of ambiguousness 
           | not validIn && opS2
              -> reduce
             where
               (opS1, valueS1, nameS1) = s1
               (opS2, valueS2, nameS2) = s2
               (opS3, valueS3, nameS3) = s3
               (assocS2, priorS2) = utLookupDef env nameS2 (InfixL, 9)
               reduce = hsDoExpr ((False, ExprApp (ExprApp valueS2 valueS3) 
                                                  valueS1, [])
                                  : ss) env toks
        s1:s2:ss
           | validIn && (opS1 || opS2) -> shift
           | otherwise -> reduce
             where
                (opS1, valueS1, nameS1) = s1
                (opS2, valueS2, nameS2) = s2
                reduce = hsDoExpr ((False, ExprApp valueS2 valueS1, []) : ss) 
                                  env toks
        (s1:[])
           | validIn -> shift
           | otherwise -> POk env toks valueS1
             where
                (opS1, valueS1, nameS1) = s1
        []
           | validIn -> shift
           | otherwise -> PFail err

--==========================================================--
--=== end                                      Parser.hs ===--
--==========================================================--


hsPrecTable = [
  ("-",		(InfixL, 6)),
  ("+",		(InfixL, 6)),
  ("*",		(InfixL, 7)),
  ("div",	(InfixN, 7)),
  ("mod", 	(InfixN, 7)),

  ("<",		(InfixN, 4)),
  ("<=",	(InfixN, 4)),
  ("==",	(InfixN, 4)),
  ("/=",	(InfixN, 4)),
  (">=",	(InfixN, 4)),
  (">",		(InfixN, 4)),

  ("C:",        (InfixR, 5)),
  ("++",        (InfixR, 5)),
  ("\\",        (InfixN, 5)),
  ("!!",        (InfixL, 9)),
  (".",         (InfixR, 9)),
  ("^",         (InfixR, 8)),
  ("elem",      (InfixN, 4)),
  ("notElem",   (InfixN, 4)),

  ("||",	(InfixR, 2)),
  ("&&",	(InfixR, 3))]

{- FIX THESE UP -}
--utLookupDef env k def
--   = head ( [ vv | (kk,vv) <- env, kk == k] ++ [def] )
panic = error
{- END FIXUPS -}

--paLiteral :: Parser Literal
paLiteral
   = pgAlts 
     [
        pgApply (LiteralInt.leStringToInt) (pgItem Lintlit),
        pgApply (LiteralChar.head)         (pgItem Lcharlit),
        pgApply LiteralString              (pgItem Lstringlit)
     ]

paExpr
   = pgAlts 
     [
        paCaseExpr, 
        paLetExpr, 
        paLamExpr,
        paIfExpr,
        paUnaryMinusExpr,
        hsDoExpr []
     ]

paUnaryMinusExpr
   = pgThen2
        (\minus (_, aexpr, _) -> 
             ExprApp (ExprApp (ExprVar "-") (ExprLiteral (LiteralInt 0))) aexpr)
        paMinus
        paAExpr

paCaseExpr
   = pgThen4
        (\casee expr off alts -> ExprCase expr alts)
        (pgItem Lcase)
        paExpr
        (pgItem Lof)
        (pgDeclList paAlt)

paAlt
   = pgAlts
     [
        pgThen4
           (\pat arrow expr wheres 
                -> MkExprCaseAlt pat (pa_MakeWhereExpr expr wheres))
           paPat
           (pgItem Larrow)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\pat agrdrhss wheres
                -> MkExprCaseAlt pat
                      (pa_MakeWhereExpr (ExprGuards agrdrhss) wheres))
           paPat
           (pgOneOrMore paGalt)
           (pgOptional paWhereClause)
     ]

paGalt
   = pgThen4
        (\bar guard arrow expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Larrow)
        paExpr

paLamExpr
   = pgThen4
        (\lam patterns arrow rhs -> ExprLam patterns rhs)
        (pgItem Lslash)
        (pgZeroOrMore paAPat)
        (pgItem Larrow)
        paExpr

paLetExpr
   = pgThen4
        (\lett decls inn rhs -> ExprLetrec decls rhs)
        (pgItem Llet)
        paValdefs
        (pgItem Lin)
        paExpr

paValdefs 
   = pgApply pa_MergeValdefs (pgDeclList paValdef)

pa_MergeValdefs 
   = id

paLhs
   = pgAlts
     [
        pgThen2 (\v ps -> LhsVar v ps) paVar (pgOneOrMore paPat),
        pgApply LhsPat paPat
     ]

paValdef
   = pgAlts
     [
        pgThen4
           (\(line, lhs) eq rhs wheres 
                -> MkValBind line lhs (pa_MakeWhereExpr rhs wheres))
           (pgGetLineNumber paLhs)
           (pgItem Lequals)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\(line, lhs) grdrhss wheres 
                -> MkValBind line lhs 
                      (pa_MakeWhereExpr (ExprGuards grdrhss) wheres))
           (pgGetLineNumber paLhs)
           (pgOneOrMore paGrhs)
           (pgOptional paWhereClause)
     ]

pa_MakeWhereExpr expr Nothing 
   = expr
pa_MakeWhereExpr expr (Just whereClauses) 
   = ExprWhere expr whereClauses

paWhereClause
   = pgThen2 (\x y -> y) (pgItem Lwhere) paValdefs
paGrhs
   = pgThen4
        (\bar guard equals expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Lequals)
        paExpr
        

paAPat
   = pgAlts
     [
        pgApply PatVar paVar,
        pgApply (\id -> PatCon id []) paCon,
        pgApply (const PatWild) (pgItem Lunder),
        pgApply PatTuple
                (pgThen3 (\l es r -> es)
                         (pgItem Llparen) 
                         (pgTwoOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrparen)),
        pgApply PatList
                (pgThen3 (\l es r -> es)
                         (pgItem Llbrack) 
                         (pgZeroOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrbrack)),
        pgThen3 (\l p r -> p)
                (pgItem Llparen)
                paPat
                (pgItem Lrparen)
     ]

paPat
   = pgAlts
     [
        pgThen2 (\c ps -> PatCon c ps)
                paCon
                (pgOneOrMore paAPat),
        pgThen3 (\ap c pa -> PatCon c [ap,pa])
                paAPat
                paConop
                paPat,
        paAPat
     ]


paIfExpr
 = pgThen4
      (\iff c thenn (t,f) -> ExprIf c t f)
      (pgItem Lif)
      paExpr
      (pgItem Lthen)
      (pgThen3
         (\t elsee f -> (t,f))
         paExpr
         (pgItem Lelse)
         paExpr
      )

paAExpr
 = pgApply (\x -> (False, x, []))
   (pgAlts 
    [
       pgApply ExprVar paVar,
       pgApply ExprCon paCon,
       pgApply ExprLiteral paLiteral,
       pgApply ExprList paListExpr,
       pgApply ExprTuple paTupleExpr,
       pgThen3 (\l e r -> e) (pgItem Llparen) paExpr (pgItem Lrparen)
    ]
   )

paListExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llbrack) 
             (pgZeroOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrbrack)

paTupleExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llparen) 
             (pgTwoOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrparen)

paVar = pgItem Lvar
paCon = pgItem Lcon
paVarop = pgItem Lvarop
paConop = pgItem Lconop
paMinus = pgItem Lminus

paOp
 = pgAlts [
            pgApply (\x -> (True, ExprVar x, x)) paVarop,
            pgApply (\x -> (True, ExprCon x, x)) paConop,
            pgApply (\x -> (True, ExprVar x, x)) paMinus
          ]

paDataDecl
   = pgThen2
        (\dataa useful -> useful)
        (pgItem Ldata)
        paDataDecl_main

paDataDecl_main
   = pgThen4
        (\name params eq drhs -> MkDataDecl name (params, drhs))
        paCon
        (pgZeroOrMore paVar)
        (pgItem Lequals)
        (pgOneOrMoreWithSep paConstrs (pgItem Lbar))

paConstrs
   = pgThen2
        (\con texprs -> (con, texprs))
        paCon
        (pgZeroOrMore paAType)

paType 
   = pgAlts
     [
        pgThen3 
           (\atype arrow typee -> TypeArr atype typee)
           paAType
           (pgItem Larrow)
           paType,
        pgThen2
           TypeCon
           paCon
           (pgOneOrMore paAType),
        paAType
     ]

paAType
   = pgAlts
     [
        pgApply TypeVar paVar,
        pgApply (\tycon -> TypeCon tycon []) paCon,
        pgThen3
           (\l t r -> t)
           (pgItem Llparen)
           paType
           (pgItem Lrparen),
        pgThen3
           (\l t r -> TypeList t)
           (pgItem Llbrack)
           paType
           (pgItem Lrbrack),
        pgThen3
           (\l t r -> TypeTuple t)
           (pgItem Llparen)
           (pgTwoOrMoreWithSep paType (pgItem Lcomma))
           (pgItem Lrparen)
     ]

paInfixDecl env toks
  = let dump (ExprVar v) = v
        dump (ExprCon c) = c
    in
    pa_UpdateFixityEnv 
       (pgThen3
          (\assoc prio name -> MkFixDecl name (assoc, prio))
          paInfixWord
          (pgApply leStringToInt (pgItem Lintlit)) 
          (pgApply (\(_, op, _) -> dump op) paOp)
          env 
          toks 
       )

paInfixWord
  = pgAlts
    [
       pgApply (const InfixL) (pgItem Linfixl),
       pgApply (const InfixR) (pgItem Linfixr),
       pgApply (const InfixN) (pgItem Linfix)
    ]

pa_UpdateFixityEnv (PFail tok) 
   = PFail tok

pa_UpdateFixityEnv (POk env toks (MkFixDecl name assoc_prio))
   = let 
         new_env = (name, assoc_prio) : env
     in
         POk new_env toks (MkFixDecl name assoc_prio)

paTopDecl
   = pgAlts
     [
        pgApply MkTopF paInfixDecl,
        pgApply MkTopD paDataDecl,
        pgApply MkTopV paValdef
     ]

paModule
   = pgThen4
        (\modyule name wheree topdecls -> MkModule name topdecls)
        (pgItem Lmodule)
        paCon
        (pgItem Lwhere)
        (pgDeclList paTopDecl)
   
parser_test toks
   = let parser_to_test
            = --paPat
              --paExpr
              --paValdef
              --pgZeroOrMore paInfixDecl
              --paDataDecl
              --paType
              paModule
              --pgTwoOrMoreWithSep (pgItem Lsemi) (pgItem Lcomma)
              
     in
         parser_to_test hsPrecTable toks

--==============================================--
--=== The Operator-Precedence parser (yuck!) ===--
--==============================================--

--
--==========================================================--
--
hsAExprOrOp 
 = pgAlts [paAExpr, paOp]

--hsDoExpr :: [PEntry] -> Parser Expr
-- [PaEntry] is a stack of operators and atomic expressions
-- hsDoExpr uses a parser (hsAexpOrOp :: Parsr PaEntry) for atomic
-- expressions or operators

hsDoExpr stack env toks = 
  let
     (validIn, restIn, parseIn, err)
        = case hsAExprOrOp env toks of
             POk env1 toks1 item1
                -> (True, toks1, item1, panic "hsDoExpr(1)")
             PFail err
                -> (False, panic "hsDoExpr(2)", panic "hsDoExpr(3)", err)
     (opIn, valueIn, nameIn)
        = parseIn
     (assocIn, priorIn)
        = utLookupDef env nameIn (InfixL, 9)
     shift
        = hsDoExpr (parseIn:stack) env restIn
  in 
     case stack of
        s1:s2:s3:ss
           | validIn && opS2 && opIn && priorS2 > priorIn
              -> reduce
           | validIn && opS2 && opIn && priorS2 == priorIn
              -> if assocS2 == InfixL && 
                    assocIn == InfixL 
                 then reduce
	         else 
                 if assocS2 == InfixR && 
                    assocIn == InfixR 
                 then shift
	         else PFail (head toks) -- Because of ambiguousness 
           | not validIn && opS2
              -> reduce
             where
               (opS1, valueS1, nameS1) = s1
               (opS2, valueS2, nameS2) = s2
               (opS3, valueS3, nameS3) = s3
               (assocS2, priorS2) = utLookupDef env nameS2 (InfixL, 9)
               reduce = hsDoExpr ((False, ExprApp (ExprApp valueS2 valueS3) 
                                                  valueS1, [])
                                  : ss) env toks
        s1:s2:ss
           | validIn && (opS1 || opS2) -> shift
           | otherwise -> reduce
             where
                (opS1, valueS1, nameS1) = s1
                (opS2, valueS2, nameS2) = s2
                reduce = hsDoExpr ((False, ExprApp valueS2 valueS1, []) : ss) 
                                  env toks
        (s1:[])
           | validIn -> shift
           | otherwise -> POk env toks valueS1
             where
                (opS1, valueS1, nameS1) = s1
        []
           | validIn -> shift
           | otherwise -> PFail err

--==========================================================--
--=== end                                      Parser.hs ===--
--==========================================================--


hsPrecTable = [
  ("-",		(InfixL, 6)),
  ("+",		(InfixL, 6)),
  ("*",		(InfixL, 7)),
  ("div",	(InfixN, 7)),
  ("mod", 	(InfixN, 7)),

  ("<",		(InfixN, 4)),
  ("<=",	(InfixN, 4)),
  ("==",	(InfixN, 4)),
  ("/=",	(InfixN, 4)),
  (">=",	(InfixN, 4)),
  (">",		(InfixN, 4)),

  ("C:",        (InfixR, 5)),
  ("++",        (InfixR, 5)),
  ("\\",        (InfixN, 5)),
  ("!!",        (InfixL, 9)),
  (".",         (InfixR, 9)),
  ("^",         (InfixR, 8)),
  ("elem",      (InfixN, 4)),
  ("notElem",   (InfixN, 4)),

  ("||",	(InfixR, 2)),
  ("&&",	(InfixR, 3))]

{- FIX THESE UP -}
--utLookupDef env k def
--   = head ( [ vv | (kk,vv) <- env, kk == k] ++ [def] )
panic = error
{- END FIXUPS -}

--paLiteral :: Parser Literal
paLiteral
   = pgAlts 
     [
        pgApply (LiteralInt.leStringToInt) (pgItem Lintlit),
        pgApply (LiteralChar.head)         (pgItem Lcharlit),
        pgApply LiteralString              (pgItem Lstringlit)
     ]

paExpr
   = pgAlts 
     [
        paCaseExpr, 
        paLetExpr, 
        paLamExpr,
        paIfExpr,
        paUnaryMinusExpr,
        hsDoExpr []
     ]

paUnaryMinusExpr
   = pgThen2
        (\minus (_, aexpr, _) -> 
             ExprApp (ExprApp (ExprVar "-") (ExprLiteral (LiteralInt 0))) aexpr)
        paMinus
        paAExpr

paCaseExpr
   = pgThen4
        (\casee expr off alts -> ExprCase expr alts)
        (pgItem Lcase)
        paExpr
        (pgItem Lof)
        (pgDeclList paAlt)

paAlt
   = pgAlts
     [
        pgThen4
           (\pat arrow expr wheres 
                -> MkExprCaseAlt pat (pa_MakeWhereExpr expr wheres))
           paPat
           (pgItem Larrow)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\pat agrdrhss wheres
                -> MkExprCaseAlt pat
                      (pa_MakeWhereExpr (ExprGuards agrdrhss) wheres))
           paPat
           (pgOneOrMore paGalt)
           (pgOptional paWhereClause)
     ]

paGalt
   = pgThen4
        (\bar guard arrow expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Larrow)
        paExpr

paLamExpr
   = pgThen4
        (\lam patterns arrow rhs -> ExprLam patterns rhs)
        (pgItem Lslash)
        (pgZeroOrMore paAPat)
        (pgItem Larrow)
        paExpr

paLetExpr
   = pgThen4
        (\lett decls inn rhs -> ExprLetrec decls rhs)
        (pgItem Llet)
        paValdefs
        (pgItem Lin)
        paExpr

paValdefs 
   = pgApply pa_MergeValdefs (pgDeclList paValdef)

pa_MergeValdefs 
   = id

paLhs
   = pgAlts
     [
        pgThen2 (\v ps -> LhsVar v ps) paVar (pgOneOrMore paPat),
        pgApply LhsPat paPat
     ]

paValdef
   = pgAlts
     [
        pgThen4
           (\(line, lhs) eq rhs wheres 
                -> MkValBind line lhs (pa_MakeWhereExpr rhs wheres))
           (pgGetLineNumber paLhs)
           (pgItem Lequals)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\(line, lhs) grdrhss wheres 
                -> MkValBind line lhs 
                      (pa_MakeWhereExpr (ExprGuards grdrhss) wheres))
           (pgGetLineNumber paLhs)
           (pgOneOrMore paGrhs)
           (pgOptional paWhereClause)
     ]

pa_MakeWhereExpr expr Nothing 
   = expr
pa_MakeWhereExpr expr (Just whereClauses) 
   = ExprWhere expr whereClauses

paWhereClause
   = pgThen2 (\x y -> y) (pgItem Lwhere) paValdefs
paGrhs
   = pgThen4
        (\bar guard equals expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Lequals)
        paExpr
        

paAPat
   = pgAlts
     [
        pgApply PatVar paVar,
        pgApply (\id -> PatCon id []) paCon,
        pgApply (const PatWild) (pgItem Lunder),
        pgApply PatTuple
                (pgThen3 (\l es r -> es)
                         (pgItem Llparen) 
                         (pgTwoOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrparen)),
        pgApply PatList
                (pgThen3 (\l es r -> es)
                         (pgItem Llbrack) 
                         (pgZeroOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrbrack)),
        pgThen3 (\l p r -> p)
                (pgItem Llparen)
                paPat
                (pgItem Lrparen)
     ]

paPat
   = pgAlts
     [
        pgThen2 (\c ps -> PatCon c ps)
                paCon
                (pgOneOrMore paAPat),
        pgThen3 (\ap c pa -> PatCon c [ap,pa])
                paAPat
                paConop
                paPat,
        paAPat
     ]


paIfExpr
 = pgThen4
      (\iff c thenn (t,f) -> ExprIf c t f)
      (pgItem Lif)
      paExpr
      (pgItem Lthen)
      (pgThen3
         (\t elsee f -> (t,f))
         paExpr
         (pgItem Lelse)
         paExpr
      )

paAExpr
 = pgApply (\x -> (False, x, []))
   (pgAlts 
    [
       pgApply ExprVar paVar,
       pgApply ExprCon paCon,
       pgApply ExprLiteral paLiteral,
       pgApply ExprList paListExpr,
       pgApply ExprTuple paTupleExpr,
       pgThen3 (\l e r -> e) (pgItem Llparen) paExpr (pgItem Lrparen)
    ]
   )

paListExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llbrack) 
             (pgZeroOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrbrack)

paTupleExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llparen) 
             (pgTwoOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrparen)

paVar = pgItem Lvar
paCon = pgItem Lcon
paVarop = pgItem Lvarop
paConop = pgItem Lconop
paMinus = pgItem Lminus

paOp
 = pgAlts [
            pgApply (\x -> (True, ExprVar x, x)) paVarop,
            pgApply (\x -> (True, ExprCon x, x)) paConop,
            pgApply (\x -> (True, ExprVar x, x)) paMinus
          ]

paDataDecl
   = pgThen2
        (\dataa useful -> useful)
        (pgItem Ldata)
        paDataDecl_main

paDataDecl_main
   = pgThen4
        (\name params eq drhs -> MkDataDecl name (params, drhs))
        paCon
        (pgZeroOrMore paVar)
        (pgItem Lequals)
        (pgOneOrMoreWithSep paConstrs (pgItem Lbar))

paConstrs
   = pgThen2
        (\con texprs -> (con, texprs))
        paCon
        (pgZeroOrMore paAType)

paType 
   = pgAlts
     [
        pgThen3 
           (\atype arrow typee -> TypeArr atype typee)
           paAType
           (pgItem Larrow)
           paType,
        pgThen2
           TypeCon
           paCon
           (pgOneOrMore paAType),
        paAType
     ]

paAType
   = pgAlts
     [
        pgApply TypeVar paVar,
        pgApply (\tycon -> TypeCon tycon []) paCon,
        pgThen3
           (\l t r -> t)
           (pgItem Llparen)
           paType
           (pgItem Lrparen),
        pgThen3
           (\l t r -> TypeList t)
           (pgItem Llbrack)
           paType
           (pgItem Lrbrack),
        pgThen3
           (\l t r -> TypeTuple t)
           (pgItem Llparen)
           (pgTwoOrMoreWithSep paType (pgItem Lcomma))
           (pgItem Lrparen)
     ]

paInfixDecl env toks
  = let dump (ExprVar v) = v
        dump (ExprCon c) = c
    in
    pa_UpdateFixityEnv 
       (pgThen3
          (\assoc prio name -> MkFixDecl name (assoc, prio))
          paInfixWord
          (pgApply leStringToInt (pgItem Lintlit)) 
          (pgApply (\(_, op, _) -> dump op) paOp)
          env 
          toks 
       )

paInfixWord
  = pgAlts
    [
       pgApply (const InfixL) (pgItem Linfixl),
       pgApply (const InfixR) (pgItem Linfixr),
       pgApply (const InfixN) (pgItem Linfix)
    ]

pa_UpdateFixityEnv (PFail tok) 
   = PFail tok

pa_UpdateFixityEnv (POk env toks (MkFixDecl name assoc_prio))
   = let 
         new_env = (name, assoc_prio) : env
     in
         POk new_env toks (MkFixDecl name assoc_prio)

paTopDecl
   = pgAlts
     [
        pgApply MkTopF paInfixDecl,
        pgApply MkTopD paDataDecl,
        pgApply MkTopV paValdef
     ]

paModule
   = pgThen4
        (\modyule name wheree topdecls -> MkModule name topdecls)
        (pgItem Lmodule)
        paCon
        (pgItem Lwhere)
        (pgDeclList paTopDecl)
   
parser_test toks
   = let parser_to_test
            = --paPat
              --paExpr
              --paValdef
              --pgZeroOrMore paInfixDecl
              --paDataDecl
              --paType
              paModule
              --pgTwoOrMoreWithSep (pgItem Lsemi) (pgItem Lcomma)
              
     in
         parser_to_test hsPrecTable toks

--==============================================--
--=== The Operator-Precedence parser (yuck!) ===--
--==============================================--

--
--==========================================================--
--
hsAExprOrOp 
 = pgAlts [paAExpr, paOp]

--hsDoExpr :: [PEntry] -> Parser Expr
-- [PaEntry] is a stack of operators and atomic expressions
-- hsDoExpr uses a parser (hsAexpOrOp :: Parsr PaEntry) for atomic
-- expressions or operators

hsDoExpr stack env toks = 
  let
     (validIn, restIn, parseIn, err)
        = case hsAExprOrOp env toks of
             POk env1 toks1 item1
                -> (True, toks1, item1, panic "hsDoExpr(1)")
             PFail err
                -> (False, panic "hsDoExpr(2)", panic "hsDoExpr(3)", err)
     (opIn, valueIn, nameIn)
        = parseIn
     (assocIn, priorIn)
        = utLookupDef env nameIn (InfixL, 9)
     shift
        = hsDoExpr (parseIn:stack) env restIn
  in 
     case stack of
        s1:s2:s3:ss
           | validIn && opS2 && opIn && priorS2 > priorIn
              -> reduce
           | validIn && opS2 && opIn && priorS2 == priorIn
              -> if assocS2 == InfixL && 
                    assocIn == InfixL 
                 then reduce
	         else 
                 if assocS2 == InfixR && 
                    assocIn == InfixR 
                 then shift
	         else PFail (head toks) -- Because of ambiguousness 
           | not validIn && opS2
              -> reduce
             where
               (opS1, valueS1, nameS1) = s1
               (opS2, valueS2, nameS2) = s2
               (opS3, valueS3, nameS3) = s3
               (assocS2, priorS2) = utLookupDef env nameS2 (InfixL, 9)
               reduce = hsDoExpr ((False, ExprApp (ExprApp valueS2 valueS3) 
                                                  valueS1, [])
                                  : ss) env toks
        s1:s2:ss
           | validIn && (opS1 || opS2) -> shift
           | otherwise -> reduce
             where
                (opS1, valueS1, nameS1) = s1
                (opS2, valueS2, nameS2) = s2
                reduce = hsDoExpr ((False, ExprApp valueS2 valueS1, []) : ss) 
                                  env toks
        (s1:[])
           | validIn -> shift
           | otherwise -> POk env toks valueS1
             where
                (opS1, valueS1, nameS1) = s1
        []
           | validIn -> shift
           | otherwise -> PFail err

--==========================================================--
--=== end                                      Parser.hs ===--
--==========================================================--


hsPrecTable = [
  ("-",		(InfixL, 6)),
  ("+",		(InfixL, 6)),
  ("*",		(InfixL, 7)),
  ("div",	(InfixN, 7)),
  ("mod", 	(InfixN, 7)),

  ("<",		(InfixN, 4)),
  ("<=",	(InfixN, 4)),
  ("==",	(InfixN, 4)),
  ("/=",	(InfixN, 4)),
  (">=",	(InfixN, 4)),
  (">",		(InfixN, 4)),

  ("C:",        (InfixR, 5)),
  ("++",        (InfixR, 5)),
  ("\\",        (InfixN, 5)),
  ("!!",        (InfixL, 9)),
  (".",         (InfixR, 9)),
  ("^",         (InfixR, 8)),
  ("elem",      (InfixN, 4)),
  ("notElem",   (InfixN, 4)),

  ("||",	(InfixR, 2)),
  ("&&",	(InfixR, 3))]

{- FIX THESE UP -}
--utLookupDef env k def
--   = head ( [ vv | (kk,vv) <- env, kk == k] ++ [def] )
panic = error
{- END FIXUPS -}

--paLiteral :: Parser Literal
paLiteral
   = pgAlts 
     [
        pgApply (LiteralInt.leStringToInt) (pgItem Lintlit),
        pgApply (LiteralChar.head)         (pgItem Lcharlit),
        pgApply LiteralString              (pgItem Lstringlit)
     ]

paExpr
   = pgAlts 
     [
        paCaseExpr, 
        paLetExpr, 
        paLamExpr,
        paIfExpr,
        paUnaryMinusExpr,
        hsDoExpr []
     ]

paUnaryMinusExpr
   = pgThen2
        (\minus (_, aexpr, _) -> 
             ExprApp (ExprApp (ExprVar "-") (ExprLiteral (LiteralInt 0))) aexpr)
        paMinus
        paAExpr

paCaseExpr
   = pgThen4
        (\casee expr off alts -> ExprCase expr alts)
        (pgItem Lcase)
        paExpr
        (pgItem Lof)
        (pgDeclList paAlt)

paAlt
   = pgAlts
     [
        pgThen4
           (\pat arrow expr wheres 
                -> MkExprCaseAlt pat (pa_MakeWhereExpr expr wheres))
           paPat
           (pgItem Larrow)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\pat agrdrhss wheres
                -> MkExprCaseAlt pat
                      (pa_MakeWhereExpr (ExprGuards agrdrhss) wheres))
           paPat
           (pgOneOrMore paGalt)
           (pgOptional paWhereClause)
     ]

paGalt
   = pgThen4
        (\bar guard arrow expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Larrow)
        paExpr

paLamExpr
   = pgThen4
        (\lam patterns arrow rhs -> ExprLam patterns rhs)
        (pgItem Lslash)
        (pgZeroOrMore paAPat)
        (pgItem Larrow)
        paExpr

paLetExpr
   = pgThen4
        (\lett decls inn rhs -> ExprLetrec decls rhs)
        (pgItem Llet)
        paValdefs
        (pgItem Lin)
        paExpr

paValdefs 
   = pgApply pa_MergeValdefs (pgDeclList paValdef)

pa_MergeValdefs 
   = id

paLhs
   = pgAlts
     [
        pgThen2 (\v ps -> LhsVar v ps) paVar (pgOneOrMore paPat),
        pgApply LhsPat paPat
     ]

paValdef
   = pgAlts
     [
        pgThen4
           (\(line, lhs) eq rhs wheres 
                -> MkValBind line lhs (pa_MakeWhereExpr rhs wheres))
           (pgGetLineNumber paLhs)
           (pgItem Lequals)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\(line, lhs) grdrhss wheres 
                -> MkValBind line lhs 
                      (pa_MakeWhereExpr (ExprGuards grdrhss) wheres))
           (pgGetLineNumber paLhs)
           (pgOneOrMore paGrhs)
           (pgOptional paWhereClause)
     ]

pa_MakeWhereExpr expr Nothing 
   = expr
pa_MakeWhereExpr expr (Just whereClauses) 
   = ExprWhere expr whereClauses

paWhereClause
   = pgThen2 (\x y -> y) (pgItem Lwhere) paValdefs
paGrhs
   = pgThen4
        (\bar guard equals expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Lequals)
        paExpr
        

paAPat
   = pgAlts
     [
        pgApply PatVar paVar,
        pgApply (\id -> PatCon id []) paCon,
        pgApply (const PatWild) (pgItem Lunder),
        pgApply PatTuple
                (pgThen3 (\l es r -> es)
                         (pgItem Llparen) 
                         (pgTwoOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrparen)),
        pgApply PatList
                (pgThen3 (\l es r -> es)
                         (pgItem Llbrack) 
                         (pgZeroOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrbrack)),
        pgThen3 (\l p r -> p)
                (pgItem Llparen)
                paPat
                (pgItem Lrparen)
     ]

paPat
   = pgAlts
     [
        pgThen2 (\c ps -> PatCon c ps)
                paCon
                (pgOneOrMore paAPat),
        pgThen3 (\ap c pa -> PatCon c [ap,pa])
                paAPat
                paConop
                paPat,
        paAPat
     ]


paIfExpr
 = pgThen4
      (\iff c thenn (t,f) -> ExprIf c t f)
      (pgItem Lif)
      paExpr
      (pgItem Lthen)
      (pgThen3
         (\t elsee f -> (t,f))
         paExpr
         (pgItem Lelse)
         paExpr
      )

paAExpr
 = pgApply (\x -> (False, x, []))
   (pgAlts 
    [
       pgApply ExprVar paVar,
       pgApply ExprCon paCon,
       pgApply ExprLiteral paLiteral,
       pgApply ExprList paListExpr,
       pgApply ExprTuple paTupleExpr,
       pgThen3 (\l e r -> e) (pgItem Llparen) paExpr (pgItem Lrparen)
    ]
   )

paListExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llbrack) 
             (pgZeroOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrbrack)

paTupleExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llparen) 
             (pgTwoOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrparen)

paVar = pgItem Lvar
paCon = pgItem Lcon
paVarop = pgItem Lvarop
paConop = pgItem Lconop
paMinus = pgItem Lminus

paOp
 = pgAlts [
            pgApply (\x -> (True, ExprVar x, x)) paVarop,
            pgApply (\x -> (True, ExprCon x, x)) paConop,
            pgApply (\x -> (True, ExprVar x, x)) paMinus
          ]

paDataDecl
   = pgThen2
        (\dataa useful -> useful)
        (pgItem Ldata)
        paDataDecl_main

paDataDecl_main
   = pgThen4
        (\name params eq drhs -> MkDataDecl name (params, drhs))
        paCon
        (pgZeroOrMore paVar)
        (pgItem Lequals)
        (pgOneOrMoreWithSep paConstrs (pgItem Lbar))

paConstrs
   = pgThen2
        (\con texprs -> (con, texprs))
        paCon
        (pgZeroOrMore paAType)

paType 
   = pgAlts
     [
        pgThen3 
           (\atype arrow typee -> TypeArr atype typee)
           paAType
           (pgItem Larrow)
           paType,
        pgThen2
           TypeCon
           paCon
           (pgOneOrMore paAType),
        paAType
     ]

paAType
   = pgAlts
     [
        pgApply TypeVar paVar,
        pgApply (\tycon -> TypeCon tycon []) paCon,
        pgThen3
           (\l t r -> t)
           (pgItem Llparen)
           paType
           (pgItem Lrparen),
        pgThen3
           (\l t r -> TypeList t)
           (pgItem Llbrack)
           paType
           (pgItem Lrbrack),
        pgThen3
           (\l t r -> TypeTuple t)
           (pgItem Llparen)
           (pgTwoOrMoreWithSep paType (pgItem Lcomma))
           (pgItem Lrparen)
     ]

paInfixDecl env toks
  = let dump (ExprVar v) = v
        dump (ExprCon c) = c
    in
    pa_UpdateFixityEnv 
       (pgThen3
          (\assoc prio name -> MkFixDecl name (assoc, prio))
          paInfixWord
          (pgApply leStringToInt (pgItem Lintlit)) 
          (pgApply (\(_, op, _) -> dump op) paOp)
          env 
          toks 
       )

paInfixWord
  = pgAlts
    [
       pgApply (const InfixL) (pgItem Linfixl),
       pgApply (const InfixR) (pgItem Linfixr),
       pgApply (const InfixN) (pgItem Linfix)
    ]

pa_UpdateFixityEnv (PFail tok) 
   = PFail tok

pa_UpdateFixityEnv (POk env toks (MkFixDecl name assoc_prio))
   = let 
         new_env = (name, assoc_prio) : env
     in
         POk new_env toks (MkFixDecl name assoc_prio)

paTopDecl
   = pgAlts
     [
        pgApply MkTopF paInfixDecl,
        pgApply MkTopD paDataDecl,
        pgApply MkTopV paValdef
     ]

paModule
   = pgThen4
        (\modyule name wheree topdecls -> MkModule name topdecls)
        (pgItem Lmodule)
        paCon
        (pgItem Lwhere)
        (pgDeclList paTopDecl)
   
parser_test toks
   = let parser_to_test
            = --paPat
              --paExpr
              --paValdef
              --pgZeroOrMore paInfixDecl
              --paDataDecl
              --paType
              paModule
              --pgTwoOrMoreWithSep (pgItem Lsemi) (pgItem Lcomma)
              
     in
         parser_to_test hsPrecTable toks

--==============================================--
--=== The Operator-Precedence parser (yuck!) ===--
--==============================================--

--
--==========================================================--
--
hsAExprOrOp 
 = pgAlts [paAExpr, paOp]

--hsDoExpr :: [PEntry] -> Parser Expr
-- [PaEntry] is a stack of operators and atomic expressions
-- hsDoExpr uses a parser (hsAexpOrOp :: Parsr PaEntry) for atomic
-- expressions or operators

hsDoExpr stack env toks = 
  let
     (validIn, restIn, parseIn, err)
        = case hsAExprOrOp env toks of
             POk env1 toks1 item1
                -> (True, toks1, item1, panic "hsDoExpr(1)")
             PFail err
                -> (False, panic "hsDoExpr(2)", panic "hsDoExpr(3)", err)
     (opIn, valueIn, nameIn)
        = parseIn
     (assocIn, priorIn)
        = utLookupDef env nameIn (InfixL, 9)
     shift
        = hsDoExpr (parseIn:stack) env restIn
  in 
     case stack of
        s1:s2:s3:ss
           | validIn && opS2 && opIn && priorS2 > priorIn
              -> reduce
           | validIn && opS2 && opIn && priorS2 == priorIn
              -> if assocS2 == InfixL && 
                    assocIn == InfixL 
                 then reduce
	         else 
                 if assocS2 == InfixR && 
                    assocIn == InfixR 
                 then shift
	         else PFail (head toks) -- Because of ambiguousness 
           | not validIn && opS2
              -> reduce
             where
               (opS1, valueS1, nameS1) = s1
               (opS2, valueS2, nameS2) = s2
               (opS3, valueS3, nameS3) = s3
               (assocS2, priorS2) = utLookupDef env nameS2 (InfixL, 9)
               reduce = hsDoExpr ((False, ExprApp (ExprApp valueS2 valueS3) 
                                                  valueS1, [])
                                  : ss) env toks
        s1:s2:ss
           | validIn && (opS1 || opS2) -> shift
           | otherwise -> reduce
             where
                (opS1, valueS1, nameS1) = s1
                (opS2, valueS2, nameS2) = s2
                reduce = hsDoExpr ((False, ExprApp valueS2 valueS1, []) : ss) 
                                  env toks
        (s1:[])
           | validIn -> shift
           | otherwise -> POk env toks valueS1
             where
                (opS1, valueS1, nameS1) = s1
        []
           | validIn -> shift
           | otherwise -> PFail err

--==========================================================--
--=== end                                      Parser.hs ===--
--==========================================================--


hsPrecTable = [
  ("-",		(InfixL, 6)),
  ("+",		(InfixL, 6)),
  ("*",		(InfixL, 7)),
  ("div",	(InfixN, 7)),
  ("mod", 	(InfixN, 7)),

  ("<",		(InfixN, 4)),
  ("<=",	(InfixN, 4)),
  ("==",	(InfixN, 4)),
  ("/=",	(InfixN, 4)),
  (">=",	(InfixN, 4)),
  (">",		(InfixN, 4)),

  ("C:",        (InfixR, 5)),
  ("++",        (InfixR, 5)),
  ("\\",        (InfixN, 5)),
  ("!!",        (InfixL, 9)),
  (".",         (InfixR, 9)),
  ("^",         (InfixR, 8)),
  ("elem",      (InfixN, 4)),
  ("notElem",   (InfixN, 4)),

  ("||",	(InfixR, 2)),
  ("&&",	(InfixR, 3))]


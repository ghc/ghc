
-- ==========================================================--
-- === Parser of Core programs          File: parse.m (1) ===--
-- ==========================================================--

module Parser2 where
import BaseDefs
import Utils
import MyUtils
import MakeDomains

import Data.List(nub)
import Data.Char(isAlpha,isDigit)

-- ====================================--
-- === Lexical analyser             ===--
-- ====================================--

-- ==========================================================--
--
paLex :: Int -> 
         [Char] -> 
         [Token]

paLex n (':':':':'=':cs) 
   = (n,"::="):paLex n cs

paLex n (c1:c2:cs) 
     | [c1,c2] `elem` ["==", ">=", "<=", "->", ";;"] = (n, [c1,c2]):paLex n cs

paLex n ('{':cs) 
   = lexcomment n cs
     where
        lexcomment n [] = paLex n []
        lexcomment n ('}':ds) = paLex n ds
        lexcomment n ('\n':ds) = lexcomment (n+1) ds
        lexcomment n (e:es) = lexcomment n es

paLex n ('\n':cs) 
   = paLex (n+1) cs

paLex n (c:cs) 
   | c `elem` " \t" = paLex n cs

paLex n (c:cs) 
     | isDigit c = (n, num_token): paLex n rest_cs
     where
        num_token = c:takeWhile isDigit cs
        rest_cs = dropWhile isDigit cs

paLex n (c:cs) 
     | isAlpha c = (n, var_tok):paLex n rest_cs
     where
        var_tok = c:takeWhile isIdChar cs
        rest_cs = dropWhile isIdChar cs
        isIdChar c = isAlpha c || isDigit c || (c == '_')

paLex n (c:cs) 
   = (n, [c]):paLex n cs

paLex n [] = [(999999, "$$$")]

-- ====================================--
-- === Generic parsing functions    ===--
-- ====================================--


-- ==========================================================--
--
paFailed (PFail _) = True
paFailed (POk _ _) = False

paGetItem :: PResult a -> a
paGetItem (POk item _) = item

paGetRest :: PResult a -> [Token]
paGetRest (POk _ rest) = rest
paGetRest (PFail rest) = rest


-- ==========================================================--
--
paLit :: [Char] -> 
         Parser [Char]

paLit lit []                          = PFail []
paLit lit ((n, t):ts)  | lit == t     = POk lit ts
                       | otherwise    = PFail ((n, t):ts)


-- ==========================================================--
--
paAlts :: [([Char] -> Bool, Parser a)] -> Parser a

paAlts pps [] = PFail []

paAlts [] toks = PFail []
paAlts ((pred, par):pps) toks@((n,t):_)
   | pred t  = par toks
   | otherwise = paAlts pps toks


-- ==========================================================--
--
paThen2 :: (a -> b -> c) ->
           Parser a ->
           Parser b ->
           Parser c

paThen2 combine p1 p2 toks
   = let p1parse = p1 toks
         p2parse = p2 (paGetRest p1parse)
     in  
             if paFailed p1parse then PFail (paGetRest p1parse)
        else if paFailed p2parse then PFail (paGetRest p2parse)
        else POk (combine (paGetItem p1parse) (paGetItem p2parse)) 
                 (paGetRest p2parse)


-- ==========================================================--
--
paThen3 :: (a -> b -> c -> d) ->
           Parser a ->
           Parser b ->
           Parser c ->
           Parser d

paThen3 combine p1 p2 p3 toks
   = let p1parse = p1 toks
         p2parse = p2 (paGetRest p1parse)
         p3parse = p3 (paGetRest p2parse)
     in  
             if paFailed p1parse then PFail (paGetRest p1parse)
        else if paFailed p2parse then PFail (paGetRest p2parse)
        else if paFailed p3parse then PFail (paGetRest p3parse)
        else POk (combine (paGetItem p1parse) (paGetItem p2parse) 
                          (paGetItem p3parse))
                (paGetRest p3parse)


-- ==========================================================--
--
paThen4 :: (a -> b -> c -> d -> e) ->
           Parser a ->
           Parser b ->
           Parser c ->
           Parser d ->
           Parser e

paThen4 combine p1 p2 p3 p4 toks
   = let p1parse = p1 toks
         p2parse = p2 (paGetRest p1parse)
         p3parse = p3 (paGetRest p2parse)
         p4parse = p4 (paGetRest p3parse)
     in  
             if paFailed p1parse then PFail (paGetRest p1parse)
        else if paFailed p2parse then PFail (paGetRest p2parse)
        else if paFailed p3parse then PFail (paGetRest p3parse)
        else if paFailed p4parse then PFail (paGetRest p4parse)
        else POk (combine (paGetItem p1parse) (paGetItem p2parse) 
                         (paGetItem p3parse) (paGetItem p4parse))
                (paGetRest p4parse)


-- ==========================================================--
--
paZeroOrMore :: Parser a -> Parser [a]

paZeroOrMore p toks
   = let pParse    = p toks
         pUnused   = paGetRest pParse
         zmParse   = paZeroOrMore p pUnused
         zmUnused  = paGetRest zmParse
     in
             if paFailed pParse then POk [] toks
        else if paFailed zmParse then POk [paGetItem pParse] pUnused
        else POk ((paGetItem pParse):paGetItem zmParse) zmUnused


-- ==========================================================--
--
paOneOrMore :: Parser a -> Parser [a]

paOneOrMore p
   = paThen2 (:) p (paZeroOrMore p)


-- ==========================================================--
--
paOneOrMoreWithSep :: Parser a -> 
                      Parser b -> 
                      Parser [a]

paOneOrMoreWithSep p psep toks
   = let pParse  = p toks
         pRest   = paGetRest pParse
         sParse  = psep pRest
         sRest   = paGetRest sParse
         mParse  = paOneOrMoreWithSep p psep sRest
         mRest   = paGetRest mParse
     in
             if paFailed pParse then PFail toks
        else if paFailed sParse then POk [paGetItem pParse] pRest
        else if paFailed mParse then POk [paGetItem pParse] pRest
        else POk ((paGetItem pParse):paGetItem mParse) mRest


-- ==========================================================--
--
paApply :: Parser a -> 
           (a -> b) -> 
           Parser b

paApply p f toks
   = let pParse = p toks
     in  
        if      paFailed pParse 
        then    PFail (paGetRest pParse)
        else    POk (f (paGetItem pParse)) (paGetRest pParse)


-- ==========================================================--
--
paSat :: (String -> Bool) ->
         Parser String

paSat pred [] = PFail []
paSat pred ((n,t):toks)
   | pred t     = POk t toks
   | otherwise  = PFail toks


-- ==========================================================--
--
paEmpty :: a -> Parser a

paEmpty v toks = POk v toks


-- ====================================--
-- === Specific parsing functions   ===--
-- ====================================--

-- ================================================--
paSyntax 
   = get_parse . paProgram
     where
        get_parse (PFail [])
           = myFail "Syntax error: Unexpected end of source text"

        get_parse (PFail ((n,t):_))
           = myFail ( "Syntax error: unexpected token \"" ++ t ++
                    "\" on line " ++ show ( n :: Int ))

        get_parse (POk _ ((n,t):_:_))
           = myFail ( "Syntax error: unexpected token \"" ++ t ++
                    "\" on line " ++ show ( n :: Int ))

        get_parse (POk prog [(999999, "$$$")]) = prog

-- ================================================--
paProgram = paThen3 f paTypeDefList (paLit ";;") paScdefs
            where f a b c = (a,c)

-- ================================================--
paName = paSat paIsName

-- ================================================--
paIsName s = isAlpha (head s) &&  not (s `elem` paKeywords)

-- ================================================--
paCname = paSat paIsCname

-- ================================================--
paIsCname s = ('A'<=(head s)) && 
              ((head s)<='Z') && 
              not (s `elem` paKeywords)

-- ================================================--
paKeywords = ["let", "letrec", "case", "in", "of", "end"]

-- ================================================--
paRelops = ["<=", "<", ">=", ">", "==", "~="]

-- ================================================--
paIsRelop op = op `elem` paRelops

-- ================================================--
paRelop = paSat paIsRelop

-- ================================================--
paNum = paSat paIsNum `paApply` paNumval

-- ================================================--
paNumval :: [Char] -> Int
paNumval cs 
   = sum (powers 1 (map (\d -> fromEnum d - 48) (reverse cs)))
     where
        powers n [] = []
        powers n (h:t) = n*h : powers ((10 :: Int) *n) t

-- ================================================--
paIsNum = isDigit.head

-- ================================================--
paWithTrailingSemi p = paThen2 const p (paLit ";")

-- ==================================--
-- === Parsing type definitions   ===--
-- ==================================--

-- ================================================--
paTypeDefList = paZeroOrMore (paThen2 f paTypeDef (paLit ";"))
                where f a b = a

-- ================================================--
paTypeDef 
   = paThen4 f paName (paZeroOrMore paName) (paLit "::=") paConstrAlts
     where f a b c d = (a,b,d)

-- ================================================--
paConstrAlts = paOneOrMoreWithSep paConstrAlt (paLit "|")

-- ================================================--
paConstrAlt = paThen2 f paCname (paZeroOrMore paTDefExpr)
              where f a b = (a,b)

-- ================================================--
paTDefExpr
    = paAlts [ (  (== "("),   paTDefExpr2  ),
               (  paIsName,   paApply paName TDefVar) ]
      where
         paTDefExpr2 = paThen3 g (paLit "(") paTDefExpr3 (paLit ")")
         g a b c = b
         paTDefExpr3 = paThen2 h paName (paZeroOrMore paTDefExpr)
         h a b = TDefCons a b


-- ===========================================--
-- === Parsing supercombinator definitions ===--
-- ===========================================--

-- ================================================--
paScdefs = paOneOrMore (paWithTrailingSemi paSc)

-- ================================================--
paSc = paThen4 mk_sc paName (paZeroOrMore paName) (paLit "=") paExpr
       where
          mk_sc sc args eq rhs = (sc, (args, rhs))

-- ================================================--
paExpr
   = paAlts [  (  (== "let"),  paLet    ),
               (  (== "letrec"), paLetrec ),
               (  (== "case"), paCase ),
               (  (== "\\"),  paLambda  ),
               (  (const True),  paExpr1 ) ]


-- ================================================--
paLet = paThen4 mk_let
              (paLit "let")
              paDefns
              (paLit "in") paExpr
        where
        mk_let lett defns inn expr = ELet False defns expr


-- ================================================--
paLetrec = paThen4 mk_letrec
              (paLit "letrec")
              paDefns
              (paLit "in") paExpr
           where
           mk_letrec letrecc defns inn expr = ELet True defns expr


-- ================================================--
paDefns = paOneOrMoreWithSep paDefn (paLit ";")

-- ================================================--
paDefn = paThen3 mk_defn paName (paLit "=") paExpr
         where
         mk_defn var equals rhs = (var,rhs)

-- ================================================--
paCase = paThen4 mk_case (paLit "case") paExpr (paLit "of") paAlters
         where
         mk_case kase e ov alts = ECase e alts

-- ================================================--
paAlters = paThen2 const (paOneOrMoreWithSep paAlter (paLit ";")) (paLit "end")

-- ================================================--
paAlter = paThen4 mk_alt paCname (paZeroOrMore paName) (paLit "->") paExpr
          where
          mk_alt tag args arrow rhs = (tag, (args, rhs))

-- ================================================--
paLambda = paThen4 mk_lam
             (paLit "\\") (paOneOrMore paName) (paLit "->") paExpr
           where
           mk_lam lam vars dot expr = ELam vars expr

-- ================================================--
paExpr1 = paThen2 paAssembleOp paExpr2 paExpr1c

-- ================================================--
paExpr1c = paAlts [((== "|"),   paThen2 FoundOp (paLit "|") paExpr1),
                   ((== "#"),   paThen2 FoundOp (paLit "#") paExpr1),
                   (const True, paEmpty NoOp)]

-- ================================================--
paExpr2 = paThen2 paAssembleOp paExpr3 paExpr2c

-- ================================================--
paExpr2c = paAlts [((== "&"),   paThen2 FoundOp (paLit "&") paExpr2),
                   (const True, paEmpty NoOp)]

-- ================================================--
paExpr3 = paThen2 paAssembleOp paExpr4 paExpr3c

-- ================================================--
paExpr3c = paAlts [(paIsRelop,  paThen2 FoundOp paRelop paExpr4),
                   (const True, paEmpty NoOp)]

-- ================================================--
paExpr4 = paThen2 paAssembleOp paExpr5 paExpr4c

-- ================================================--
paExpr4c = paAlts [((== "+"),   paThen2 FoundOp (paLit "+") paExpr4),
                   ((== "-"),   paThen2 FoundOp (paLit "-") paExpr5),
                   (const True, paEmpty NoOp)]

-- ================================================--
paExpr5 = paThen2 paAssembleOp paExpr6 paExpr5c

-- ================================================--
paExpr5c = paAlts [((== "*"),   paThen2 FoundOp (paLit "*") paExpr5),
                   ((== "/"),   paThen2 FoundOp (paLit "/") paExpr6),
                   (const True, paEmpty NoOp)]

-- ================================================--
paExpr6 = (paOneOrMore paAtomic) `paApply` mk_ap_chain
            where
              mk_ap_chain (fn:args) = foldl EAp fn args

-- ================================================--
paAtomic = paAlts [(paIsCname, paConstr),
                   ((== "("), paBracExpr),
                   (paIsName, paName `paApply` EVar),
                   (paIsNum,  paNum `paApply` ENum)]

-- ================================================--
paBracExpr = paThen3 mk_brack (paLit "(") paExpr (paLit ")")
             where
             mk_brack open expr close = expr

-- ================================================--
paConstr = paApply paCname EConstr


-- ================================================--
paAssembleOp e1 NoOp = e1
paAssembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2



-- ===================================================--
-- === Validation & transformation of parsed trees ===--
-- ===================================================--

-- ==========================================================--
--
paProgramToAtomic :: CoreProgram -> 
                     AtomicProgram

paProgramToAtomic (tds, scdefs) 
   = (tds, ce)
     where
        ce = ELet True
                [(name, ELam ns b) | (name, (ns, b)) <- scdefs]
                (ENum 42)

-- ==========================================================--
--
paValidTypeDefs :: [TypeDef] ->       -- all type definitions
                   TypeDependancy ->  -- type dependancy info
                   [Char]             -- wordy description of any problems

paValidTypeDefs tds rda
   = if  not uniqueTNames    then  "Non-unique type names"  else
     if  not uniqueParNames  then "Non-unique parameter names" else
     if  not uniqueCNames    then "Non-unique constructor names" else
     if  not balanced   then "Declared parameters do not match used parameters" else
     if  not allDefined then "Undefined types are present" else
     if  not rightArity then "Types are used at wrong arities" else
     if  not allSimple  then  "Perverse type definitions are present"
                        else ""
     where
        arityMap = map f tds
                   where
                       f (tname, tvs, cal) = (tname, length tvs)
        allTNames = map f tds
                    where
                       f (tname, tvs, cal) = tname
        allCNames = concat (map f tds)
                    where
                       f (tname, tvs, cal) = map first cal
        uniqueTNames = length allTNames == ((length.nub) allTNames)
        uniqueParNames = and (map f tds)
                         where
                            f (tname, tvs, cal) = length tvs == ((length.nub) tvs)
        uniqueCNames = length allCNames == ((length.nub) allCNames)
        balanced = and (map isBalanced tds)
                   where
                      tvsIn (TDefVar n) = [n]
                      tvsIn (TDefCons n tel) = concat (map tvsIn tel)
                      g tDefExprList = concat (map tvsIn tDefExprList)
                      isBalanced (tname, tvs, cal) 
                         = (utSetFromList tvs) == 
                           (utSetFromList (concat (map (g.second) cal)))
        allDefined = utSetSubsetOf
                        (utSetFromList (concat (map mdFreeTVarsIn tds)))
                        (utSetFromList allTNames)
        rightArity = and (map f tds)
                     where
                        f (tname, tvs, cal) = and (map (g.second) cal)
                        g tDefExprList = and (map rArity tDefExprList)
                        rArity (TDefVar v) = True
                        rArity (TDefCons n tel) 
                           = (length tel == utSureLookup arityMap "paVTD`rA`rA" n) && 
                             (and (map rArity tel))
        allSimple = and (map f tds)
                    where
                       f (tname, tvs, cal) = 
                          utSetSubsetOf (utSetFromList (allVars cal))
                                        (utSetFromList (tvs++(groupOf tname rda)))
                       allVars cal = concat (map g cal)
                       g (n, tel) = concat (map allTVs tel)
                       allTVs (TDefVar n) = [n]
                       allTVs (TDefCons n tel) = n:concat (map allTVs tel)
                       groupOf tname ((rf, group):rest) 
                           | tname `elem` group &&  rf    = group
                           | tname `elem` group && not rf = []                
                           | otherwise                    = groupOf tname rest

            
-- ==========================================================--
--
paParse :: [Char] -> (TypeDependancy, AtomicProgram)

paParse fileContents
   = if typeDefErrors == "" 
        then (dependResult, (typeDefs, mainExpr)) 
        else myFail typeDefErrors
     where
        (typeDefs, mainExpr) = paProgramToAtomic parsedProgram
        dependResult = mdTypeDependancy typeDefs
        typeDefErrors = paValidTypeDefs typeDefs dependResult
        tokens = paLex 1 fileContents
        parsedProgram = paSyntax tokens


-- ==========================================================--
-- === End                                    parse.m (1) ===--
-- ==========================================================--

 
-- ==========================================================--
-- === A type-checker -- v5        File: TypeCheck5.m (1) ===--
-- === Corrected version for 0.210a                       ===--
-- ==========================================================--

module TypeCheck5 where
import BaseDefs
import Utils
import MyUtils

import Data.List(nub) -- 1.3

-- ==========================================================--
-- === Formatting of results                              ===--
-- ==========================================================--

tcMapAnnExpr :: (a -> b) ->
                AnnExpr c a ->
                AnnExpr c b

tcMapAnnExpr f (ann, node) 
   = (f ann, mapAnnExpr' node)
     where
        mapAnnExpr' (AVar v) = AVar v
        mapAnnExpr' (ANum n) = ANum n
        mapAnnExpr' (AConstr c) = AConstr c
        mapAnnExpr' (AAp ae1 ae2) 
           = AAp (tcMapAnnExpr f ae1) (tcMapAnnExpr f ae2)
        mapAnnExpr' (ALet recFlag annDefs mainExpr)
           = ALet recFlag (map mapAnnDefn annDefs) (tcMapAnnExpr f mainExpr)
        mapAnnExpr' (ACase switchExpr annAlts)
           = ACase (tcMapAnnExpr f switchExpr) (map mapAnnAlt annAlts)
        mapAnnExpr' (ALam vs e) = ALam vs (tcMapAnnExpr f e)

        mapAnnDefn (naam, expr) 
           = (naam, tcMapAnnExpr f expr)

        mapAnnAlt (naam, (pars, resExpr))
           = (naam, (pars, tcMapAnnExpr f resExpr))


-- ======================================================--
--
tcSubstAnnTree :: Subst -> 
                  AnnExpr Naam TExpr -> 
                  AnnExpr Naam TExpr

tcSubstAnnTree phi tree = tcMapAnnExpr (tcSub_type phi) tree


-- ======================================================--
--
tcTreeToEnv :: AnnExpr Naam TExpr ->
               TypeEnv

tcTreeToEnv tree
   = t2e tree
     where
        t2e (nodeType, node) = t2e' node

        t2e' (AVar v) = []
        t2e' (ANum n) = []
        t2e' (AConstr c) = []
        t2e' (AAp ae1 ae2) = (t2e ae1) ++ (t2e ae2)
        t2e' (ALam cs e) = t2e e
        t2e' (ALet rf dl me) 
           = (concat (map aFN dl)) ++ (t2e me)
        t2e' (ACase sw alts)
           = (t2e sw) ++ (concat (map (t2e.second.second) alts))
   
        aFN (naam, (tijp, body)) 
          = (naam, tijp):(t2e' body)



-- ======================================================--
--
tcShowtExpr :: TExpr ->
               [Char]

tcShowtExpr t 
   = pretty' False t
     where 
       pretty' b (TVar tvname) = [' ', toEnum (96+(lookup tvname tvdict))]
       pretty' b (TCons "int" []) = " int"
       pretty' b (TCons "bool" []) = " bool"
       pretty' b (TCons "char" []) = " char"
       pretty' True (TArr t1 t2) 
	   = " (" ++ (pretty' True t1) ++ " -> " ++
	     (pretty' False t2) ++ ")" 
       pretty' False (TArr t1 t2) 
	   = (pretty' True t1) ++ " -> " ++
	     (pretty' False t2)
       pretty' b (TCons notArrow cl) 
	   = " (" ++ notArrow ++ 
	      concat (map (pretty' True) cl) ++ ")"
       lookup tvname [] 
          = panic "tcShowtExpr: Type name lookup failed"
       lookup tvname (t:ts) | t==tvname = 1
			    | otherwise = 1 + (lookup tvname ts)
       tvdict = nub (tvdict' t)
       tvdict' (TVar t) = [t]
       tvdict' (TCons c ts) = concat (map tvdict' ts)
       tvdict' (TArr t1 t2) = tvdict' t1 ++ tvdict' t2


-- ======================================================--
--
tcPretty :: (Naam, TExpr) -> 
            [Char]

tcPretty (naam, tipe)
   = "\n   " ++ (ljustify 25 (naam ++ " :: ")) ++ 
            (tcShowtExpr tipe)


-- ======================================================--
tcCheck :: TcTypeEnv -> 
           TypeNameSupply ->
           AtomicProgram -> 
           ([Char],  Reply (AnnExpr Naam TExpr, TypeEnv) Message)

tcCheck baseTypes ns (tdefs, expr)
   = if good tcResult 
         then (fullEnvWords,  Ok (rootTree, fullEnv))
         else ("",            Fail "No type")
     where
        tcResult = tc (tdefs++builtInTypes)
                   (baseTypes++finalConstrTypes) finalNs expr

        good (Ok x) = True
        good (Fail x2) = False
        
        (rootSubst, rootType, annoTree) = f tcResult where f (Ok x) = x

        rootTree = tcSubstAnnTree rootSubst annoTree

        rootEnv = tcTreeToEnv rootTree

        fullEnv = rootEnv ++ map f finalConstrTypes
                  where
                     f (naam, (Scheme vs t)) = (naam, t)

        fullEnvWords = concat (map tcPretty fullEnv)

        (finalNs, constrTypes) = 
           mapAccuml tcConstrTypeSchemes ns (tdefs++builtInTypes)
        finalConstrTypes = concat constrTypes

        builtInTypes 
           = [ ("bool", [], [("True", []), ("False", [])]) ]
        


-- ==========================================================--
-- === 9.2 Representation of type expressions             ===--
-- ==========================================================--

-- ======================================================--
--tcArrow :: TExpr -> 
--           TExpr -> 
--           TExpr
--
--tcArrow t1 t2 = TArr t1 t2



-- ======================================================--
tcInt :: TExpr

tcInt = TCons "int" []



-- ======================================================--
tcBool :: TExpr

tcBool = TCons "bool" []



-- ======================================================--
tcTvars_in :: TExpr -> 
              [TVName]

tcTvars_in t = tvars_in' t []
               where
                  tvars_in' (TVar x) l = x:l
                  tvars_in' (TCons y ts) l = foldr tvars_in' l ts
                  tvars_in' (TArr t1 t2) l = tvars_in' t1 (tvars_in' t2 l)


-- ==========================================================--
-- === 9.41 Substitutions                                 ===--
-- ==========================================================--

-- ======================================================--
tcApply_sub :: Subst ->
               TVName ->
               TExpr

tcApply_sub phi tvn 
   = if TVar tvn == lookUpResult
        then TVar tvn
        else tcSub_type phi lookUpResult
     where
        lookUpResult = utLookupDef phi tvn (TVar tvn)


-- ======================================================--
tcSub_type :: Subst -> 
              TExpr -> 
              TExpr

tcSub_type phi (TVar tvn) = tcApply_sub phi tvn

tcSub_type phi (TCons tcn ts) = TCons tcn (map (tcSub_type phi) ts)

tcSub_type phi (TArr t1 t2) = TArr (tcSub_type phi t1) (tcSub_type phi t2)


-- ======================================================--
tcScomp :: Subst -> 
           Subst -> 
           Subst

tcScomp sub2 sub1 = sub1 ++ sub2



-- ======================================================--
tcId_subst :: Subst

tcId_subst = []



-- ======================================================--
tcDelta :: TVName -> 
           TExpr -> 
           Subst
-- all TVar -> TVar substitutions lead downhill
tcDelta tvn (TVar tvn2) 
   | tvn == tvn2   = []
   | tvn >  tvn2   = [(tvn, TVar tvn2)]
   | tvn <  tvn2   = [(tvn2, TVar tvn)]

tcDelta tvn non_var_texpr = [(tvn, non_var_texpr)]


-- ==========================================================--
-- === 9.42 Unification                                   ===--
-- ==========================================================--

-- ======================================================--
tcExtend :: Subst -> 
            TVName -> 
            TExpr -> 
            Reply Subst Message

tcExtend phi tvn t 
    | t == TVar tvn   
    = Ok phi
    | tvn `notElem` (tcTvars_in t)
    = Ok ((tcDelta tvn t) `tcScomp` phi)
    | otherwise
    = myFail
         (   "Type error in source program:\n\n"         ++
             "Circular substitution:\n      "            ++
	      tcShowtExpr (TVar tvn)                     ++ 
              "\n   going to\n"                          ++
	      "      "                                   ++ 
              tcShowtExpr t                              ++ 
              "\n")



-- ======================================================--
tcUnify :: Subst -> 
           (TExpr, TExpr) -> 
           Reply Subst Message

tcUnify phi (TVar tvn, t) 
  = if phitvn == TVar tvn
       then tcExtend phi tvn phit
       else tcUnify phi (phitvn, phit)
     where
	phitvn = tcApply_sub phi tvn
	phit = tcSub_type phi t

tcUnify phi (p@(TCons _ _), q@(TVar _))
   = tcUnify phi (q, p)

tcUnify phi (p@(TArr _ _), q@(TVar _))
   = tcUnify phi (q, p)

tcUnify phi (TArr t1 t2, TArr t1' t2')
   = tcUnifyl phi [(t1, t1'), (t2, t2')]

tcUnify phi (TCons tcn ts, TCons tcn' ts') 
   | tcn == tcn' 
   = tcUnifyl phi (ts `zip` ts')

tcUnify phi (t1, t2)
   = myFail
        (   "Type error in source program:\n\n"          ++
            "Cannot unify\n      "                       ++
            tcShowtExpr t1                               ++
            "\n   with\n      "                          ++
            tcShowtExpr t2                               ++
            "\n"
        )



-- ======================================================--
tcUnifyl :: Subst ->  
            [(TExpr, TExpr)] -> 
            Reply Subst Message

tcUnifyl phi eqns 
   = foldr unify' (Ok phi) eqns
     where
	unify' eqn (Ok phi) = tcUnify phi eqn
	unify' eqn (Fail m) = Fail m



-- ==========================================================--
-- === 9.42.2 Merging of substitutions                    ===--
-- ==========================================================--

-- ======================================================--
tcMergeSubs :: Subst ->
               Subst

tcMergeSubs phi 
   = if newBinds == []
        then unifiedOlds
        else tcMergeSubs (unifiedOlds ++ newBinds)
     where
        (newBinds, unifiedOlds) = tcMergeSubsMain phi



-- ======================================================--
tcMergeSubsMain :: Subst -> 
                   (Subst, Subst)   -- pair of new binds, unified olds

tcMergeSubsMain phi
   = (concat newUnifiersChecked,
      zip oldVars (tcOldUnified newUnifiersChecked oldGroups))
     where
        oldVars = nub (utDomain phi)
        oldGroups = map (utLookupAll phi) oldVars
        newUnifiers = map (tcUnifySet tcId_subst) oldGroups
        newUnifiersChecked = map tcCheckUnifier newUnifiers



-- ======================================================--
tcCheckUnifier :: Reply Subst Message -> Subst

tcCheckUnifier (Ok r) = r
tcCheckUnifier (Fail m) 
   = panic ("tcCheckUnifier: " ++ m)



-- ======================================================--
tcOldUnified :: [Subst] -> [[TExpr]] -> [TExpr]

tcOldUnified [] [] = []
tcOldUnified (u:us) (og:ogs) 
      = (tcSub_type u (head og)): tcOldUnified us ogs


-- ==========================================================--
-- === 9.5 Keeping track of types                         ===--
-- ==========================================================--

-- ======================================================--
tcUnknowns_scheme :: TypeScheme -> 
                     [TVName]

tcUnknowns_scheme (Scheme scvs t) = tcTvars_in t `tcBar` scvs



-- ======================================================--
tcBar :: (Eq a) => [a] -> 
                   [a] -> 
                   [a]

tcBar xs ys = [ x | x <- xs,  not (x `elem` ys)]



-- ======================================================--
tcSub_scheme :: Subst -> 
                TypeScheme -> 
                TypeScheme

tcSub_scheme phi (Scheme scvs t)
    = Scheme scvs (tcSub_type (tcExclude phi scvs) t)
      where
         tcExclude phi scvs = [(n,e) | (n,e) <- phi,  not (n `elem` scvs)]



-- ==========================================================--
-- === 9.53 Association lists                             ===--
-- ==========================================================--

-- ======================================================--
tcCharVal :: AList Naam b -> Naam -> b

tcCharVal al k
   = utLookupDef al k (panic ("tcCharVal: no such variable: " ++ k))


-- ======================================================--
tcUnknowns_te :: TcTypeEnv -> 
                 [TVName]

tcUnknowns_te gamma = concat (map tcUnknowns_scheme (utRange gamma))



-- ======================================================--
tcSub_te :: Subst -> 
            TcTypeEnv -> 
            TcTypeEnv

tcSub_te phi gamma = [(x, tcSub_scheme phi st) | (x, st) <- gamma]


-- ==========================================================--
-- === 9.6 New variables                                  ===--
-- ==========================================================--

-- ======================================================--
tcNext_name :: TypeNameSupply -> 
               TVName

tcNext_name ns@(f, s) = ns



-- ======================================================--
tcDeplete :: TypeNameSupply -> 
             TypeNameSupply

tcDeplete (f, s) = (f, tcNSSucc s)



-- ======================================================--
tcSplit :: TypeNameSupply -> 
           (TypeNameSupply, TypeNameSupply)

tcSplit (f, s) = ((f2, [0]), (tcNSSucc f2, [0]))
                 where f2 = tcNSDouble f



-- ======================================================--
tcName_sequence :: TypeNameSupply -> 
                   [TVName]

tcName_sequence ns = tcNext_name ns: tcName_sequence (tcDeplete ns)


-- ======================================================--
tcNSSucc :: [Int] ->
            [Int]

tcNSSucc []     = [1]
tcNSSucc (n:ns) | n < tcNSslimit  = n+1: ns
                | otherwise       = 0: tcNSSucc ns


-- ======================================================--
tcNSDouble :: [Int] ->
              [Int]

tcNSDouble []   = []
tcNSDouble (n:ns) 
    = 2*n': ns'
       where n' | n > tcNSdlimit  = n - tcNSdlimit
		| otherwise       = n
	     ns' | n' == n    = tcNSDouble ns
		 | otherwise  = tcNSSucc (tcNSDouble ns)

                       
tcNSdlimit :: Int
tcNSdlimit = 2^30

tcNSslimit :: Int
tcNSslimit = tcNSdlimit + (tcNSdlimit - 1)


-- ==========================================================--
-- === 9.7 The type-checker                               ===--
-- ==========================================================--


-- ======================================================--
tc :: [TypeDef] ->
      TcTypeEnv -> 
      TypeNameSupply -> 
      CExpr -> 
      Reply TypeInfo Message

tc tds gamma ns (ENum n) 
   = Ok (tcId_subst, TCons "int" [], (TCons "int" [], ANum n))

tc tds gamma ns (EVar x) 
   = tcvar tds gamma ns x

tc tds gamma ns (EConstr c)
   = tcvar tds gamma ns c

tc tds gamma ns (EAp e1 e2)
   = tcap tds gamma ns e1 e2

tc tds gamma ns (ELam [] e)
   = tc tds gamma ns e
tc tds gamma ns (ELam [x] e)
   = tclambda tds gamma ns x e
tc tds gamma ns (ELam (x:y:xs) e)
   = tclambda tds gamma ns x (ELam (y:xs) e)

tc tds gamma ns (ELet recursive dl e)
   = if not recursive
        then tclet tds gamma ns xs es e
        else tcletrec tds gamma ns xs es e
     where
       (xs, es) = unzip2 dl

tc tds gamma ns (ECase switch alts)
   = tccase tds gamma ns switch constructors arglists exprs
     where
        (constructors, alters) = unzip2 alts
        (arglists, exprs) = unzip2 alters
 

-- ==========================================================--
-- === 0.00 Type-checking case-expressions                ===--
-- ==========================================================--

tcConstrTypeSchemes :: TypeNameSupply ->
                       TypeDef ->
                       (TypeNameSupply, AList Naam TypeScheme)

tcConstrTypeSchemes ns (tn, stvs, cal)
   = (finalNameSupply, map2nd enScheme cAltsCurried)
     where
        -- associates new type vars with each poly var
        -- in the type
        newTVs = tcNewTypeVars (tn, stvs, cal) ns

        -- the actual type variables themselves
        tVs = map second newTVs

        -- the types of the constructor functions         
        cAltsCurried = map2nd (foldr TArr tdSignature) cAltsXLated
        cAltsXLated = map2nd (map (tcTDefSubst newTVs)) cal
        tdSignature = TCons tn (map TVar tVs)
        enScheme texp = Scheme ((nub.tcTvars_in) texp) texp

        -- the revised name supply
        finalNameSupply = applyNtimes ( length tVs + 2) tcDeplete ns

        -- apply a function n times to an arg
        applyNtimes n func arg 
           | n ==0       = arg
           | otherwise   = applyNtimes (n-1) func (func arg)
                    


-- ======================================================--
--
tccase :: [TypeDef] ->         -- constructor type definitions
          TcTypeEnv ->         -- current type bindings
          TypeNameSupply ->    -- name supply
          CExpr ->             -- switch expression
          [Naam] ->            -- constructors
          [[Naam]] ->          -- argument lists
          [CExpr] ->           -- resulting expressions
          Reply TypeInfo Message


tccase tds gamma ns sw cs als res
-- get the type definition in use, & an association of
-- variables therein to type vars & pass
-- Also, reorder the argument lists
-- and resulting expressions so as to reflect the 
-- sequence of constructors in the definition
 = if length tdCNames /=  length (nub cs)
      then  myFail
            "Error in source program: missing alternatives in CASE"
      else tccase1 tds gamma ns1 sw reOals reOres newTVs tdInUse
     where
        tdInUse = tcGetTypeDef tds cs
        newTVs = tcNewTypeVars tdInUse ns2
        (ns1, ns2) = tcSplit ns
        merge = zip cs (zip als res)
        tdCNames = map first (tcK33 tdInUse)
        (reOals, reOres) = unzip2 (tcReorder tdCNames merge)



-- ======================================================--
--
tcReorder :: [Naam] -> [(Naam,b)] -> [b]

tcReorder []     uol =  []
tcReorder (k:ks) uol 
   = (utLookupDef uol k 
        (myFail
            ("Error in source program: undeclared constructor '" ++ k ++
               "' in CASE") ) )
        : tcReorder ks uol 


-- ======================================================--
-- Projection functions and similar rubbish.
tcDeOksel (Ok x) = x
tcDeOksel (Fail m) = panic ("tcDeOkSel: " ++ m)
tcOk13sel (Ok (a, b, c)) = a
tcOk13sel (Fail m) = panic ("tcOk13sel: " ++ m)
tcOk23sel (Ok (a, b, c)) = b
tcOk23sel (Fail m) = panic ("tcOk23sel: " ++ m)
tcOk33sel (Ok (a, b, c)) = c
tcOk33sel (Fail m) = panic ("tcOk33sel: " ++ m)
tcK31sel (a, b, c) = a
tcK33 (a,b,c) = c



-- ======================================================--
--
tccase1 :: [TypeDef] ->
           TcTypeEnv -> 
           TypeNameSupply ->
           CExpr -> 
           [[Naam]] ->
           [CExpr] ->
           AList Naam TVName ->
           TypeDef ->
           Reply TypeInfo Message

tccase1 tds gamma ns sw reOals reOres newTVs tdInUse
-- calculate all the gammas for the RHS's
-- call tc for each RHS, so as to gather all the
-- sigmas and types for each RHS, then pass on
   = tccase2 tds gamma ns2 sw reOals newTVs tdInUse rhsTcs
     where
        rhsGammas = tcGetAllGammas newTVs (tcK33 tdInUse) reOals
        rhsTcs = rhsTc1 ns1 rhsGammas reOres
        rhsTc1 nsl []     []     = []
        rhsTc1 nsl (g:gs) (r:rs) 
           = tc tds (g++gamma) nsl1 r : rhsTc1 nsl2 gs rs
             where (nsl1, nsl2) = tcSplit nsl
        (ns1, ns2) = tcSplit ns  
        

-- ======================================================--
--
tccase2 :: [TypeDef] ->
           TcTypeEnv -> 
           TypeNameSupply ->
           CExpr -> 
           [[Naam]] ->
           AList Naam TVName ->
           TypeDef ->
           [Reply TypeInfo Message] ->
           Reply TypeInfo Message

tccase2 tds gamma ns sw reOals newTVs tdInUse rhsTcs
-- get the unifiers for T1 to Tk and hence the unifier for all
-- type variables in the type definition.  Also compute the
-- unifier of the result types.
   = tccase3 tds gamma ns sw reOals newTVs tdInUse rhsTcs 
             phi_1_to_n tau_1_to_n phi_rhs
     where
        phi_1_to_n = map tcOk13sel rhsTcs
        tau_1_to_n = map tcOk23sel rhsTcs
        phi_rhs = tcDeOksel (tcUnifySet tcId_subst tau_1_to_n)

 

-- ======================================================--
--
tccase3 :: [TypeDef] ->                    -- tds
           TcTypeEnv ->                    -- gamma
           TypeNameSupply ->               -- ns
           CExpr ->                        -- sw
           [[Naam]] ->                     -- reOals
           AList Naam TVName ->            -- newTVs
           TypeDef ->                      -- tdInUse
           [Reply TypeInfo Message] ->     -- rhsTcs
           [Subst] ->                      -- phi_1_to_n
           [TExpr] ->                      -- tau_1_to_n
           Subst ->                        -- phi_rhs
           Reply TypeInfo Message

tccase3 tds gamma ns sw reOals newTVs tdInUse rhsTcs
        phi_1_to_n tau_1_to_n phi_rhs
-- make up substitutions for each of the unknown tvars
-- merge the substitutions into one
-- apply the substitution to the typedef's signature to get the
-- most general allowable input type
-- call tc to get the type of the switch expression
-- check that this is an instance of the deduced input type
-- gather the new bindings from the RHSs and switch expression
-- return Ok (the big substitution, the result type, gathered bindings)
   = Ok (phi_Big, tau_final, 
            (tau_final, ACase tree_s 
                        (zip tdCNames (zip reOals annotatedRHSs))))
     where
        phi_sTau_sTree_s = tc tds gamma ns sw 
        phi_s  = tcOk13sel phi_sTau_sTree_s
        tau_s  = tcOk23sel phi_sTau_sTree_s
        tree_s = tcOk33sel phi_sTau_sTree_s
        
        phi = tcMergeSubs (concat phi_1_to_n ++ phi_rhs ++ phi_s)

        tau_lhs = tcSub_type phi tdSignature

        phi_lhs = tcUnify tcId_subst (tau_lhs, tau_s) -- reverse these?

        phi_Big = tcMergeSubs (tcDeOksel phi_lhs ++ phi) 

        tau_final = tcSub_type phi_Big (head (map tcOk23sel rhsTcs))

        annotatedRHSs = map tcOk33sel rhsTcs
        tVs = map second newTVs
        tdSignature = TCons (tcK31sel tdInUse) (map TVar tVs)
        tdCNames = map first (tcK33 tdInUse)


-- ======================================================--
--
tcUnifySet :: Subst -> 
              [TExpr] -> 
              Reply Subst Message

tcUnifySet sub (e1:[]) = Ok sub
tcUnifySet sub (e1:e2:[]) 
   = tcUnify sub (e1, e2)
tcUnifySet sub (e1:e2:e3:es) 
   = tcUnifySet newSub (e2:e3:es)
     where 
        newSub = tcDeOksel (tcUnify sub (e1, e2))


-- ======================================================--
--
tcNewTypeVars :: TypeDef -> 
                 TypeNameSupply ->
                 AList Naam TVName

tcNewTypeVars (t, vl, c) ns = zip vl (tcName_sequence ns)



-- ======================================================--
--
tcGetGammaN :: AList Naam TVName ->
               ConstrAlt -> 
               [Naam] ->
               AList Naam TypeScheme

tcGetGammaN tvl (cname, cal) cparams 
   = zip cparams (map (Scheme [] . tcTDefSubst tvl) cal)



-- ======================================================--
--
tcTDefSubst :: AList Naam TVName ->
               TDefExpr ->
               TExpr

tcTDefSubst nameMap (TDefVar n)
   = f result 
     where
        f (Just tvn) = TVar tvn
        f Nothing    = TCons n []
        result = utLookup nameMap n

tcTDefSubst nameMap (TDefCons c al)
   = TCons c (map (tcTDefSubst nameMap) al)


-- ======================================================--
--
tcGetAllGammas :: AList Naam TVName ->
                  [ConstrAlt] ->
                  [[Naam]] ->
                  [AList Naam TypeScheme]

tcGetAllGammas tvl []           [] = []
-- note param lists cparamss must be ordered in
-- accordance with calts
tcGetAllGammas tvl (calt:calts) (cparams:cparamss) = 
      tcGetGammaN tvl calt cparams : 
         tcGetAllGammas tvl calts cparamss


-- ======================================================--
--
tcGetTypeDef :: [TypeDef] ->    -- type definitions
                [Naam] ->       -- list of constructors used here
                TypeDef

tcGetTypeDef tds cs 
   = if length tdefset == 0 
        then myFail "Undeclared constructors in use"
     else if length tdefset > 1
        then myFail "CASE expression contains mixed constructors"
     else head tdefset
     where
        tdefset = nub
                  [ (tname, ftvs, cl) |
                    (tname, ftvs, cl) <- tds,
                    usedc <- cs,
                    usedc `elem` (map first cl) ]


-- ==========================================================--
-- === 9.71 Type-checking lists of expressions            ===--
-- ==========================================================--

-- ======================================================--
--
tcl :: [TypeDef] ->
       TcTypeEnv     -> 
       TypeNameSupply  -> 
       [CExpr]       -> 
       Reply (Subst, [TExpr], [AnnExpr Naam TExpr]) Message

tcl tds gamma ns []
   = Ok (tcId_subst, [], [])
tcl tds gamma ns (e:es) 
   = tcl1 tds gamma ns0 es (tc tds gamma ns1 e)
     where
        (ns0, ns1) = tcSplit ns


-- ======================================================--
--
tcl1 tds gamma ns es (Fail m) = Fail m
tcl1 tds gamma ns es (Ok (phi, t, annotatedE)) 
   = tcl2 phi t (tcl tds (tcSub_te phi gamma) ns es) annotatedE


-- ======================================================--
--
tcl2 phi t (Fail m) annotatedE = Fail m
tcl2 phi t (Ok (psi, ts, annotatedEs)) annotatedE 
   = Ok (psi `tcScomp` phi, (tcSub_type psi t):ts, 
         annotatedE:annotatedEs)


-- ==========================================================--
-- === 9.72 Type-checking variables                       ===--
-- ==========================================================--

-- ======================================================--
--
tcvar :: [TypeDef] ->
         TcTypeEnv     -> 
         TypeNameSupply  -> 
         Naam        -> 
         Reply TypeInfo Message

tcvar tds gamma ns x = Ok (tcId_subst, finalType, (finalType, AVar x))
                       where
                          scheme = tcCharVal gamma x
                          finalType = tcNewinstance ns scheme


-- ======================================================--
--
tcNewinstance :: TypeNameSupply -> 
                 TypeScheme -> 
                 TExpr

tcNewinstance ns (Scheme scvs t) = tcSub_type phi t
                                   where 
                                      al  = scvs `zip` (tcName_sequence ns)
                                      phi = tcAl_to_subst al


-- ======================================================--
--
tcAl_to_subst :: AList TVName TVName -> 
                 Subst

tcAl_to_subst al = map2nd TVar al


-- ==========================================================--
-- === 9.73 Type-checking applications                    ===--
-- ==========================================================--

-- ======================================================--
--
tcap :: [TypeDef] ->
        TcTypeEnv     -> 
        TypeNameSupply  -> 
        CExpr         -> 
        CExpr         -> 
        Reply TypeInfo Message

tcap tds gamma ns e1 e2 = tcap1 tvn (tcl tds gamma ns' [e1, e2])
                          where
                             tvn = tcNext_name ns
                             ns' = tcDeplete ns


-- ======================================================--
--
tcap1 tvn (Fail m)
   = Fail m
tcap1 tvn (Ok (phi, [t1, t2], [ae1, ae2])) 
   = tcap2 tvn (tcUnify phi (t1, t2 `TArr` (TVar tvn))) [ae1, ae2]


-- ======================================================--
--
tcap2 tvn (Fail m) [ae1, ae2]
   = Fail m
tcap2 tvn (Ok phi) [ae1, ae2] 
   = Ok (phi, finalType, (finalType, AAp ae1 ae2))
     where
        finalType = tcApply_sub phi tvn


-- ==========================================================--
-- === 9.74 Type-checking lambda abstractions             ===--
-- ==========================================================--

-- ======================================================--
--
tclambda :: [TypeDef] ->
            TcTypeEnv     -> 
            TypeNameSupply  -> 
            Naam        -> 
            CExpr         -> 
            Reply TypeInfo Message

tclambda tds gamma ns x e = tclambda1 tvn x (tc tds gamma' ns' e)
                            where
                               ns' = tcDeplete ns
                               gamma' = tcNew_bvar (x, tvn): gamma
                               tvn = tcNext_name ns


-- ======================================================--
--
tclambda1 tvn x (Fail m) = Fail m

tclambda1 tvn x (Ok (phi, t, annotatedE)) = 
   Ok (phi, finalType, (finalType, ALam [x] annotatedE))
   where
      finalType = (tcApply_sub phi tvn) `TArr` t


-- ======================================================--
--
tcNew_bvar (x, tvn) = (x, Scheme [] (TVar tvn))


-- ==========================================================--
-- === 9.75 Type-checking let-expressions                 ===--
-- ==========================================================--

-- ======================================================--
--
tclet :: [TypeDef] ->
         TcTypeEnv     -> 
         TypeNameSupply  -> 
         [Naam]       -> 
         [CExpr]       -> 
         CExpr         -> 
         Reply TypeInfo Message

tclet tds gamma ns xs es e 
   = tclet1 tds gamma ns0 xs e rhsTypes
     where
        (ns0, ns1) = tcSplit ns
        rhsTypes = tcl tds gamma ns1 es
        

-- ======================================================--
--
tclet1 tds gamma ns xs e (Fail m) = Fail m

tclet1 tds gamma ns xs e (Ok (phi, ts, rhsAnnExprs)) 
   = tclet2 phi xs False (tc tds gamma'' ns1 e) rhsAnnExprs
     where
        gamma'' = tcAdd_decls gamma' ns0 xs ts
        gamma'  = tcSub_te phi gamma
        (ns0, ns1) = tcSplit ns


-- ======================================================--
--
tclet2 phi xs recFlag (Fail m) rhsAnnExprs = Fail m

tclet2 phi xs recFlag (Ok (phi', t, annotatedE)) rhsAnnExprs
   = Ok (phi' `tcScomp` phi, t, (t, ALet recFlag (zip xs rhsAnnExprs) annotatedE))


-- ======================================================--
--
tcAdd_decls :: TcTypeEnv     ->
               TypeNameSupply  -> 
               [Naam]       ->
               [TExpr]   ->
               TcTypeEnv

tcAdd_decls gamma ns xs ts = (xs `zip` schemes) ++ gamma
                             where
                                schemes = map (tcGenbar unknowns ns) ts
                                unknowns = tcUnknowns_te gamma


-- ======================================================--
--
tcGenbar unknowns ns t = Scheme (map second al) t'
                         where
                            al = scvs `zip` (tcName_sequence ns)
                            scvs = (nub (tcTvars_in t)) `tcBar` unknowns
                            t' = tcSub_type (tcAl_to_subst al) t



-- ==========================================================--
-- === 9.76 Type-checking letrec-expressions              ===--
-- ==========================================================--

-- ======================================================--
--
tcletrec :: [TypeDef] ->
            TcTypeEnv     -> 
            TypeNameSupply  -> 
            [Naam]       -> 
            [CExpr]       -> 
            CExpr         -> 
            Reply TypeInfo Message

tcletrec tds gamma ns xs es e 
   = tcletrec1 tds gamma ns0 xs nbvs e 
               (tcl tds (nbvs ++ gamma) ns1 es)
     where
        (ns0, ns') = tcSplit ns
        (ns1, ns2) = tcSplit ns'
        nbvs = tcNew_bvars xs ns2


-- ======================================================--
--
tcNew_bvars xs ns = map tcNew_bvar (xs `zip` (tcName_sequence ns))



-- ======================================================--
--
tcletrec1 tds gamma ns xs nbvs e (Fail m) = (Fail m)

tcletrec1 tds gamma ns xs nbvs e (Ok (phi, ts, rhsAnnExprs)) 
   = tcletrec2 tds gamma' ns xs nbvs' e (tcUnifyl phi (ts `zip` ts')) rhsAnnExprs
     where
        ts' = map tcOld_bvar nbvs'
        nbvs' = tcSub_te phi nbvs
        gamma' = tcSub_te phi gamma


-- ======================================================--
--
tcOld_bvar (x, Scheme [] t) = t


-- ======================================================--
--
tcletrec2 tds gamma ns xs nbvs e (Fail m) rhsAnnExprs = (Fail m)

tcletrec2 tds gamma ns xs nbvs e (Ok phi) rhsAnnExprs
   = tclet2 phi xs True (tc tds gamma'' ns1 e) rhsAnnExprs 
     where
        ts = map tcOld_bvar nbvs'
        nbvs' = tcSub_te phi nbvs
        gamma' = tcSub_te phi gamma
        gamma'' = tcAdd_decls gamma' ns0 (map first nbvs) ts
        (ns0, ns1) = tcSplit ns
        subnames = map first nbvs


-- ==========================================================--
-- === End                               TypeCheck5.m (1) ===--
-- ==========================================================--

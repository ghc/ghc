{-# OPTIONS -cpp #-}
------------------------------------------------------------------
-- A primop-table mangling program                              --
------------------------------------------------------------------

module Main where

import Parser
import Syntax

import Data.Char
import Data.List
import Data.Maybe ( catMaybes )
import System.Environment ( getArgs )

vecOptions :: Entry -> [(String,String,Int)]
vecOptions i =
    concat [vecs | OptionVector vecs <- opts i]

desugarVectorSpec :: Entry -> [Entry]
desugarVectorSpec i@(Section {}) = [i]
desugarVectorSpec i              = case vecOptions i of
                                     []  -> [i]
                                     vos -> map genVecEntry vos
  where
    genVecEntry :: (String,String,Int) -> Entry
    genVecEntry (con,repCon,n) =
        case i of
          PrimOpSpec {} ->
              PrimVecOpSpec { cons    = "(" ++ concat (intersperse " " [cons i, vecCat, show n, vecWidth]) ++ ")"
                            , name    = name'
                            , prefix  = pfx
                            , veclen  = n
                            , elemrep = con ++ "ElemRep"
                            , ty      = desugarTy (ty i)
                            , cat     = cat i
                            , desc    = desc i
                            , opts    = opts i
                            }
          PrimTypeSpec {} ->
              PrimVecTypeSpec { ty      = desugarTy (ty i)
                              , prefix  = pfx
                              , veclen  = n
                              , elemrep = con ++ "ElemRep"
                              , desc    = desc i
                              , opts    = opts i
                              }
          _ ->
              error "vector options can only be given for primops and primtypes"
      where
        vecCons       = con++"X"++show n++"#"
        vecCat        = conCat con
        vecWidth      = conWidth con
        pfx           = lowerHead con++"X"++show n
        vecTyName     = pfx++"PrimTy"

        name' | Just pre <- splitSuffix (name i) "Array#"     = pre++vec++"Array#"
              | Just pre <- splitSuffix (name i) "OffAddr#"   = pre++vec++"OffAddr#"
              | Just pre <- splitSuffix (name i) "ArrayAs#"   = pre++con++"ArrayAs"++vec++"#"
              | Just pre <- splitSuffix (name i) "OffAddrAs#" = pre++con++"OffAddrAs"++vec++"#"
              | otherwise                                     = init (name i)++vec ++"#"
          where
            vec = con++"X"++show n

        splitSuffix :: Eq a => [a] -> [a] -> Maybe [a]
        splitSuffix s suf
            | drop len s == suf = Just (take len s)
            | otherwise         = Nothing
          where
            len = length s - length suf                            

        lowerHead s = toLower (head s) : tail s

        desugarTy :: Ty -> Ty
        desugarTy (TyF s d)           = TyF (desugarTy s) (desugarTy d)
        desugarTy (TyC s d)           = TyC (desugarTy s) (desugarTy d)
        desugarTy (TyApp SCALAR [])   = TyApp (TyCon repCon) []
        desugarTy (TyApp VECTOR [])   = TyApp (VecTyCon vecCons vecTyName) []
        desugarTy (TyApp VECTUPLE []) = TyUTup (replicate n (TyApp (TyCon repCon) []))
        desugarTy (TyApp tycon ts)    = TyApp tycon (map desugarTy ts)
        desugarTy t@(TyVar {})        = t
        desugarTy (TyUTup ts)         = TyUTup (map desugarTy ts)

    conCat :: String -> String
    conCat "Int8"   = "IntVec"
    conCat "Int16"  = "IntVec"
    conCat "Int32"  = "IntVec"
    conCat "Int64"  = "IntVec"
    conCat "Word8"  = "WordVec"
    conCat "Word16" = "WordVec"
    conCat "Word32" = "WordVec"
    conCat "Word64" = "WordVec"
    conCat "Float"  = "FloatVec"
    conCat "Double" = "FloatVec"
    conCat con      = error $ "conCat: unknown type constructor " ++ con ++ "\n"

    conWidth :: String -> String
    conWidth "Int8"   = "W8"
    conWidth "Int16"  = "W16"
    conWidth "Int32"  = "W32"
    conWidth "Int64"  = "W64"
    conWidth "Word8"  = "W8"
    conWidth "Word16" = "W16"
    conWidth "Word32" = "W32"
    conWidth "Word64" = "W64"
    conWidth "Float"  = "W32"
    conWidth "Double" = "W64"
    conWidth con      = error $ "conWidth: unknown type constructor " ++ con ++ "\n"

main :: IO ()
main = getArgs >>= \args ->
       if length args /= 1 || head args `notElem` known_args
       then error ("usage: genprimopcode command < primops.txt > ...\n"
                   ++ "   where command is one of\n"
                   ++ unlines (map ("            "++) known_args)
                  )
       else
       do s <- getContents
          case parse s of
             Left err -> error ("parse error at " ++ (show err))
             Right p_o_specs@(Info _ entries)
                -> seq (sanityTop p_o_specs) (
                   case head args of

                      "--data-decl" 
                         -> putStr (gen_data_decl p_o_specs)

                      "--has-side-effects" 
                         -> putStr (gen_switch_from_attribs 
                                       "has_side_effects" 
                                       "primOpHasSideEffects" p_o_specs)

                      "--out-of-line" 
                         -> putStr (gen_switch_from_attribs 
                                       "out_of_line" 
                                       "primOpOutOfLine" p_o_specs)

                      "--commutable" 
                         -> putStr (gen_switch_from_attribs 
                                       "commutable" 
                                       "commutableOp" p_o_specs)

                      "--code-size"
                         -> putStr (gen_switch_from_attribs 
                                       "code_size"
                                       "primOpCodeSize" p_o_specs)

                      "--can-fail"
                         -> putStr (gen_switch_from_attribs
                                       "can_fail" 
                                       "primOpCanFail" p_o_specs)

                      "--strictness" 
                         -> putStr (gen_switch_from_attribs 
                                       "strictness" 
                                       "primOpStrictness" p_o_specs)

                      "--fixity"
                         -> putStr (gen_switch_from_attribs
                                       "fixity"
                                       "primOpFixity" p_o_specs)

                      "--primop-primop-info" 
                         -> putStr (gen_primop_info p_o_specs)

                      "--primop-tag" 
                         -> putStr (gen_primop_tag p_o_specs)

                      "--primop-list" 
                         -> putStr (gen_primop_list p_o_specs)

                      "--primop-vector-uniques" 
                         -> putStr (gen_primop_vector_uniques p_o_specs)

                      "--primop-vector-tys" 
                         -> putStr (gen_primop_vector_tys p_o_specs)

                      "--primop-vector-tys-exports" 
                         -> putStr (gen_primop_vector_tys_exports p_o_specs)

                      "--primop-vector-tycons" 
                         -> putStr (gen_primop_vector_tycons p_o_specs)

                      "--make-haskell-wrappers" 
                         -> putStr (gen_wrappers p_o_specs)
                        
                      "--make-haskell-source" 
                         -> putStr (gen_hs_source p_o_specs)

                      "--make-ext-core-source"
                         -> putStr (gen_ext_core_source entries)

                      "--make-latex-doc"
                         -> putStr (gen_latex_doc p_o_specs)

                      _ -> error "Should not happen, known_args out of sync?"
                   )

known_args :: [String]
known_args 
   = [ "--data-decl",
       "--has-side-effects",
       "--out-of-line",
       "--commutable",
       "--code-size",
       "--can-fail",
       "--strictness",
       "--fixity",
       "--primop-primop-info",
       "--primop-tag",
       "--primop-list",
       "--primop-vector-uniques",
       "--primop-vector-tys",
       "--primop-vector-tys-exports",
       "--primop-vector-tycons",
       "--make-haskell-wrappers",
       "--make-haskell-source",
       "--make-ext-core-source",
       "--make-latex-doc"
     ]

------------------------------------------------------------------
-- Code generators -----------------------------------------------
------------------------------------------------------------------

gen_hs_source :: Info -> String
gen_hs_source (Info defaults entries) =
       "{-\n"
    ++ "This is a generated file (generated by genprimopcode).\n"
    ++ "It is not code to actually be used. Its only purpose is to be\n"
    ++ "consumed by haddock.\n"
    ++ "-}\n"
    ++ "\n"
        ++ "-----------------------------------------------------------------------------\n"
        ++ "-- |\n"
        ++ "-- Module      :  GHC.Prim\n"
        ++ "-- \n"
        ++ "-- Maintainer  :  ghc-devs@haskell.org\n"
        ++ "-- Stability   :  internal\n"
        ++ "-- Portability :  non-portable (GHC extensions)\n"
        ++ "--\n"
        ++ "-- GHC\'s primitive types and operations.\n"
        ++ "-- Use GHC.Exts from the base package instead of importing this\n"
        ++ "-- module directly.\n"
        ++ "--\n" 
        ++ "-----------------------------------------------------------------------------\n"
        ++ "{-# LANGUAGE MagicHash, MultiParamTypeClasses, NoImplicitPrelude, UnboxedTuples #-}\n"
        ++ "module GHC.Prim (\n"
        ++ unlines (map (("\t" ++) . hdr) entries')
        ++ ") where\n"
    ++ "\n"
    ++ "{-\n"
        ++ unlines (map opt defaults)
    ++ "-}\n"
    ++ "import GHC.Types (Coercible)\n"
        ++ unlines (concatMap ent entries') ++ "\n\n\n"
     where entries' = concatMap desugarVectorSpec entries

           opt (OptionFalse n)    = n ++ " = False"
           opt (OptionTrue n)     = n ++ " = True"
           opt (OptionString n v) = n ++ " = { " ++ v ++ "}"
           opt (OptionInteger n v) = n ++ " = " ++ show v
           opt (OptionVector _)    = ""
           opt (OptionFixity mf) = "fixity" ++ " = " ++ show mf

           hdr s@(Section {})                                    = sec s
           hdr (PrimOpSpec { name = n })                         = wrapOp n ++ ","
           hdr (PrimVecOpSpec { name = n })                      = wrapOp n ++ ","
           hdr (PseudoOpSpec { name = n })                       = wrapOp n ++ ","
           hdr (PrimTypeSpec { ty = TyApp (TyCon n) _ })         = wrapTy n ++ ","
           hdr (PrimTypeSpec {})                                 = error $ "Illegal type spec"
           hdr (PrimClassSpec { cls = TyApp (TyCon n) _ })       = wrapTy n ++ ","
           hdr (PrimClassSpec {})                                = error "Illegal class spec"
           hdr (PrimVecTypeSpec { ty = TyApp (VecTyCon n _) _ }) = wrapTy n ++ ","
           hdr (PrimVecTypeSpec {})                              = error $ "Illegal type spec"

           ent   (Section {})         = []
           ent o@(PrimOpSpec {})      = spec o
           ent o@(PrimVecOpSpec {})   = spec o
           ent o@(PrimTypeSpec {})    = spec o
           ent o@(PrimClassSpec {})   = spec o
           ent o@(PrimVecTypeSpec {}) = spec o
           ent o@(PseudoOpSpec {})    = spec o

           sec s = "\n-- * " ++ escape (title s) ++ "\n"
                        ++ (unlines $ map ("-- " ++ ) $ lines $ unlatex $ escape $ "|" ++ desc s) ++ "\n"

           spec o = comm : decls
             where decls = case o of
                        PrimOpSpec { name = n, ty = t, opts = options } ->
                            [ pprFixity fixity n | OptionFixity (Just fixity) <- options ]
                            ++
                            [ wrapOp n ++ " :: " ++ pprTy t,
                              wrapOp n ++ " = let x = x in x" ]
                        PrimVecOpSpec { name = n, ty = t, opts = options } ->
                            [ pprFixity fixity n | OptionFixity (Just fixity) <- options ]
                            ++
                            [ wrapOp n ++ " :: " ++ pprTy t,
                              wrapOp n ++ " = let x = x in x" ]
                        PseudoOpSpec { name = n, ty = t } ->
                            [ wrapOp n ++ " :: " ++ pprTy t,
                              wrapOp n ++ " = let x = x in x" ]
                        PrimTypeSpec { ty = t }   ->
                            [ "data " ++ pprTy t ]
                        PrimClassSpec { cls = t }   ->
                            [ "class " ++ pprTy t ]
                        PrimVecTypeSpec { ty = t }   ->
                            [ "data " ++ pprTy t ]
                        Section { } -> []

                   comm = case (desc o) of
                        [] -> ""
                        d -> "\n" ++ (unlines $ map ("-- " ++ ) $ lines $ unlatex $ escape $ "|" ++ d)

           wrapOp nm | isAlpha (head nm) = nm
                     | otherwise         = "(" ++ nm ++ ")"
           wrapTy nm | isAlpha (head nm) = nm
                     | otherwise         = "(" ++ nm ++ ")"
           unlatex s = case s of
                '\\':'t':'e':'x':'t':'t':'t':'{':cs -> markup "@" "@" cs
                '{':'\\':'t':'t':cs -> markup "@" "@" cs
                '{':'\\':'i':'t':cs -> markup "/" "/" cs
                c : cs -> c : unlatex cs
                [] -> []
           markup s t xs = s ++ mk (dropWhile isSpace xs)
                where mk ""        = t
                      mk ('\n':cs) = ' ' : mk cs
                      mk ('}':cs)  = t ++ unlatex cs
                      mk (c:cs)    = c : mk cs
           escape = concatMap (\c -> if c `elem` special then '\\':c:[] else c:[])
                where special = "/'`\"@<"

           pprFixity (Fixity i d) n = pprFixityDir d ++ " " ++ show i ++ " " ++ n

pprTy :: Ty -> String
pprTy = pty
    where
          pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
          pty (TyC t1 t2) = pbty t1 ++ " => " ++ pty t2
          pty t      = pbty t
          pbty (TyApp tc ts) = show tc ++ concat (map (' ' :) (map paty ts))
          pbty (TyUTup ts)   = "(# "
                            ++ concat (intersperse "," (map pty ts))
                            ++ " #)"
          pbty t             = paty t

          paty (TyVar tv)    = tv
          paty t             = "(" ++ pty t ++ ")"
--
-- Generates the type environment that the stand-alone External Core tools use.
gen_ext_core_source :: [Entry] -> String
gen_ext_core_source entries =
      "-----------------------------------------------------------------------\n"
   ++ "-- This module is automatically generated by the GHC utility\n"
   ++ "-- \"genprimopcode\". Do not edit!\n"
   ++ "-----------------------------------------------------------------------\n"
   ++ "module Language.Core.PrimEnv(primTcs, primVals, intLitTypes, ratLitTypes,"
   ++ "\n charLitTypes, stringLitTypes) where\nimport Language.Core.Core"
   ++ "\nimport Language.Core.Encoding\n\n"
   ++ "primTcs :: [(Tcon, Kind)]\n"
   ++ "primTcs = [\n"
   ++ printList tcEnt entries 
   ++ "   ]\n"
   ++ "primVals :: [(Var, Ty)]\n"
   ++ "primVals = [\n"
   ++ printList valEnt entries
   ++ "]\n"
   ++ "intLitTypes :: [Ty]\n"
   ++ "intLitTypes = [\n"
   ++ printList tyEnt (intLitTys entries)
   ++ "]\n"
   ++ "ratLitTypes :: [Ty]\n"
   ++ "ratLitTypes = [\n"
   ++ printList tyEnt (ratLitTys entries)
   ++ "]\n"
   ++ "charLitTypes :: [Ty]\n"
   ++ "charLitTypes = [\n"
   ++ printList tyEnt (charLitTys entries)
   ++ "]\n"
   ++ "stringLitTypes :: [Ty]\n"
   ++ "stringLitTypes = [\n"
   ++ printList tyEnt (stringLitTys entries)
   ++ "]\n\n"

  where printList f = concat . intersperse ",\n" . filter (not . null) . map f   
        tcEnt  (PrimTypeSpec {ty=t}) = 
           case t of
            TyApp tc args -> parens (show tc) (tcKind tc args)
            _             -> error ("tcEnt: type in PrimTypeSpec is not a type"
                              ++ " constructor: " ++ show t)  
        tcEnt  _                = ""
        -- hack alert!
        -- The primops.txt.pp format doesn't have enough information in it to 
        -- print out some of the information that ext-core needs (like kinds,
        -- and later on in this code, module names) so we special-case. An
        -- alternative would be to refer to things indirectly and hard-wire
        -- certain things (e.g., the kind of the Any constructor, here) into
        -- ext-core's Prims module again.
        tcKind (TyCon "Any") _               = "Klifted"
        tcKind tc [] | last (show tc) == '#' = "Kunlifted"
        tcKind _  [] | otherwise             = "Klifted"
        -- assumes that all type arguments are lifted (are they?)
        tcKind tc (_v:as)                    = "(Karrow Klifted " ++ tcKind tc as
                                               ++ ")"
        valEnt (PseudoOpSpec {name=n, ty=t}) = valEntry n t
        valEnt (PrimOpSpec {name=n, ty=t})   = valEntry n t
        valEnt _                             = ""
        valEntry name' ty' = parens name' (mkForallTy (freeTvars ty') (pty ty'))
            where pty (TyF t1 t2) = mkFunTy (pty t1) (pty t2)
                  pty (TyC t1 t2) = mkFunTy (pty t1) (pty t2)
                  pty (TyApp tc ts) = mkTconApp (mkTcon tc) (map pty ts)  
                  pty (TyUTup ts)   = mkUtupleTy (map pty ts)
                  pty (TyVar tv)    = paren $ "Tvar \"" ++ tv ++ "\""

                  mkFunTy s1 s2 = "Tapp " ++ (paren ("Tapp (Tcon tcArrow)" 
                                               ++ " " ++ paren s1))
                                          ++ " " ++ paren s2
                  mkTconApp tc args = foldl tapp tc args
                  mkTcon tc = paren $ "Tcon " ++ paren (qualify True (show tc))
                  mkUtupleTy args = foldl tapp (tcUTuple (length args)) args   
                  mkForallTy [] t = t
                  mkForallTy vs t = foldr 
                     (\ v s -> "Tforall " ++ 
                               (paren (quote v ++ ", " ++ vKind v)) ++ " "
                               ++ paren s) t vs

                  -- hack alert!
                  vKind "o" = "Kopen"
                  vKind _   = "Klifted"

                  freeTvars (TyF t1 t2)   = freeTvars t1 `union` freeTvars t2
                  freeTvars (TyC t1 t2)   = freeTvars t1 `union` freeTvars t2
                  freeTvars (TyApp _ tys) = freeTvarss tys
                  freeTvars (TyVar v)     = [v]
                  freeTvars (TyUTup tys)  = freeTvarss tys
                  freeTvarss = nub . concatMap freeTvars

                  tapp s nextArg = paren $ "Tapp " ++ s ++ " " ++ paren nextArg
                  tcUTuple n = paren $ "Tcon " ++ paren (qualify False $ "Z" 
                                                          ++ show n ++ "H")

        tyEnt (PrimTypeSpec {ty=(TyApp tc _args)}) = "   " ++ paren ("Tcon " ++
                                                       (paren (qualify True (show tc))))
        tyEnt _ = ""

        -- more hacks. might be better to do this on the ext-core side,
        -- as per earlier comment
        qualify _ tc | tc == "Bool" = "Just boolMname" ++ ", " 
                                                ++ ze True tc
        qualify _ tc | tc == "()"  = "Just baseMname" ++ ", "
                                                ++ ze True tc
        qualify enc tc = "Just primMname" ++ ", " ++ (ze enc tc)
        ze enc tc      = (if enc then "zEncodeString " else "")
                                      ++ "\"" ++ tc ++ "\""

        intLitTys = prefixes ["Int", "Word", "Addr", "Char"]
        ratLitTys = prefixes ["Float", "Double"]
        charLitTys = prefixes ["Char"]
        stringLitTys = prefixes ["Addr"]
        prefixes ps = filter (\ t ->
                        case t of
                          (PrimTypeSpec {ty=(TyApp tc _args)}) ->
                            any (\ p -> p `isPrefixOf` show tc) ps
                          _ -> False)

        parens n ty' = "      (zEncodeString \"" ++ n ++ "\", " ++ ty' ++ ")"
        paren s = "(" ++ s ++ ")"
        quote s = "\"" ++ s ++ "\""

gen_latex_doc :: Info -> String
gen_latex_doc (Info defaults entries)
   = "\\primopdefaults{" 
         ++ mk_options defaults
         ++ "}\n"
     ++ (concat (map mk_entry entries))
     where mk_entry (PrimOpSpec {cons=constr,name=n,ty=t,cat=c,desc=d,opts=o}) =
                 "\\primopdesc{" 
                 ++ latex_encode constr ++ "}{"
                 ++ latex_encode n ++ "}{"
                 ++ latex_encode (zencode n) ++ "}{"
                 ++ latex_encode (show c) ++ "}{"
                 ++ latex_encode (mk_source_ty t) ++ "}{"
                 ++ latex_encode (mk_core_ty t) ++ "}{"
                 ++ d ++ "}{"
                 ++ mk_options o
                 ++ "}\n"
           mk_entry (PrimVecOpSpec {}) =
                 ""
           mk_entry (Section {title=ti,desc=d}) =
                 "\\primopsection{" 
                 ++ latex_encode ti ++ "}{"
                 ++ d ++ "}\n"
           mk_entry (PrimTypeSpec {ty=t,desc=d,opts=o}) =
                 "\\primtypespec{"
                 ++ latex_encode (mk_source_ty t) ++ "}{"
                 ++ latex_encode (mk_core_ty t) ++ "}{"
                 ++ d ++ "}{"
                 ++ mk_options o
                 ++ "}\n"
           mk_entry (PrimClassSpec {cls=t,desc=d,opts=o}) =
                 "\\primclassspec{"
                 ++ latex_encode (mk_source_ty t) ++ "}{"
                 ++ latex_encode (mk_core_ty t) ++ "}{"
                 ++ d ++ "}{"
                 ++ mk_options o
                 ++ "}\n"
           mk_entry (PrimVecTypeSpec {}) =
                 ""
           mk_entry (PseudoOpSpec {name=n,ty=t,desc=d,opts=o}) =
                 "\\pseudoopspec{"
                 ++ latex_encode (zencode n) ++ "}{"
                 ++ latex_encode (mk_source_ty t) ++ "}{"
                 ++ latex_encode (mk_core_ty t) ++ "}{"
                 ++ d ++ "}{"
                 ++ mk_options o
                 ++ "}\n"
           mk_source_ty typ = pty typ
             where pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
                   pty (TyC t1 t2) = pbty t1 ++ " => " ++ pty t2
                   pty t = pbty t
                   pbty (TyApp tc ts) = show tc ++ (concat (map (' ':) (map paty ts)))
                   pbty (TyUTup ts) = "(# " ++ (concat (intersperse "," (map pty ts))) ++ " #)"
                   pbty t = paty t
                   paty (TyVar tv) = tv
                   paty t = "(" ++ pty t ++ ")"
           
           mk_core_ty typ = foralls ++ (pty typ)
             where pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
                   pty (TyC t1 t2) = pbty t1 ++ " => " ++ pty t2
                   pty t = pbty t
                   pbty (TyApp tc ts) = (zencode (show tc)) ++ (concat (map (' ':) (map paty ts)))
                   pbty (TyUTup ts) = (zencode (utuplenm (length ts))) ++ (concat ((map (' ':) (map paty ts))))
                   pbty t = paty t
                   paty (TyVar tv) = zencode tv
                   paty (TyApp tc []) = zencode (show tc)
                   paty t = "(" ++ pty t ++ ")"
                   utuplenm 1 = "(# #)"
                   utuplenm n = "(#" ++ (replicate (n-1) ',') ++ "#)"
                   foralls = if tvars == [] then "" else "%forall " ++ (tbinds tvars)
                   tvars = tvars_of typ
                   tbinds [] = ". " 
                   tbinds ("o":tbs) = "(o::?) " ++ (tbinds tbs)
                   tbinds (tv:tbs) = tv ++ " " ++ (tbinds tbs)
           tvars_of (TyF t1 t2) = tvars_of t1 `union` tvars_of t2
           tvars_of (TyC t1 t2) = tvars_of t1 `union` tvars_of t2
           tvars_of (TyApp _ ts) = foldl union [] (map tvars_of ts)
           tvars_of (TyUTup ts) = foldr union [] (map tvars_of ts)
           tvars_of (TyVar tv) = [tv]
           
           mk_options o =
             "\\primoptions{"
              ++ mk_has_side_effects o ++ "}{"
              ++ mk_out_of_line o ++ "}{"
              ++ mk_commutable o ++ "}{"
              ++ mk_needs_wrapper o ++ "}{"
              ++ mk_can_fail o ++ "}{"
              ++ mk_fixity o ++ "}{"
              ++ latex_encode (mk_strictness o) ++ "}{"
              ++ "}"

           mk_has_side_effects o = mk_bool_opt o "has_side_effects" "Has side effects." "Has no side effects."
           mk_out_of_line o = mk_bool_opt o "out_of_line" "Implemented out of line." "Implemented in line."
           mk_commutable o = mk_bool_opt o "commutable" "Commutable." "Not commutable."
           mk_needs_wrapper o = mk_bool_opt o "needs_wrapper" "Needs wrapper." "Needs no wrapper."
           mk_can_fail o = mk_bool_opt o "can_fail" "Can fail." "Cannot fail."

           mk_bool_opt o opt_name if_true if_false =
             case lookup_attrib opt_name o of
               Just (OptionTrue _) -> if_true
               Just (OptionFalse _) -> if_false
               Just (OptionString _ _) -> error "String value for boolean option"
               Just (OptionInteger _ _) -> error "Integer value for boolean option"
               Just (OptionFixity _) -> error "Fixity value for boolean option"
               Just (OptionVector _) -> error "vector template for boolean option"
               Nothing -> ""
           
           mk_strictness o = 
             case lookup_attrib "strictness" o of
               Just (OptionString _ s) -> s  -- for now
               Just _ -> error "Wrong value for strictness"
               Nothing -> "" 

           mk_fixity o = case lookup_attrib "fixity" o of
             Just (OptionFixity (Just (Fixity i d)))
               -> pprFixityDir d ++ " " ++ show i
             _ -> ""

           zencode xs =
             case maybe_tuple xs of
                Just n  -> n            -- Tuples go to Z2T etc
                Nothing -> concat (map encode_ch xs)
             where
               maybe_tuple "(# #)" = Just("Z1H")
               maybe_tuple ('(' : '#' : cs) = case count_commas (0::Int) cs of
                                                (n, '#' : ')' : _) -> Just ('Z' : shows (n+1) "H")
                                                _                  -> Nothing
               maybe_tuple "()" = Just("Z0T")
               maybe_tuple ('(' : cs)       = case count_commas (0::Int) cs of
                                                (n, ')' : _) -> Just ('Z' : shows (n+1) "T")
                                                _            -> Nothing
               maybe_tuple _                 = Nothing
               
               count_commas :: Int -> String -> (Int, String)
               count_commas n (',' : cs) = count_commas (n+1) cs
               count_commas n cs          = (n,cs)
               
               unencodedChar :: Char -> Bool    -- True for chars that don't need encoding
               unencodedChar 'Z' = False
               unencodedChar 'z' = False
               unencodedChar c   = isAlphaNum c
               
               encode_ch :: Char -> String
               encode_ch c | unencodedChar c = [c]      -- Common case first
               
               -- Constructors
               encode_ch '('  = "ZL"    -- Needed for things like (,), and (->)
               encode_ch ')'  = "ZR"    -- For symmetry with (
               encode_ch '['  = "ZM"
               encode_ch ']'  = "ZN"
               encode_ch ':'  = "ZC"
               encode_ch 'Z'  = "ZZ"
               
               -- Variables
               encode_ch 'z'  = "zz"
               encode_ch '&'  = "za"
               encode_ch '|'  = "zb"
               encode_ch '^'  = "zc"
               encode_ch '$'  = "zd"
               encode_ch '='  = "ze"
               encode_ch '>'  = "zg"
               encode_ch '#'  = "zh"
               encode_ch '.'  = "zi"
               encode_ch '<'  = "zl"
               encode_ch '-'  = "zm"
               encode_ch '!'  = "zn"
               encode_ch '+'  = "zp"
               encode_ch '\'' = "zq"
               encode_ch '\\' = "zr"
               encode_ch '/'  = "zs"
               encode_ch '*'  = "zt"
               encode_ch '_'  = "zu"
               encode_ch '%'  = "zv"
               encode_ch c    = 'z' : shows (ord c) "U"
                       
           latex_encode [] = []
           latex_encode (c:cs) | c `elem` "#$%&_^{}" = "\\" ++ c:(latex_encode cs)
           latex_encode ('~':cs) = "\\verb!~!" ++ (latex_encode cs)
           latex_encode ('\\':cs) = "$\\backslash$" ++ (latex_encode cs)
           latex_encode (c:cs) = c:(latex_encode cs)

gen_wrappers :: Info -> String
gen_wrappers (Info _ entries)
   = "{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}\n"
        -- Dependencies on Prelude must be explicit in libraries/base, but we
        -- don't need the Prelude here so we add NoImplicitPrelude.
     ++ "module GHC.PrimopWrappers where\n" 
     ++ "import qualified GHC.Prim\n" 
     ++ "import GHC.Tuple ()\n"
     ++ "import GHC.Prim (" ++ types ++ ")\n"
     ++ unlines (concatMap f specs)
     where
        specs = filter (not.dodgy) $
                filter (not.is_llvm_only) $
                filter is_primop entries
        tycons = foldr union [] $ map (tyconsIn . ty) specs
        tycons' = filter (`notElem` [TyCon "()", TyCon "Bool"]) tycons
        types = concat $ intersperse ", " $ map show tycons'
        f spec = let args = map (\n -> "a" ++ show n) [1 .. arity (ty spec)]
                     src_name = wrap (name spec)
                     lhs = src_name ++ " " ++ unwords args
                     rhs = "(GHC.Prim." ++ name spec ++ ") " ++ unwords args
                 in ["{-# NOINLINE " ++ src_name ++ " #-}",
                     src_name ++ " :: " ++ pprTy (ty spec),
                     lhs ++ " = " ++ rhs]
        wrap nm | isLower (head nm) = nm
                | otherwise = "(" ++ nm ++ ")"

        dodgy spec
           = name spec `elem` 
             [-- C code generator can't handle these
              "seq#", 
              "tagToEnum#",
              -- not interested in parallel support
              "par#", "parGlobal#", "parLocal#", "parAt#", 
              "parAtAbs#", "parAtRel#", "parAtForNow#" 
             ]

        is_llvm_only :: Entry -> Bool
        is_llvm_only entry =
            case lookup_attrib "llvm_only" (opts entry) of
              Just (OptionTrue _) -> True
              _                   -> False

gen_primop_list :: Info -> String
gen_primop_list (Info _ entries)
   = unlines (
        [      "   [" ++ cons first       ]
        ++
        map (\p -> "   , " ++ cons p) rest
        ++ 
        [     "   ]"     ]
     ) where (first:rest) = concatMap desugarVectorSpec (filter is_primop entries)

mIN_VECTOR_UNIQUE :: Int
mIN_VECTOR_UNIQUE = 300

gen_primop_vector_uniques :: Info -> String
gen_primop_vector_uniques (Info _ entries)
   = unlines $
     concatMap mkVecUnique (specs `zip` [mIN_VECTOR_UNIQUE..])
  where
    specs = concatMap desugarVectorSpec (filter is_vector (filter is_primtype entries))

    mkVecUnique :: (Entry, Int) -> [String]
    mkVecUnique (i, unique) =
        [ key_id ++ " :: Unique"
        , key_id ++ " = mkPreludeTyConUnique " ++ show unique
        ]
      where
        key_id = prefix i ++ "PrimTyConKey"

gen_primop_vector_tys :: Info -> String
gen_primop_vector_tys (Info _ entries)
   = unlines $
     concatMap mkVecTypes specs
  where
    specs = concatMap desugarVectorSpec (filter is_vector (filter is_primtype entries))

    mkVecTypes :: Entry -> [String]
    mkVecTypes i =
        [ name_id ++ " :: Name"
        , name_id ++ " = mkPrimTc (fsLit \"" ++ pprTy (ty i) ++ "\") " ++ key_id ++ " " ++ tycon_id
        , ty_id ++ " :: Type"
        , ty_id ++ " = mkTyConTy " ++ tycon_id
        , tycon_id ++ " :: TyCon"
        , tycon_id ++ " = pcPrimTyCon0 " ++ name_id ++
                      " (VecRep " ++ show (veclen i) ++ " " ++ elemrep i ++ ")"
        ]
      where
        key_id   = prefix i ++ "PrimTyConKey"
        name_id  = prefix i ++ "PrimTyConName"
        ty_id    = prefix i ++ "PrimTy"
        tycon_id = prefix i ++ "PrimTyCon"

gen_primop_vector_tys_exports :: Info -> String
gen_primop_vector_tys_exports (Info _ entries)
   = unlines $
    map mkVecTypes specs
  where
    specs = concatMap desugarVectorSpec (filter is_vector (filter is_primtype entries))

    mkVecTypes :: Entry -> String
    mkVecTypes i =
        "\t" ++ ty_id ++ ", " ++ tycon_id ++ ","
      where
        ty_id    = prefix i ++ "PrimTy"
        tycon_id = prefix i ++ "PrimTyCon"

gen_primop_vector_tycons :: Info -> String
gen_primop_vector_tycons (Info _ entries)
   = unlines $
     map mkVecTypes specs
  where
    specs = concatMap desugarVectorSpec (filter is_vector (filter is_primtype entries))

    mkVecTypes :: Entry -> String
    mkVecTypes i =
        "    , " ++ tycon_id
      where
        tycon_id = prefix i ++ "PrimTyCon"

gen_primop_tag :: Info -> String
gen_primop_tag (Info _ entries)
   = unlines (max_def_type : max_def :
              tagOf_type : zipWith f primop_entries [1 :: Int ..])
     where
        primop_entries = concatMap desugarVectorSpec $ filter is_primop entries
        tagOf_type = "tagOf_PrimOp :: PrimOp -> FastInt"
        f i n = "tagOf_PrimOp " ++ cons i ++ " = _ILIT(" ++ show n ++ ")"
        max_def_type = "maxPrimOpTag :: Int"
        max_def      = "maxPrimOpTag = " ++ show (length primop_entries)

gen_data_decl :: Info -> String
gen_data_decl (Info _ entries) =
    "data PrimOp\n   = " ++ head conss ++ "\n"
     ++ unlines (map ("   | "++) (tail conss))
  where
    conss = map genCons (filter is_primop entries)
            
    genCons :: Entry -> String
    genCons entry =
        case vecOptions entry of
          [] -> cons entry
          _  -> cons entry ++ " PrimOpVecCat Length Width"

gen_switch_from_attribs :: String -> String -> Info -> String
gen_switch_from_attribs attrib_name fn_name (Info defaults entries)
   = let defv = lookup_attrib attrib_name defaults
         alternatives = catMaybes (map mkAlt (filter is_primop entries))

         getAltRhs (OptionFalse _)    = "False"
         getAltRhs (OptionTrue _)     = "True"
         getAltRhs (OptionInteger _ i) = show i
         getAltRhs (OptionString _ s) = s
         getAltRhs (OptionVector _) = "True"
         getAltRhs (OptionFixity mf) = show mf

         mkAlt po
            = case lookup_attrib attrib_name (opts po) of
                 Nothing -> Nothing
                 Just xx -> case vecOptions po of
                              [] -> Just (fn_name ++ " " ++ cons po ++ " = " ++ getAltRhs xx)
                              _  -> Just (fn_name ++ " (" ++ cons po ++ " _ _ _) = " ++ getAltRhs xx)

     in
         case defv of
            Nothing -> error ("gen_switch_from: " ++ attrib_name)
            Just xx 
               -> unlines alternatives
                  ++ fn_name ++ " _ = " ++ getAltRhs xx ++ "\n"

------------------------------------------------------------------
-- Create PrimOpInfo text from PrimOpSpecs -----------------------
------------------------------------------------------------------

gen_primop_info :: Info -> String
gen_primop_info (Info _ entries)
   = unlines (map mkPOItext (concatMap desugarVectorSpec (filter is_primop entries)))

mkPOItext :: Entry -> String
mkPOItext i = mkPOI_LHS_text i ++ mkPOI_RHS_text i

mkPOI_LHS_text :: Entry -> String
mkPOI_LHS_text i
   = "primOpInfo " ++ cons i ++ " = "

mkPOI_RHS_text :: Entry -> String
mkPOI_RHS_text i
   = case cat i of
        Compare 
           -> case ty i of
                 TyF t1 (TyF _ _) 
                    -> "mkCompare " ++ sl_name i ++ ppType t1
                 _ -> error "Type error in comparison op"
        Monadic
           -> case ty i of
                 TyF t1 _
                    -> "mkMonadic " ++ sl_name i ++ ppType t1
                 _ -> error "Type error in monadic op"
        Dyadic
           -> case ty i of
                 TyF t1 (TyF _ _)
                    -> "mkDyadic " ++ sl_name i ++ ppType t1
                 _ -> error "Type error in dyadic op"
        GenPrimOp
           -> let (argTys, resTy) = flatTys (ty i)
                  tvs = nub (tvsIn (ty i))
              in
                  "mkGenPrimOp " ++ sl_name i ++ " " 
                      ++ listify (map ppTyVar tvs) ++ " "
                      ++ listify (map ppType argTys) ++ " "
                      ++ "(" ++ ppType resTy ++ ")"

sl_name :: Entry -> String
sl_name i = "(fsLit \"" ++ name i ++ "\") "

ppTyVar :: String -> String
ppTyVar "a" = "alphaTyVar"
ppTyVar "b" = "betaTyVar"
ppTyVar "c" = "gammaTyVar"
ppTyVar "s" = "deltaTyVar"
ppTyVar "o" = "openAlphaTyVar"
ppTyVar _   = error "Unknown type var"

ppType :: Ty -> String
ppType (TyApp (TyCon "Any")         []) = "anyTy"
ppType (TyApp (TyCon "Bool")        []) = "boolTy"

ppType (TyApp (TyCon "Int#")        []) = "intPrimTy"
ppType (TyApp (TyCon "Int32#")      []) = "int32PrimTy"
ppType (TyApp (TyCon "Int64#")      []) = "int64PrimTy"
ppType (TyApp (TyCon "Char#")       []) = "charPrimTy"
ppType (TyApp (TyCon "Word#")       []) = "wordPrimTy"
ppType (TyApp (TyCon "Word32#")     []) = "word32PrimTy"
ppType (TyApp (TyCon "Word64#")     []) = "word64PrimTy"
ppType (TyApp (TyCon "Addr#")       []) = "addrPrimTy"
ppType (TyApp (TyCon "Float#")      []) = "floatPrimTy"
ppType (TyApp (TyCon "Double#")     []) = "doublePrimTy"
ppType (TyApp (TyCon "ByteArray#")  []) = "byteArrayPrimTy"
ppType (TyApp (TyCon "RealWorld")   []) = "realWorldTy"
ppType (TyApp (TyCon "ThreadId#")   []) = "threadIdPrimTy"
ppType (TyApp (TyCon "ForeignObj#") []) = "foreignObjPrimTy"
ppType (TyApp (TyCon "BCO#")        []) = "bcoPrimTy"
ppType (TyApp (TyCon "()")          []) = "unitTy"      -- unitTy is TysWiredIn's name for ()

ppType (TyVar "a")                      = "alphaTy"
ppType (TyVar "b")                      = "betaTy"
ppType (TyVar "c")                      = "gammaTy"
ppType (TyVar "s")                      = "deltaTy"
ppType (TyVar "o")                      = "openAlphaTy"

ppType (TyApp (TyCon "State#") [x])             = "mkStatePrimTy " ++ ppType x
ppType (TyApp (TyCon "MutVar#") [x,y])          = "mkMutVarPrimTy " ++ ppType x 
                                                   ++ " " ++ ppType y
ppType (TyApp (TyCon "MutableArray#") [x,y])    = "mkMutableArrayPrimTy " ++ ppType x
                                                   ++ " " ++ ppType y
ppType (TyApp (TyCon "MutableArrayArray#") [x]) = "mkMutableArrayArrayPrimTy " ++ ppType x
ppType (TyApp (TyCon "MutableByteArray#") [x])  = "mkMutableByteArrayPrimTy " 
                                                   ++ ppType x
ppType (TyApp (TyCon "Array#") [x])             = "mkArrayPrimTy " ++ ppType x
ppType (TyApp (TyCon "ArrayArray#") [])         = "mkArrayArrayPrimTy"


ppType (TyApp (TyCon "Weak#")       [x]) = "mkWeakPrimTy " ++ ppType x
ppType (TyApp (TyCon "StablePtr#")  [x]) = "mkStablePtrPrimTy " ++ ppType x
ppType (TyApp (TyCon "StableName#") [x]) = "mkStableNamePrimTy " ++ ppType x

ppType (TyApp (TyCon "MVar#") [x,y])     = "mkMVarPrimTy " ++ ppType x 
                                           ++ " " ++ ppType y
ppType (TyApp (TyCon "TVar#") [x,y])     = "mkTVarPrimTy " ++ ppType x 
                                           ++ " " ++ ppType y

ppType (TyApp (VecTyCon _ pptc) [])      = pptc

ppType (TyUTup ts) = "(mkTupleTy UnboxedTuple " 
                     ++ listify (map ppType ts) ++ ")"

ppType (TyF s d) = "(mkFunTy (" ++ ppType s ++ ") (" ++ ppType d ++ "))"
ppType (TyC s d) = "(mkFunTy (" ++ ppType s ++ ") (" ++ ppType d ++ "))"

ppType other
   = error ("ppType: can't handle: " ++ show other ++ "\n")

pprFixityDir :: FixityDirection -> String
pprFixityDir InfixN = "infix"
pprFixityDir InfixL = "infixl"
pprFixityDir InfixR = "infixr"

listify :: [String] -> String
listify ss = "[" ++ concat (intersperse ", " ss) ++ "]"

flatTys :: Ty -> ([Ty],Ty)
flatTys (TyF t1 t2) = case flatTys t2 of (ts,t) -> (t1:ts,t)
flatTys (TyC t1 t2) = case flatTys t2 of (ts,t) -> (t1:ts,t)
flatTys other       = ([],other)

tvsIn :: Ty -> [TyVar]
tvsIn (TyF t1 t2)    = tvsIn t1 ++ tvsIn t2
tvsIn (TyC t1 t2)    = tvsIn t1 ++ tvsIn t2
tvsIn (TyApp _ tys)  = concatMap tvsIn tys
tvsIn (TyVar tv)     = [tv]
tvsIn (TyUTup tys)   = concatMap tvsIn tys

tyconsIn :: Ty -> [TyCon]
tyconsIn (TyF t1 t2)    = tyconsIn t1 `union` tyconsIn t2
tyconsIn (TyC t1 t2)    = tyconsIn t1 `union` tyconsIn t2
tyconsIn (TyApp tc tys) = foldr union [tc] $ map tyconsIn tys
tyconsIn (TyVar _)      = []
tyconsIn (TyUTup tys)   = foldr union [] $ map tyconsIn tys

arity :: Ty -> Int
arity = length . fst . flatTys

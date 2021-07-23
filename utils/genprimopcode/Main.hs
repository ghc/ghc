------------------------------------------------------------------
-- A primop-table mangling program                              --
--
-- See Note [GHC.Prim] in primops.txt.pp for details.
------------------------------------------------------------------

module Main where

import Parser
import Syntax

import Data.Char
import Data.List (union, intersperse, intercalate, nub)
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
             Right p_o_specs@(Info _ _)
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

                      "--make-latex-doc"
                         -> putStr (gen_latex_doc p_o_specs)

                      "--wired-in-docs"
                         -> putStr (gen_wired_in_docs p_o_specs)

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
       "--make-latex-doc",
       "--wired-in-docs"
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
        ++ (replicate 77 '-' ++ "\n") -- For 80-col cleanliness
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
        ++ (replicate 77 '-' ++ "\n") -- For 80-col cleanliness
        ++ "{-# LANGUAGE Unsafe #-}\n"
        ++ "{-# LANGUAGE MagicHash #-}\n"
        ++ "{-# LANGUAGE MultiParamTypeClasses #-}\n"
        ++ "{-# LANGUAGE NoImplicitPrelude #-}\n"
        ++ "{-# LANGUAGE UnboxedTuples #-}\n"
        ++ "{-# LANGUAGE NegativeLiterals #-}\n"

        ++ "{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}\n"
                -- We generate a binding for coerce, like
                --   coerce :: Coercible a b => a -> b
                --   coerce = let x = x in x
                -- and we don't want a complaint that the constraint is redundant
                -- Remember, this silly file is only for Haddock's consumption

        ++ "module GHC.Prim (\n"
        ++ unlines (map (("        " ++) . hdr) entries')
        ++ ") where\n"
    ++ "\n"
    ++ "{-\n"
        ++ unlines (map opt defaults)
    ++ "-}\n"
    ++ "import GHC.Types (Coercible)\n"

    ++ "default ()"  -- If we don't say this then the default type include Integer
                     -- so that runs off and loads modules that are not part of
                     -- package ghc-prim at all.  And that in turn somehow ends up
                     -- with Declaration for $fEqMaybe:
                     --       attempting to use module ‘GHC.Classes’
                     --       (libraries/ghc-prim/./GHC/Classes.hs) which is not loaded
                     -- coming from GHC.Iface.Load.homeModError
                     -- I'm not sure precisely why; but I *am* sure that we don't need
                     -- any type-class defaulting; and it's clearly wrong to need
                     -- the base package when haddocking ghc-prim

       -- Now the main payload
    ++ "\n" ++ unlines (concatMap ent entries') ++ "\n\n\n"

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
           hdr (PrimTypeSpec { ty = TyApp (TyCon n) _ })         = wrapOp n ++ ","
           hdr (PrimTypeSpec {})                                 = error $ "Illegal type spec"
           hdr (PrimVecTypeSpec { ty = TyApp (VecTyCon n _) _ }) = wrapOp n ++ ","
           hdr (PrimVecTypeSpec {})                              = error $ "Illegal type spec"

           sec s = "\n-- * " ++ escape (title s) ++ "\n"
                    ++ (unlines $ map ("-- " ++ ) $ lines $ unlatex $ escape $ "|" ++ desc s)


           ent   (Section {})         = []
           ent o@(PrimOpSpec {})      = spec o
           ent o@(PrimVecOpSpec {})   = spec o
           ent o@(PrimTypeSpec {})    = spec o
           ent o@(PrimVecTypeSpec {}) = spec o
           ent o@(PseudoOpSpec {})    = spec o

           spec o = ([ "" ] ++) . concat $
             -- Doc comments
             [ case unlatex (escape (desc o)) ++ extra (opts o) of
                 "" -> []
                 cmmt -> map ("-- " ++) $ lines $ "|" ++ cmmt

             -- Deprecations
             , [ d | Just n <- [getName o], d <- prim_deprecated (opts o) n ]

             -- Fixity
             , [ f | Just n <- [getName o], f <- prim_fixity (opts o) n ]

             -- Declarations (see Note [Placeholder declarations])
             , case o of
                 PrimOpSpec { name = n, ty = t }    -> prim_func n t
                 PrimVecOpSpec { name = n, ty = t } -> prim_func n t
                 PseudoOpSpec { name = n, ty = t }  -> prim_func n t
                 PrimTypeSpec { ty = t }    -> prim_data t
                 PrimVecTypeSpec { ty = t } -> prim_data t
                 Section { } -> error "Section is not an entity"
             ]

           extra options = case on_llvm_only options ++ can_fail options of
             [m1,m2] -> "\n\n__/Warning:/__ this " ++ m1 ++ " and " ++ m2 ++ "."
             [m] -> "\n\n__/Warning:/__ this " ++ m ++ "."
             _ -> ""

           on_llvm_only options
             = [ "is only available on LLVM"
               | Just (OptionTrue _) <- [lookup_attrib "llvm_only" options] ]

           can_fail options
             = [ "can fail with an unchecked exception"
               | Just (OptionTrue _) <- [lookup_attrib "can_fail" options] ]

           prim_deprecated options n
              = [ "{-# DEPRECATED " ++ wrapOp n ++ " \"" ++ msg ++ "\" #-}"
                | Just (OptionString _ msg)
                    <- [lookup_attrib "deprecated_msg" options] ]

           prim_fixity options n
              = [ pprFixityDir d ++ " " ++ show i ++ " " ++ asInfix n
                | OptionFixity (Just (Fixity _ i d)) <- options ]

           prim_func n t = [ wrapOp n ++ " :: " ++ pprTy t,
                             wrapOp n ++ " = " ++ funcRhs n ]

           funcRhs "tagToEnum#" = "let x = x in x"
           funcRhs nm           = wrapOp nm
              -- Special case for tagToEnum#: see Note [Placeholder declarations]

           prim_data t = [ "data " ++ pprTy t ]

           escape = concatMap (\c -> if c `elem` special then '\\':c:[] else c:[])
                where special = "/'`\"@<"

unlatex :: String -> String
unlatex s = case s of
  '\\':'t':'e':'x':'t':'t':'t':'{':cs -> markup "@" "@" cs
  '{':'\\':'t':'e':'x':'t':'t':'t':' ':cs -> markup "@" "@" cs
  '{':'\\':'t':'t':cs -> markup "@" "@" cs
  '{':'\\':'i':'t':cs -> markup "/" "/" cs
  '{':'\\':'e':'m':cs -> markup "/" "/" cs
  c : cs -> c : unlatex cs
  "" -> ""
  where markup b e xs = b ++ mk (dropWhile isSpace xs)
          where mk ""        = e
                mk ('\n':cs) = ' ' : mk cs
                mk ('}':cs)  = e ++ unlatex cs
                mk (c:cs)    = c : mk cs

-- | Extract a string representation of the name
getName :: Entry -> Maybe String
getName PrimOpSpec{ name = n } = Just n
getName PrimVecOpSpec{ name = n } = Just n
getName PseudoOpSpec{ name = n } = Just n
getName PrimTypeSpec{ ty = TyApp tc _ } = Just (show tc)
getName PrimVecTypeSpec{ ty = TyApp tc _ } = Just (show tc)
getName _ = Nothing

{- Note [Placeholder declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are generating fake declarations for things in GHC.Prim, just to
keep GHC's renamer and typechecker happy enough for what Haddock
needs.  Our main plan is to say
        foo :: <type>
        foo = foo

That works for all the primitive functions except tagToEnum#.
If we generate the binding
        tagToEnum# = tagToEnum#
GHC will complain about "tagToEnum# must appear applied to one argument".
We could hack GHC to silence this complaint when compiling GHC.Prim,
but it seems easier to generate
        tagToEnum# = let x = x in x
We don't do this for *all* bindings because for ones with an unboxed
RHS we would get other complaints (e.g.can't unify "*" with "#").
-}

-- | "Pretty"-print a type
pprTy :: Ty -> String
pprTy = pty
    where
          pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
          pty (TyC t1 t2) = pbty t1 ++ " => " ++ pty t2
          pty t      = pbty t

          pbty (TyApp tc ts) = unwords (wrapOp (show tc) : map paty ts)
          pbty (TyUTup ts)   = "(# "
                            ++ concat (intersperse "," (map pty ts))
                            ++ " #)"
          pbty t             = paty t

          paty (TyVar tv)    = tv
          paty t             = "(" ++ pty t ++ ")"

-- | Turn an identifier or operator into its prefix form
wrapOp :: String -> String
wrapOp nm | isAlpha (head nm) = nm
          | otherwise         = "(" ++ nm ++ ")"

-- | Turn an identifier or operator into its infix form
asInfix :: String -> String
asInfix nm | isAlpha (head nm) = "`" ++ nm ++ "`"
           | otherwise         = nm

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
                   tbinds ("p":tbs) = "(p::?) " ++ (tbinds tbs)
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
             Just (OptionFixity (Just (Fixity _ i d)))
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

{- Note [OPTIONS_GHC in GHC.PrimopWrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In PrimopWrappers we set some crucial GHC options

* Eta reduction: -fno-do-eta-reduction
  In PrimopWrappers we builds a wrapper for each primop, thus
      plusInt# = \a b. plusInt# a b
  That's a pretty odd definition, becaues it looks recursive. What
  actually happens is that it makes a curried, top-level bindings for
  `plusInt#`.  When we compile PrimopWrappers, the code generator spots
  (plusInt# a b) and generates an add instruction.

  Its very important that we don't eta-reduce this to
      plusInt# = plusInt#
  because then the special rule in the code generator doesn't fire.

* Worker-wrapper: performing WW on this module is harmful even, two reasons:
  1. Inferred strictness signatures are all bottom (because of the apparent
     recursion), which is a lie
  2. Doing the worker/wrapper split based on that information will
     introduce references to absentError, which isn't available at
     this point.

  We prevent strictness analyis and w/w by simply doing -O0.  It's
  a very simple module and there is no optimisation to be done
-}

gen_wrappers :: Info -> String
gen_wrappers (Info _ entries)
   =    "{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}\n"
        -- Dependencies on Prelude must be explicit in libraries/base, but we
        -- don't need the Prelude here so we add NoImplicitPrelude.
     ++ "{-# OPTIONS_GHC -Wno-deprecations -O0 -fno-do-eta-reduction #-}\n"
        -- Very important OPTIONS_GHC!  See Note [OPTIONS_GHC in GHC.PrimopWrappers]
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
             [-- tagToEnum# is really magical, and can't have
              -- a wrapper since its implementation depends on
              -- the type of its result
              "tagToEnum#"
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
        "        " ++ ty_id ++ ", " ++ tycon_id ++ ","
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
        tagOf_type = "primOpTag :: PrimOp -> Int"
        f i n = "primOpTag " ++ cons i ++ " = " ++ show n
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

{-
Note [GHC.Prim Docs]
~~~~~~~~~~~~~~~~~~~~
For haddocks of GHC.Prim we generate a dummy haskell file (gen_hs_source) that
contains the type signatures and the commends (but no implementations)
specifically for consumption by haddock.

GHCi's :doc command reads directly from ModIface's though, and GHC.Prim has a
wired-in iface that has nothing to do with the above haskell file. The code
below converts primops.txt into an intermediate form that would later be turned
into a proper DeclDocMap.

We output the docs as a list of pairs (name, docs). We use stringy names here
because mapping names to "Name"s is difficult for things like primtypes and
pseudoops.
-}
gen_wired_in_docs :: Info -> String
gen_wired_in_docs (Info _ entries)
  = "primOpDocs =\n  [ " ++ intercalate "\n  , " (catMaybes $ map mkDoc $ concatMap desugarVectorSpec entries) ++ "\n  ]\n"
    where
      mkDoc po | Just poName <- getName po
               , not $ null $ desc po = Just $ show (poName, unlatex $ desc po)
               | otherwise = Nothing

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
ppTyVar "a" = "alphaTyVarSpec"
ppTyVar "b" = "betaTyVarSpec"
ppTyVar "c" = "gammaTyVarSpec"
ppTyVar "s" = "deltaTyVarSpec"
ppTyVar "o" = "runtimeRep1TyVarInf, openAlphaTyVarSpec"
ppTyVar "p" = "runtimeRep2TyVarInf, openBetaTyVarSpec"
ppTyVar "v" = "levity1TyVarInf, levPolyAlphaTyVarSpec"
ppTyVar "w" = "levity2TyVarInf, levPolyBetaTyVarSpec"
ppTyVar _   = error "Unknown type var"
-- o, p, v and w have a special meaning. See primops.txt.pp
-- Note [Levity and representation polymorphic primops]

ppType :: Ty -> String
ppType (TyApp (TyCon "Any")         []) = "anyTy"
ppType (TyApp (TyCon "Bool")        []) = "boolTy"

ppType (TyApp (TyCon "Int#")        []) = "intPrimTy"
ppType (TyApp (TyCon "Int8#")       []) = "int8PrimTy"
ppType (TyApp (TyCon "Int16#")      []) = "int16PrimTy"
ppType (TyApp (TyCon "Int32#")      []) = "int32PrimTy"
ppType (TyApp (TyCon "Int64#")      []) = "int64PrimTy"
ppType (TyApp (TyCon "Char#")       []) = "charPrimTy"
ppType (TyApp (TyCon "Word#")       []) = "wordPrimTy"
ppType (TyApp (TyCon "Word8#")      []) = "word8PrimTy"
ppType (TyApp (TyCon "Word16#")     []) = "word16PrimTy"
ppType (TyApp (TyCon "Word32#")     []) = "word32PrimTy"
ppType (TyApp (TyCon "Word64#")     []) = "word64PrimTy"
ppType (TyApp (TyCon "Addr#")       []) = "addrPrimTy"
ppType (TyApp (TyCon "Float#")      []) = "floatPrimTy"
ppType (TyApp (TyCon "Double#")     []) = "doublePrimTy"
ppType (TyApp (TyCon "ByteArray#")  []) = "byteArrayPrimTy"
ppType (TyApp (TyCon "RealWorld")   []) = "realWorldTy"
ppType (TyApp (TyCon "ThreadId#")   []) = "threadIdPrimTy"
ppType (TyApp (TyCon "ForeignObj#") []) = "foreignObjPrimTy"
ppType (TyApp (TyCon "BCO")         []) = "bcoPrimTy"
ppType (TyApp (TyCon "Compact#")    []) = "compactPrimTy"
ppType (TyApp (TyCon "StackSnapshot#") []) = "stackSnapshotPrimTy"
ppType (TyApp (TyCon "()")          []) = "unitTy"      -- unitTy is GHC.Builtin.Types's name for ()

ppType (TyVar "a")                      = "alphaTy"
ppType (TyVar "b")                      = "betaTy"
ppType (TyVar "c")                      = "gammaTy"
ppType (TyVar "s")                      = "deltaTy"
ppType (TyVar "o")                      = "openAlphaTy"
ppType (TyVar "p")                      = "openBetaTy"
ppType (TyVar "v")                      = "levPolyAlphaTy"
ppType (TyVar "w")                      = "levPolyBetaTy"
-- o, p, v and w have a special meaning. See primops.txt.pp
-- Note [Levity and representation polymorphic primops]

ppType (TyApp (TyCon "State#") [x])             = "mkStatePrimTy " ++ ppType x
ppType (TyApp (TyCon "MutVar#") [x,y])          = "mkMutVarPrimTy " ++ ppType x
                                                   ++ " " ++ ppType y
ppType (TyApp (TyCon "MutableArray#") [x,y])    = "mkMutableArrayPrimTy " ++ ppType x
                                                   ++ " " ++ ppType y
ppType (TyApp (TyCon "MutableArrayArray#") [x]) = "mkMutableArrayArrayPrimTy " ++ ppType x
ppType (TyApp (TyCon "SmallMutableArray#") [x,y]) = "mkSmallMutableArrayPrimTy " ++ ppType x
                                                    ++ " " ++ ppType y
ppType (TyApp (TyCon "MutableByteArray#") [x])  = "mkMutableByteArrayPrimTy "
                                                   ++ ppType x
ppType (TyApp (TyCon "Array#") [x])             = "mkArrayPrimTy " ++ ppType x
ppType (TyApp (TyCon "ArrayArray#") [])         = "mkArrayArrayPrimTy"
ppType (TyApp (TyCon "SmallArray#") [x])        = "mkSmallArrayPrimTy " ++ ppType x


ppType (TyApp (TyCon "Weak#")       [x]) = "mkWeakPrimTy " ++ ppType x
ppType (TyApp (TyCon "StablePtr#")  [x]) = "mkStablePtrPrimTy " ++ ppType x
ppType (TyApp (TyCon "StableName#") [x]) = "mkStableNamePrimTy " ++ ppType x

ppType (TyApp (TyCon "MVar#") [x,y])     = "mkMVarPrimTy " ++ ppType x
                                           ++ " " ++ ppType y
ppType (TyApp (TyCon "IOPort#") [x,y])   = "mkIOPortPrimTy " ++ ppType x
                                           ++ " " ++ ppType y
ppType (TyApp (TyCon "TVar#") [x,y])     = "mkTVarPrimTy " ++ ppType x
                                           ++ " " ++ ppType y
ppType (TyApp (VecTyCon _ pptc) [])      = pptc

ppType (TyUTup ts) = "(mkTupleTy Unboxed "
                     ++ listify (map ppType ts) ++ ")"

ppType (TyF s d) = "(mkVisFunTyMany ("   ++ ppType s ++ ") (" ++ ppType d ++ "))"
ppType (TyC s d) = "(mkInvisFunTyMany (" ++ ppType s ++ ") (" ++ ppType d ++ "))"

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

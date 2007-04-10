{-# OPTIONS -cpp #-}
------------------------------------------------------------------
-- A primop-table mangling program                              --
------------------------------------------------------------------

module Main where

import Parser
import Syntax

import Monad
import Char
import List
import System ( getArgs )
import Maybe ( catMaybes )

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
             Right p_o_specs
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

                      "--needs-wrapper" 
                         -> putStr (gen_switch_from_attribs 
                                       "needs_wrapper" 
                                       "primOpNeedsWrapper" p_o_specs)

                      "--can-fail" 
                         -> putStr (gen_switch_from_attribs 
                                       "can_fail" 
                                       "primOpCanFail" p_o_specs)

                      "--strictness" 
                         -> putStr (gen_switch_from_attribs 
                                       "strictness" 
                                       "primOpStrictness" p_o_specs)

                      "--usage" 
                         -> putStr (gen_switch_from_attribs 
                                       "usage" 
                                       "primOpUsg" p_o_specs)

                      "--primop-primop-info" 
                         -> putStr (gen_primop_info p_o_specs)

                      "--primop-tag" 
                         -> putStr (gen_primop_tag p_o_specs)

                      "--primop-list" 
                         -> putStr (gen_primop_list p_o_specs)

                      "--make-haskell-wrappers" 
                         -> putStr (gen_wrappers p_o_specs)
			
                      "--make-haskell-source" 
                         -> putStr (gen_hs_source p_o_specs)

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
       "--needs-wrapper",
       "--can-fail",
       "--strictness",
       "--usage",
       "--primop-primop-info",
       "--primop-tag",
       "--primop-list",
       "--make-haskell-wrappers",
       "--make-haskell-source",
       "--make-latex-doc"
     ]

------------------------------------------------------------------
-- Code generators -----------------------------------------------
------------------------------------------------------------------

gen_hs_source :: Info -> String
gen_hs_source (Info defaults entries) =
	   "-----------------------------------------------------------------------------\n"
	++ "-- |\n"
	++ "-- Module      :  GHC.Arr\n"
	++ "-- \n"
	++ "-- Maintainer  :  cvs-ghc@haskell.org\n"
	++ "-- Stability   :  internal\n"
	++ "-- Portability :  non-portable (GHC extensions)\n"
	++ "--\n"
	++ "-- GHC\'s primitive types and operations.\n"
	++ "--\n" 
	++ "-----------------------------------------------------------------------------\n"
	++ "module GHC.Prim (\n"
	++ unlines (map (("\t" ++) . hdr) entries)
	++ ") where\n\n{-\n"
	++ unlines (map opt defaults) ++ "-}\n"
	++ unlines (map ent entries) ++ "\n\n\n"
     where opt (OptionFalse n)	  = n ++ " = False"
           opt (OptionTrue n)	  = n ++ " = True"
	   opt (OptionString n v) = n ++ " = { " ++ v ++ "}"

	   hdr s@(Section {})			 = sec s
	   hdr (PrimOpSpec { name = n })	 = wrapOp n ++ ","
	   hdr (PseudoOpSpec { name = n })	 = wrapOp n ++ ","
	   hdr (PrimTypeSpec { ty = TyApp n _ }) = wrapTy n ++ ","
	   hdr (PrimTypeSpec {})                 = error "Illegal type spec"

	   ent   (Section {})	   = ""
	   ent o@(PrimOpSpec {})   = spec o
	   ent o@(PrimTypeSpec {}) = spec o
	   ent o@(PseudoOpSpec {}) = spec o

	   sec s = "\n-- * " ++ escape (title s) ++ "\n"
			++ (unlines $ map ("-- " ++ ) $ lines $ unlatex $ escape $ "|" ++ desc s) ++ "\n"

	   spec o = comm ++ decl
	     where decl = case o of
			PrimOpSpec { name = n, ty = t }	  -> wrapOp n ++ " :: " ++ pty t
			PseudoOpSpec { name = n, ty = t } -> wrapOp n ++ " :: " ++ pty t
			PrimTypeSpec { ty = t }	  -> "data " ++ pty t
			Section { } -> ""

		   comm = case (desc o) of
		   	[] -> ""
			d -> "\n" ++ (unlines $ map ("-- " ++ ) $ lines $ unlatex $ escape $ "|" ++ d)

		   pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
		   pty t	   = pbty t

		   pbty (TyApp tc ts) = tc ++ (concat (map (' ':) (map paty ts)))
		   pbty (TyUTup ts)   = "(# " ++ (concat (intersperse "," (map pty ts))) ++ " #)"
		   pbty t	      = paty t

		   paty (TyVar tv)	= tv
		   paty t		= "(" ++ pty t ++ ")"

	   wrapOp nm | isAlpha (head nm) = nm
		     | otherwise	 = "(" ++ nm ++ ")"
	   wrapTy nm | isAlpha (head nm) = nm
		     | otherwise	 = "(" ++ nm ++ ")"
	   unlatex s = case s of
	   	'\\':'t':'e':'x':'t':'t':'t':'{':cs -> markup "@" "@" cs
		'{':'\\':'t':'t':cs -> markup "@" "@" cs
		'{':'\\':'i':'t':cs -> markup "/" "/" cs
		c : cs -> c : unlatex cs
		[] -> []
	   markup s t xs = s ++ mk (dropWhile isSpace xs)
	   	where mk ""	   = t
	              mk ('\n':cs) = ' ' : mk cs
	              mk ('}':cs)  = t ++ unlatex cs
	              mk (c:cs)	   = c : mk cs
	   escape = concatMap (\c -> if c `elem` special then '\\':c:[] else c:[])
	   	where special = "/'`\"@<"

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
		   pty t = pbty t
		   pbty (TyApp tc ts) = tc ++ (concat (map (' ':) (map paty ts)))
		   pbty (TyUTup ts) = "(# " ++ (concat (intersperse "," (map pty ts))) ++ " #)"
		   pbty t = paty t
		   paty (TyVar tv) = tv
		   paty t = "(" ++ pty t ++ ")"
	   
	   mk_core_ty typ = foralls ++ (pty typ)
	     where pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
		   pty t = pbty t
		   pbty (TyApp tc ts) = (zencode tc) ++ (concat (map (' ':) (map paty ts)))
		   pbty (TyUTup ts) = (zencode (utuplenm (length ts))) ++ (concat ((map (' ':) (map paty ts))))
		   pbty t = paty t
		   paty (TyVar tv) = zencode tv
		   paty (TyApp tc []) = zencode tc
		   paty t = "(" ++ pty t ++ ")"
		   utuplenm 1 = "(# #)"
		   utuplenm n = "(#" ++ (replicate (n-1) ',') ++ "#)"
		   foralls = if tvars == [] then "" else "%forall " ++ (tbinds tvars)
		   tvars = tvars_of typ
		   tbinds [] = ". " 
		   tbinds ("o":tbs) = "(o::?) " ++ (tbinds tbs)
		   tbinds (tv:tbs) = tv ++ " " ++ (tbinds tbs)
	   tvars_of (TyF t1 t2) = tvars_of t1 `union` tvars_of t2
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
	      ++ latex_encode (mk_strictness o) ++ "}{"
 	      ++ latex_encode (mk_usage o)
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
	       Nothing -> ""
	   
	   mk_strictness o = 
	     case lookup_attrib "strictness" o of
	       Just (OptionString _ s) -> s  -- for now
	       Just _ -> error "Boolean value for strictness"
	       Nothing -> "" 

	   mk_usage o =
	     case lookup_attrib "usage" o of
	       Just (OptionString _ s) -> s  -- for now
	       Just _ -> error "Boolean value for usage"
	       Nothing -> "" 

	   zencode xs =
	     case maybe_tuple xs of
		Just n  -> n		-- Tuples go to Z2T etc
		Nothing -> concat (map encode_ch xs)
	     where
	       maybe_tuple "(# #)" = Just("Z1H")
	       maybe_tuple ('(' : '#' : cs) = case count_commas (0::Int) cs of
						(n, '#' : ')' : _) -> Just ('Z' : shows (n+1) "H")
						_		   -> Nothing
	       maybe_tuple "()" = Just("Z0T")
	       maybe_tuple ('(' : cs)       = case count_commas (0::Int) cs of
						(n, ')' : _) -> Just ('Z' : shows (n+1) "T")
						_	     -> Nothing
	       maybe_tuple _    	     = Nothing
	       
	       count_commas :: Int -> String -> (Int, String)
	       count_commas n (',' : cs) = count_commas (n+1) cs
	       count_commas n cs	  = (n,cs)
	       
	       unencodedChar :: Char -> Bool	-- True for chars that don't need encoding
	       unencodedChar 'Z' = False
	       unencodedChar 'z' = False
	       unencodedChar c   = isAlphaNum c
	       
	       encode_ch :: Char -> String
	       encode_ch c | unencodedChar c = [c]	-- Common case first
	       
	       -- Constructors
	       encode_ch '('  = "ZL"	-- Needed for things like (,), and (->)
	       encode_ch ')'  = "ZR"	-- For symmetry with (
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
   = "{-# OPTIONS -fno-implicit-prelude #-}\n" 
	-- Dependencies on Prelude must be explicit in libraries/base, but we
	-- don't need the Prelude here so we add -fno-implicit-prelude.
     ++ "module GHC.PrimopWrappers where\n" 
     ++ "import qualified GHC.Prim\n" 
     ++ unlines (map f (filter (not.dodgy) (filter is_primop entries)))
     where
        f spec = let args = map (\n -> "a" ++ show n) [1 .. arity (ty spec)]
                     src_name = wrap (name spec)
                 in "{-# NOINLINE " ++ src_name ++ " #-}\n" ++ 
                    src_name ++ " " ++ unwords args 
                     ++ " = (GHC.Prim." ++ name spec ++ ") " ++ unwords args
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

gen_primop_list :: Info -> String
gen_primop_list (Info _ entries)
   = unlines (
        [      "   [" ++ cons first       ]
        ++
        map (\p -> "   , " ++ cons p) rest
        ++ 
        [     "   ]"     ]
     ) where (first:rest) = filter is_primop entries

gen_primop_tag :: Info -> String
gen_primop_tag (Info _ entries)
   = unlines (max_def : zipWith f primop_entries [1 :: Int ..])
     where
	primop_entries = filter is_primop entries
        f i n = "tagOf_PrimOp " ++ cons i 
                ++ " = _ILIT(" ++ show n ++ ") :: FastInt"
	max_def = "maxPrimOpTag = " ++ show (length primop_entries) ++ " :: Int"

gen_data_decl :: Info -> String
gen_data_decl (Info _ entries)
   = let conss = map cons (filter is_primop entries)
     in  "data PrimOp\n   = " ++ head conss ++ "\n"
         ++ unlines (map ("   | "++) (tail conss))

gen_switch_from_attribs :: String -> String -> Info -> String
gen_switch_from_attribs attrib_name fn_name (Info defaults entries)
   = let defv = lookup_attrib attrib_name defaults
         alternatives = catMaybes (map mkAlt (filter is_primop entries))

         getAltRhs (OptionFalse _)    = "False"
         getAltRhs (OptionTrue _)     = "True"
         getAltRhs (OptionString _ s) = s

         mkAlt po
            = case lookup_attrib attrib_name (opts po) of
                 Nothing -> Nothing
                 Just xx -> Just (fn_name ++ " " ++ cons po ++ " = " ++ getAltRhs xx)

     in
         case defv of
            Nothing -> error ("gen_switch_from: " ++ attrib_name)
            Just xx 
               -> unlines alternatives
                  ++ fn_name ++ " other = " ++ getAltRhs xx ++ "\n"

------------------------------------------------------------------
-- Create PrimOpInfo text from PrimOpSpecs -----------------------
------------------------------------------------------------------

gen_primop_info :: Info -> String
gen_primop_info (Info _ entries)
   = unlines (map mkPOItext (filter is_primop entries))

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
sl_name i = "FSLIT(\"" ++ name i ++ "\") "

ppTyVar :: String -> String
ppTyVar "a" = "alphaTyVar"
ppTyVar "b" = "betaTyVar"
ppTyVar "c" = "gammaTyVar"
ppTyVar "s" = "deltaTyVar"
ppTyVar "o" = "openAlphaTyVar"
ppTyVar _   = error "Unknown type var"

ppType :: Ty -> String
ppType (TyApp "Bool"        []) = "boolTy"

ppType (TyApp "Int#"        []) = "intPrimTy"
ppType (TyApp "Int32#"      []) = "int32PrimTy"
ppType (TyApp "Int64#"      []) = "int64PrimTy"
ppType (TyApp "Char#"       []) = "charPrimTy"
ppType (TyApp "Word#"       []) = "wordPrimTy"
ppType (TyApp "Word32#"     []) = "word32PrimTy"
ppType (TyApp "Word64#"     []) = "word64PrimTy"
ppType (TyApp "Addr#"       []) = "addrPrimTy"
ppType (TyApp "Float#"      []) = "floatPrimTy"
ppType (TyApp "Double#"     []) = "doublePrimTy"
ppType (TyApp "ByteArr#"    []) = "byteArrayPrimTy"
ppType (TyApp "RealWorld"   []) = "realWorldTy"
ppType (TyApp "ThreadId#"   []) = "threadIdPrimTy"
ppType (TyApp "ForeignObj#" []) = "foreignObjPrimTy"
ppType (TyApp "BCO#"        []) = "bcoPrimTy"
ppType (TyApp "()"          []) = "unitTy" 	-- unitTy is TysWiredIn's name for ()

ppType (TyVar "a")               = "alphaTy"
ppType (TyVar "b")               = "betaTy"
ppType (TyVar "c")               = "gammaTy"
ppType (TyVar "s")               = "deltaTy"
ppType (TyVar "o")               = "openAlphaTy"
ppType (TyApp "State#" [x])      = "mkStatePrimTy " ++ ppType x
ppType (TyApp "MutVar#" [x,y])   = "mkMutVarPrimTy " ++ ppType x 
                                   ++ " " ++ ppType y
ppType (TyApp "MutArr#" [x,y])   = "mkMutableArrayPrimTy " ++ ppType x 
                                   ++ " " ++ ppType y

ppType (TyApp "MutByteArr#" [x]) = "mkMutableByteArrayPrimTy " 
                                   ++ ppType x

ppType (TyApp "Array#" [x])      = "mkArrayPrimTy " ++ ppType x


ppType (TyApp "Weak#"  [x])      = "mkWeakPrimTy " ++ ppType x
ppType (TyApp "StablePtr#"  [x])      = "mkStablePtrPrimTy " ++ ppType x
ppType (TyApp "StableName#"  [x])      = "mkStableNamePrimTy " ++ ppType x

ppType (TyApp "MVar#" [x,y])     = "mkMVarPrimTy " ++ ppType x 
                                   ++ " " ++ ppType y
ppType (TyApp "TVar#" [x,y])     = "mkTVarPrimTy " ++ ppType x 
                                   ++ " " ++ ppType y
ppType (TyUTup ts)               = "(mkTupleTy Unboxed " ++ show (length ts)
                                   ++ " "
                                   ++ listify (map ppType ts) ++ ")"

ppType (TyF s d) = "(mkFunTy (" ++ ppType s ++ ") (" ++ ppType d ++ "))"

ppType other
   = error ("ppType: can't handle: " ++ show other ++ "\n")

listify :: [String] -> String
listify ss = "[" ++ concat (intersperse ", " ss) ++ "]"

flatTys :: Ty -> ([Ty],Ty)
flatTys (TyF t1 t2) = case flatTys t2 of (ts,t) -> (t1:ts,t)
flatTys other       = ([],other)

tvsIn :: Ty -> [TyVar]
tvsIn (TyF t1 t2)    = tvsIn t1 ++ tvsIn t2
tvsIn (TyApp _ tys)  = concatMap tvsIn tys
tvsIn (TyVar tv)     = [tv]
tvsIn (TyUTup tys)   = concatMap tvsIn tys

arity :: Ty -> Int
arity = length . fst . flatTys


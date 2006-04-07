{-# OPTIONS -cpp #-}
------------------------------------------------------------------
-- A primop-table mangling program                              --
------------------------------------------------------------------

module Main where

#if __GLASGOW_HASKELL__ >= 504
import Text.ParserCombinators.Parsec
#else
import Parsec
#endif

import Monad
import Char
import List
import System ( getArgs )
import Maybe ( catMaybes )

main = getArgs >>= \args ->
       if length args /= 1 || head args `notElem` known_args
       then error ("usage: genprimopcode command < primops.txt > ...\n"
                   ++ "   where command is one of\n"
                   ++ unlines (map ("            "++) known_args)
                  )
       else
       do s <- getContents
          let pres = parse pTop "" s
          case pres of
             Left err -> error ("parse error at " ++ (show err))
             Right p_o_specs
                -> myseq (sanityTop p_o_specs) (
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
                   )


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

gen_hs_source (Info defaults entries)
   = "module GHC.Prim (\n"
	    ++ unlines (map (("\t" ++) . hdr) entries)
            ++ ") where\n\n{-\n"
	    ++ unlines (map opt defaults) ++ "-}\n"
            ++ unlines (map ent entries) ++ "\n\n\n"
     where opt (OptionFalse n) = n ++ " = False"
           opt (OptionTrue n) = n ++ " = True"
	   opt (OptionString n v) = n ++ " = { " ++ v ++ "}"

	   hdr s@(Section {}) = sec s
	   hdr o@(PrimOpSpec {}) = wrap (name o) ++ ","

	   ent s@(Section {}) = ""
	   ent o@(PrimOpSpec {}) = spec o

	   sec s = "\n-- * " ++ escape (title s) ++ "\n"
			++ (unlines $ map ("-- " ++ ) $ lines $ unlatex $ escape $ "|" ++ desc s) ++ "\n"

	   spec o = comm ++ decl
	     where decl = wrap (name o) ++ " :: " ++ pty (ty o)
		   comm = case (desc o) of
		   	[] -> ""
			d -> "\n" ++ (unlines $ map ("-- " ++ ) $ lines $ unlatex $ escape $ "|" ++ d)

		   pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
		   pty t = pbty t

		   pbty (TyApp tc ts) = tc ++ (concat (map (' ':) (map paty ts)))
		   pbty (TyUTup ts) = "(# " ++ (concat (intersperse "," (map pty ts))) ++ " #)"
		   pbty t = paty t

		   paty (TyVar tv) = tv
		   paty t = "(" ++ pty t ++ ")"

	   wrap nm | isLower (head nm) = nm
		   | otherwise = "(" ++ nm ++ ")"
	   unlatex s = case s of
	   	'\\':'t':'e':'x':'t':'t':'t':'{':cs -> markup "@" "@" cs
		'{':'\\':'t':'t':cs -> markup "@" "@" cs
		c : cs -> c : unlatex cs
		[] -> []
	   markup s t cs = s ++ mk (dropWhile isSpace cs)
	   	where mk "" = t
	              mk ('\n':cs) = ' ' : mk cs
	              mk ('}':cs) = t ++ unlatex cs
	              mk (c:cs) = c : mk cs
	   escape = concatMap (\c -> if c `elem` special then '\\':c:[] else c:[])
	   	where special = "/'`\"@<"

gen_latex_doc (Info defaults entries)
   = "\\primopdefaults{" 
	 ++ mk_options defaults
	 ++ "}\n"
     ++ (concat (map mk_entry entries))
     where mk_entry (PrimOpSpec {cons=cons,name=name,ty=ty,cat=cat,desc=desc,opts=opts}) =
   		 "\\primopdesc{" 
		 ++ latex_encode cons ++ "}{"
		 ++ latex_encode name ++ "}{"
		 ++ latex_encode (zencode name) ++ "}{"
		 ++ latex_encode (show cat) ++ "}{"
		 ++ latex_encode (mk_source_ty ty) ++ "}{"
		 ++ latex_encode (mk_core_ty ty) ++ "}{"
		 ++ desc ++ "}{"
		 ++ mk_options opts
		 ++ "}\n"
           mk_entry (Section {title=title,desc=desc}) =
		 "\\primopsection{" 
		 ++ latex_encode title ++ "}{" 
		 ++ desc ++ "}\n"
	   mk_source_ty t = pty t
	     where pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
		   pty t = pbty t
		   pbty (TyApp tc ts) = tc ++ (concat (map (' ':) (map paty ts)))
		   pbty (TyUTup ts) = "(# " ++ (concat (intersperse "," (map pty ts))) ++ " #)"
		   pbty t = paty t
		   paty (TyVar tv) = tv
		   paty t = "(" ++ pty t ++ ")"
	   
	   mk_core_ty t = foralls ++ (pty t)
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
		   tvars = tvars_of t
		   tbinds [] = ". " 
		   tbinds ("o":tbs) = "(o::?) " ++ (tbinds tbs)
		   tbinds (tv:tbs) = tv ++ " " ++ (tbinds tbs)
	   tvars_of (TyF t1 t2) = tvars_of t1 `union` tvars_of t2
	   tvars_of (TyApp tc ts) = foldl union [] (map tvars_of ts)
	   tvars_of (TyUTup ts) = foldr union [] (map tvars_of ts)
	   tvars_of (TyVar tv) = [tv]
	   
           mk_options opts = 
	     "\\primoptions{"
	      ++ mk_has_side_effects opts ++ "}{"
	      ++ mk_out_of_line opts ++ "}{"
	      ++ mk_commutable opts ++ "}{"
 	      ++ mk_needs_wrapper opts ++ "}{"
	      ++ mk_can_fail opts ++ "}{"
	      ++ latex_encode (mk_strictness opts) ++ "}{"
 	      ++ latex_encode (mk_usage opts)
	      ++ "}"

  	   mk_has_side_effects opts = mk_bool_opt opts "has_side_effects" "Has side effects." "Has no side effects."
	   mk_out_of_line opts = mk_bool_opt opts "out_of_line" "Implemented out of line." "Implemented in line."
  	   mk_commutable opts = mk_bool_opt opts "commutable" "Commutable." "Not commutable."
  	   mk_needs_wrapper opts = mk_bool_opt opts "needs_wrapper" "Needs wrapper." "Needs no wrapper."
	   mk_can_fail opts = mk_bool_opt opts "can_fail" "Can fail." "Cannot fail."

	   mk_bool_opt opts opt_name if_true if_false =
	     case lookup_attrib opt_name opts of
	       Just (OptionTrue _) -> if_true
	       Just (OptionFalse _) -> if_false
	       Nothing -> ""
	   
	   mk_strictness opts = 
	     case lookup_attrib "strictness" opts of
	       Just (OptionString _ s) -> s  -- for now
	       Nothing -> "" 

	   mk_usage opts = 
	     case lookup_attrib "usage" opts of
	       Just (OptionString _ s) -> s  -- for now
	       Nothing -> "" 

	   zencode cs = 
	     case maybe_tuple cs of
		Just n  -> n		-- Tuples go to Z2T etc
		Nothing -> concat (map encode_ch cs)
	     where
	       maybe_tuple "(# #)" = Just("Z1H")
	       maybe_tuple ('(' : '#' : cs) = case count_commas (0::Int) cs of
						(n, '#' : ')' : cs) -> Just ('Z' : shows (n+1) "H")
						other		     -> Nothing
	       maybe_tuple "()" = Just("Z0T")
	       maybe_tuple ('(' : cs)       = case count_commas (0::Int) cs of
						(n, ')' : cs) -> Just ('Z' : shows (n+1) "T")
						other	       -> Nothing
	       maybe_tuple other    	     = Nothing
	       
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

gen_wrappers (Info defaults entries)
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


gen_primop_list (Info defaults entries)
   = unlines (
        [      "   [" ++ cons first       ]
        ++
        map (\pi -> "   , " ++ cons pi) rest
        ++ 
        [     "   ]"     ]
     ) where (first:rest) = filter is_primop entries

gen_primop_tag (Info defaults entries)
   = unlines (max_def : zipWith f primop_entries [1..])
     where
	primop_entries = filter is_primop entries
        f i n = "tagOf_PrimOp " ++ cons i 
                ++ " = _ILIT(" ++ show n ++ ") :: FastInt"
	max_def = "maxPrimOpTag = " ++ show (length primop_entries) ++ " :: Int"

gen_data_decl (Info defaults entries)
   = let conss = map cons (filter is_primop entries)
     in  "data PrimOp\n   = " ++ head conss ++ "\n"
         ++ unlines (map ("   | "++) (tail conss))

gen_switch_from_attribs :: String -> String -> Info -> String
gen_switch_from_attribs attrib_name fn_name (Info defaults entries)
   = let defv = lookup_attrib attrib_name defaults
         alts = catMaybes (map mkAlt (filter is_primop entries))

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
               -> unlines alts 
                  ++ fn_name ++ " other = " ++ getAltRhs xx ++ "\n"

------------------------------------------------------------------
-- Create PrimOpInfo text from PrimOpSpecs -----------------------
------------------------------------------------------------------


gen_primop_info (Info defaults entries)
   = unlines (map mkPOItext (filter is_primop entries))

mkPOItext i = mkPOI_LHS_text i ++ mkPOI_RHS_text i

mkPOI_LHS_text i
   = "primOpInfo " ++ cons i ++ " = "

mkPOI_RHS_text i
   = case cat i of
        Compare 
           -> case ty i of
                 TyF t1 (TyF t2 td) 
                    -> "mkCompare " ++ sl_name i ++ ppType t1
        Monadic
           -> case ty i of
                 TyF t1 td
                    -> "mkMonadic " ++ sl_name i ++ ppType t1
        Dyadic
           -> case ty i of
                 TyF t1 (TyF t2 td)
                    -> "mkDyadic " ++ sl_name i ++ ppType t1
        GenPrimOp
           -> let (argTys, resTy) = flatTys (ty i)
                  tvs = nub (tvsIn (ty i))
              in
                  "mkGenPrimOp " ++ sl_name i ++ " " 
                      ++ listify (map ppTyVar tvs) ++ " "
                      ++ listify (map ppType argTys) ++ " "
                      ++ "(" ++ ppType resTy ++ ")"
            
sl_name i = "FSLIT(\"" ++ name i ++ "\") "

ppTyVar "a" = "alphaTyVar"
ppTyVar "b" = "betaTyVar"
ppTyVar "c" = "gammaTyVar"
ppTyVar "s" = "deltaTyVar"
ppTyVar "o" = "openAlphaTyVar"


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

flatTys (TyF t1 t2) = case flatTys t2 of (ts,t) -> (t1:ts,t)
flatTys other       = ([],other)

tvsIn (TyF t1 t2)    = tvsIn t1 ++ tvsIn t2
tvsIn (TyApp tc tys) = concatMap tvsIn tys
tvsIn (TyVar tv)     = [tv]
tvsIn (TyUTup tys)   = concatMap tvsIn tys

arity = length . fst . flatTys


------------------------------------------------------------------
-- Abstract syntax -----------------------------------------------
------------------------------------------------------------------

-- info for all primops; the totality of the info in primops.txt(.pp)
data Info
   = Info [Option] [Entry]   -- defaults, primops
     deriving Show

-- info for one primop
data Entry
    = PrimOpSpec { cons  :: String,      -- PrimOp name
                   name  :: String,      -- name in prog text
                   ty    :: Ty,          -- type
                   cat   :: Category,    -- category
		   desc  :: String,      -- description
                   opts  :: [Option] }   -- default overrides
    | Section { title :: String,	 -- section title
		desc  :: String }        -- description
    deriving Show

is_primop (PrimOpSpec _ _ _ _ _ _) = True
is_primop _ = False

-- a binding of property to value
data Option
   = OptionFalse  String          -- name = False
   | OptionTrue   String          -- name = True
   | OptionString String String   -- name = { ... unparsed stuff ... }
     deriving Show

-- categorises primops
data Category
   = Dyadic | Monadic | Compare | GenPrimOp
     deriving Show

-- types
data Ty
   = TyF    Ty Ty
   | TyApp  TyCon [Ty]
   | TyVar  TyVar
   | TyUTup [Ty]   -- unboxed tuples; just a TyCon really, 
                   -- but convenient like this
   deriving (Eq,Show)

type TyVar = String
type TyCon = String


------------------------------------------------------------------
-- Sanity checking -----------------------------------------------
------------------------------------------------------------------

{- Do some simple sanity checks:
    * all the default field names are unique
    * for each PrimOpSpec, all override field names are unique
    * for each PrimOpSpec, all overriden field names   
          have a corresponding default value
    * that primop types correspond in certain ways to the 
      Category: eg if Comparison, the type must be of the form
         T -> T -> Bool.
   Dies with "error" if there's a problem, else returns ().
-}
myseq () x = x
myseqAll (():ys) x = myseqAll ys x
myseqAll []      x = x

sanityTop :: Info -> ()
sanityTop (Info defs entries)
   = let opt_names = map get_attrib_name defs
	 primops = filter is_primop entries
     in  
     if   length opt_names /= length (nub opt_names)
     then error ("non-unique default attribute names: " ++ show opt_names ++ "\n")
     else myseqAll (map (sanityPrimOp opt_names) primops) ()

sanityPrimOp def_names p
   = let p_names = map get_attrib_name (opts p)
         p_names_ok
            = length p_names == length (nub p_names)
              && all (`elem` def_names) p_names
         ty_ok = sane_ty (cat p) (ty p)
     in
         if   not p_names_ok
         then error ("attribute names are non-unique or have no default in\n" ++
                     "info for primop " ++ cons p ++ "\n")
         else
         if   not ty_ok
         then error ("type of primop " ++ cons p ++ " doesn't make sense w.r.t" ++
                     " category " ++ show (cat p) ++ "\n")
         else ()

sane_ty Compare (TyF t1 (TyF t2 td)) 
   | t1 == t2 && td == TyApp "Bool" []  = True
sane_ty Monadic (TyF t1 td) 
   | t1 == td  = True
sane_ty Dyadic (TyF t1 (TyF t2 td))
   | t1 == t2 && t2 == t2  = True
sane_ty GenPrimOp any_old_thing
   = True
sane_ty _ _
   = False

get_attrib_name (OptionFalse nm) = nm
get_attrib_name (OptionTrue nm)  = nm
get_attrib_name (OptionString nm _) = nm

lookup_attrib nm [] = Nothing
lookup_attrib nm (a:as) 
    = if get_attrib_name a == nm then Just a else lookup_attrib nm as

------------------------------------------------------------------
-- The parser ----------------------------------------------------
------------------------------------------------------------------

-- Due to lack of proper lexing facilities, a hack to zap any
-- leading comments
pTop :: Parser Info
pTop = then4 (\_ ds es _ -> Info ds es) 
             pCommentAndWhitespace pDefaults (many pEntry)
             (lit "thats_all_folks")

pEntry :: Parser Entry
pEntry 
  = alts [pPrimOpSpec, pSection]

pSection :: Parser Entry
pSection = then3 (\_ n d -> Section {title = n, desc = d}) 
		 (lit "section") stringLiteral pDesc

pDefaults :: Parser [Option]
pDefaults = then2 sel22 (lit "defaults") (many pOption)

pOption :: Parser Option
pOption 
   = alts [
        then3 (\nm eq ff -> OptionFalse nm)  pName (lit "=") (lit "False"),
        then3 (\nm eq tt -> OptionTrue nm)   pName (lit "=") (lit "True"),
        then3 (\nm eq zz -> OptionString nm zz)
              pName (lit "=") pStuffBetweenBraces
     ]

pPrimOpSpec :: Parser Entry
pPrimOpSpec
   = then7 (\_ c n k t d o -> PrimOpSpec { cons = c, name = n, ty = t, 
                                           cat = k, desc = d, opts = o } )
           (lit "primop") pConstructor stringLiteral 
           pCategory pType pDesc pOptions

pOptions :: Parser [Option]
pOptions = optdef [] (then2 sel22 (lit "with") (many pOption))

pCategory :: Parser Category
pCategory 
   = alts [
        apply (const Dyadic)    (lit "Dyadic"),
        apply (const Monadic)   (lit "Monadic"),
        apply (const Compare)   (lit "Compare"),
        apply (const GenPrimOp) (lit "GenPrimOp")
     ]

pDesc :: Parser String
pDesc = optdef "" pStuffBetweenBraces

pStuffBetweenBraces :: Parser String
pStuffBetweenBraces
    = lexeme (
	do char '{'
	   ass <- many pInsides
	   char '}'
           return (concat ass) )

pInsides :: Parser String
pInsides 
    = (do char '{' 
	  stuff <- many pInsides
          char '}'
          return ("{" ++ (concat stuff) ++ "}"))
      <|> 
      (do c <- satisfy (/= '}')
          return [c])



-------------------
-- Parsing types --
-------------------

pType :: Parser Ty
pType = then2 (\t maybe_tt -> case maybe_tt of 
                                 Just tt -> TyF t tt
                                 Nothing -> t)
              paT 
              (opt (then2 sel22 (lit "->") pType))

-- Atomic types
paT = alts [ then2 TyApp pTycon (many ppT),
             pUnboxedTupleTy,
             then3 sel23 (lit "(") pType (lit ")"),
             ppT 
      ]

-- the magic bit in the middle is:  T (,T)*  so to speak
pUnboxedTupleTy
   = then3 (\ _ ts _ -> TyUTup ts)
           (lit "(#")
           (then2 (:) pType (many (then2 sel22 (lit ",") pType)))
           (lit "#)")

-- Primitive types
ppT = alts [apply TyVar pTyvar,
            apply (\tc -> TyApp tc []) pTycon
           ]

pTyvar       = sat (`notElem` ["section","primop","with"]) pName
pTycon       = alts [pConstructor, lexeme (string "()")]
pName        = lexeme (then2 (:) lower (many isIdChar))
pConstructor = lexeme (then2 (:) upper (many isIdChar))

isIdChar = satisfy (`elem` idChars)
idChars  = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "#_"

sat pred p
   = do x <- try p
        if pred x
         then return x
         else pzero

------------------------------------------------------------------
-- Helpful additions to Daan's parser stuff ----------------------
------------------------------------------------------------------

alts [p1]       = try p1
alts (p1:p2:ps) = (try p1) <|> alts (p2:ps)

then2 f p1 p2 
   = do x1 <- p1 ; x2 <- p2 ; return (f x1 x2)
then3 f p1 p2 p3
   = do x1 <- p1 ; x2 <- p2 ; x3 <- p3 ; return (f x1 x2 x3)
then4 f p1 p2 p3 p4
   = do x1 <- p1 ; x2 <- p2 ; x3 <- p3 ; x4 <- p4 ; return (f x1 x2 x3 x4)
then5 f p1 p2 p3 p4 p5
   = do x1 <- p1 ; x2 <- p2 ; x3 <- p3 ; x4 <- p4 ; x5 <- p5
        return (f x1 x2 x3 x4 x5)
then6 f p1 p2 p3 p4 p5 p6
   = do x1 <- p1 ; x2 <- p2 ; x3 <- p3 ; x4 <- p4 ; x5 <- p5 ; x6 <- p6
        return (f x1 x2 x3 x4 x5 x6)
then7 f p1 p2 p3 p4 p5 p6 p7
   = do x1 <- p1 ; x2 <- p2 ; x3 <- p3 ; x4 <- p4 ; x5 <- p5 ; x6 <- p6 ; x7 <- p7
        return (f x1 x2 x3 x4 x5 x6 x7)
opt p
   = (do x <- p; return (Just x)) <|> return Nothing
optdef d p
   = (do x <- p; return x) <|> return d

sel12 a b = a
sel22 a b = b
sel23 a b c = b
apply f p = liftM f p

-- Hacks for zapping whitespace and comments, unfortunately needed
-- because Daan won't let us have a lexer before the parser :-(
lexeme  :: Parser p -> Parser p
lexeme p = then2 sel12 p pCommentAndWhitespace

lit :: String -> Parser ()
lit s = apply (const ()) (lexeme (string s))

pCommentAndWhitespace :: Parser ()
pCommentAndWhitespace
   = apply (const ()) (many (alts [pLineComment, 
                                   apply (const ()) (satisfy isSpace)]))
     <|>
     return ()

pLineComment :: Parser ()
pLineComment
   = try (then3 (\_ _ _ -> ()) (string "--") (many (satisfy (/= '\n'))) (char '\n'))

stringLiteral :: Parser String
stringLiteral   = lexeme (
                      do { between (char '"')                   
                                   (char '"' <?> "end of string")
                                   (many (noneOf "\"")) 
                         }
                      <?> "literal string")



------------------------------------------------------------------
-- end                                                          --
------------------------------------------------------------------




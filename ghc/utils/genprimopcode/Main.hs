
------------------------------------------------------------------
-- A primop-table mangling program                              --
------------------------------------------------------------------

module Main where

import Parsec
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
             Left err -> do putStr "parse error at "
                            print err
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
			
		      "--make-latex-table"
			 -> putStr (gen_latex_table p_o_specs)
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
       "--make-latex-table"
     ]

------------------------------------------------------------------
-- Code generators -----------------------------------------------
------------------------------------------------------------------

gen_latex_table (Info defaults pos)
   = "\\begin{tabular}{|l|l|}\n"
     ++ "\\hline\nName &\t Type\\\\\n\\hline\n"
     ++ (concat (map f pos))
     ++ "\\end{tabular}"
     where 
       f spec = "@" ++ (encode (name spec)) ++ "@ &\t@" ++ (pty (ty spec)) ++ "@\\\\\n"
       encode s = s
       pty (TyF t1 t2) = pbty t1 ++ " -> " ++ pty t2
       pty t = pbty t
       pbty (TyApp tc ts) = (encode tc) ++ (concat (map (' ':) (map paty ts)))
       pbty (TyUTup ts) = (mkUtupnm (length ts)) ++ (concat (map (' ':) (map paty ts)))
       pbty t = paty t
       paty (TyVar tv) = encode tv
       paty t = "(" ++ pty t ++ ")"
       mkUtupnm 1 = "ZL#z32U#ZR"
       mkUtupnm n = "Z" ++ (show (n-1)) ++ "U"

gen_wrappers (Info defaults pos)
   = "module PrelPrimopWrappers where\n" 
     ++ "import qualified PrelGHC\n" 
     ++ unlines (map f (filter (not.dodgy) pos))
     where
        f spec = let args = map (\n -> "a" ++ show n) [1 .. arity (ty spec)]
                     src_name = wrap (name spec)
                 in "{-# NOINLINE " ++ src_name ++ " #-}\n" ++ 
                    src_name ++ " " ++ unwords args 
                     ++ " = (PrelGHC." ++ name spec ++ ") " ++ unwords args
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


gen_primop_list (Info defaults pos)
   = unlines (
        [      "   [" ++ cons (head pos)       ]
        ++
        map (\pi -> "   , " ++ cons pi) (tail pos)
        ++ 
        [     "   ]"     ]
     )

gen_primop_tag (Info defaults pos)
   = unlines (zipWith f pos [1..])
     where
        f i n = "tagOf_PrimOp " ++ cons i 
                ++ " = _ILIT(" ++ show n ++ ") :: FastInt"

gen_data_decl (Info defaults pos)
   = let conss = map cons pos
     in  "data PrimOp\n   = " ++ head conss ++ "\n"
         ++ unlines (map ("   | "++) (tail conss))

gen_switch_from_attribs :: String -> String -> Info -> String
gen_switch_from_attribs attrib_name fn_name (Info defaults pos)
   = let defv = lookup_attrib attrib_name defaults
         alts = catMaybes (map mkAlt pos)

         getAltRhs (OptionFalse _)    = "False"
         getAltRhs (OptionTrue _)     = "True"
         getAltRhs (OptionString _ s) = s

         mkAlt po
            = case lookup_attrib attrib_name (opts po) of
                 Nothing -> Nothing
                 Just xx -> Just (fn_name ++ " " ++ cons po ++ " = " ++ getAltRhs xx)

         lookup_attrib nm [] = Nothing
         lookup_attrib nm (a:as) 
            = if get_attrib_name a == nm then Just a else lookup_attrib nm as
     in
         case defv of
            Nothing -> error ("gen_switch_from: " ++ attrib_name)
            Just xx 
               -> unlines alts 
                  ++ fn_name ++ " other = " ++ getAltRhs xx ++ "\n"

------------------------------------------------------------------
-- Create PrimOpInfo text from PrimOpSpecs -----------------------
------------------------------------------------------------------


gen_primop_info (Info defaults pos)
   = unlines (map mkPOItext pos)

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
            
sl_name i = "SLIT(\"" ++ name i ++ "\") "

ppTyVar "a" = "alphaTyVar"
ppTyVar "b" = "betaTyVar"
ppTyVar "c" = "gammaTyVar"
ppTyVar "s" = "deltaTyVar"
ppTyVar "o" = "openAlphaTyVar"


ppType (TyApp "Bool"        []) = "boolTy"

ppType (TyApp "Int#"        []) = "intPrimTy"
ppType (TyApp "Int64#"      []) = "int64PrimTy"
ppType (TyApp "Char#"       []) = "charPrimTy"
ppType (TyApp "Word#"       []) = "wordPrimTy"
ppType (TyApp "Word64#"     []) = "word64PrimTy"
ppType (TyApp "Addr#"       []) = "addrPrimTy"
ppType (TyApp "Float#"      []) = "floatPrimTy"
ppType (TyApp "Double#"     []) = "doublePrimTy"
ppType (TyApp "ByteArr#"    []) = "byteArrayPrimTy"
ppType (TyApp "RealWorld"   []) = "realWorldTy"
ppType (TyApp "ThreadId#"   []) = "threadIdPrimTy"
ppType (TyApp "ForeignObj#" []) = "foreignObjPrimTy"
ppType (TyApp "BCO#"        []) = "bcoPrimTy"
ppType (TyApp "Unit"        []) = "unitTy"   -- dodgy


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

-- info for all primops; the totality of the info in primops.txt
data Info
   = Info [Option] [PrimOpSpec]   -- defaults, primops
     deriving Show

-- info for one primop
data PrimOpSpec
    = PrimOpSpec { cons  :: String,      -- PrimOp name
                   name  :: String,      -- name in prog text
                   ty    :: Ty,          -- type
                   cat   :: Category,    -- category
                   opts  :: [Option] }   -- default overrides
    deriving Show

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
sanityTop (Info defs primops)
   = let opt_names = map get_attrib_name defs
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

------------------------------------------------------------------
-- The parser ----------------------------------------------------
------------------------------------------------------------------

-- Due to lack of proper lexing facilities, a hack to zap any
-- leading comments
pTop :: Parser Info
pTop = then4 (\_ ds ss _ -> Info ds ss) 
             pCommentAndWhitespace pDefaults (many pPrimOpSpec)
             (lit "thats_all_folks")

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

pPrimOpSpec :: Parser PrimOpSpec
pPrimOpSpec
   = then6 (\_ c n k t o -> PrimOpSpec { cons = c, name = n, ty = t, 
                                         cat = k, opts = o } )
           (lit "primop") pConstructor stringLiteral 
           pCategory pType pOptions

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

pStuffBetweenBraces
    = lexeme (then3 sel23 
                    (char '{') (many (satisfy (not . (== '}')))) 
                    (char '}'))

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

pTyvar       = sat (`notElem` ["primop","with"]) pName
pTycon       = pConstructor
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




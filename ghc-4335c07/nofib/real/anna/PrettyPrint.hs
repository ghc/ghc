
-- ==========================================================--
-- === Pretty-printer                   prettyprint.m (1) ===--
-- ==========================================================--

module PrettyPrint where
import BaseDefs
import Utils
import MyUtils

-- ==========================================================--
--
ppPrintCExpr :: CExpr -> [Char]

ppPrintCExpr = utiMkStr . ppPrintCExprMain


-- ==========================================================--
--
ppPrintCExprMain (EVar v) = utiStr v
ppPrintCExprMain (ENum n) = utiNum n
ppPrintCExprMain (EConstr c) = utiStr c

ppPrintCExprMain (EAp e1 e2) 
   = (ppPrintLAp e1) `utiAppend`
     ((utiStr " ") `utiAppend`
     (ppPrintRAp e2))
 
ppPrintCExprMain (ELet isRec ds e)
   = (utiStr "let") `utiAppend` 
       (rec `utiAppend`
           ((utiIndent
                (utiInterleave (utiStr ";\n")
	                      [(utiStr n) `utiAppend`
                               ((utiStr " = ") `utiAppend`        
                     	       (ppPrintCExprMain e)) | (n,e) <- ds]
                )
            ) `utiAppend`
                ((utiStr "\nin ") `utiAppend`  (ppPrintCExprMain e))))

     where rec | isRec      = utiStr "rec\n  "
	       | otherwise  = utiStr "\n  "

ppPrintCExprMain (ELam vs e)
   = (utiStr "\\") `utiAppend` 
	((utiInterleave (utiStr " ") (map utiStr vs)) `utiAppend`
     ((utiStr " -> ") `utiAppend` (utiIndent (ppPrintCExprMain e))))

ppPrintCExprMain (ECase sw al)
   = (utiStr "case ") `utiAppend` ((ppPrintCExprMain sw) 
     `utiAppend` ((utiStr " of\n" ) `utiAppend`
     ((utiIndent
	(utiInterleave (utiStr ";\n")
	(map ppPrintAlter al) ) )
     `utiAppend` (utiStr "\nend"))))


-- ==========================================================--
--
ppPrintAlter (cn, (cal, cexp)) 
   = (utiStr "  ") `utiAppend` ((utiStr cn) 
      `utiAppend` ((utiStr " ") `utiAppend`
      ((utiInterleave (utiStr " ")
      [(utiStr ca) | ca <- cal]) `utiAppend`
      ((utiStr " -> ") `utiAppend` ((utiIndent (ppPrintCExprMain cexp)))))))



-- ==========================================================--
--
ppPrintRAp    (EVar v)      = utiStr v
ppPrintRAp    (ENum n)      = utiNum n
ppPrintRAp    (EConstr c)   = utiStr c
ppPrintRAp    e             = (utiStr "(") `utiAppend` ((ppPrintCExprMain e) 
                               `utiAppend` (utiStr ")"))


-- ==========================================================--
--
ppPrintLAp    (EVar v)      = utiStr v
ppPrintLAp    (ENum n)      = utiNum n
ppPrintLAp    (EConstr c)   = utiStr c
ppPrintLAp    (EAp e1 e2)   = (ppPrintLAp e1) `utiAppend`
                              ((utiStr " ") `utiAppend`
                              (ppPrintRAp e2))
ppPrintLAp    e             = (utiStr "(") `utiAppend` ((ppPrintCExprMain e) 
                              `utiAppend` (utiStr ")"))


-- ==========================================================--
--
ppPrintTypeDef :: TypeDef -> [Char]

ppPrintTypeDef = utiMkStr . ppPrintTypeDefMain

ppPrintTypeDefMain (tn, tal, tcl) 
   = (utiStr tn) `utiAppend` 
     ((utiStr " ") `utiAppend`
     ((utiInterleave (utiStr " ") (map utiStr tal)) `utiAppend`
     ((utiStr " ::= ") `utiAppend` 
     ((utiIndent
         (utiInterleave (utiStr " |\n") 
         (map ppPrintConstrAlt tcl) ) )))))


-- ==========================================================--
--
ppPrintConstrAlt (cn, ctes) 
   = (utiStr cn) `utiAppend` ((utiStr " ") `utiAppend`
     ((utiInterleave (utiStr " ") (map ppPrintTDefExpr ctes) )))


-- ==========================================================--
--
ppPrintTDefExpr (TDefVar n) = utiStr n

ppPrintTDefExpr (TDefCons n te) 
   = (utiStr "(") `utiAppend` ((utiStr n) `utiAppend` 
     ((utiStr " ") `utiAppend`
     ((utiInterleave (utiStr " ") (map ppPrintTDefExpr te)) `utiAppend`
     ((utiStr ")" )))))


-- ==========================================================--
--
ppPrintParsed :: AtomicProgram -> [Char]

ppPrintParsed (tds, ce) 
   = (tdsChars tds) ++ ";;\n\n" ++ (ppPrintCExpr ce)
     where
        tdsChars [] = ""
        tdsChars (t:ts) = "\n" ++ (ppPrintTypeDef t) ++ ";\n\n" 
                          ++ (tdsChars ts)

-- ==========================================================--
-- === End                              prettyprint.m (1) ===--
-- ==========================================================--


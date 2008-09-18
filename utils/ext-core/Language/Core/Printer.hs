{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Language.Core.Printer where

import Text.PrettyPrint.HughesPJ
import Data.Char

import Language.Core.Core
import Language.Core.Encoding

instance Show Module where
  showsPrec _ m = shows (pmodule m)

instance Show Tdef where
  showsPrec _ t = shows (ptdef t)

instance Show Cdef where
  showsPrec _ c = shows (pcdef c)

instance Show Vdefg where
  showsPrec _ v = shows (pvdefg v)

instance Show Vdef where
  showsPrec _ v = shows (pvdef v)

instance Show Exp where
  showsPrec _ e = shows (pexp e)

instance Show Alt where
  showsPrec _ a = shows (palt a)

instance Show Ty where
  showsPrec _ t = shows (pty t)

instance Show Kind where
  showsPrec _ k = shows (pkind k)

instance Show Lit where
  showsPrec _ l = shows (plit l)

instance Show CoreLit where
  showsPrec _ l = shows (pclit l)

instance Show KindOrCoercion where
  showsPrec _ (Kind k) = shows (text "<K" <+> pkind k <> text ">")
  showsPrec _ (Coercion (DefinedCoercion tbs (t1,t2))) = 
     shows (text "<C" <+> hsep (map ptbind tbs) <+>
              parens (pkind (Keq t1 t2)) <> text ">") 

instance Show AnMname where
  showsPrec _ mn = shows (panmname mn)

indent = nest 2

-- seems like this is asking for a type class...

pmodule (Module mname tdefs vdefgs) =
  (text "%module" <+> panmname mname)
  $$ indent ((vcat (map ((<> char ';') . ptdef) tdefs))
	     $$ (vcat (map ((<> char ';') . pvdefg) vdefgs)))
  <> (if ((not.null) tdefs) || ((not.null) vdefgs) then char '\n' else empty)
         -- add final newline; sigh.

ptdef (Data qtcon tbinds cdefs) =
  (text "%data" <+> pqname qtcon <+> (hsep (map ptbind tbinds)) <+> char '=')
  $$ indent (braces ((vcat (punctuate (char ';') (map pcdef cdefs)))))

ptdef (Newtype qtcon coercion tbinds tyopt) =
  text "%newtype" <+> pqname qtcon <+> pqname coercion 
    <+> (hsep (map ptbind tbinds)) $$ indent repclause
       where repclause = char '=' <+> pty tyopt

pcdef (Constr qdcon tbinds tys)  =
  (pqname qdcon) <+> (sep [hsep (map pattbind tbinds),sep (map paty tys)])

pname = text

pqname (m,v) = pmname m <> pname v

-- be sure to print the '.' here so we don't print out
-- ".foo" for unqualified foo...
pmname Nothing = empty
pmname (Just m) = panmname m <> char '.'

panmname (M (P pkgName, parents, name)) =
  let parentStrs = map pname parents in
         pname pkgName <> char ':' <>
         -- This is to be sure to not print out:
         -- main:.Main for when there's a single module name
         -- with no parents.
             (case parentStrs of
                [] -> empty
                _  -> hcat (punctuate hierModuleSeparator 
                        (map pname parents)) 
                      <> hierModuleSeparator)
             <> pname name

-- note that this is not a '.' but a Z-encoded '.':
-- GHCziIOBase.IO, not GHC.IOBase.IO.
-- What a pain.
hierModuleSeparator = text (zEncodeString ".")

ptbind (t,Klifted) = pname t
ptbind (t,k) = parens (pname t <> text "::" <> pkind k)

pattbind (t,k) = char '@' <> ptbind (t,k)

pakind (Klifted) = char '*'
pakind (Kunlifted) = char '#'
pakind (Kopen) = char '?'
pakind k = parens (pkind k)

pkind (Karrow k1 k2) = parens (pakind k1 <> text "->" <> pkind k2)
pkind (Keq from to) = peqkind (from,to)
pkind k = pakind k

peqkind (t1, t2) = parens (parens (pty t1) <+> text ":=:" <+> parens (pty t2)) 

paty (Tvar n) = pname n
paty (Tcon c) = pqname c
paty t = parens (pty t)

pbty (Tapp(Tapp(Tcon tc) t1) t2) | tc == tcArrow = parens(fsep [pbty t1, text "->",pty t2])
pbty (Tapp t1 t2) = pappty t1 [t2] 
pbty t = paty t

pty (Tapp(Tapp(Tcon tc) t1) t2) | tc == tcArrow = fsep [pbty t1, text "->",pty t2]
pty (Tforall tb t) = text "%forall" <+> pforall [tb] t
pty (TransCoercion t1 t2) = 
    (sep ([text "%trans", paty t1, paty t2]))
pty (SymCoercion t) = 
    (sep [text "%sym", paty t])
pty (UnsafeCoercion t1 t2) = 
    (sep [text "%unsafe", paty t1, paty t2])
pty (LeftCoercion t) = 
    (text "%left" <+> paty t)
pty (RightCoercion t) = 
    (text "%right" <+> paty t)
pty (InstCoercion t1 t2) = 
    (sep [text "%inst", paty t1, paty t2])
pty t = pbty t

pappty (Tapp t1 t2) ts = pappty t1 (t2:ts)
pappty t ts = sep (map paty (t:ts))

pforall tbs (Tforall tb t) = pforall (tbs ++ [tb]) t
pforall tbs t = hsep (map ptbind tbs) <+> char '.' <+> pty t

pvdefg (Rec vdefs) = text "%rec" $$ braces (indent (vcat (punctuate (char ';') (map pvdef vdefs))))
pvdefg (Nonrec vdef) = pvdef vdef

pvdef (Vdef (qv,t,e)) = sep [pqname qv <+> text "::" <+> pty t <+> char '=',
		     indent (pexp e)]

paexp (Var x) = pqname x
paexp (Dcon x) = pqname x
paexp (Lit l) = plit l
paexp e = parens(pexp e)

plamexp bs (Lam b e) = plamexp (bs ++ [b]) e
plamexp bs e = sep [sep (map pbind bs) <+> text "->",
		    indent (pexp e)]

pbind (Tb tb) = char '@' <+> ptbind tb
pbind (Vb vb) = pvbind vb

pfexp (App e1 e2) = pappexp e1 [Left e2]
pfexp (Appt e t) = pappexp e [Right t]
pfexp e = paexp e

pappexp (App e1 e2) as = pappexp e1 (Left e2:as)
pappexp (Appt e t) as = pappexp e (Right t:as)
pappexp e as = fsep (paexp e : map pa as)
           where pa (Left ex) = paexp ex
		 pa (Right t) = char '@' <+> paty t

pexp (Lam b e) = char '\\' <+> plamexp [b] e
pexp (Let vd e) = (text "%let" <+> pvdefg vd) $$ (text "%in" <+> pexp e)
pexp (Case e vb t alts) = sep [text "%case" <+> paty t <+> paexp e,
			     text "%of" <+> pvbind vb]
			$$ (indent (braces (vcat (punctuate (char ';') (map palt alts)))))
pexp (Cast e t) = (text "%cast" <+> parens (pexp e)) $$ paty t
pexp (Note s e) = (text "%note" <+> pstring s) $$ pexp e
-- TODO: ccall shouldn't really be there
pexp (External n t) = (text "%external ccall" <+> pstring n) $$ paty t
pexp e = pfexp e


pvbind (x,t) = parens(pname x <> text "::" <> pty t)

palt (Acon c tbs vbs e) =
	sep [pqname c, 
	     sep (map pattbind tbs),
	     sep (map pvbind vbs) <+> text "->"]
        $$ indent (pexp e)
palt (Alit l e) = 
	(plit l <+>  text "->")
	$$ indent (pexp e)
palt (Adefault e) = 
	(text "%_ ->")
	$$ indent (pexp e)

plit (Literal cl t) = parens (pclit cl <> text "::" <> pty t)

pclit (Lint i) = integer i
-- makes sure to print it out as n % d
pclit (Lrational r) = text (show r)
pclit (Lchar c) = text ("\'" ++ escape [c] ++ "\'")
pclit (Lstring s) = pstring s

pstring s = doubleQuotes(text (escape s))

escape :: String -> String
escape s = foldr f [] (map ord s)
    where 
     f cv rest
	| cv > 0xFF = '\\':'x':hs ++ rest
	| (cv < 0x20 || cv > 0x7e || cv == 0x22 || cv == 0x27 || cv == 0x5c) = 
	 '\\':'x':h1:h0:rest
           where (q1,r1) = quotRem cv 16
		 h1 = intToDigit q1
                 h0 = intToDigit r1
		 hs = dropWhile (=='0') $ reverse $ mkHex cv
		 mkHex 0 = ""
		 mkHex num = intToDigit r : mkHex q
		    where (q,r) = quotRem num 16
     f cv rest = (chr cv):rest

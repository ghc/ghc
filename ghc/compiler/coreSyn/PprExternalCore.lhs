%
% (c) The University of Glasgow 2001
%
\begin{code}

module PprExternalCore () where

import Pretty
import ExternalCore
import Char

instance Show Module where
  showsPrec d m = shows (pmodule m)

instance Show Tdef where
  showsPrec d t = shows (ptdef t)

instance Show Cdef where
  showsPrec d c = shows (pcdef c)

instance Show Vdefg where
  showsPrec d v = shows (pvdefg v)

instance Show Exp where
  showsPrec d e = shows (pexp e)

instance Show Alt where
  showsPrec d a = shows (palt a)

instance Show Ty where
  showsPrec d t = shows (pty t)

instance Show Kind where
  showsPrec d k = shows (pkind k)

instance Show Lit where
  showsPrec d l = shows (plit l)


indent = nest 2

pmodule (Module mname tdefs vdefgs) =
  (text "%module" <+> text mname)
    $$ indent ((vcat (map ((<> char ';') . ptdef) tdefs))
	       $$ (vcat (map ((<> char ';') . pvdefg) vdefgs)))

ptdef (Data tcon tbinds cdefs) =
  (text "%data" <+> pqname tcon <+> (hsep (map ptbind tbinds)) <+> char '=')
  $$ indent (braces ((vcat (punctuate (char ';') (map pcdef cdefs)))))

ptdef (Newtype tcon tbinds rep ) =
  text "%newtype" <+> pqname tcon <+> (hsep (map ptbind tbinds)) <+> repclause
       where repclause = case rep of
                           Just ty -> char '=' <+> pty ty 
			   Nothing -> empty

pcdef (Constr dcon tbinds tys)  =
  (pname dcon) <+> (sep [hsep (map pattbind tbinds),sep (map paty tys)])
pcdef (GadtConstr dcon ty)  =
  (pname dcon) <+> text "::" <+> pty ty

pname id = text id

pqname ("",id) = pname id
pqname (m,id)  = pname m <> char '.' <> pname id

ptbind (t,Klifted) = pname t
ptbind (t,k) = parens (pname t <> text "::" <> pkind k)

pattbind (t,k) = char '@' <> ptbind (t,k)

pakind (Klifted) = char '*'
pakind (Kunlifted) = char '#'
pakind (Kopen) = char '?'
pakind k = parens (pkind k)

pkind (Karrow k1 k2) = parens (pakind k1 <> text "->" <> pkind k2)
pkind k = pakind k

paty (Tvar n) = pname n
paty (Tcon c) = pqname c
paty t = parens (pty t)

pbty (Tapp(Tapp(Tcon tc) t1) t2) | tc == tcArrow = parens(fsep [pbty t1, text "->",pty t2])
pbty (Tapp t1 t2) = pappty t1 [t2] 
pbty t = paty t

pty (Tapp(Tapp(Tcon tc) t1) t2) | tc == tcArrow = fsep [pbty t1, text "->",pty t2]
pty (Tforall tb t) = text "%forall" <+> pforall [tb] t
pty t = pbty t

pappty (Tapp t1 t2) ts = pappty t1 (t2:ts)
pappty t ts = sep (map paty (t:ts))

pforall tbs (Tforall tb t) = pforall (tbs ++ [tb]) t
pforall tbs t = hsep (map ptbind tbs) <+> char '.' <+> pty t

pvdefg (Rec vtes) = text "%rec" $$ braces (indent (vcat (punctuate (char ';') (map pvte vtes))))
pvdefg (Nonrec vte) = pvte vte

pvte (v,t,e) = sep [pname v <+> text "::" <+> pty t <+> char '=',
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
           where pa (Left e) = paexp e
		 pa (Right t) = char '@' <+> paty t

pexp (Lam b e) = char '\\' <+> plamexp [b] e
pexp (Let vd e) = (text "%let" <+> pvdefg vd) $$ (text "%in" <+> pexp e)
pexp (Case e vb ty alts) = sep [text "%case" <+> parens (paty ty) <+> paexp e,
			     text "%of" <+> pvbind vb]
			$$ (indent (braces (vcat (punctuate (char ';') (map palt alts)))))
pexp (Coerce t e) = (text "%coerce" <+> paty t) $$ pexp e
pexp (Note s e) = (text "%note" <+> pstring s) $$ pexp e
pexp (External n t) = (text "%external" <+> pstring n) $$ paty t
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

plit (Lint i t) = parens (integer i <> text "::" <> pty t)
plit (Lrational r t) = parens (rational r <>  text "::" <> pty t)  -- might be better to print as two integers
plit (Lchar c t) = parens (text ("\'" ++ escape [c] ++ "\'") <> text "::" <> pty t)
plit (Lstring s t) = parens (pstring s <> text "::" <> pty t)

pstring s = doubleQuotes(text (escape s))

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
		 mkHex cv = intToDigit r : mkHex q
		    where (q,r) = quotRem cv 16
     f cv rest = (chr cv):rest

\end{code}





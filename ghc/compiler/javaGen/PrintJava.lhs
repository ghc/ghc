%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Generate Java}

\begin{code}
module PrintJava( compilationUnit ) where

import Java
import Outputable
import Char( toLower )
\end{code}

\begin{code}
indent :: SDoc -> SDoc
indent = nest 2
\end{code}
  
%************************************************************************
%*									*
\subsection{Pretty printer}
%*									*
%************************************************************************

\begin{code}
compilationUnit :: CompilationUnit -> SDoc
compilationUnit (Package n ds) = package n (decls ds)

package = \n -> \ds ->
  text "package" <+> name n <> text ";"
  $$
  ds
  
decls []     = empty
decls (d:ds) = decl d $$ decls ds
    
decl = \d ->
  case d of
    { Import n -> importDecl (name n)
    ; Field mfs t n e -> field (modifiers mfs) (typ t) (name n) e  
    ; Constructor mfs n as ss -> constructor (modifiers mfs) (name n) (parameters as) (statements ss)
    ; Method mfs t n as ts ss -> method (modifiers mfs) (typ t) (name n) (parameters as) (throws ts) (statements ss)
    ; Comment s -> comment s
    ; Interface mfs n is ms -> interface (modifiers mfs) (name n) (extends is) (decls ms)
    ; Class mfs n x is ms -> clazz (modifiers mfs) (name n) (extends x) (implements is) (decls ms)
    }

importDecl n = text "import" <+> n <> text ";"
  
field = \mfs -> \t -> \n -> \e ->
  case e of
    { Nothing -> mfs <+> t <+> n <> text ";" 
    ; Just e  -> lay [mfs <+> t <+> n <+> text "=", indent (expr e <> text ";")]
	     where
		lay | isSimple e = hsep
		    | otherwise  = sep
    }

constructor = \mfs -> \n -> \as -> \ss ->
  mfs <+> n <+> parens (hsep (punctuate comma as)) <+> text "{"
  $$ indent ss 
  $$ text "}"

method = \mfs -> \t -> \n -> \as -> \ts -> \ss -> 
  mfs <+> t <+> n <+> parens (hsep (punctuate comma as)) <+> ts <+> text "{" 
  $$ indent ss 
  $$ text "}"

comment = \ss ->
  text "/**"
  $$ indent (vcat [ text s | s <- ss])
  $$ text "**/"

interface = \mfs -> \n -> \xs -> \ms -> 
  mfs <+> n <+> xs <+> text "{"
  $$ indent ms
  $$ text "}"
     
clazz = \mfs -> \n -> \x -> \is -> \ms ->
  mfs <+> text "class" <+> n <+> x <+> is <+> text "{" 
  $$ indent ms 
  $$ text "}"

staticblock = \ss ->
  text "static" <+> text "{"
  $$ indent ss
  $$ text "}"
    
modifiers mfs = hsep (map modifier mfs)
    
modifier mf = text $ map toLower (show mf)
  
extends [] = empty
extends xs = text "extends" <+> hsep (punctuate comma (map name xs))

implements [] = empty
implements xs = text "implements" <+> hsep (punctuate comma (map name xs))

throws [] = empty
throws xs = text "throws" <+> hsep (punctuate comma (map name xs))

name n = text n

parameters as = map parameter as

parameter (Parameter mfs t n) = modifiers mfs <+> typ t <+> name n

typ (PrimType s)  = primtype s
typ (Type n)      = name n
typ (ArrayType t) = typ t <> text "[]"

primtype PrimInt     = text "int"
primtype PrimBoolean = text "boolean"
primtype PrimChar    = text "char"
primtype PrimLong    = text "long"
primtype PrimFloat   = text "float"
primtype PrimDouble  = text "double"
primtype PrimByte    = text "byte"



statements ss = vcat (map statement ss)
  
statement = \s ->
  case s of
    { Skip -> skip
    ; Return e -> returnStat (expr e)
    ; Block ss -> vcat [statement s | s <- ss]
    ; ExprStatement e -> exprStatement (expr e)
    ; Declaration d -> declStatement (decl d)
    ; IfThenElse ecs s -> ifthenelse [ (expr e, statement s) | (e,s) <- ecs ] (maybe Nothing (Just .statement) s)
    ; Switch e as d -> switch (expr e) (arms as) (deflt d)
    } 

skip = empty
  
returnStat e = sep [text "return", indent e <> semi]

exprStatement e = e <> semi

declStatement d = d

ifthenelse ((e,s):ecs) ms = sep [text "if" <+> parens e <+> text "{", 
			        indent s, 
			      thenelse ecs ms]

thenelse ((e,s):ecs) ms = sep [	text "} else if" <+> parens e <+> text "{", 
				indent s,
				thenelse ecs ms]

thenelse [] Nothing  = text "}"
thenelse [] (Just s) = sep [text "} else {", indent s, text "}"]
    
switch = \e -> \as -> \d ->
  text "switch" <+> parens e <+> text "{" 
  $$ indent (as $$ d)
  $$ text "}"
  
deflt Nothing   = empty
deflt (Just ss) = text "default:" $$ indent (statements ss)  
    
arms [] = empty
arms ((e,ss):as) = text "case" <+> expr e <> colon
                   $$ indent (statements ss)
                   $$ arms as

maybeExpr Nothing  = Nothing
maybeExpr (Just e) = Just (expr e)
           
expr = \e ->
 case e of
   { Var n _ -> name n
   ; Literal l _ -> literal l
   ; Cast t e -> cast (typ t) e
   ; Access e n -> expr e <> text "." <> name n
   ; Assign l r -> assign (expr l) r
   ; New n es ds -> new (typ n) es (maybeClass ds)
   ; Call e n es -> call (expr e) (name n) es
   ; Op e1 o e2 -> op e1 o e2
   ; InstanceOf e t -> expr e <+> text "instanceof" <+> typ t
   }
   
op = \e1 -> \o -> \e2 ->
  ( if isSimple e1 
    then expr e1 
    else parens (expr e1)
  ) 
  <+> 
  text o
  <+>
  ( if isSimple e2
    then expr e2 
    else parens (expr e2)
  )
  
assign = \l -> \r ->
  if isSimple r
  then l <+> text "=" <+> (expr r)
  else l <+> text "=" $$ indent (expr r)

cast = \t -> \e ->
  if isSimple e
  then parens (parens t <> expr e)
  else parens (parens t $$ indent (expr e))

new n [] (Just ds) = sep [text "new" <+> n <+> text "()" <+> text "{",
			     indent ds,
			     text "}"]
new n es Nothing = text "new" <+> n <> parens (hsep (punctuate comma (map expr es)))

      
call e n es = e <> dot <> n <> parens (hsep (punctuate comma (map expr es)))

literal = \l ->
  case l of
    { IntLit i    -> text (show i)
    ; UIntLit i	  -> text (show i)
    ; CharLit c   -> text (show c)
    ; UCharLit c  -> text (show c)
    ; StringLit s -> text (show s)
    }

maybeClass Nothing   = Nothing
maybeClass (Just ds) = Just (decls ds)
\end{code}

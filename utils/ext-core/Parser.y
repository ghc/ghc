{
module Parser ( parse ) where

import Core
import ParseGlue
import Lex

}

%name parse
%expect 0
%tokentype { Token }

%token
 '%module'	{ TKmodule }
 '%data'	{ TKdata }
 '%newtype'	{ TKnewtype }
 '%forall'	{ TKforall }
 '%rec'		{ TKrec }
 '%let'		{ TKlet }
 '%in'		{ TKin }
 '%case'	{ TKcase }
 '%of'		{ TKof }
 '%cast'	{ TKcast }
 '%note'	{ TKnote }
 '%external'	{ TKexternal }
 '%_'		{ TKwild }
 '('		{ TKoparen }
 ')'		{ TKcparen }
 '{'		{ TKobrace }
 '}'		{ TKcbrace }
 '#' 		{ TKhash}
 '='		{ TKeq }
 '::'		{ TKcoloncolon }
 '*'		{ TKstar }
 '->'		{ TKrarrow }
 '\\'		{ TKlambda}
 '@'		{ TKat }
 '.'		{ TKdot }
 ':'		{ TKcolon }
 '?'		{ TKquestion}
 ';'            { TKsemicolon }
 NAME		{ TKname $$ }
 CNAME 		{ TKcname $$ }
 INTEGER	{ TKinteger $$ }
 RATIONAL	{ TKrational $$ }
 STRING		{ TKstring $$ }
 CHAR		{ TKchar $$ }

%monad { P } { thenP } { returnP }
%lexer { lexer } { TKEOF }

%%

module	:: { Module }
	: '%module' mname tdefs  vdefgs 
		{ Module $2 $3 $4 }

tdefs	:: { [Tdef] }
	: {- empty -}	{[]}
	| tdef ';' tdefs	{$1:$3}

tdef	:: { Tdef }
	: '%data' qcname tbinds '=' '{' cons1 '}'
		{ Data $2 $3 $6 }
	| '%newtype' qcname tbinds trep 
		{ Newtype $2 $3 $4 }

trep    :: { Maybe Ty }
        : {- empty -}   {Nothing}
        | '=' ty        { Just $2 }

tbind	:: { Tbind }
	:  name { ($1,Klifted) }
	|  '(' name '::' akind ')'
		{ ($2,$4) }

tbinds 	:: { [Tbind] }
	: {- empty -}	{ [] }
	| tbind tbinds	{ $1:$2 }


vbind	:: { Vbind }
	: '(' name '::' ty')'	{ ($2,$4) }

vbinds	:: { [Vbind] }
	: {-empty -} 	{ [] }
	| vbind vbinds	{ $1:$2 }

bind	:: { Bind }
	: '@' tbind 	{ Tb $2 }
	| vbind		{ Vb $1 }

binds1 	:: { [Bind] }
	: bind		{ [$1] }
	| bind binds1	{ $1:$2 }

attbinds :: { [Tbind] }
	: {- empty -} 	{ [] }
	| '@' tbind attbinds 
			{ $2:$3 }

akind	:: { Kind }
	: '*' 		{Klifted}	
	| '#'		{Kunlifted}
	| '?'		{Kopen}
        | '(' kind ')'	{ $2 }

kind 	:: { Kind }
	: akind 	{ $1 }
	| akind '->' kind 
		{ Karrow $1 $3 }

cons1	:: { [Cdef] }
	: con		{ [$1] }
	| con ';' cons1	{ $1:$3 }

con	:: { Cdef }
	: qcname attbinds atys 
		{ Constr $1 $2 $3 }

atys	:: { [Ty] }
	: {- empty -} { [] }
	| aty atys      { $1:$2 }

aty	:: { Ty }
	: name	{ Tvar $1 }
	| qcname { Tcon $1 }
	| '(' ty ')' { $2 }


bty	:: { Ty }
	: aty	{ $1 }
        | bty aty { Tapp $1 $2 }

ty	:: { Ty }
	: bty	{$1}
	| bty '->' ty 
		{ tArrow $1 $3 }
	| '%forall' tbinds '.' ty 
		{ foldr Tforall $4 $2 }

vdefgs	:: { [Vdefg] }
	: {- empty -}	        { [] }
	| vdefg ';' vdefgs	{$1:$3 }

vdefg	:: { Vdefg }
	: '%rec' '{' vdefs1 '}'
		       { Rec $3 }
	|  vdef { Nonrec $1}

vdefs1	:: { [Vdef] }
	: vdef		{ [$1] }
	| vdef ';' vdefs1 { $1:$3 }

vdef	:: { Vdef }
	: qname '::' ty '=' exp 
		{ Vdef ($1,$3,$5) }

aexp    :: { Exp }
	: qname 	{ Var $1 }
        | qcname 	{ Dcon $1 } 
	| lit		{ Lit $1 }
	| '(' exp ')' 	{ $2 }

fexp	:: { Exp }
	: fexp aexp	{ App $1 $2 }
	| fexp '@' aty	{ Appt $1 $3 }
	| aexp		{ $1 }

exp	:: { Exp }
	: fexp		{ $1 }
	| '\\' binds1 '->' exp
		{ foldr Lam $4 $2 }
	| '%let' vdefg '%in' exp 
		{ Let $2 $4 }
	| '%case' '(' ty ')' aexp '%of' vbind '{' alts1 '}'
		{ Case $5 $7 $3 $9 }
-- Note: ty, not aty! You can cast something to a forall type
-- Though now we have shift/reduce conflicts like woah
	| '%cast' exp ty
		{ Cast $2 $3 }
	| '%note' STRING exp 
		{ Note $2 $3 }
        | '%external' STRING aty
                { External $2 $3 }

alts1	:: { [Alt] }
	: alt		{ [$1] }
	| alt ';' alts1	{ $1:$3 }

alt	:: { Alt }
	: qcname attbinds vbinds '->' exp 
		{ Acon $1 $2 $3 $5 } 
	| lit '->' exp
		{ Alit $1 $3 }
	| '%_' '->' exp
		{ Adefault $3 }

lit	:: { Lit }
	: '(' INTEGER '::' aty ')'
		{ Lint $2 $4 }
	| '(' RATIONAL '::' aty ')'
		{ Lrational $2 $4 }
	| '(' CHAR '::' aty ')'
		{ Lchar $2 $4 }
	| '(' STRING '::' aty ')'
		{ Lstring $2 $4 }

name	:: { Id }
	: NAME	{ $1 }

cname	:: { Id }
	: CNAME	{ $1 }
         
mname	:: { AnMname }
        : pkgName ':' cname
             { let (parentNames, childName) = splitModuleName $3 in
                 ($1, parentNames, childName) }

pkgName :: { Id }
        : NAME { $1 }

-- TODO: Clean this up. Now hierarchical names are z-encoded.

-- note that a sequence of mnames is either:
-- empty, or a series of cnames separated by
-- dots, with a leading dot
-- See the definition of mnames: the "name" part
-- is required.
mnames :: { [Id] } 
         : {- empty -} {[]}
         | '.' cname mnames {$2:$3}

-- it sucks to have to repeat the Maybe-checking twice,
-- but otherwise we get reduce/reduce conflicts

-- TODO: this is the ambiguity here. mname '.' name --
-- but by maximal-munch, in GHC.Base.Bool the entire 
-- thing gets counted as the module name. What to do,
-- besides z-encoding the dots in the hierarchy again?
-- (Or using syntax other than a dot to separate the
-- module name from the variable name...)
qname	:: { (Mname,Id) }
        : name { (Nothing, $1) }
	| mname '.' name 
		{ (Just $1,$3) }

qcname	:: { (Mname,Id) }
        : cname { (Nothing, $1) }
        | mname '.' cname 
		{ (Just $1,$3) }


{

happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l

}

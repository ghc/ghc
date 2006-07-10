-----------------------------------------------------------------------------
$Id: HsParser.ly,v 1.21 2004/08/09 11:55:07 simonmar Exp $

(c) Simon Marlow, Sven Panne 1997-2002

Haskell grammar.
-----------------------------------------------------------------------------

ToDo: Is (,) valid as exports? We don't allow it.
ToDo: Check exactly which names must be qualified with Prelude (commas and friends)
ToDo: Inst (MPCs?)
ToDo: Polish constr a bit
ToDo: Ugly: exp0b is used for lhs, pat, exp0, ...
ToDo: Differentiate between record updates and labeled construction.

> {
> module HsParser (parse) where
> 
> import Monad
> import HsSyn2
> import HsParseMonad
> import HsLexer
> import HsParseUtils
> import HaddockLex2 	hiding (Token)
> import HaddockParse2
> import HaddockUtil 	hiding (splitTyConApp)
> import Char 		( isSpace )
> }

-----------------------------------------------------------------------------
Conflicts: 3 shift/reduce

2 for ambiguity in 'case x of y | let z = y in z :: a -> b'
	(don't know whether to reduce 'True' as a btype or shift the '->'.
	 Similarly lambda and if.  This is a rather arcane special case:
	 the default resolution in favour of the shift does what the Report
	 specifies, but the result will always fail to type-check.)

1 for ambiguity in 'x @ Rec{..}'.  
	Only sensible parse is 'x @ (Rec{..})', which is what resolving
	to shift gives us.

-----------------------------------------------------------------------------

> %token
>	VARID 	 { VarId $$ }
>       IPVARID  { IPVarId $$ }
>	QVARID 	 { QVarId $$ }
>	CONID	 { ConId $$ }
>	QCONID   { QConId $$ }
>	VARSYM	 { VarSym $$ }
>	CONSYM	 { ConSym $$ }
>	QVARSYM	 { QVarSym $$ }
>	QCONSYM  { QConSym $$ }
>	INT	 { IntTok $$ }
>	RATIONAL { FloatTok $$ }
>	CHAR	 { Character $$ }
>	STRING   { StringTok $$ }

>	PRIMINT    { PrimInt $$ }
>	PRIMSTRING { PrimString $$ }
>	PRIMFLOAT  { PrimFloat $$ }
>	PRIMDOUBLE { PrimDouble $$ }
>	PRIMCHAR   { PrimChar $$ }

Docs

>	DOCNEXT    { DocCommentNext $$ }
>	DOCPREV    { DocCommentPrev $$ }
>	DOCNAMED   { DocCommentNamed $$ }
>	DOCSECTION { DocSection _ _ }
>	DOCOPTIONS { DocOptions $$ }

Symbols

>	'('	{ LeftParen }
>	')'	{ RightParen }
>	'(#'	{ LeftUT }
>	'#)'	{ RightUT }
>	';'	{ SemiColon }
>	'{'	{ LeftCurly }
>	'}'	{ RightCurly }
>	vccurly { VRightCurly }			-- a virtual close brace
>	'['	{ LeftSquare }
>	']'	{ RightSquare }
>  	','	{ Comma }
>	'_'	{ Underscore }
>	'`'	{ BackQuote }

Reserved operators

>	'.'	{ Dot }
>	'..'	{ DotDot }
>	'::'	{ DoubleColon }
>	'='	{ Equals }
>	'\\'	{ Backslash }
>	'|'	{ Bar }
>	'<-'	{ LeftArrow }
>	'->'	{ RightArrow }
>	'@'	{ At }
>	'~'	{ Tilde }
>	'=>'	{ DoubleArrow }
>	'-'	{ Minus }
>	'!'	{ Exclamation }

Reserved Ids

>	'as'		{ KW_As }
>	'case'		{ KW_Case }
>	'ccall'		{ KW_CCall }
>	'class'		{ KW_Class }
>	'data'		{ KW_Data }
>	'default'	{ KW_Default }
>	'deriving'	{ KW_Deriving }
>	'do'		{ KW_Do }
>	'dotnet'	{ KW_DotNet }
>	'else'		{ KW_Else }
>	'export'	{ KW_Export }
>	'forall'	{ KW_Forall }
>	'foreign'	{ KW_Foreign }
>	'hiding'	{ KW_Hiding }
>	'if'		{ KW_If }
>	'import'	{ KW_Import }
>	'in'		{ KW_In }
>	'infix'		{ KW_Infix }
>	'infixl'	{ KW_InfixL }
>	'infixr'	{ KW_InfixR }
>	'instance'	{ KW_Instance }
>	'let'		{ KW_Let }
>	'module'	{ KW_Module }
>	'newtype'	{ KW_NewType }
>	'of'		{ KW_Of }
>	'safe'		{ KW_Safe }
>	'stdcall'	{ KW_StdCall }
>	'then'		{ KW_Then }
>	'threadsafe'	{ KW_ThreadSafe }
>	'type'		{ KW_Type }
>	'unsafe'	{ KW_Unsafe }
>	'where'		{ KW_Where }
>	'qualified'	{ KW_Qualified }

> %monad { P } { thenP } { returnP }
> %lexer { lexer } { EOF }
> %name parse
> %tokentype { Token }
> %%

-----------------------------------------------------------------------------
Module Header

> module :: { HsModule }
> 	: optdoc 'module' srcloc modid maybeexports 'where' body
>	    { case $1 of { (opts,info,doc) ->
>	      HsModule $3 $4 $5 (reverse (fst $7)) (snd $7)
>		opts info doc } }
>	| body srcloc
>	    { HsModule $2 main_mod Nothing (reverse (fst $1)) (snd $1)
>		Nothing emptyModuleInfo Nothing }

> optdoc :: { (Maybe String,ModuleInfo,Maybe Doc) }
>	: moduleheader			{ (Nothing, fst $1, snd $1) }
>	| DOCOPTIONS			{ (Just $1, emptyModuleInfo,Nothing) }
>	| DOCOPTIONS moduleheader	{ (Just $1, fst $2, snd $2) }
>	| moduleheader DOCOPTIONS	{ (Just $2, fst $1, snd $1) }
>	| {- empty -}			{ (Nothing, emptyModuleInfo,Nothing) } 

> body :: { ([HsImportDecl],[HsDecl]) }
>	:  '{' bodyaux '}'				{ $2 }
> 	|      layout_on  bodyaux close			{ $2 }

> bodyaux :: { ([HsImportDecl],[HsDecl]) }
>	: impdecls ';' topdecls				{ ($1, $3) }
>	|              topdecls				{ ([], $1) }
>	| impdecls             				{ ($1, []) }

> optsemi :: { () }
>	: ';'						{ () }
>	| {- empty -}					{ () }

-----------------------------------------------------------------------------
The Export List

> maybeexports :: { Maybe [HsExportSpec] }
> 	:  exports				{ Just $1 }
> 	|  {- empty -}				{ Nothing }

> exports :: { [HsExportSpec] }
>	: '(' exportlist ')'			{ $2 }

> exportlist :: { [HsExportSpec] }
>	:  export exportlist1			{ $1 : $2 }
>	|  exp_doc exportlist			{ $1 : $2 }
> 	|  {- empty -}				{ [] }

> exportlist1 :: { [HsExportSpec] }
>	:  exp_doc exportlist1			{ $1 : $2 }
> 	|  ',' exportlist			{ $2 }
>	|  {- empty -}				{ [] }

> exp_doc :: { HsExportSpec }
> 	: docsection			{ case $1 of { (i,s) -> HsEGroup i s } }
>	| docnamed			{ HsEDocNamed (fst $1) }
>	| docnext			{ HsEDoc $1 }

> export :: { HsExportSpec }
> 	:  qvar					{ HsEVar $1 }
> 	|  qgtycon				{ HsEAbs $1 }
> 	|  qgtycon '(' '..' ')'			{ HsEThingAll $1 }
> 	|  qgtycon '(' ')'		        { HsEThingWith $1 [] }
> 	|  qgtycon '(' qcnames ')'		{ HsEThingWith $1 (reverse $3) }
> 	|  'module' modid			{ HsEModuleContents $2 }

> qcnames :: { [HsQName] }
> 	:  qcnames ',' qcname			{ $3 : $1 }
> 	|  qcname				{ [$1]  }

> qcname :: { HsQName }
>	:  qvar					{ $1 }
> 	|  gcon					{ $1 }

-----------------------------------------------------------------------------
Import Declarations

> impdecls :: { [HsImportDecl] }
>	: impdecls ';' impdecl			{ $3 : $1 }
>	| impdecl				{ [$1] }

> impdecl :: { HsImportDecl }
>	: 'import' srcloc optqualified modid maybeas maybeimpspec
> 		  		{ HsImportDecl $2 $4 $3 $5 $6 }

> optqualified :: { Bool }
>       : 'qualified'                           { True  }
>       | {- empty -}				{ False }

> maybeas :: { Maybe Module }
>       : 'as' modid                            { Just $2 }
>       | {- empty -}				{ Nothing }


> maybeimpspec :: { Maybe (Bool, [HsImportSpec]) }
>	: impspec				{ Just $1 }
>	| {- empty -}				{ Nothing }

> impspec :: { (Bool, [HsImportSpec]) }
> 	:  '(' importlist ')'  			{ (False, reverse $2) }
> 	|  'hiding' '(' importlist ')' 		{ (True,  reverse $3) }

> importlist :: { [HsImportSpec] }
>	:  importlist ',' import		{ $3 : $1 }
> 	|  importlist ','			{ $1 }
>	|  import				{ [$1] }
> 	|  {- empty -}				{ [] }

> import :: { HsImportSpec }
> 	:  var					{ HsIVar $1 }
> 	|  gtycon				{ HsIAbs $1 }
> 	|  gtycon '(' '..' ')'			{ HsIThingAll $1 }
> 	|  gtycon '(' ')'		        { HsIThingWith $1 [] }
> 	|  gtycon '(' cnames ')'		{ HsIThingWith $1 (reverse $3) }

> gtycon :: { HsName }
>	: tyconorcls			{ $1 }
>	| '(' ')'			{ unit_tycon_name }
>	| '(' '->' ')'			{ fun_tycon_name }
>	| '[' ']'			{ list_tycon_name }
>	| '(' commas ')'		{ tuple_tycon_name $2 }

> cnames :: { [HsName] }
> 	:  cnames ',' cname			{ $3 : $1 }
> 	|  cname				{ [$1]  }

> cname :: { HsName }
>	:  var					{ $1 }
> 	|  con					{ $1 }

-----------------------------------------------------------------------------
Fixity Declarations

> fixdecl :: { HsDecl }
> 	: srcloc infix prec ops			{ HsInfixDecl $1 $2 $3 (reverse $4) }

> prec :: { Int }
>	: {- empty -}				{ 9 }
>	| INT					{%  checkPrec $1 `thenP` \p ->
>						    returnP (fromIntegral $1) }

> infix :: { HsAssoc }
>	: 'infix'				{ HsAssocNone  }
>	| 'infixl'				{ HsAssocLeft  }
>	| 'infixr'				{ HsAssocRight }

> ops   :: { [HsName] }
>	: ops ',' op				{ $3 : $1 }
>	| op					{ [$1] }

-----------------------------------------------------------------------------
Top-Level Declarations

> topdecls :: { [HsDecl] }
>	: topdecl ';' topdecls		{ $1 : $3 }
> 	| ';' topdecls			{ $2 }
>	| docdecl topdecls		{ $1 : $2 }
>	| topdecl			{ [$1] }
>	| {- empty -}			{ [] }

> topdecl :: { HsDecl }
>	: 'type' simpletype srcloc '=' ctypedoc
>		{ HsTypeDecl $3 (fst $2) (snd $2) $5 Nothing }
>	| 'data' ctype srcloc constrs deriving
>		{% checkDataHeader $2 `thenP` \(cs,c,t) ->
>		   returnP (HsDataDecl $3 cs c t $4 $5 Nothing) }
>	| 'newtype' ctype srcloc '=' constr deriving
>		{% checkDataHeader $2 `thenP` \(cs,c,t) ->
>		   returnP (HsNewTypeDecl $3 cs c t $5 $6 Nothing) }
>	| 'class' srcloc ctype fds optcbody
>		{% checkClassHeader $3 `thenP` \(ctxt,n,tys) ->
>		   returnP (HsClassDecl $2 ctxt n tys $4 $5 Nothing) }
>	| 'instance' srcloc ctype optvaldefs
>		{% checkInstHeader $3 `thenP` \(ctxt,asst) ->
>		   returnP (HsInstDecl $2 ctxt asst $4) }
>	| 'default' srcloc '(' typelist ')'
>		{ HsDefaultDecl $2 $4 }
>	| 'foreign' fdecl { $2 }
>       | decl		{ $1 }

> typelist :: { [HsType] }
>	: types				{ $1 }
>	| type				{ [$1] }
>	| {- empty -}			{ [] }

> decls :: { [HsDecl] }
>	: decl ';' decls		{ $1 : $3 }
> 	| docdecl decls			{ $1 : $2 }
>	| ';' decls			{ $2 }
>	| decl				{ [$1] }
>	| {- empty -}			{ [] }

> decl :: { HsDecl }
>	: signdecl		{ $1 }
>	| fixdecl		{ $1 }
>	| valdef		{ $1 }

> docdecl :: { HsDecl }
>	: srcloc docnext	{ HsDocCommentNext $1 $2 }
>	| srcloc docprev	{ HsDocCommentPrev $1 $2 }
>	| srcloc docnamed	{ case $2 of { (n,s) -> 
>					HsDocCommentNamed $1 n s } }
>	| srcloc docsection	{ case $2 of { (i,s) -> HsDocGroup $1 i s } }

> decllist :: { [HsDecl] }
>	: '{' decls '}'			{ $2 }
>	|     layout_on  decls close	{ $2 }

> signdecl :: { HsDecl }
>	: vars srcloc '::' ctypedoc	{ HsTypeSig $2 (reverse $1) $4 Nothing }

ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
instead of qvar, we get another shift/reduce-conflict. Consider the
following programs:

   { (+) :: ... }          only var
   { (+) x y  = ... }      could (incorrectly) be qvar

We re-use expressions for patterns, so a qvar would be allowed in patterns
instead of a var only (which would be correct). But deciding what the + is,
would require more lookahead. So let's check for ourselves...

> vars	:: { [HsName] }
>	: vars ',' var			{ $3 : $1 }
>	| qvar				{% checkUnQual $1 `thenP` \n ->
>					   returnP [n] }

-----------------------------------------------------------------------------
Foreign Declarations

> fdecl :: { HsDecl }
> fdecl : srcloc 'import' callconv safety fspec
>	  { case $5 of (spec,nm,ty) -> HsForeignImport $1 $3 $4 spec nm ty Nothing }
> 	| srcloc 'import' callconv fspec
>	  { case $4 of (spec,nm,ty) -> HsForeignImport $1 $3 HsFISafe spec nm ty Nothing }
> 	| srcloc 'export' callconv fspec
>	  { case $4 of (spec,nm,ty) -> HsForeignExport $1 $3 spec nm ty }

> callconv :: { HsCallConv }
> 	    : 'stdcall'			{ HsStdCall }
> 	    | 'ccall'			{ HsCCall   }
> 	    | 'dotnet'			{ HsDotNetCall }

> safety :: { HsFISafety }
> 	  : 'unsafe'			{ HsFIUnsafe }
> 	  | 'safe'			{ HsFISafe }
> 	  | 'threadsafe'		{ HsFIThreadSafe  }

> fspec :: { (String, HsName, HsType) }
> 	 : STRING varid '::' ctypedoc   { ($1, $2, $4) }
> 	 |        varid '::' ctypedoc   { ("", $1, $3) }

-----------------------------------------------------------------------------
Types

> doctype :: { HsType }
>	: tydoc '->' doctype		{ HsTyFun $1 $3 }
>	| tydoc				{ $1 }

> tydoc :: { HsType }
> 	: btype				{ $1 }
>	| btype docprev			{ HsTyDoc $1 $2 }

> type :: { HsType }
>       : ipvar '::' type1              { HsTyIP $1 $3 }
>       | type1                         { $1 }

> type1 :: { HsType }
>       : btype				{ $1 }
> 	| btype '->' type1		{ HsTyFun $1 $3 }

> btype :: { HsType }
>	: btype atype			{ HsTyApp $1 $2 }
>	| atype				{ $1 }

> atype :: { HsType }
>	: qgtycon			{ HsTyCon $1 }
>	| tyvar				{ HsTyVar $1 }
>	| '(' types ')'			{ HsTyTuple True  $2 }
>	| '(#' type '#)'		{ HsTyTuple False [$2] }
>	| '(#' types '#)'		{ HsTyTuple False $2 }
>	| '[' type ']'			{ HsTyApp list_tycon $2 }
>	| '(' ctype ')'			{ $2 }

> qgtycon :: { HsQName }
>	: qtycls			{ $1 }
>	| '(' ')'			{ unit_tycon_qname }
>	| '(' '->' ')'			{ fun_tycon_qname }
>	| '[' ']'			{ list_tycon_qname }
>	| '(' commas ')'		{ tuple_tycon_qname $2 }

(Slightly edited) Comment from GHC's hsparser.y:
"context => type" vs  "type" is a problem, because you can't distinguish between

	foo :: (Baz a, Baz a)
	bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

with one token of lookahead.  The HACK is to parse the context as a btype
(more specifically as a tuple type), then check that it has the right form
C a, or (C1 a, C2 b, ... Cn z) and convert it into a context.  Blaach!

> ctype :: { HsType }
>	: 'forall' tyvars '.' ctype	{ mkHsForAllType (Just $2) [] $4 }
>	| context '=>' type		{ mkHsForAllType Nothing $1 $3 }
>	| type				{ $1 }

> ctypedoc :: { HsType }
>	: 'forall' tyvars '.' ctypedoc	{ mkHsForAllType (Just $2) [] $4 }
>	| context '=>' doctype		{ mkHsForAllType Nothing $1 $3 }
>	| doctype			{ $1 }

> context :: { HsIPContext }
> 	: btype				{% checkContext $1 }

> types	:: { [HsType] }
>	: type ',' types		{ $1 : $3 }
>	| type  ',' type		{ [$1,$3] }

> simpletype :: { (HsName, [HsName]) }
>	: tycon tyvars			{ ($1,$2) }

> tyvars :: { [HsName] }
>	: tyvar tyvars			{ $1 : $2 }
>	| {- empty -}			{ [] }

-----------------------------------------------------------------------------
Datatype declarations

> constrs :: { [HsConDecl] }
> 	  : {- empty; a GHC extension -}  { [] }
> 	  | maybe_docnext '=' constrs1    { addConDocs $3 $1 }

> constrs1 :: { [HsConDecl] }
>	: constr maybe_docnext '|' maybe_docprev constrs1
>			{ addConDoc $1 $4 : addConDocs $5 $2 }
>	| constr			{ [$1] }

> constr :: { HsConDecl }
>	: srcloc maybe_docnext forall_stuff constr_stuff maybe_docprev
>		{ HsConDecl $1 (fst $4) $3 [] (snd $4) ($2 `mplus` $5) }
>	| srcloc maybe_docnext forall_stuff context '=>' constr_stuff maybe_docprev
>		{% checkIPContext $4 `thenP` \ ctxt -> returnP (HsConDecl $1 (fst $6) $3 ctxt (snd $6) ($2 `mplus` $7)) }
> 	| srcloc maybe_docnext forall_stuff con '{' fielddecls '}' maybe_docprev
> 		{ HsRecDecl $1 $4 $3 [] $6 ($2 `mplus` $8) }
> 	| srcloc maybe_docnext forall_stuff context '=>' con '{' fielddecls '}' maybe_docprev
> 		{% checkIPContext $4 `thenP` \ ctxt -> returnP (HsRecDecl $1 $6 $3 ctxt $8 ($2 `mplus` $10)) }

> forall_stuff :: { [HsName] }
> 	: 'forall' tyvars '.'		 	{ $2 }
> 	| {- empty -}				{ [] }

> constr_stuff :: { (HsName, [HsBangType]) }
> 	: scontype 				{ $1 }
>	| sbtype conop sbtype			{ ($2, [$1,$3]) }

> scontype :: { (HsName, [HsBangType]) }
>	: btype				{% splitTyConApp $1 `thenP` \(c,ts) ->
>					   returnP (toVarHsName c,
>						    map HsUnBangedTy ts) }
>	| scontype1			{ $1 }

> scontype1 :: { (HsName, [HsBangType]) }
>	: btype '!' atype		{% splitTyConApp $1 `thenP` \(c,ts) ->
>					   returnP (toVarHsName c,
>						     map HsUnBangedTy ts++
>						 	[HsBangedTy $3]) }
>	| scontype1 satype		{ (fst $1, snd $1 ++ [$2] ) }

> satype :: { HsBangType }
>	: atype				{ HsUnBangedTy $1 }
>	| '!' atype			{ HsBangedTy   $2 }

> sbtype :: { HsBangType }
>	: btype				{ HsUnBangedTy $1 }
>	| '!' atype			{ HsBangedTy   $2 }

> fielddecls :: { [HsFieldDecl] }
>	: fielddecl maybe_docnext ',' maybe_docprev fielddecls
>		{ addFieldDoc $1 $4 : addFieldDocs $5 $2 }
>	| ',' fielddecls		{ $2 }
>	| fielddecl			{ [$1] }
>	| {- empty -}			{ [] }

> fielddecl :: { HsFieldDecl }
>	: maybe_docnext vars '::' stype maybe_docprev
>		{ HsFieldDecl (reverse $2) $4 ($1 `mplus` $5) }

> stype :: { HsBangType }
>	: ctype				{ HsUnBangedTy $1 }	
>	| '!' atype			{ HsBangedTy   $2 }

> deriving :: { [HsQName] }
>	: {- empty -}			{ [] }
>	| 'deriving' qtycls		{ [$2] }
>	| 'deriving' '('          ')'	{ [] }
>	| 'deriving' '(' dclasses ')'	{ reverse $3 }

> dclasses :: { [HsQName] }
>	: dclasses ',' qtycls		{ $3 : $1 }
>       | qtycls			{ [$1] }

-----------------------------------------------------------------------------
Class declarations

> fds :: { [HsFunDep] }
> 	: {- empty -}			{ [] }
> 	| '|' fds1			{ reverse $2 }

> fds1 :: { [HsFunDep] }
> 	: fds1 ',' fd			{ $3 : $1 }
> 	| fd				{ [$1] }

> fd :: { HsFunDep }
> 	: varids0 '->' varids0		{ (reverse $1, reverse $3) }

> varids0	:: { [HsName] }
> 	: {- empty -}			{ [] }
>	| varids0 tyvar			{ $2 : $1 }

> optcbody :: { [HsDecl] }
>	: 'where' decllist		{ $2 }
>	| {- empty -}			{ [] }

> dbinds :: { [HsDecl] }
> 	 : dbinds ';' dbind		{ $3 : $1 }
> 	 | dbinds ';'			{ $1 }
> 	 | dbind			{ [$1] }
> 
> dbind	:: { HsDecl }
> dbind	: ipvar '=' srcloc exp		{% checkValDef ($3, HsVar (UnQual $1), HsUnGuardedRhs $4, []) }

> binds	::  { [HsDecl] }
>	: decllist			{ $1 }
>	| '{'            dbinds '}'	{ $2 }
> 	|     layout_on  dbinds close	{ $2 }


> wherebinds :: { [HsDecl] }
>	: 'where' binds			{ $2 }
>	| {- empty -}			{ [] }

-----------------------------------------------------------------------------
Instance declarations

> optvaldefs :: { [HsDecl] }
>	: 'where' '{' valdefs '}'		{ $3 }
>	| 'where' layout_on valdefs close	{ $3 }
>	| {- empty -}				{ [] }

> valdefs :: { [HsDecl] }
>	: valdefs ';' valdef			{ $3 : $1 }
>	| valdefs ';'				{ $1 }
>	| valdef				{ [$1] }
>	| {- empty -}				{ [] }

-----------------------------------------------------------------------------
Value definitions

> valdef :: { HsDecl }
>	: exp0b srcloc rhs 
>					{% checkValDef ($2, $1, $3, [])}

> rhs	:: { HsRhs }
>	: '=' exp wherebinds		{% checkExpr $2 `thenP` \e ->
>					   returnP (HsUnGuardedRhs e) }
>	| gdrhs	wherebinds		{ HsGuardedRhss  (reverse $1) }

> gdrhs :: { [HsGuardedRhs] }
>	: gdrhs gdrh			{ $2 : $1 }
>	| gdrh				{ [$1] }

> gdrh :: { HsGuardedRhs }
>	: '|' srcloc quals '=' exp	{% checkExpr $5 `thenP` \e ->
>					   returnP (HsGuardedRhs $2 $3 e) }

-----------------------------------------------------------------------------
Expressions

Note: The Report specifies a meta-rule for lambda, let and if expressions
(the exp's that end with a subordinate exp): they extend as far to
the right as possible.  That means they cannot be followed by a type
signature or infix application.  To implement this without shift/reduce
conflicts, we split exp10 into these expressions (exp10a) and the others
(exp10b).  That also means that only an exp0 ending in an exp10b (an exp0b)
can followed by a type signature or infix application.  So we duplicate
the exp0 productions to distinguish these from the others (exp0a).

> exp   :: { HsExp }
>	: exp0b '::' srcloc ctype  	{ HsExpTypeSig $3 $1 $4 }
>	| exp0				{ $1 }

> exp0 :: { HsExp }
>	: exp0a				{ $1 }
>	| exp0b				{ $1 }

> exp0a :: { HsExp }
>	: exp0b qop exp10a		{ HsInfixApp $1 $2 $3 }
>	| exp10a			{ $1 }

> exp0b :: { HsExp }
>	: exp0b qop exp10b		{ HsInfixApp $1 $2 $3 }
>	| exp10b			{ $1 }

> exp10a :: { HsExp }
>	: '\\' aexps '->' exp		{% checkPatterns (reverse $2) `thenP` \ps ->
>					   returnP (HsLambda ps $4) }
>  	| 'let' binds 'in' exp	         { HsLet $2 $4 }
>	| 'if' exp 'then' exp 'else' exp { HsIf $2 $4 $6 }

> exp10b :: { HsExp }
>	: 'case' exp 'of' altslist	{ HsCase $2 $4 }
>	| '-' fexp			{ HsNegApp $2 }
>  	| 'do' stmtlist			{ HsDo $2 }
>	| fexp				{ $1 }

> fexp :: { HsExp }
>	: fexp aexp			{ HsApp $1 $2 }
>  	| aexp				{ $1 }

> aexps :: { [HsExp] }
>	: aexps aexp			{ $2 : $1 }
>  	| aexp				{ [$1] }

UGLY: Because patterns and expressions are mixed, aexp has to be split into
two rules: One left-recursive and one right-recursive. Otherwise we get two
reduce/reduce-errors (for as-patterns and irrefutable patters).

Note: The first alternative of aexp is not neccessarily a record update, it
could be a labeled construction, too.

> aexp	:: { HsExp }
>  	: aexp '{' '}' 			{% mkRecConstrOrUpdate $1 [] }
>  	| aexp '{' fbinds '}' 		{% mkRecConstrOrUpdate $1 (reverse $3) }
>  	| aexp1				{ $1 }

Even though the variable in an as-pattern cannot be qualified, we use
qvar here to avoid a shift/reduce conflict, and then check it ourselves
(as for vars above).

Bug: according to the Report, left sections should be (exp0 qop), but
that would cause a shift/reduce conflict in which shifting would be no
different from specifying (exp0b qop).  The only consolation is that
other implementations don't manage this either.

> aexp1	:: { HsExp }
>	: ipvar                         { HsIPVar (UnQual $1) }
>       | qvar				{ HsVar $1 }
>	| gcon				{ HsCon $1 }
>  	| literal			{ $1 }
>	| '(' exp ')'			{ HsParen $2 }
>	| '(' texps ')'			{ HsTuple True  $2 }
>	| '(#' exp '#)'			{ HsTuple False [$2] }
>	| '(#' texps '#)'		{ HsTuple False $2 }
>	| '[' list ']'                  { $2 }
>	| '(' exp0b qop ')'		{ HsLeftSection $3 $2  }
>	| '(' qopm exp0 ')'		{ HsRightSection $3 $2 }
>	| qvar '@' aexp			{% checkUnQual $1 `thenP` \n ->
>					   returnP (HsAsPat n $3) }
>	| '_'				{ HsWildCard }
>	| '~' aexp1			{ HsIrrPat $2 }

> commas :: { Int }
>	: commas ','			{ $1 + 1 }
>	| ','				{ 1 }

> texps :: { [HsExp] }
>	: exp ',' texps			{ $1 : $3 }
>	| exp ',' exp			{ [$1,$3] }

-----------------------------------------------------------------------------
List expressions

The rules below are little bit contorted to keep lexps left-recursive while
avoiding another shift/reduce-conflict.

> list :: { HsExp }
>	: exp				{ HsList [$1] }
>	| lexps 			{ HsList (reverse $1) }
>	| exp '..'			{ HsEnumFrom $1 }
>	| exp ',' exp '..' 		{ HsEnumFromThen $1 $3 }
>	| exp '..' exp	 		{ HsEnumFromTo $1 $3 }
>	| exp ',' exp '..' exp		{ HsEnumFromThenTo $1 $3 $5 }
>	| exp pquals			{ HsListComp $1 (reverse $2) }

> lexps :: { [HsExp] }
>	: lexps ',' exp 		{ $3 : $1 }
>	| exp ',' exp			{ [$3,$1] }

-----------------------------------------------------------------------------
List comprehensions

> pquals :: { [HsStmt] }
>	 : pquals1			{ case $1 of
> 					    [qs] -> qs
> 					    qss  -> [HsParStmt (concat qss)]
> 					}
			
> pquals1 :: { [[HsStmt]] }
> 	: pquals1 '|' quals		{ $3 : $1 }
> 	| '|' quals			{ [$2] }

> quals :: { [HsStmt] }
>	: quals ',' qual		{ $3 : $1 }
>	| qual				{ [$1] }

> qual  :: { HsStmt }
>	: pat '<-' exp			{ HsGenerator $1 $3 }
>	| exp				{ HsQualifier $1 }
>  	| 'let' binds   		{ HsLetStmt $2 }

-----------------------------------------------------------------------------
Case alternatives

> altslist :: { [HsAlt] }
>	: '{' alts optsemi '}'			{ reverse $2 }
>	|     layout_on  alts optsemi close	{ reverse $2 }


> alts :: { [HsAlt] }
>	: alts ';' alt			{ $3 : $1 }
>	| alt				{ [$1] }

> alt :: { HsAlt }
>	: pat srcloc ralt wherebinds
>				 	{ HsAlt $2 $1 $3 $4 }

> ralt :: { HsGuardedAlts }
>	: '->' exp			{ HsUnGuardedAlt $2 }
>	| gdpats			{ HsGuardedAlts (reverse $1) }

> gdpats :: { [HsGuardedAlt] }
>	: gdpats gdpat			{ $2 : $1 }
>	| gdpat				{ [$1] }

> gdpat	:: { HsGuardedAlt }
>	: '|' srcloc quals '->' exp 	{ HsGuardedAlt $2 $3 $5 }

> pat :: { HsPat }
>	: exp0b				{% checkPattern $1 }

-----------------------------------------------------------------------------
Statement sequences

> stmtlist :: { [HsStmt] }
>	  : '{' stmts '}'		{ $2 }
>	  |     layout_on  stmts close	{ $2 }

The last Stmt should be a HsQualifier, but that's hard to enforce here,
because we need too much lookahead if we see do { e ; }, so it has to
be checked for later.

> stmts :: { [HsStmt] }
> 	  : qual stmts1			{ $1 : $2 }
> 	  | ';' stmts			{ $2 }
> 	  | {- empty -}			{ [] }

> stmts1 :: { [HsStmt] }
> 	  : ';' stmts			{ $2 }
> 	  | {- empty -}			{ [] }

-----------------------------------------------------------------------------
Record Field Update/Construction

> fbinds :: { [HsFieldUpdate] }
>	: fbinds ',' fbind		{ $3 : $1 }
>	| fbind				{ [$1] }

> fbind	:: { HsFieldUpdate }
>	: qvar '=' exp			{ HsFieldUpdate $1 $3 }

-----------------------------------------------------------------------------
Variables, Constructors and Operators.

> gcon :: { HsQName }
>  	: '(' ')'		{ unit_con_name }
>	| '[' ']'		{ nil_con_name }
>	| '(' commas ')'	{ tuple_con_name $2 }
>  	| qcon			{ $1 }

> var 	:: { HsName }
>	: varid			{ $1 }
>	| '(' varsym ')'	{ $2 }

> qvar 	:: { HsQName }
>	: qvarid		{ $1 }
>	| '(' qvarsym ')'	{ $2 }

> con	:: { HsName }
>	: conid			{ $1 }
>	| '(' consym ')'        { $2 }

> ipvar :: { HsName }
>       : IPVARID               { HsVarName (HsIdent $1) }

> qcon	:: { HsQName }
>	: qconid		{ $1 }
>	| '(' qconsym ')'	{ $2 }

> varop	:: { HsName }
>	: varsym		{ $1 }
>	| '`' varid '`'		{ $2 }

> qvarop :: { HsQName }
>	: qvarsym		{ $1 }
>	| '`' qvarid '`'	{ $2 }

> qvaropm :: { HsQName }
>	: qvarsymm		{ $1 }
>	| '`' qvarid '`'	{ $2 }

> conop :: { HsName }
>	: consym		{ $1 }	
>	| '`' conid '`'		{ $2 }

> qconop :: { HsQName }
>	: qconsym		{ $1 }
>	| '`' qconid '`'	{ $2 }

> op	:: { HsName }
>	: varop			{ $1 }
>	| conop 		{ $1 }

> qop	:: { HsExp }
>	: qvarop		{ HsVar $1 }
>	| qconop		{ HsCon $1 }

> qopm	:: { HsExp }
>	: qvaropm		{ HsVar $1 }
>	| qconop		{ HsCon $1 }

> qvarid :: { HsQName }
>	: varid			{ UnQual $1 }
>	| QVARID		{ Qual (Module (fst $1)) (HsVarName (HsIdent (snd $1))) }

> varid :: { HsName }
>	: 'forall'		{ forall_name }
>	| varid_no_forall	{ $1 }

> varid_no_forall :: { HsName }
>	: VARID			{ HsVarName (HsIdent $1) }
>	| 'as'			{ as_name }
> 	| 'unsafe'		{ unsafe_name }
>	| 'safe'		{ safe_name }
>	| 'threadsafe'		{ threadsafe_name }
>	| 'qualified'		{ qualified_name }
>	| 'hiding'		{ hiding_name }
>	| 'export'		{ export_name }
>	| 'stdcall'		{ stdcall_name }
>	| 'ccall'		{ ccall_name }
>	| 'dotnet'		{ dotnet_name }

> qconid :: { HsQName }
>	: conid			{ UnQual $1 }
>	| QCONID		{ Qual (Module (fst $1)) (HsVarName (HsIdent (snd $1))) }

> conid :: { HsName }
>	: CONID			{ HsVarName (HsIdent $1) }

> qconsym :: { HsQName }
>	: consym		{ UnQual $1 }
>	| QCONSYM		{ Qual (Module (fst $1)) (HsVarName (HsSymbol (snd $1))) }

> consym :: { HsName }
>	: CONSYM		{ HsVarName (HsSymbol $1) }

> qvarsym :: { HsQName }
>	: varsym		{ UnQual $1 }
>	| qvarsym1		{ $1 }

> qvarsymm :: { HsQName }
>	: varsymm		{ UnQual $1 }
>	| qvarsym1		{ $1 }

> varsym :: { HsName }
>	: VARSYM		{ HsVarName (HsSymbol $1) }
>	| '.'			{ dot_name }
>	| '-'			{ minus_name }
>	| '!'			{ pling_name }

> varsymm :: { HsName } -- varsym not including '-'
>	: VARSYM		{ HsVarName (HsSymbol $1) }
>	| '.'			{ dot_name }
>	| '!'			{ pling_name }

> qvarsym1 :: { HsQName }
>	: QVARSYM		{ Qual (Module (fst $1)) (HsVarName (HsSymbol (snd $1))) }

> literal :: { HsExp }
>	: INT 			{ HsLit (HsInt $1) }
>	| CHAR 			{ HsLit (HsChar $1) }
>	| RATIONAL		{ HsLit (HsFrac (readRational $1)) }
>	| STRING		{ HsLit (HsString $1) }
>	| PRIMINT		{ HsLit (HsIntPrim $1) }
>	| PRIMCHAR		{ HsLit (HsCharPrim $1) }
>	| PRIMFLOAT		{ HsLit (HsFloatPrim (readRational $1)) }
>	| PRIMDOUBLE		{ HsLit (HsDoublePrim (readRational $1)) }
>	| PRIMSTRING		{ HsLit (HsStringPrim $1) }

>  srcloc :: { SrcLoc }	:	{% getSrcLoc }
 
-----------------------------------------------------------------------------
Layout

> close :: { () }
>	: vccurly		{ () } -- context popped in lexer.
>	| error			{% popContext }

> layout_on  :: { () }	:	{% getSrcLoc `thenP` \(SrcLoc r c f) ->
>				   pushContext (Layout c) }

-----------------------------------------------------------------------------
Miscellaneous (mostly renamings)

> modid :: { Module }
>	: CONID			{ Module $1 }
>	| QCONID		{ Module (fst $1 ++ '.':snd $1) }

> tyconorcls :: { HsName }
>	: CONID			{ HsTyClsName (HsIdent $1) }

> tycon :: { HsName }
>	: CONID			{ HsTyClsName (HsIdent $1) }

> qtycls :: { HsQName }
>	: CONID			{ UnQual (HsTyClsName (HsIdent $1)) }
>	| QCONID		{ Qual (Module (fst $1)) (HsTyClsName (HsIdent (snd $1))) }

> tyvar :: { HsName }
>	: varid_no_forall	{ $1 }

-----------------------------------------------------------------------------
Documentation comments

> docnext :: { Doc }
>	: DOCNEXT	{% case parseParas (tokenise $1) of {
>				Left  err -> parseError err;
>				Right doc -> returnP doc } }

> docprev :: { Doc }
>	: DOCPREV	{% case parseParas (tokenise $1) of {
>				Left  err -> parseError err;
>				Right doc -> returnP doc } }

> docnamed :: { (String,Doc) }
>	: DOCNAMED	{% let (name,rest) = break isSpace $1 in
>			   case parseParas (tokenise rest) of {
>				Left  err -> parseError err;
>				Right doc -> returnP (name,doc) } }

> docsection :: { (Int,Doc) }
> 	: DOCSECTION	{% case $1 of { DocSection n s -> 
>			   case parseString (tokenise s) of {
>				Left err -> parseError err;
>				Right doc -> returnP (n, doc) } } }

> maybe_docprev :: { Maybe Doc }
> 	: docprev			{ Just $1 }
>	| {- empty -}			{ Nothing }

> maybe_docnext :: { Maybe Doc }
> 	: docnext			{ Just $1 }
>	| {- empty -}			{ Nothing }

> moduleheader :: { (ModuleInfo,Maybe Doc) }
>       : DOCNEXT       {% case parseModuleHeader $1 of {
>                          Right (str,info) -> 
>                             case parseParas (tokenise str) of {
>                                Left err -> parseError err;
>                                Right doc -> returnP (info,Just doc);
>                                };
>                          Left err -> parseError err
>                          } }

-----------------------------------------------------------------------------

> {
> happyError = parseError "Parse error"
> }

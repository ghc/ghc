{
module CmdParser (parseScript) where

import CmdSyntax
import CmdLexer
}

%tokentype	{ Token }

%token
	STRING  	{ Tok _ _ (LString  $$) }
	TEXT		{ Tok _ _ (LText    $$) }
	VAR		{ Tok _ _ (LVar     $$) }
	BOOL		{ Tok _ _ (LBool    $$) }

	'framefail'	{ Tok _ _ L_Framefail }
	'defined'	{ Tok _ _ L_Defined }
	'contents'	{ Tok _ _ L_Contents }
	'def'		{ Tok _ _ L_Def }
	'run'		{ Tok _ _ L_Run }
	'if'		{ Tok _ _ L_If }
	'then'		{ Tok _ _ L_Then }
	'else'		{ Tok _ _ L_Else }
	'fi'		{ Tok _ _ L_Fi }
	'print'		{ Tok _ _ L_Print }
	'test'		{ Tok _ _ L_Test }
	'exists'	{ Tok _ _ L_Exists }
	'when'		{ Tok _ _ L_When }
	'expect'	{ Tok _ _ L_Expect }
	'pass'		{ Tok _ _ L_Pass }
	'fail'		{ Tok _ _ L_Fail }
	'unknown'	{ Tok _ _ L_Unknown }
	'skip'		{ Tok _ _ L_Skip }
	'contains'	{ Tok _ _ L_Contains }
	'lacks'		{ Tok _ _ L_Lacks }
	'return'	{ Tok _ _ L_Return }
	'otherwise'	{ Tok _ _ L_Otherwise }
	'include'	{ Tok _ _ L_Include }

	'&&'		{ Tok _ _ L_And }
	'||'		{ Tok _ _ L_Or }
	'++'		{ Tok _ _ L_Append }
	'=='		{ Tok _ _ L_Eq }
	'/='		{ Tok _ _ L_NEq }
	'='		{ Tok _ _ L_Assign }
	'('		{ Tok _ _ L_Open }
	')'		{ Tok _ _ L_Close }
	'{'		{ Tok _ _ L_LBrace }
	'}'		{ Tok _ _ L_RBrace }
	','		{ Tok _ _ L_Comma }
	'|'		{ Tok _ _ L_Bar }

%name parser

%nonassoc 'else' 'contents' 'exists' 'run' 'defined' 'framefail'
%left  '||'
%left '&&'
%nonassoc '==' '/=' 'contains' 'lacks'
%nonassoc '|'
%left '++'

%%

file :: { [TopDef] }
 	: {- empty -}			{ [] }
	| topdef file			{ $1 : $2 }

topdef :: { TopDef }
	: 'include' expr		{ TInclude $2 }
	| 'def'  TEXT '(' maybe_vars ')' stmtblock
					{ TMacroDef $2 (MacroDef $4 $6) } 
	| 'test' STRING stmtblock	{ TTest $2 $3 }
	| VAR '=' expr			{ TAssign $1 $3 }

stmtblock :: { [Stmt] }
	: '{' stmts '}'			{ $2 }

stmts 	:: { [Stmt] }
	: {- empty -}			{ [] }
	| stmt stmts			{ $1 : $2 }

stmt	:: { Stmt }
	: VAR '=' expr			{ SAssign $1 $3 }
	| 'print' expr			{ SPrint $2 }
	| 'if' expr 'then' stmts maybe_else 'fi' { SCond $2 $4 $5 }
	| TEXT '(' maybe_args ')'	{ SMacro $1 $3 }
	| 'return' expr			{ SReturn $2 }
	| 'skip' 'when' expr		{ SSkip $3 }
	| result 'when' expr		{ SResult $1 $3 }
	| 'expect' result		{ SExpect $2 }
	| 'framefail' expr		{ SFFail $2 }

maybe_else :: { Maybe [Stmt] }
	: {- empty -}			{ Nothing }
	| 'else' stmts 			{ Just $2 }

expr	:: { Expr }
	: expr '||' expr		{ EOp OpOr  $1 $3 }
	| expr '&&' expr		{ EOp OpAnd $1 $3 }
	| expr '==' expr		{ EOp OpEq  $1 $3 }
	| expr '/=' expr		{ EOp OpNEq $1 $3 }
	| expr 'contains' expr		{ EOp OpContains $1 $3 }
	| expr 'lacks' expr		{ EOp OpLacks $1 $3 }
	| expr '++' expr		{ EOp OpAppend $1 $3 }
	| expr '|' expr			{ EPipe $1 $3 }
	| VAR				{ EVar $1 }
	| STRING			{ EString $1 }
	| BOOL				{ EBool $1 }
	| 'contents' expr		{ EContents $2 }
	| 'exists' expr			{ EExists $2 }
	| 'run' expr			{ ERun $2 }
	| TEXT '(' maybe_args ')'	{ EMacro $1 $3 }
	| 'if' expr 'then' expr 'else' expr { ECond $2 $4 $6 }
	| 'otherwise'			{ EOtherwise }
	| 'defined' VAR			{ EDefined $2 }
	| 'framefail' expr		{ EFFail $2 }
	| '(' expr ')'			{ $2 }

maybe_vars :: { [Var] }
	: {- empty -}			{ [] }
	| vars				{ $1 }

vars :: { [Var] }
	: VAR				{ [$1] }
	| VAR ',' vars			{ $1 : $3 }

maybe_args :: { [Expr] }
	: {- empty -}			{ [] }
	| args				{ $1 }

args :: { [Expr] }
	: expr				{ [$1] }
	| expr ',' args			{ $1 : $3 }

result :: { Result }
	: 'pass'			{ Pass }
	| 'fail'			{ Fail }
	| 'unknown'			{ Unknown }

{
parseScript :: String -> String -> Either String [TopDef]
parseScript fname fcontents 
    = Right (parser (tokenise 1 fcontents))

happyError (Tok _ l x : _) = error ("parse error, line " ++ show l ++ ", before  " ++ show x)
}

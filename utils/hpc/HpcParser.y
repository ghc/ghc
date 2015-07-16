{ 
module HpcParser where

import HpcLexer
}

%name parser
%expect 0
%tokentype { Token }

%token
	MODULE 		{ ID "module" }
	TICK		{ ID "tick" }
	EXPRESSION	{ ID "expression" }
	ON		{ ID "on" }
	LINE		{ ID "line" }
	POSITION	{ ID "position" }
	FUNCTION	{ ID "function" }
	INSIDE          { ID "inside" }
	AT		{ ID "at" }
	':'		{ SYM ':' }
	'-'		{ SYM '-' }
	';'		{ SYM ';' }
	'{'		{ SYM '{' }
	'}'		{ SYM '}' }
	int		{ INT $$ }
	string		{ STR $$ }
	cat		{ CAT $$ }
%%

Spec    :: { Spec }
Spec 	: Ticks Modules 	{ Spec ($1 []) ($2 []) }

Modules :: { L (ModuleName,[Tick]) }
Modules	: Modules Module	{ $1 . ((:) $2) }
	|			{ id }
	
Module :: { (ModuleName,[Tick]) }
Module  : MODULE string '{' TopTicks '}'
				{ ($2,$4 []) }

TopTicks :: { L Tick }
TopTicks : TopTicks TopTick	{ $1 . ((:) $2) }
	 | 			{ id }
	
TopTick :: { Tick }
TopTick : Tick			{ ExprTick $1 }
	| TICK FUNCTION string optQual optCat ';'
				{ TickFunction $3 $4 $5 }
	| INSIDE string '{' TopTicks '}'
				{ InsideFunction $2 ($4 []) }
				 
Ticks   :: { L ExprTick }
Ticks   : Ticks  Tick          	{ $1 . ((:) $2) }
	|  		       	{ id } 
	
Tick   :: { ExprTick }
Tick    : TICK optString optQual optCat ';'
				{ TickExpression False $2 $3 $4 }

optString :: { Maybe String }
optString : string		{ Just $1 }
	  |			{ Nothing }
	
optQual :: { Maybe Qualifier }
optQual : ON LINE int 		{ Just (OnLine $3) }
	| AT POSITION int ':' int '-' int ':' int
				{ Just (AtPosition $3 $5 $7 $9) }
	| 			{ Nothing }
optCat  :: { Maybe String }
optCat  : cat			{ Just $1 }
	| 			{ Nothing }

{
type L a = [a] -> [a]
	
type ModuleName = String

data Spec 
  = Spec [ExprTick] [(ModuleName,[Tick])]
   deriving (Show)

data ExprTick
  = TickExpression Bool (Maybe String) (Maybe Qualifier) (Maybe String)
   deriving (Show)

data Tick
  = ExprTick ExprTick
  | TickFunction   String (Maybe Qualifier) (Maybe String)
  | InsideFunction String [Tick]
   deriving (Show)

data Qualifier = OnLine Int
               | AtPosition Int Int Int Int
   deriving (Show)             



hpcParser :: String -> IO Spec
hpcParser filename = do
  txt <- readFile filename
  let tokens = initLexer txt
  return $ parser tokens  	

happyError e = error $ show (take 10 e)
}

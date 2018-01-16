> module Token(tokenise) where

> import Core_datatype

> import Kernel

> import Type_defs



> endofline  = "\n\f\r"

> whitespace = " \t" ++ endofline

> a_symbol = "!#$&*-+=~<>?/\\.:|@"

> singlechrs = ",;\"`\'(){}[]\176\185"

> binder = "\208\211\236\177\178\229\196"

> ifx_binder = "\183\184\167\182\187"

> ifx_op = "\179\180=\172:"


> reserved = [ "empty", "signature", "extend", "combine", "with", "sharing" ,
>	       "++", "=", "(", ")", "@", "operator", ";", ":", "\189", 
>	       "datatype", "|", "case", "of", "\167", "recurse", "end", 
>	       "typed", "fn", "rec", "if", "then", "else", "let", "in", 
>	       "\168", "\169", "{", "}", "[", "]", "\208", "\211", "\236", 
>	       "\229", "\177", "\178", ".", ",", "\181", "\179", "\180", 
>	       "\182", "\172", "\183", "\184", "\185", "\176", "BinL", 
>	       "BinR", "Pre", "Post", "\196", "\187" ]





> tokenise :: String -> [Token]

> tokenise string
> 	= denull ( scan' string "" )



> denull :: [Token] -> [Token]

> denull ( Clr "" : tkl ) = denull tkl

> denull ( Clr tk : tkl ) 
>	= tag rev_tk : denull tkl
>	  where
>	  tag | rev_tk `elem` reserved = Rvd
>	      | otherwise            = Clr 
>	  rev_tk = reverse tk

> denull ( Rvd tk : tkl ) = error " optimise to trap some reserved earlier and reduce length of reserved set"

> denull ( Scan_Err mess : _ ) = [ Scan_Err mess ]

> denull ( tk : tkl ) = tk : denull tkl

> denull [] = []

> denull x = error ( show x )





> scan' ( a : x ) current 
>	| a `elem` binder     = Clr current : ( Bdr ( mk_bdr a ) : scan' x "" )
>	| a `elem` ifx_binder = Clr current : ( IfxBdr [a] : scan' x "" )
>	| a `elem` ifx_op     = Clr current : ( IfxOp [a] : scan' x "" )
> 	| single a          = Clr current : ( Clr [a] : scan' x "" )
>	| a `elem` a_symbol     = Clr current : symbol_scan x [a]
>	| a `elem` whitespace = Clr current : scan' x "" 
>	| alphanum' a       = scan' x ( a : current )
>	| a == '%'          = Clr current : scan' ( discard x ) ""
>	| a == '^'          = super_scan x ( '^' : current ) 
>	| otherwise         = [ Scan_Err "\nInvalid character\n" ]

> scan' [] current = [ Clr current ]




> symbol_scan :: String -> String -> [Token]

> symbol_scan ( a : x ) current
>	| a `elem` a_symbol = symbol_scan x ( a : current )
>	| a == '^'      = super_scan x ( a : current ) 
>	| otherwise     = Clr current : scan' ( a : x ) ""

> symbol_scan [] current = [ Clr current ]



> super_scan _ "^" = [ Scan_Err "\nAttempt to superscript invalid string\n"]

> super_scan ( a : x ) current
>	| alphanum a || a `elem` a_symbol 
>			= Clr ( a : current ) : scan' x ""
> 	| otherwise     = [ Scan_Err "\nInvalid superscript character\n" ]

> super_scan [] _ = [ Scan_Err "\nEmpty superscript\n" ]








> alphanum' ch = alphanum ch || ch == '_'

> alphanum ch
>	= ( ch >= 'A' && ch <= 'Z' ) ||
>	  ( ch >= 'a' && ch <= 'z' ) ||
>	  ( ch >= '0' && ch <= '9' )



> single ch
> 	= ch `elem` singlechrs || ch > '\DEL'

> discard :: String -> String

> discard ( ch : x ) 
>	| ch `elem` endofline = x
>	| otherwise         = discard x

> discard [] = ""


> mk_bdr :: Char -> Binder_conn

>{-
> mk_bdr x | fromEnum x == 208 = Pi

> mk_bdr x | fromEnum x == 211 = Sigma

> mk_bdr x | fromEnum x == 236 = Lambda

> mk_bdr x | fromEnum x == 177 = Forall

> mk_bdr x | fromEnum x == 178 = Exists

> mk_bdr x | fromEnum x == 229 = Choose
>-}

> mk_bdr '\208' = Pi

> mk_bdr '\211' = Sigma

> mk_bdr '\236' = Lambda

> mk_bdr '\177' = Forall

> mk_bdr '\178' = Exists

> mk_bdr '\229' = Choose

> mk_bdr '\196' = Delta

> mk_bdr oth = error ( "Mk_bdr: " ++ [oth])



token display function used in debugging messages

> disp_tk :: Token -> String

> disp_tk ( Rvd str ) = str

> disp_tk ( Clr str ) = str

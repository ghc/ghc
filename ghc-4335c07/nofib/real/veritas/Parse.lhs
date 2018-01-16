
> module Parse where

> import Data.Char(isDigit)--1.3
> import Token

> import Unparse

> import Build_Tm

> import Build_itrm

> import Core_datatype

> import Type_defs

> import Attributes

> import Kernel

> import Sub_Core1

> import Sub_Core2

> import Sub_Core3

> import Sub_Core4

> import Vtslib



> type Parse_State = ( [ Tag ] , Sgn )

-- for parser without Maybe types -- simplified if Maybes pushed into parser
-- push fns into parser?

> parse_trm sg tgL s
>        = case parse_itm ( tgL , sg ) s of
>              ( Opnd ( Itrm itm )) -> Ok itm
>              Prs_Err mesg         -> Bad mesg

> parse_Thm_M sg ps str
>	= if is_valid_Thm th 
>		then Ok th
>		else Bad mesg
>	  where
>	  th = parse_Thm sg ps str
>	  ( TH_Err mesg ) = th

> parse_Thm sg ps = parse_deriv (ps, sg) . parse_itm (ps, sg)

> parse_Trm sg ps = parse_tm ( ps , sg )

> trm_to_Trm = build_trm'

> dec_to_Dec = build_dc

> sgn_to_Sgn = build_sg



> parse_tm (tgL , sg ) str 
>	= build_trm sg flag_itm
>	  where
>	  ( flag_itm , _ ) = drive_parse ( tgL , sg ) str 

> parse_itm sg str
>	= itm
>	  where
>	  (itm , _ ) = drive_parse sg str





The driver function for the parser takes a signature and a string, 
returning an ITrm (with will contain the wrapper Opnd or Prs_Err depending
on success) and an unused list of tokens. (In normal usage this will be
empty?) The term parser is given the terminator token (Rvd "") which
cannot occur naturally in the input list but is appended to the token
list by the tokeniser.

> drive_parse :: Parse_State -> String -> ( Flagged_ITrm , [Token] )

> drive_parse sg str 
>	= term sg [Rvd ""] ( tokenise str )








A term is formed by folding the term constructor App into the list
of individual terms produced by term'. A term is first parsed into a list
by term' in order to allow for possible re-ordering of the component
terms by `prioritise'.

The arguments are the parse state on which parsing is to take place, a
list of terminating tokens whose presence on the input list of tokens
will indicate that the parse is complete and the input list of tokens
itself.	The result is as for `parse_tm' above.

> term :: Parse_State -> [Token] -> [Token] -> (Flagged_ITrm , [Token]) 

> term sg tmnL tkL
>	= ( tm , tkL2 )
>	  where
>	  tm = foldl1 app' ( prioritise tmL )
>	  ( tmL , tkL2 ) = term' sg tmnL tkL







term' is the 'meat' of the term parser. It has a case for each alternative
defined for terms is the syntax. It is defined as a case analysis taking
the same arguments as its driving function `term' above.

In all clauses the term generator functions ( cond' , let' etc ) pass back
errors in the first argument in which one is found without forcing
evaluation of the remaining arguments. This allows parsing of these
remaining arguments to proceed on the assumption that their prerequisite
components are valid. (see if expression below for a documented example)
Also, in all cases where only a single terminator token is being sought,
the element at the head of the returned list may be discarded as it must
match the required token for a successful parse. ( If the parse was
unsuccessful, the list will not be required anyway--functions must
merely ensure that their returned list contain at least one dummy element)
Again, see the if clause below for a documented example of this. 
NOTE: these two features are not documented in subsequent clauses where
they are employed.

Note also that many of the necessary attributes are added by the generator
funcions (if',let' etc.) contained in the module `build_itrm'.

> term' :: Parse_State -> [Token] -> [Token] -> ([Flagged_ITrm] , [Token]) 


if expression

An if expression consists of three sub-expressions, a declaration and two
terms (dc, t_tm & f_tm) representing the predicate and the cases for true
and false respectively. These are parsed used `hyp', `term' and `term'
respectively. Note that the hypothesis may only be terminated when
the `then' is encountered and `t_tm' only when the `else' is encountered.
i.e. if the original termination tokens `tmnL' are found here an error
has occured. These original tokens come back into force to terminate
the `else' clause.

Note that the else clause extends "as far as possible" to the right.
Hence the list of terms returned has only one element `cond_tm'. (The
component terms t_tm and f_tm have already been made into a single ITrm rather
than a list by means of the `term' function rather than `term''.

Also note that since `hyp' and the t_tm must have been terminated
by the single terminator token indicated, it must occur at the head
of the returned list of tokens `tkL2' and `tkL3' and hence may simply
be discarded. (If a parse error occured the list will not be examined
further as all such errors are considered fatal. The if generator function
`cond'' takes care to return a parse error of the first of its arguments
which indicates one. i.e. if an error occurs during the parsing of `dc',
no attempt will be made to parse `t_tm' or `f_tm'.

The variable introduced in the predicate is in scope throughout the
terms `t_tm' and `f_tm' hence the use of the extended signature `sg2'.
(`sg2' will only be evaluated if no parse error occured during the parsing
of `dc'--see paragraph above and `pst_extend' below.)

> term' sg tmnL ( Rvd "if" : tkL )
> 	= ( [ cond_tm ] , tkL4 )
>	  where
>	  cond_tm = cond' dc t_tm f_tm
>	  ( dc   , _ : tkL2 ) = hyp sg [ Rvd "then" ] tkL 
>	  ( t_tm , _ : tkL3 ) = term sg2 [ Rvd "else" ] tkL2
>	  ( f_tm , tkL4 )     = term sg2 tmnL tkL3
>	  sg2 = pst_extend dc sg 



let expression

A let expression, similarly to `if'  consists of a declaration (dc) 
and two terms (tm1) and (tm2) and has the semantics:-

let dc = tm1 in tm2     is equivalent to    ( \dc.tm2 ) tm1

Again similarly to `if', the second term (tm2) extends "as far as possible"
to the right. The intermediate declaration and term (dc & tm1) are
terminated by the tokens "=" & "in" as shown with the second term (and
hence the entire expression) being terminated by the original termination
tokens (tmnL). 

Note that the variable introduced in `dc' is in scope for `tm2' but
not for `tm1'. The reason for this is apparent if the semantics above
are examined as `tm2' is the body of the lambda abstraction.

> term' sg tmnL ( Rvd "let" : tkL )
>	= ( [ let' dc tm1 tm2 ] , tkL4 )
>	  where
>	  ( dc  , _ : tkL2 ) = abdec sg [ IfxOp "=" ] tkL
>	  ( tm1 , _ : tkL3 ) = term sg [ Rvd "in" ] tkL2
>	  ( tm2 , tkL4 )     = term sg2 tmnL tkL3
>	  sg2 = pst_extend dc sg



recurse

recurse is the primitive recursion function. It consists of a series
if atomic terms (tmL) defining the operations on each of the
constructors of a type and an obligatory typing for the term (srt).

The function `recurse_cls' deals with the details of the parsing
of the component terms. The empty list indicated is an 
initialisation of the accumulated list of terms returned by the 
function.

Note : recurse clause (recurse_cls) throws away separators ( no need for _:)

> term' sg tmnL ( Rvd "recurse" : tkL )
>	= ( [ recurse' tmL srt ] , tkL3 )
>	  where
>	  ( tmL , tkL2 ) = recurse_cls sg [] tkL
>	  ( srt , tkL3 ) = term sg tmnL tkL2



fn

>{-
> term' sg tmnl ( Rvd "fn" : tkl )
>	= case tkl? of
>		Rvd "|" : tkl?' -> 
>-}



subscripted application

All applications are treated as a dummy operator ( Spl "" ). A subscripted 
application is treated as a BinR operator of precedence 100. (Normal
application is left associative (BinL)). The function being applied
will have already been parsed prior to encountering the open subscript
character. The argument (argtm) follows immediately and is terminated
by the close subscript character

Unlike `if' and `let' expressions, the subscripted application may
be followed by further terms taking their place in the term list 
produced by term' (tmL). They are placed in the list after `argtm'.
The prioritise function will rearrange them into their correct positions
in the expression. `argtm' itself is already folded into a single expression
by use of the `term' function rather than `term''. Note that the
only valid terminator during the parsing of this term is the close
subscript character.

(185 = open subscript , 176 = close subscript )

> term' sg tmnL ( Rvd "\185" : tkL )
>	= ( app_op : argtm : tmL , tkL3 )
>	  where
>	  ( argtm , _ : tkL2 ) = term sg [ Rvd "\176" ] tkL
>	  ( tmL   , tkL3 )     = term' sg tmnL tkL2
>	  app_op = Opr ( Spl "" ) BinR 100



prefix binders

Prefix binders will already have been identified by the tokeniser. They
all follow the same pattern, a declaration (dc) followed by a "." and
a body (tm). Similarly to 'if' and 'let', the body of the term extends
as far to the right as possible, hence `bdr_tm' forms a single element
list. The function `binder'' maps the binder name (nm) into the correct
binder.

Note that the declaration is in scope within the body of the binder
expression (tm).

> term' sg tmnL ( Bdr nm : tkL )
>	= ( [ bdr_tm ] , tkL3 )
>	  where
>	  bdr_tm = binder' nm dc tm
>	  ( dc , _ : tkL2 ) = bdec sg [ Rvd "." ] tkL
>	  ( tm , tkL3 )     = term sg2 tmnL tkL2
>	  sg2 = pst_extend dc sg 



pairs

A pair consists of two terms separated by "," character. A pair is
treated as a special operator (Spl ",") which actually forms the pair when
it is expanded by `app'' ( see module build_itrm).

The pair is optionally followed by a `typed' expression, hence the
keyword `typed' is added to the list of termination tokens for
the second argument (as for subscripted application, the first argument
will have already been parsed prior to encountering the ","). The "typed"
term itself (if present) will be dealt with by a separate clause.

Note that the second argument is left as an unfolded list (term' used) to
allow for expressions of higher precedence than pair. It also extends
as far to the right as possible.

> term' sg tmnL ( Rvd "," : tkL )
>	= ( pair_op : argtmL , tkL2 )
>	  where
>	  ( argtmL, tkL2 ) = term' sg ( Rvd "typed" : tmnL ) tkL
>	  pair_op = Opr ( Spl "," ) BinR 20



The 'typed' extension (of pairs etc.) is, similarly to pairs, treated as
a special operator ( Spl "typed" ). Its argument is a single term and
extends as far to the right as possible. As for pairs, the first argument
to "typed" will already have been parsed prior to encountering the
"typed".

> term' sg tmnL ( Rvd "typed" : tkL )
>	= ( [ tpe_op , tpe ] , tkL2 )
>	  where
>	  ( tpe , tkL2 ) = term sg tmnL tkL
>	  tpe_op  = Opr ( Spl "typed" ) BinL 1



symbols

Explicit reference to symbols may be pattern matched directly off the
token list. The function 'sym'' ensures that the arguments `no1' and
`no2' are valid integers.

The symbol so formed is added to the term list together with the parsed
remainder of the input (tmL).

(168 = open < , 169 = close > )

> term' sg tmnL ( Rvd "\168": Clr no1 : Rvd ",": Clr no2 : Rvd "\169": tkL )
>	= ( sym' no1 no2 : tmL , tkL2 )
>	  where
>	  ( tmL , tkL2 ) = term' sg tmnL tkL



constructors

Explicit reference to constructors are handled analogously to symbols
described above.

(168 = open < , 169 = close > )

> term' sg tmnL ( Rvd "\168" : Clr no1 : Rvd "," : Clr no2 : Rvd "," : 
>						Clr no3 : Rvd "\169" : tkL )
> 	= ( const' no1 no2 no3 : tmL , tkL2 )
>	  where
>	  ( tmL , tkL2 ) = term' sg tmnL tkL



subtypes

Subtypes consist of a declaration followed by a term in curly braces.
i.e. { dc | tm }. The declaration is in scope in the term (tm,sg2).
Since the subtype only extends as far as the `}' character, `term' must
continue to parse the remainder of the term (tmL).

Subtypes are treated as binders (binder') similarly to prefix binders.

> term' sg tmnL	 ( Rvd "{" : tkL )
>	= ( binder' Subtype dc tm : tmL , tkL4 )
>	  where
>	  ( dc , _ : tkL2 ) = bdec sg [ Rvd "|" ] tkL
>	  ( tm , _ : tkL3 ) = term sg2 [ Rvd "}" ] tkL2
>	  ( tmL , tkL4 )    = term' sg tmnL tkL3 
>	  sg2 = pst_extend dc sg 



declarations  in '['']'

Declarations in square brackets will occur in the input stream before
the object to which they refer (infix binders). The declaration is
parsed and added to the list of "terms" produced as output. (Use of
the type Flagged_ITrm allows declarations to be included as well as terms. If
no errors are present, they will be removed when the function `app'' is
folded into the list.) Special operators (for infix binders) will allow
for declarations occuring as their first argument.

Note that the remainder of the list of terms being generated (tmL) is parsed
on the extended signature (sg2).

> term' sg tmnL ( Rvd "[" : tkL )
>	= ( dc : tmL , tkL3 ) 
>	  where
>	  ( dc , _ : tkL2 ) = bdec sg [ Rvd "]" ] tkL
>	  ( tmL , tkL3 )    = term' sg2 tmnL tkL2
>	  sg2 = pst_extend dc sg 



infix binders

Infix binders are mapped into operators by the function `make_prebdr' and
then added to the result list of terms (bdrop).  (The operator contains
the corresponding prefix binder constructor (Lambda etc.), associativity 
and precedence of the binder.) The operator and its arguments will be mapped 
into a binder ITrm construction when the function `app'' (defined in module 
build_itrm) is folded into the list of terms produced as a result of
this function.

The first agument (which may be a declaration or a term) will have already
been parsed. (the function app' deals with the two cases of term or dec.)
The second argument will be contained in the list `tmL'.)

> term' sg tmnL ( IfxBdr bdr : tkL )
>	= ( bdrop : tmL , tkL2 )
>	  where
>	  bdrop = make_prebdr bdr
>	  ( tmL , tkL2 ) = term' sg tmnL tkL



infix operators ( And Or etc ) 

The handling of infix operators is analogous to the handling of infix 
binders above. The function `make_iop' generates an operator (iop) which will
be mapped into an ITrm operator construction by the function `app''.

In this case however, the first argument must be a term. (This
is enforced by `app''.)

> term' sg tmnL ( IfxOp op : tkL )
>	= ( iop : tmL , tkL2 )
>	  where
>	  iop = make_iop op
>	  ( tmL , tkL2 ) = term' sg tmnL tkL



prefix (not) operator 

The prefix operator, not, is treated analogously to the infix operators
described above. Since only one such operator is present, the operator
representing it is explicitely generated here (not_op). Not's single
argument will be contained in the list `tmL'. No argument will have
been parsed previously as it is a prefix operator.

> term' sg tmnL ( Rvd "\181" : tkL )
>	= ( not_op : tmL , tkL2 )
>	  where
>	  not_op = Opr ( Spl "Not" ) Pre 60
>	  ( tmL , tkL2 ) = term' sg tmnL tkL



parenthesised term

A parenthesised term is dealt with simple by calling the function `term'
to parse upto the closing `)'. The function `term' folds its
result into a single term, hence `tm' may be added as a single entity into
the function result list achieving the desired result of binding the
entities within the parentheses into a single term. Note that the
closing `)' is the only valid terminator here. If one of the terminators
in `tmnL' occurs before the closing `)', an error has occured.

> term' sg tmnL ( Rvd "(" : tkL )
>	= ( tm : tmL , tkL3 )
>	  where
>	  ( tm , _ : tkL2 )  = term sg [ Rvd ")" ] tkL
>	  ( tmL , tkL3 ) = term' sg tmnL tkL2



\@ is not implemented yet.

> term' sg tmnL ( Rvd "@" : tkL )
> 	= ( [ Prs_Err "@ not implemented " ] , dmy )



universes

Any identifier beginning with a `U' and continuing with non-empty list
of integers (arg) represents a universe (the pattern checks for this). If so,
the universe ITrm may be build directly using the `read' function to
convert the argument into the desired integer (i).

Note that negative universes cannot occur as they require the character `-'
which is not a digit.

> term' sg tmnL ( Clr ( 'U' : arg@(_:_) ) : tkL ) | and ( map isDigit arg )
>	= ( universe : tmL , tkL2 )
>	  where
>	  universe = ( Opnd . Itrm ) ( Constant ( Univ i ) [] [] ) 
>	  i        = read arg
>	  ( tmL , tkL2 ) = term' sg tmnL tkL



constant symbols

The constant symbols `true', `false' and `bool' may be mapped 
directly into their ITrm constant representations. As for Universes,
the remainder or the input (tkL) is parsed for the remainder of the 
result list of terms (tmL).

> term' sg tmnL ( Clr nm : tkL ) | nm == "true" || nm == "false" || nm == "bool"
>	= ( cst : tmL , tkL2 )
>	  where
>	  cst = ( Opnd . Itrm ) ( Constant ( ctr nm ) [] [] )
>	  ctr "true"  = T
>	  ctr "false" = F
>	  ctr "bool"  = Bool'
>	  ( tmL , tkL2 ) = term' sg tmnL tkL



tags

> term' pst@( tgL , sg ) tmnL ( Clr nm : tkL ) 
>	= if in_tgL then ( tag' tg argL : tmL2 , tkL3 ) 
>	            else ( sym_id : tmL1 , tkL2' ) 
>	  where
>	  sym_id = lookUp nm sg 
>	  ( tmL1 , tkL2' ) = term' pst tmnL tkL
>	  ( tmL2 , tkL3 )  = term' pst tmnL tkL2
>	  ( in_tgL , tg@( _ , arg_kndL , _ )) = fetch_tg nm tgL 
>	  ( argL , tkL2 ) = parse_tag_arg [] arg_kndL tkL

>	  parse_tag_arg tg_resL ( knd : kndL ) lcl_tkL
>		= parse_tag_arg ( tg_res : tg_resL ) kndL lcl_tkL3
>		  where
>		  ( tg_res , lcl_tkL3 ) 
>			= case knd of
>				Term_Arg  -> ( Tg_Trm trm_res , lcl_tkL2 )
>				Deriv_Arg -> ( Tg_Thm thm_res , lcl_tkL2 )
>				Int_Arg   -> parse_iL lcl_tkL 
>		  trm_res = build_trm sg res_tm 
>		  thm_res = parse_deriv pst res_tm 
>		  ( res_tm , lcl_tkL2 ) = aterm pst [] lcl_tkL

>	  parse_tag_arg tg_resL [] lcl_tkL
>		= ( reverse tg_resL , lcl_tkL )



symbol names

Any identifier which has not been trapped by the above two clauses is
assumed to represent an entry on the signature (sg). The function `lookup'
is used to retrieve the entry. It returns the necessary Flagged_ITrm for
the symbol if it is present on the signature (sym_id) or alternatively a parse
error if it is not found.

>{-
> term' pst@( _ , sg)  tmnL ( Clr nm : tkL )
>	= ( sym_id : tmL , tkL2 ) -- also checks tag list
>	  where
>	  sym_id = lookup nm sg 
>	  ( tmL , tkL2 ) = term' pst tmnL tkL
>-}



last clauses - termination and error conditions

check for terminator token 

If the next token is in the termination list (tmnL), then the parse is
complete and the empty list of terms is returned. The input token list
(tkL) is returned unchanged.

> term' sg tmnL tkL@( tk : _ ) | tk `elem` tmnL 
>	= ( [] , tkL )



Any reserved word which has not been already dealt with and was not in the 
termination list (tmnL), is unexpected and an error. (The termination list
was checked in the above clause.)

> term' sg tmnl ( Rvd str : tkl )
>--	= ( [ Prs_Err (" unexpected '" ++ str ++ "' (term)") ], dmy )	
>	= ( [ Prs_Err (" unexpected '" ++ str ++ "'"++ " (term) tmnl: " ++ concat ( map disp_tk tmnl )++ "|") ], dmy )	



If the input stream is empty then cease parsing (enhance later)

> term' sg tmnl [] |  Rvd ""  `elem` tmnl
>	= ( [] , [] )

> term' sg tmnl []
>	= ( [] , [] )
>{- end of file with tmnl not empty is not necessarily an error e.g. ';' in sig
>  	= ( [ Prs_Err mesg ] , dmy )
>	  where
>  	  mesg = " expecting '" ++ concat ( map disp_tk tmnl ) ++ "'" 
>-}


pass on tokeniser error unchanged as a parse error. No attempt is made to
parse the remainder of the input. (`dmy' is a single element dummy token
list returned to ensure that pattern matches on non-empty token lists do
not fail even if a parse error has occured.)

> term' sg _ ( Scan_Err mesg : _ )
>	= ( [ Prs_Err mesg ] , dmy )







aterm -- added for sig , merge with term'?

aterm and the corresponding clauses in term' are to be merged. (documentation
on the clauses is given in term' above.)

symbols
(168 = open < , 169 = close > )

> aterm sg tmnl ( Rvd "\168": Clr no1 : Rvd ",": Clr no2 : Rvd "\169": tkl )
>	= ( sym' no1 no2 , tkl )

constructors
(168 = open < , 169 = close > )

> aterm sg tmnl ( Rvd "\168" : Clr no1 : Rvd "," : Clr no2 : Rvd "," : 
>						Clr no3 : Rvd "\169" : tkl )
> 	= ( const' no1 no2 no3 , tkl )

subtypes

> aterm sg tmnl ( Rvd "{" : tkl )
>	= ( binder' Subtype dc tm , tkl3 )
>	  where
>	  ( dc , _ : tkl2 ) = bdec sg [ Rvd "|" ] tkl
>	  ( tm , tkl3 ) = term sg2 [ Rvd "}" ] tkl2
>	  sg2 = pst_extend dc sg 

universes
(Note : ensure argument is non-empty list of digits)

> aterm sg tmnl ( Clr ( 'U' : arg@(_:_) ) : tkl ) | and ( map isDigit arg )
>	= ( universe , tkl )
>	  where
>	  universe = ( Opnd . Itrm ) ( Constant ( Univ i ) [] [] ) 
>	  i        = read arg

constant symbols

> aterm sg tmnl ( Clr nm : tkl ) | nm == "true" || nm == "false" || nm == "bool"
>	= ( cst , tkl )
>	  where
>	  cst = ( Opnd . Itrm ) ( Constant ( ctr nm ) [] [] )
>	  ctr "true"  = T
>	  ctr "false" = F
>	  ctr "bool"  = Bool'

parenthesised term

> aterm sg tmnl ( Rvd "(" : tkl )
>	= ( tm , tkl2 )
>	  where
>	  ( tm , _ : tkl2 )  = term sg [ Rvd ")" ] tkl

symbol names

> aterm ( _ , sg ) tmnl ( Clr nm : tkl )
>	= ( sym_id , tkl ) -- also checks tag list
>	  where
>	  sym_id = lookUp nm sg 

> aterm sg tmnl ( tk : tkl )
>	= ( Prs_Err (" unexpected '" ++ disp_tk tk ++ "' (aterm -- no tmnl check)" ) , dmy )

end aterm








`hyp' generates a declaration from the list contained between `[' and
`]' or alternatively an anonymous declaration if no square brackets are
present.

If a declaration (btm) is contained within `[' and `]' then a valid
terminator token should immediately follow it (head of `tkL2'). An error
also occurs if `tkL2' is empty. The case analysis checks for these 
conditions.

{-
Is this clause necessary since term' can catch [] on its own? (Or
optimise just to include a check for term' returning dc or tm and
take appropriate action.
note if dec in [ ] then a terminator must immediately follow the ']'
-}

> hyp :: Parse_State -> [Token] -> [Token] -> ( Flagged_ITrm , [Token] )

> hyp sg tmnL ( Rvd "[" : tkL ) 
>	= case tkL2 of
>		nxt : _ | nxt `elem` tmnL 
>			-> ( btm , tkL2 )
>               nxt : _ -> ( Prs_Err mesg1 , dmy )
>			   where
>	  		   mesg1 = "Unexpected token '"++ disp_tk nxt ++"'"
>		_	-> ( Prs_Err mesg2 , dmy )
>	  where
>	  ( btm , _ : tkL2 ) = bdec sg [ Rvd "]" ] tkL 
>	  mesg2 = "Unexpected end of file"



If no square brackets are found then an anonymous declaration is generated.
The input stream (tkL) is parsed as a term which is taken as the sort of
the anonymous declaration. The name of the declaration is defined to be "_"
and the attribute `hyp_ndpnd' is added to the attribute list to show
that it is anonymous.

> hyp sg tmnL tkL 
>	= ( anon_dc , tkL2 )
>	  where
>	  anon_dc = symbol_dec' tm  ( Name "_" ) [ hyp_ndpnd ]
>	  ( tm , tkL2 ) = term sg tmnL tkL 








binding declarations

This is the driver function for the declaration parser. It uses the
function `bdec'' to generate a declaration (nxt_dc) and then checks
the remaining input stream (tkL2) for further declarations. 

If there are further declarations (`;' is at the head of the list),
`bdec' itself is used to generate them (rest) on a signature extended 
with the first declaration (sg2). (Using `bdec' rather than `bdec'' here
allows for further `;' characters after the next declaration.) 
The component declarations are joined together using `decpair''. The
attribute `un_grp' is added to indicate that the declarations were
ungrouped. (Grouped declarations are dealt with by `bdec_name' below.)

Note that since `decpair'' applies a single declaration (nxt_dc) to the already
paired declarations (rest) (paired by the recursive call to `bdec'), 
`;' is right associative.

Note also that a valid terminator token should occur at the head of
the remaining input (tkL2) for a correct termination. (This is a design
decision to charge `bdec' with ensuring that the valid terminator occurs
after the declarations. The alternative would involve placing a check
for terminator after each call to `bdec'.) Note that the terminator token
is still passed back at the head of the remaining input (tkL2).

> bdec :: Parse_State -> [Token] -> [Token] -> ( Flagged_ITrm , [Token] )

> bdec sg tmnL tkL 
>	= case tkL2 of
>		Rvd ";" : tkL2' 
>			-> ( decpair' [un_grp] nxt_dc rest , tkL3 ) 
>			   where
>	                   ( rest , tkL3 ) = bdec sg2 tmnL tkL2'
>		tk : _ | tk `elem` tmnL
>			->  ( nxt_dc , tkL2 ) 
>--		oth     ->  ( Prs_Err "Malformed declaration" , dmy )
>		oth     ->  error ( "Bdec: " ++ concat ( map disp_tk oth ) ++ "\ntmnl: " ++ concat ( map disp_tk tmnL ) ++ "|" )
> 	  where
>	  ( nxt_dc , tkL2 ) = bdec' sg tmnL tkL
>	  sg2 = pst_extend nxt_dc sg 







bdec' performs a check for parentheses in a declaration. If they are present,
it uses the function `bdec' to generate a single declaration from the tokens
contained within the parentheses. If they are not, the function 
`bdec_name' is used to generate a grouped set of declarations. The empty
list represents the initialisation of the accumulated list of declarations
within the group generated by `bdec_name'.

> bdec' :: Parse_State -> [Token] -> [Token] -> ( Flagged_ITrm , [Token] )

> bdec' sg tmnL ( Rvd "(" : tkL ) 
>	= ( nxt_dc , tkL2 )
>	  where
>         ( nxt_dc , _ : tkL2 ) = bdec sg [ Rvd ")" ] tkL 

> bdec' sg tmnL tkL  
>	= bdec_name sg ( Rvd ";" : tmnL ) [] tkL








`bdec_name' generates a grouped set of declarations (separated by ',').
Firstly a name is generated from the input stream (nm). If the name
is valid (case nm), a case analysis is performed on the following token
(head of tkL2). If a ',' is found, another element in the group follows.
In this case, `bdec_name' is called recusively with the current name (nm)
being added to the accumulated list of group names (nmL). 

If ':' is found, a term representing the sort of each element in the group 
follows. This is parsed by `term'. The function `make_dc' is then used
to assign this sort to each name in the accumulated name list (nmL). Since
only one sort declaration can occur per group, parsing of the group list
is now complete. The attribute `dec_tpe' is added to indicate that a sort
was explicitely given for the group.

A group will also terminate if any token other than ',' or ':' is encountered.
In this case a default sort (currently temporrarily set to U0 (dft), is
assigned to each name in the group. The function `make_dc' is again used
for this purpose. The attribute `dec_untpe' is added to indicate that
no explicit sort was given for the group.

> bdec_name :: Parse_State -> [Token] -> [Name'] -> [Token] 
>					-> (Flagged_ITrm , [Token])

> bdec_name sg tmnL nmL tkL
>	= case nm of
>		Ok inm   -> switch inm tkL2
>		Bad mesg -> ( Prs_Err mesg , dmy )
>	  where
>	  ( nm , tkL2 ) = name tkL

>         switch inm ( Rvd "," : tkL2' ) = bdec_name sg tmnL ( inm : nmL ) tkL2'

>	  switch inm ( IfxOp ":" : tkL2' ) 
>		= ( make_dc ( reverse ( inm : nmL )) srt dec_tpe , tkL3 )
>		  where
>		  ( srt , tkL3 ) = term sg ( Rvd ";" : tmnL ) tkL2'

>	  switch inm tkL 
>		= ( make_dc ( reverse ( inm : nmL )) dft dec_untpe , tkL )

>	  dft = Opnd ( Itrm ( Constant ( Univ 0 ) [] [] )) --temporary default








`make_dc' is an auxiliary function to `bdec_name' defined above. Its 
function is to take a list of names and a sort and generate a single 
paired declaration containing all the given names assigned with the given sort.
It also adds the supplied attribute to each individual declaration (tped). 

`make_dcL' assigns the given sort to each name in nmL producing a list of
declaration (dcL). 
The function `decpair'' is then folded into this list to produce a single
declaration. The attribute `grp' is added to each paired declaration indicating that it is part of a group.

(Note that attributes indicating grouped/ungrouped are added to each
decpair declaration while attributes indicating typed/untyped are added
to the component symbol declarations.)

> make_dc :: [Name'] -> Flagged_ITrm -> Attribute -> Flagged_ITrm

> make_dc nmL ( Opnd ( Itrm srt )) tped
>	= case dcL of
>		[] -> Prs_Err "empty declaration" 
>		_  -> foldr1 ( decpair' [grp] ) dcL 
>	  where
>	  dcL = make_dcL 0 nmL 
>	  make_dcL cnt ( nm : nmL ) 
>		= dc : ( make_dcL ( cnt + 1 ) nmL )
>		  where
>		  dc = Opnd ( Idec ( Symbol_dec shft_srt [ sym_nm nm , tped ]))
>		  shft_srt = shift_trm [] cnt srt
>	  make_dcL _ [] = []

> make_dc nmL ( Prs_Err mesg ) _
>	= Prs_Err mesg 

> make_dc nmL _ _ = error "unexpected term in make_dc"








Atomic binding declarations consist of either a binding declaration 
enclosed in `(' `)' or an untyped, single named declaration. 

If a `(' token appears at the head of the list, the function `bdec' may
be used to build a declaration from the contents of the parentheses.

If not, a `name' is generated from the input list (2nd clause tkL).
A declaration is then build from this name using a default sort (dft)
(currently temporarily set to U0.) If a parse error occured during
the name parse, the error is passed back unchanged (case nm).

> abdec :: Parse_State -> [Token] -> [Token] -> ( Flagged_ITrm , [Token] )

> abdec sg tmnL ( Rvd "(" : tkL ) 
>	= ( nxt_dc , tkL2 )
>	  where
>         ( nxt_dc , _ : tkL2 ) = bdec sg [ Rvd ")" ] tkL 

> abdec sg tmnL tkL  
>	= case nm of
>		Ok inm   -> ( symbol_dec' dft inm [] , tkL2 )
>		Bad mesg -> ( Prs_Err mesg , dmy )
>	  where
>	  ( nm , tkL2 ) = name tkL
>	  dft = Opnd ( Itrm ( Constant ( Univ 0 ) [] [] )) --temporary default








Names consist of a single string identifier or an operator declaration
contained in `{' `}'.

>-- name :: [Token] -> ( MayBe Name' , [Token] )

If a `{' character is found, the identifier of the operator will be next on
the list (id). The functions `optyp' and `opprc' are then used to find
the type (in the sense of prefix, postfix etc.) and precedence of the operator
respectively. They will return defaults if no explicit values are found.

The token `}' should occur at the head of the list returned by the precedence
parser `opprc' (tkl3). If this is not found an error is returned. 

> name ( Rvd "{" : Clr id : tkL )
>	= case tkL3 of 
>		Rvd "}" : tkL3' -> ( Ok ( Operator' id prc optype ) , tkL3' )
>		_		-> ( Bad "missing '}'" , dmy )
>	  where
>	  ( optype , tkL2 ) = optyp tkL
>	  ( prc    , tkL3 ) = opprc tkL2

If no '{' is found, the single identifer (id) represents the name of
the object. i.e. the name is a string identifier. The name cannot be a
reserved word (must be `Clr' token constructor.) If a reserved word
is found (oth in following cluase) an error message is generated. An
error message is also generated if the input list is empty.

> name ( Clr id : tkL )
>	= ( Ok ( Name id ) , tkL ) 

> name ( oth : tkL )
>	= ( Bad ( " unexpected '" ++ disp_tk oth ++ "' (name)" ) , dmy )

> name [] = ( Bad "empty identifier" , dmy )







`optyp' returns the type of an operator (in the sense of prefix, postfix etc.)
The function merely maps the string representing the type into its
corresponding constructor of type `Oprtype').

If the token does not match one of the four types of operators, a default
of binary left associative (BinL) is returned (final clause). Note here
that the input token list (tkL) is returned unchanged as it will be 
required by the precedence parser `opprc'. (Since neither the type or
precedence is compulsory, no input should be `eaten' if no matching
token is found.)

> optyp :: [Token] -> ( Oprtype , [Token] )

> optyp ( Rvd "Pre" : tkL )
>	= ( Pre , tkL )

> optyp ( Rvd "BinL" : tkL )
>	= ( BinL , tkL )

> optyp ( Rvd "BinR" : tkL )
>	= ( BinR , tkL )

> optyp ( Rvd "Post" : tkL )
>	= ( Post , tkL )

default BinL

> optyp tkL
>	= ( BinL , tkL )








`opprc' returns the precedence of an operator.

If the token at the head of the input list is a string of digits, the string
is converted to an integer by `read' and returned. Otherwise a default
precedence (currently 1) is returned. The input list is returned unchanged
in this case.

> opprc :: [Token] -> ( Int , [Token] )

> opprc ( Clr prcid : tkL ) | and ( map isDigit prcid )
>	= ( read prcid , tkL )

> opprc tkL
>	= ( 1 , tkL )	








recurse fns

This is an auxiliary function dealing with the Recurse expression. 
The function accumulates atomic terms (fnL) representing the cases of
a Recurse expression until the keywords `end' followed by `typed' are 
found. (recurse must be typed)

The argument to the keyword `typed' is dealt with by the calling function
(a clause of term') as is the generation of the recurse expression
itself.

> recurse_cls :: Parse_State -> [Flagged_ITrm] -> [Token] -> ( [Flagged_ITrm] , [Token])

> recurse_cls sg fnL tkL
>	= case tkL2 of
>		Rvd "end" : Rvd "typed" : tkL2'
>			-> ( fn : fnL , tkL2' )
>		Rvd "end" : _
>			-> ( [ Prs_Err " recurse must be typed " ] , dmy )
>		_       -> recurse_cls sg ( fn : fnL ) tkL2
>	  where
>	  ( fn , tkL2 ) = aterm sg [ Rvd "end" ] tkL








fn clauses -- incomplete

>{-
> fn_clauses 
>	= case ident_type tkL of
>		Ok ( i , j , k ) 
>			 -> match ctrL nmL ( i , j ) tkL 
>			    where
>			    ( Data _ ctrL [ dat_nm nmL ] ) 
>					= fetch_type sg i j 
>	  	Bad mesg -> ( Prs_Err mesg , dmy )


> match sg ctrL ( tpe_nm : nmL ) tp_id tkl
>	= ( recurse' fnL srt , tkl2 )
>	  where
>	  ( fnL , tkl2 ) = match_cls 

> match_cls ctr_argL nm
>	= case tkl2 of
>		Rvd "\167" : tkl2' -> make_rhs
>		otherwise          -> error ""
>	  where
>	  ( ( nm , fmls ) , tkl2 ) = parg sg nm tmnl abdec True tkl 
>	  sg2 = extend' sg ( fmls ++ rcL )
>	  rcL = find_recursive ctr_argL tp_id fmls 

> make_rhs
>	= case tkl2 of
>		Rvd "|" : tkl2'       -> match_cls 
>		tk : _ | tk `elem` tmnl -> ( clsL , tkl2 )
>   	  where
>	  tm = add_lambda  
>	  ( rhs , tkl2 ) = term sg2 tmnl tkl2'




> find_recursive ( arg : argL ) tp_id


> ident_type sg ( Clr nm : tkl )
>	= case lookUp nm sg of
>		( True , Const i j k _ _ ) -> Ok ( i , j , k )	
>	        otherwise                  -> ident_type sg tkl

> ident_type sg ( Rvd "\167" : _ )
>	= Bad "No constructor before '\167'"

> ident_type sg ( Rvd tk : _ )	
>	= Bad " Unexpected '" ++ disp_tk tk ++ "'"

>-}

--END FN--








Prioritise

The prioritiser rearranges the list of terms produced by `term'' into
their correct precedence order before the `app'' function is folded 
into resulting list.

The following driver function initialises the `stacks' for the main
prioritisor function `ptse' and checks that the resulting list is
non-empty.

> prioritise :: [Flagged_ITrm] -> [Flagged_ITrm]

> prioritise tmL = case ptse [] [] False tmL of
>			[]  -> [ Prs_Err "Empty term" ]
>			oth -> oth








The precedence algorithm is described in the clauses of the function which
follows. The algorithm uses two list, an operand list (opnds) on which
the final, reordered term list will emerge, and an operator stack (oprs),
which is used as a temporary store for operators while they are being 
reordered. The constructor at the head of each Flagged_ITrm indicates
whether its component ITrm is an operator or an operand. The final list
of terms is in prefix form (function before arguments) thus allowing
the `app'' function to be folded into the list to produce a single term.

> type Flag_I = Flagged_ITrm -- shorthand

> ptse :: [Flag_I] -> [Flag_I] -> Bool -> [Flag_I] -> [Flag_I]

If a component of the term list contains an error, the error is passed back
without examining the remaining terms (opL) (which in normal circumstances
will be empty anyway).

> ptse opnds oprs _ ( Prs_Err tm : opL )
>	= [ Prs_Err tm ] 

The boolean flag (third argument to `ptse') is used to indicate that the
previous term was another operand. If this case a dummy operator for
application is inserted between the two operands. This allows function
application to be treated as any other operator and allows operators
of higher precedence than application to be included.

If an operand is encountered with the flag False (previous term was not an
operand) the operand is merely pushed onto the operand list and `ptse'
called on the next remaining term list (opL). The flag is now set to True
to indicate the presence of an operand.

> ptse opnds oprs False ( Opnd tm : opL )
>	= ptse ( Opnd tm : opnds ) oprs True opL 

If an operand is encountered and the flag is True, (previous term was an
operand), the dummy operator (app_op) is added to the current list 
of terms (opL) and `ptse' reinvoked to deal with the new operator.

Note: the operand is left on the term list (opL) to be reexamined after the
dummy operator has been dealt with. It is not immediately added to the
operand list. This is because the dummy operator for application is
treated as a binary infix operator and hence is expected to occur between
its two operands. 

> ptse opnds oprs True opL@( Opnd _ : _ )
>	= ptse opnds oprs True ( app_op : opL )
>	  where
>	  app_op = Opr ( Spl "" ) BinL 100 


The following clauses deal with the different kinds of operators. Note
that operators are placed on the operand list (in a way dependent on its
lind) as well as the operator stack. (Operand list is therefore a slight
misnomer and is really a resulting list of terms.)

prefix operators (op) are always added to the operator stack irregardless
of the precedence of the operator on top of the stack. Its precedence
only takes note of operators to its right in the original expression. 
(e.g. the `not' in `not x' will be expected to bind with `x' irregardless 
of the precedence of the operator preceding `not'). 

The operator may be added to the operand list directly as it is already in
prefix form (its argument follows it in the original expression).

> ptse opnds oprs _ ( op@( Opr _ Pre prc ) : opL )
>	= ptse ( op : opnds ) ( op : oprs ) False opL 

For infix operators, one argument preceeds the operator. The `swap_op'
function is thus required to place the operator before its first argument
(which will be at the head of the argument list) in the operand list.
The operator will precede its second argument. No further reordering is
thus necessary for this argument.

Before this however, all operators of higher precedence that the operator
are popped for the operator stack and added to the operand list (flush).
(the function `flush' also allows for an extra test as well as higher
precedence to be used to determine if an operator is to be popped. This is
not necessary here so `null_op' is employed.) The new operator (op) may now
be added to the operator stack (oprs).

> ptse opnds oprs _ ( op@( Opr _ BinL prc ) : opL )
>	= ptse ( swap_op op opnds' ) ( op : oprs' ) False opL
>	  where
>	  ( opnds' , oprs' ) = flush opnds oprs prc null_op

Right associative infix operators are identical to left associative
infix operators except that another test is necessary to determine
if the operator at the head of the operator stack (oprs) should be
popped before the current operator (op) is added to it.

The operator at the head of `oprs' should also be popped if it is 
also right associative and its precedence is LESS than or equal to the 
precedence of the current operator (right associative operator precedences
work "backwards"). The function `cmp_op' defines the test.

> ptse opnds oprs _ ( op@( Opr op_nm BinR prc ) : opL )
>	= ptse ( swap_op op opnds' ) ( op : oprs' ) False opL
>	  where
>	  ( opnds' , oprs' ) = flush opnds oprs prc cmp_op
>	  cmp_op ( Opr _ BinR prc' ) = prc' <= prc 
>	  cmp_op _ = False

The argument to a postfix operator precedes it in the original expression.
The `swap_op' function is hence required to transpose them in the resulting 
list.

Again, the operator stack is `flushed' of higher precence operators
first. The operator is not added to the operator stack however as 
(conversely to prefix operators above) its precedence only affects operators
to its left. (The "'" in "a'" should also bind to "a" even if a higher
precedence operator occurs to the right of the "'".)

Also for this reason, the flag (fl) indicating juxtaposition of operands
is not reset to False. A dummy application operator needs to be generated
for two operands separated by one or more postfix operators operators.

> ptse opnds oprs fl ( op@( Opr _ Post prc ) : opL )
>--	= ptse ( swap_op op opnds' ) ( op : oprs' ) False opL
>	= ptse ( swap_op op opnds' ) oprs'  fl opL
>	  where
>	  ( opnds' , oprs' ) = flush opnds oprs prc null_op

When all the terms have been considered, the operator stack should be
`flushed' of all remaining operators. The operand list will now
represent the correctly reordered term list.

> ptse opnds oprs _ [] 
>	= opnds'
>	  where
>	  ( opnds' , _ ) = flush opnds oprs (-1) null_op








Dummy comparison operator for use with `flush' function when no
further comparison is required.

> null_op :: a -> Bool

> null_op _ = False








`swap_op' addes the given operator (op) prior to the head item on the
operand list. It also forms an application of the two terms to ensure
that any future `swap_op' operations take the two terms as a single term.

e.g in 3 / 4 + 5,  the + should be placed before the application of / to 3
(and also 4) not only the 3 itself which would be the case if the / was
not bound to the 3 as soon as the `swap' takes place. This results
in + ( / 3 4 ) 5 not (/ + 3 4 5 ) which is incorrect.

> swap_op :: Flagged_ITrm -> [Flagged_ITrm] -> [Flagged_ITrm]

> swap_op op ( top_op : opnds )
>	= app' op top_op : opnds

> swap_op op [] = [ op ]








Clear the operator stack of operators of higher precedence than the 
precedence of the current operator (pprc) plus any operators 
meeting the additional properties of the predicate `cmp' (in practice, 
this applies only to right associative infix operators---see above).

If the operator at the head of the operator stack is of higher precedence,
the function `add_op' is applied to the operand list to perform any
term binding and operand swapping required (see `add_op' below). `flush'
is then called recursively on the tail of the operand stack (opL). The
operand list (opnds) and operator stack (oprs) are returned unchanged
when an operator of lower precedence is encountered or the operator stack
is empty (2nd clause) (the calling function is responsible for adding
the new operator onto the operator stack).

> flush :: [Flagged_ITrm] -> [Flagged_ITrm] -> Int 
>	     -> ( Flagged_ITrm -> Bool ) -> ( [Flagged_ITrm] , [Flagged_ITrm] )

> flush opnds oprs@( op@(Opr op_nm op_tpe prc) : opL ) pprc cmp
>	= if pprc > prc || cmp op
>		then ( opnds , oprs )
>		else flush opnds' opL pprc cmp
>	  where
>	  opnds' = add_op op_nm op_tpe opnds

> flush opnds [] _ _ = ( opnds , [] )








`add_op' is called by 'flush' when an operator is flushed from the operator
stack. The operator itself will already be on the operand list (`ptse' adds
all operators to the operand list---see above) hence all that is required is
to bind the operator to an argument.

`add_op' binds a function to an argument which occurs after it in the
original expression. The function 'swap_op' will have already been used
to bind it to an argument which precedes it in the original expression. 
(An example is given in `swap_op' showing why `app'' is used to bind 
the function and argument pair at this stage. It is to prevent future 
swapping operation `getting between them' in the operand list as it 
has now been established that the two elements on the top of the
operand list should be applied to each other. It is the result of this
application which will be the argument to a further function, not the
components themselves.) 

Prefix and infix operators all have arguments following them in the
original expression and hence the `app'' function is applied to the
two elements on the top of the list in these cases ( arg & fn ).
The resulting application is added back to the head of the operand list.

Note that postfix operators have no argument following them in the
original expression and hence the operand list is returned unchanged
in this case. (Compare this with `ptse' above where `swap_op' is applied
in all but the prefix case to form the bond between an operator and an
argument which occurs before it in the original expression. Infix 
operators have an argument occuring before and after them in the 
original expression and hence `swap_op' and `app_op' will be applied
at the appreopriate time (by `ptse' and `flush')).

Note that if the operand list does not contain two elements an error
has occured.

Also note that for infix operators, the term `fn' will consist of the
application of the relevant operator applied to its first argument rather
than the operator itself.

(The first argument (op) is used only in the generation of the error
message by the final clause.)

> add_op :: Operator -> Oprtype -> [Flagged_ITrm] -> [Flagged_ITrm]

> add_op op Pre ( arg : fn : opnds )
>	= app' fn arg : opnds

> add_op op BinL ( arg : fn : opnds )
>	= app' fn arg : opnds

> add_op op BinR ( arg : fn : opnds )
>	= app' fn arg : opnds

postfix operator has already been applied to operand (as have BinL and BinR
to their first operand )

> add_op op Post opnds 
>	= opnds

> add_op op _ _ 
>	= [ Prs_Err ( " Insufficient arguments for operator" ) ] -- ++ op ) ]


end prioritise








infix binders

The following function assigns the ITrm binder constructor and precedence
of an infix binder. (All infix binders are infix right associative.) An
operator is returned representing the binder. (This allows it to be 
reordered by `prioritise' into its correct place in the resulting ITrm.)
The special operator `OpBdr' is used to represent infix beinders. These
will be expanded by `app'' later to form an ITrm when the arguments to
the binder are known.

> make_prebdr :: String -> Flagged_ITrm

> make_prebdr bdr
>	= Opr ( OpBdr prefix_form ) BinR prc
>	  where
>	  ( prefix_form , prc ) = map_bdr bdr
>	  map_bdr "\167" = ( Lambda , 5 )
>	  map_bdr "\183" = ( Pi     , 80 )
>	  map_bdr "\184" = ( Sigma  , 90 )
>	  map_bdr "\182" = ( Imp    , 20 )
>	  map_bdr "\187" = ( Delta  , 10 )








infix operators

This function is analogous to `make_prebdr' above except it returns an
operator representing infix operators with their respective precedences.
The special operator `OpIfx' is used for infix operators.


":" is a special case (assigning type of first operand to the second)
treated as shown. (app' appies a separate partial op for ":" as it
does not map into a Binary_conn in the resultant itrm.)

> make_iop :: String -> Flagged_ITrm

> make_iop ":"
>	= Opr ( Spl ":" ) BinL 15

> make_iop op
>	= Opr ( OpIfx oprep ) BinR prc
>	  where
>	  ( oprep , prc ) = map_op op
>	  map_op "\179" = ( And , 40 )
>	  map_op "\180" = ( Or , 30 )
>	  map_op "="    = ( Eq' , 15 )
>	  map_op "\172" = ( Issubtype , 70 )





lookup function analogous to table lookup (used outside parser)
(returns option type)

> lookup_name sg nm
>	= case lookUp nm sg of
>		Opnd (Itrm tm ) -> SOME tm	
>		Prs_Err _       -> NONE
>		_               -> error "could be ok, check 'lookup_name' in parse.lhs"




temporary lookup function

The lookup function returns a term (symbol or constructor) representing
the identifier `nm' if it is defined on the current signature (sg).
The counter `i', `j' and `k' (for contructors only) are the indices of
the symbol or constructor term. They are incremented as the signature is
searched and will represent the correct values for an identifier, if and
when it is encountered.

The first (top level) function initialises the count for `i' and calls 
`lookup'' to search each declaration of the signature (sg).

> lookUp :: String -> Sgn -> Flagged_ITrm

> lookUp nm sg 
>	= lookup' isg nm 0
>	  where
>	  isg = internal_Sgn sg






The function `lookup_dc' is used to search the next declaration on the
signature (dc). If the identifier is found (in_dc True), the accompanying
term (tm) given by `lookup_dc' is returned. If the identifier is not found
(in_dc False), `lookup'' is called recursively on the rest of the signature
(sg). The count `i' is incremented to indicate that the next declaration on
the signature is under consideration.

If the signature is empty (2nd clause), the identifier is undefined and
an error message is returned.

Note that the initialised values to `lookup_dc' ( [] and 0 ) are the list
of unsearched declarations (the second components of decpairs which have
not been searched yet) and the count `j' of the symbol's position in the 
decpair "tree" respectively. 

> lookup' :: ISgn -> String -> Int -> Flagged_ITrm

> lookup' ( Extend dc sg _ ) nm i
>	= if in_dc then tm
>		   else lookup' sg nm ( i + 1 )
>	  where
>	  ( in_dc , tm ) = lookup_dc dc [] nm i 0 

> lookup' ( Empty _ ) nm i
>	= Prs_Err ( " Undefined symbol: " ++ nm )








`lookup_dc' is the driver function for the declaration search function. It 
uses the function `lookup_dc'' to examine a given declaration (dc) for a 
match. If a match is found (found True), the accompanying term (tm) is 
returned. If no match is found (found False), any remaining unsearched 
declarations (dcL) are examined. This is achieved by calling `lookup_dc' 
recursively incrementing the compound declaration pointer `j'.  If the 
list (dcL) is empty, the symbol is undefined and a False flag is returned
(the accompanying term is undefined in this case (error "").

> lookup_dc :: IDec -> [IDec] -> String -> Int -> Int -> ( Bool , Flagged_ITrm )

> lookup_dc dc dcL nm  i j
>	= if found then ( found , tm )
>		   else case dcL of
>				dc : dcL' -> lookup_dc dc dcL' nm i ( j + 1 )
>				[]        -> ( False , error "" )
>	  where
>	  ( found , tm ) = lookup_dc' dc dcL nm i j








`lookup_dc'' examines a declaration for a given identifier (nm) by performing
a case analysis on the declaration.

> lookup_dc' :: IDec -> [IDec] -> String -> Int -> Int -> (Bool , Flagged_ITrm)

If the declaration is a pair, search the first component declaration (dc1)
using 'lookup_dc'. Add the second component (dc2) to the list of as yet 
unsearched declaration. This will be searched by `lookup_dc' is the search
of `dc1' is unsuccessful. 

Note that the compound declaration position count `j' is incremented.

> lookup_dc' ( Decpair dc1 dc2 _ ) dcL nm  i j
>	= lookup_dc dc1 ( dc2 : dcL ) nm i ( j + 1 )

If the declaration is a symbol, the function `lookup_nm' is used to
compare the required identifier (nm) against the symbol name (nm').

> lookup_dc' ( Symbol_dec _ ( ( _ , Symbol_Name nm' ) : _ ) ) _ nm i j
>	= lookup_nm nm' nm i j 

> lookup_dc' ( Axiom_dec _ ( ( _ , Symbol_Name nm' ) : _ ) ) _ nm i j
>	= lookup_nm nm' nm i j 

If the declaration is a datatype, the list of constructor names (nmL)
is searched for a match using the function `lookup_nml'.

> lookup_dc' ( Data _ _ [ ( _ , Datatype_Name nmL ) ] ) _  nm i j 
>	= lookup_nml nmL nm i j 0

If the declaration is a definition, the function `lookup_nm' may again
be used to compare the definition name (nm') against the required 
identifier (nm).

> lookup_dc' ( Def _ _ [ ( _ , Symbol_Name nm' )] ) _ nm i j
>	= lookup_nm nm' nm i j







Compare the given string identifier (nm) against a given name. A name is
either a string identifier itself or an operator.

> lookup_nm :: Name' -> String -> Int -> Int -> ( Bool , Flagged_ITrm )

If the name is a string identifier, a match occurs if the identifiers are
the same ( nm == nm' ). In this case a symbol is returned representing the
name (the required indices `i' and `j' are passed into the function.
(The attribute `sym_nmd' is the name of the symbol.)
If no match occurs, the flag False is returned. The accompanying term is
undefined (error "") in this case.

> lookup_nm ( Name nm' ) nm i j 
> 	| nm == nm' = ( True , ( Opnd . Itrm ) ( Sym i j [] [sym_nmd] ) )
>	| otherwise = ( False , error "" )

If the `name' is an operator, a match occurs if the name of the operator (nm')
matches the required identifier (nm). Again a symbol is returned for the
identifier in this case (the symbol is flagged to be an operator (Opr) rather
than an operand (Opnd) in this case however). If no match occurs the flag
False is returned together with an undefined term as above.

> lookup_nm ( Operator' nm' prc opt ) nm i j 
> 	| nm == nm' = ( True , Opr ( OpItrm ( Sym i j [] [sym_nmd] )) opt prc )
>	| otherwise = ( False , error "" )







`lookup_nml' is similar to `lookup_nm' defined above except that a list
of names is searched for a match rather that a single name. Here, if no
match occurs, the tail of the list (nmL) is searched for a further match.
Only if this list is empty is a flag returned indicating that no match
has occured (3rd clause). 

The function is used to search a list of constructor names for a match. A
constructor ITrm (Const) is hence returned rather than a symbol ITrm (Sym)
The count `k' indicates that position in the list where a match occurs and
is thus increment whenever a match fails and the tail of the list is 
considered. The other indices `i' and `j' are passed into the function.

> lookup_nml :: [Name'] -> String -> Int -> Int -> Int -> (Bool , Flagged_ITrm)

> lookup_nml ( Name nm' : nml ) nm i j k 
> 	| nm == nm' = ( True , ( Opnd . Itrm ) ( Const i j k [] [sym_nmd] ) )
>	| otherwise = lookup_nml nml nm i j ( k + 1 )

> lookup_nml ( Operator' nm' prc opt : nml ) nm i j k 
> 	| nm == nm' = ( True , Opr 
>			       ( OpItrm ( Const i j k [] [sym_nmd] )) opt prc )
>	| otherwise = lookup_nml nml nm i j ( k + 1 )

> lookup_nml [] _ _ _ _
>	= ( False , error "" )








dummy token list returned with error messages. Used to ensure
parse function looking for a separator is given one hence preventing
failure to pattern match in ( _ : tkl ) expresions (see introduction to
`term'' function above).

> dmy :: [Token]

> dmy = [ Rvd "" ]



extend a signature embedded in a parse state with a declaration expressed
as a flagged_itrm. (lazy evaluation ensures that this function will
not be evaluated if the declaration argument is not Idec.)

> pst_extend :: Flagged_ITrm -> Parse_State -> Parse_State

> pst_extend ( Opnd ( Idec idc )) ( tgL , sg ) 
>	= ( tgL , extend dc sg )	
>	  where
>	  dc = build_dc sg idc





check for a tag in tag list

> fetch_tg tg_nm1 ( tg@( tg_nm2 , _ , _ ) : tgL )
>	| tg_nm1 == tg_nm2 = ( True , tg )
>	| otherwise        = fetch_tg tg_nm1 tgL

> fetch_tg _ [] = ( False , ( "" , [] , [] ) )



derivation parser



build derivations

> parse_deriv :: Parse_State -> Flagged_ITrm -> Thm

> parse_deriv pst ( Opnd ( Itrm itm ))
>	= deriv pst itm

> parse_deriv _ _
>	= TH_Err "unimplemented feature"




> deriv :: Parse_State -> ITrm -> Thm


> deriv pst@( tgL , sg ) ( Tagid ( str , _ , cnv_fnL ) argL ) 
>	= case fetch_fn cnv_fnL of
>		Ok cnv_fn -> cnv_fn argL
>		Bad mesg  -> TH_Err mesg
>	  where
>	  fetch_fn ( Thm_Fn fn : _ ) = Ok fn
>	  fetch_fn ( _ : oth )       = fetch_fn oth
>	  fetch_fn []                = Bad ( "cannot convert tag " ++ str ++ " to theorem" )

> deriv pst@( tgL , sg ) ( Binder Delta idc itm _ _ ) 
>	= case typ_of_trm sg2 dc_typ of
>		Constant Bool' _ _      -> discharge th
>		Constant ( Univ _ ) _ _ -> generalise th
>		otherwise               -> TH_Err "dc type not bool or univ"
>	  where
>	  th = deriv ( tgL , extend dc sg ) itm
>	  ( dc_typ , _ , sg2 ) = internal_Trm ( typ_of_Dec dc )
>	  dc = build_dc sg idc


> deriv pst@( _ , sg ) ( App itm1 itm2 _ _ )
>	= case th_tm of
>		Binder Forall _ _ _ _ -> specialise th1 tm
>		Binder Imp    _ _ _ _ -> modus_ponens th1 th2
>		otherwise             -> TH_Err "not \177 or \182"
>	  where
>	  th1 = deriv pst itm1
>	  th2 = deriv pst itm2
>	  tm  = build_trm' sg itm2
>	  ( th_tm , _ ) = internal_Thm th1

> deriv pst@( _ , sg ) ( Pair itm1 itm2 _ _ _ )
>	= modus_ponens ( modus_ponens spec_th th1 ) th2
>	  where
>	  spec_th = specialise ( specialise th tm1 ) tm2
>	  tm1 = typ_of_Thm th1
>	  tm2 = typ_of_Thm th2
>	  th1 = deriv pst itm1
>	  th2 = deriv pst itm2
>	  th  = taut ( parse_tm pst str )
>	  str = "\177 a:bool. \177 b:bool.a \182 b \182 a \179 b"

> deriv ( _ , sg ) ( Sym i j _ _ )
>	= axiom sg i j 

> deriv pst@( _ , SG isg ) itm 
>	= TH_Err (" Invalid construction: " ++ unparse' isg itm )

> deriv _ _ = error "deriv error"











integer list parser

errors made into term argument (Tag_Trm) and passed back as malformed term.
(integer lists do not have an error constructor). Otherwise Tag_Int with
accompanying list returned. 

> parse_iL :: [Token] -> ( Tag_Arg , [Token] )

> parse_iL ( Rvd "\168" : tkL )
>	= parse_iL' [] tkL

> parse_iL _ = ( Tg_Trm ( TM_Err "Malformed integer list" ) , dmy )






> parse_iL' iL ( Clr str : tkL ) | and ( map isDigit str )
>	= parse_iL'' ( read str : iL ) tkL

> parse_iL' iL ( Rvd "\169" : tkL )
>	= ( Tg_Int iL , tkL )

> parse_iL' iL ( tk : _ )
>	= ( Tg_Trm ( TM_Err ( "Malformed item in integer list: " ++ disp_tk tk )) , dmy )

> parse_iL' iL []
>	= ( Tg_Trm ( TM_Err "Unexpected end of input" ) , dmy )




> parse_iL'' iL ( Rvd "\169" : tkL )
>	= ( Tg_Int iL , tkL )

> parse_iL'' iL ( Rvd "," : tkL )
>	= parse_iL' iL tkL

> parse_iL'' iL _ 
>	= ( Tg_Trm ( TM_Err " ',' or '\169' expected " ) , dmy )

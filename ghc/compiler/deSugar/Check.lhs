%
% (c) The GRASP/AQUA Project, Glasgow University, 1997-1998
%
% Author: Juan J. Quintela    <quintela@krilin.dc.fi.udc.es>
\section{Module @Check@ in @deSugar@}

\begin{code}


module Check ( check , ExhaustivePat ) where


import HsSyn		
import TcHsSyn		( TypecheckedPat )
import DsHsSyn		( outPatType ) 
import CoreSyn		

import DsUtils		( EquationInfo(..),
			  MatchResult(..),
			  EqnSet,
			  CanItFail(..),
			  tidyLitPat
 			)
import Id		( idType )
import DataCon		( DataCon, dataConTyCon, dataConArgTys,
			  dataConSourceArity, dataConFieldLabels )
import Name             ( Name, mkLocalName, getOccName, isDataSymOcc, getName, mkSrcVarOcc )
import Type		( Type, splitAlgTyConApp, mkTyVarTys,
                          splitTyConApp_maybe
			)
import TysWiredIn	( nilDataCon, consDataCon, 
                          mkListTy, mkTupleTy, tupleCon
			)
import Unique		( unboundKey )
import TyCon            ( tyConDataCons, tupleTyConBoxity, isTupleTyCon )
import BasicTypes	( Boxity(..) )
import SrcLoc		( noSrcLoc )
import UniqSet
import Outputable

#include "HsVersions.h"
\end{code}

This module performs checks about if one list of equations are:
\begin{itemize}
\item Overlapped
\item Non exhaustive
\end{itemize}
To discover that we go through the list of equations in a tree-like fashion.

If you like theory, a similar algorithm is described in:
\begin{quotation}
	{\em Two Techniques for Compiling Lazy Pattern Matching},
	Luc Maranguet,
	INRIA Rocquencourt (RR-2385, 1994)
\end{quotation}
The algorithm is based on the first technique, but there are some differences:
\begin{itemize}
\item We don't generate code
\item We have constructors and literals (not only literals as in the 
	  article)
\item We don't use directions, we must select the columns from 
	  left-to-right
\end{itemize}
(By the way the second technique is really similar to the one used in 
 @Match.lhs@ to generate code)

This function takes the equations of a pattern and returns:
\begin{itemize}
\item The patterns that are not recognized
\item The equations that are not overlapped
\end{itemize}
It simplify the patterns and then call @check'@ (the same semantics), and it 
needs to reconstruct the patterns again ....

The problem appear with things like:
\begin{verbatim}
  f [x,y]   = ....
  f (x:xs)  = .....
\end{verbatim}
We want to put the two patterns with the same syntax, (prefix form) and 
then all the constructors are equal:
\begin{verbatim}
  f (: x (: y []))   = ....
  f (: x xs)         = .....
\end{verbatim}
(more about that in @simplify_eqns@)

We would prefer to have a @WarningPat@ of type @String@, but Strings and the 
Pretty Printer are not friends.

We use @InPat@ in @WarningPat@ instead of @OutPat@
because we need to print the 
warning messages in the same way they are introduced, i.e. if the user 
wrote:
\begin{verbatim}
	f [x,y] = ..
\end{verbatim}
He don't want a warning message written:
\begin{verbatim}
        f (: x (: y [])) ........
\end{verbatim}
Then we need to use InPats.
\begin{quotation}
     Juan Quintela 5 JUL 1998\\
	  User-friendliness and compiler writers are no friends.
\end{quotation}
\begin{code}

type WarningPat = InPat Name
type ExhaustivePat = ([WarningPat], [(Name, [HsLit])])


check :: [EquationInfo] -> ([ExhaustivePat],EqnSet)
check qs = (untidy_warns, incomplete)
      where
	(warns, incomplete) = check' (simplify_eqns qs)
	untidy_warns = map untidy_exhaustive warns 

untidy_exhaustive :: ExhaustivePat -> ExhaustivePat
untidy_exhaustive ([pat], messages) = 
		  ([untidy_no_pars pat], map untidy_message messages)
untidy_exhaustive (pats, messages) = 
		  (map untidy_pars pats, map untidy_message messages)

untidy_message :: (Name, [HsLit]) -> (Name, [HsLit])
untidy_message (string, lits) = (string, map untidy_lit lits)
\end{code}

The function @untidy@ does the reverse work of the @simplify_pat@ funcion.

\begin{code}

type NeedPars = Bool 

untidy_no_pars :: WarningPat -> WarningPat
untidy_no_pars p = untidy False p

untidy_pars :: WarningPat -> WarningPat
untidy_pars p = untidy True p

untidy :: NeedPars -> WarningPat -> WarningPat
untidy _ p@WildPatIn = p
untidy _ p@(VarPatIn name) = p
untidy _ (LitPatIn lit) = LitPatIn (untidy_lit lit)
untidy _ p@(ConPatIn name []) = p
untidy b (ConPatIn name pats)  = 
       pars b (ConPatIn name (map untidy_pars pats)) 
untidy b (ConOpPatIn pat1 name fixity pat2) = 
       pars b (ConOpPatIn (untidy_pars pat1) name fixity (untidy_pars pat2)) 
untidy _ (ListPatIn pats)  = ListPatIn (map untidy_no_pars pats) 
untidy _ (TuplePatIn pats boxed) = TuplePatIn (map untidy_no_pars pats) boxed

untidy _ (SigPatIn pat ty)      = panic "Check.untidy: SigPatIn"
untidy _ (LazyPatIn pat)        = panic "Check.untidy: LazyPatIn"
untidy _ (AsPatIn name pat)     = panic "Check.untidy: AsPatIn"
untidy _ (NPlusKPatIn name lit) = panic "Check.untidy: NPlusKPatIn"
untidy _ (NegPatIn ipat)        = panic "Check.untidy: NegPatIn"
untidy _ (ParPatIn pat)         = panic "Check.untidy: ParPatIn"
untidy _ (RecPatIn name fields) = panic "Check.untidy: RecPatIn"

pars :: NeedPars -> WarningPat -> WarningPat
pars True p = ParPatIn p
pars _    p = p

untidy_lit :: HsLit -> HsLit
untidy_lit (HsCharPrim c) = HsChar c
--untidy_lit (HsStringPrim s) = HsString s
untidy_lit lit = lit
\end{code}

This equation is the same that check, the only difference is that the
boring work is done, that work needs to be done only once, this is
the reason top have two functions, check is the external interface,
@check'@ is called recursively.

There are several cases:

\begin{itemize} 
\item There are no equations: Everything is OK. 
\item There are only one equation, that can fail, and all the patterns are
      variables. Then that equation is used and the same equation is 
      non-exhaustive.
\item All the patterns are variables, and the match can fail, there are 
      more equations then the results is the result of the rest of equations 
      and this equation is used also.

\item The general case, if all the patterns are variables (here the match 
      can't fail) then the result is that this equation is used and this 
      equation doesn't generate non-exhaustive cases.

\item In the general case, there can exist literals ,constructors or only 
      vars in the first column, we actuate in consequence.

\end{itemize}


\begin{code}

check' :: [EquationInfo] -> ([ExhaustivePat],EqnSet)  
check' []                                              = ([([],[])],emptyUniqSet)

check' [EqnInfo n ctx ps (MatchResult CanFail _)] 
   | all_vars ps  = ([(take (length ps) (repeat new_wild_pat),[])],  unitUniqSet n)

check' qs@((EqnInfo n ctx ps (MatchResult CanFail _)):rs)
   | all_vars ps  = (pats,  addOneToUniqSet indexs n)
  where
    (pats,indexs) = check' rs

check' qs@((EqnInfo n ctx ps result):_) 
   | all_vars ps  = ([],  unitUniqSet n)
--   | nplusk       = panic "Check.check': Work in progress: nplusk"
--   | npat         = panic "Check.check': Work in progress: npat ?????"
   | literals     = split_by_literals qs
   | constructors = split_by_constructor qs
   | only_vars    = first_column_only_vars qs
   | otherwise    = panic "Check.check': Not implemented :-("
  where
     -- Note: RecPats will have been simplified to ConPats
     --       at this stage.
    constructors = or (map is_con qs)
    literals     = or (map is_lit qs)    
    only_vars    = and (map is_var qs) 
--    npat         = or (map is_npat qs)
--    nplusk       = or (map is_nplusk qs)
\end{code}

Here begins the code to deal with literals, we need to split the matrix
in different matrix beginning by each literal and a last matrix with the 
rest of values.

\begin{code}
split_by_literals :: [EquationInfo] -> ([ExhaustivePat],EqnSet)
split_by_literals qs = process_literals used_lits qs
           where
             used_lits = get_used_lits qs
\end{code}

@process_explicit_literals@ is a function that process each literal that appears 
in the column of the matrix. 

\begin{code}
process_explicit_literals :: [HsLit] -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
process_explicit_literals lits qs = (concat pats, unionManyUniqSets indexs)
    where                  
      pats_indexs   = map (\x -> construct_literal_matrix x qs) lits
      (pats,indexs) = unzip pats_indexs 

\end{code}


@process_literals@ calls @process_explicit_literals@ to deal with the literals 
that appears in the matrix and deal also with the rest of the cases. It 
must be one Variable to be complete.

\begin{code}

process_literals :: [HsLit] -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
process_literals used_lits qs 
  | length default_eqns == 0 = ([make_row_vars used_lits (head qs)]++pats,indexs)
  | otherwise                = (pats_default,indexs_default)
     where
       (pats,indexs)   = process_explicit_literals used_lits qs
       default_eqns    = (map remove_var (filter is_var qs))
       (pats',indexs') = check' default_eqns 
       pats_default    = [(new_wild_pat:ps,constraints) | (ps,constraints) <- (pats')] ++ pats 
       indexs_default  = unionUniqSets indexs' indexs
\end{code}

Here we have selected the literal and we will select all the equations that 
begins for that literal and create a new matrix.

\begin{code}
construct_literal_matrix :: HsLit -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
construct_literal_matrix lit qs =
    (map (\ (xs,ys) -> (new_lit:xs,ys)) pats,indexs) 
  where
    (pats,indexs) = (check' (remove_first_column_lit lit qs)) 
    new_lit = LitPatIn lit 

remove_first_column_lit :: HsLit
                        -> [EquationInfo] 
                        -> [EquationInfo]
remove_first_column_lit lit qs = 
    map shift_pat (filter (is_var_lit lit) qs)
  where
     shift_pat (EqnInfo n ctx []     result) =  panic "Check.shift_var: no patterns"
     shift_pat (EqnInfo n ctx (_:ps) result) =  EqnInfo n ctx ps result

\end{code}

This function splits the equations @qs@ in groups that deal with the 
same constructor.

\begin{code}

split_by_constructor :: [EquationInfo] -> ([ExhaustivePat],EqnSet)

split_by_constructor qs | length unused_cons /= 0 = need_default_case used_cons unused_cons qs 
                        | otherwise               = no_need_default_case used_cons qs 
                       where 
                          used_cons   = get_used_cons qs 
                          unused_cons = get_unused_cons used_cons 

\end{code}

The first column of the patterns matrix only have vars, then there is 
nothing to do.

\begin{code}
first_column_only_vars :: [EquationInfo] -> ([ExhaustivePat],EqnSet)
first_column_only_vars qs = (map (\ (xs,ys) -> (new_wild_pat:xs,ys)) pats,indexs)
                          where
                            (pats,indexs) = check' (map remove_var qs)
       
\end{code}

This equation takes a matrix of patterns and split the equations by 
constructor, using all the constructors that appears in the first column 
of the pattern matching.

We can need a default clause or not ...., it depends if we used all the 
constructors or not explicitly. The reasoning is similar to @process_literals@,
the difference is that here the default case is not always needed.

\begin{code}
no_need_default_case :: [TypecheckedPat] -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
no_need_default_case cons qs = (concat pats, unionManyUniqSets indexs)
    where                  
      pats_indexs   = map (\x -> construct_matrix x qs) cons
      (pats,indexs) = unzip pats_indexs 

need_default_case :: [TypecheckedPat] -> [DataCon] -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
need_default_case used_cons unused_cons qs 
  | length default_eqns == 0 = (pats_default_no_eqns,indexs)
  | otherwise                = (pats_default,indexs_default)
     where
       (pats,indexs)   = no_need_default_case used_cons qs
       default_eqns    = (map remove_var (filter is_var qs))
       (pats',indexs') = check' default_eqns 
       pats_default    = [(make_whole_con c:ps,constraints) | 
                          c <- unused_cons, (ps,constraints) <- pats'] ++ pats
       new_wilds       = make_row_vars_for_constructor (head qs)
       pats_default_no_eqns =  [(make_whole_con c:new_wilds,[]) | c <- unused_cons] ++ pats
       indexs_default  = unionUniqSets indexs' indexs

construct_matrix :: TypecheckedPat -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
construct_matrix con qs =
    (map (make_con con) pats,indexs) 
  where
    (pats,indexs) = (check' (remove_first_column con qs)) 
\end{code}

Here remove first column is more difficult that with literals due to the fact 
that constructors can have arguments.

For instance, the matrix
\begin{verbatim}
 (: x xs) y
 z        y
\end{verbatim}
is transformed in:
\begin{verbatim}
 x xs y
 _ _  y
\end{verbatim}

\begin{code}
remove_first_column :: TypecheckedPat                -- Constructor 
                    -> [EquationInfo] 
                    -> [EquationInfo]
remove_first_column (ConPat con _ _ _ con_pats) qs = 
    map shift_var (filter (is_var_con con) qs)
  where
     new_wilds = [WildPat (outPatType arg_pat) | arg_pat <- con_pats]
     shift_var (EqnInfo n ctx (ConPat _ _ _ _ ps':ps) result) = 
                EqnInfo n ctx (ps'++ps)               result 
     shift_var (EqnInfo n ctx (WildPat _     :ps)     result) = 
                EqnInfo n ctx (new_wilds ++   ps)     result
     shift_var _ = panic "Check.Shift_var:No done"

make_row_vars :: [HsLit] -> EquationInfo -> ExhaustivePat
make_row_vars used_lits (EqnInfo _ _ pats _ ) = 
   (VarPatIn new_var:take (length (tail pats)) (repeat new_wild_pat),[(new_var,used_lits)])
  where new_var = hash_x

hash_x = mkLocalName unboundKey {- doesn't matter much -}
		     (mkSrcVarOcc SLIT("#x"))
		     noSrcLoc

make_row_vars_for_constructor :: EquationInfo -> [WarningPat]
make_row_vars_for_constructor (EqnInfo _ _ pats _ ) = take (length (tail pats)) (repeat new_wild_pat)

compare_cons :: TypecheckedPat -> TypecheckedPat -> Bool
compare_cons (ConPat id1 _ _ _ _) (ConPat id2 _ _ _ _) = id1 == id2  

remove_dups :: [TypecheckedPat] -> [TypecheckedPat]
remove_dups []     = []
remove_dups (x:xs) | or (map (\y -> compare_cons x y) xs) = remove_dups  xs
                   | otherwise                            = x : remove_dups xs

get_used_cons :: [EquationInfo] -> [TypecheckedPat]
get_used_cons qs = remove_dups [con | (EqnInfo _ _ (con@(ConPat _ _ _ _ _):_) _) <- qs ]

remove_dups' :: [HsLit] -> [HsLit] 
remove_dups' []                   = []
remove_dups' (x:xs) | x `elem` xs = remove_dups' xs
                    | otherwise   = x : remove_dups' xs 


get_used_lits :: [EquationInfo] -> [HsLit]
get_used_lits qs = remove_dups' all_literals
	         where
	           all_literals = get_used_lits' qs

get_used_lits' :: [EquationInfo] -> [HsLit]
get_used_lits' [] = []
get_used_lits' ((EqnInfo _ _ ((LitPat lit _):_) _):qs) = 
	       lit : get_used_lits qs
get_used_lits' ((EqnInfo _ _ ((NPat lit _ _):_) _):qs) = 
	       lit : get_used_lits qs
get_used_lits' (q:qs)                                  =       
	       get_used_lits qs

get_unused_cons :: [TypecheckedPat] -> [DataCon]
get_unused_cons used_cons = unused_cons
     where
       (ConPat _ ty _ _ _) = head used_cons
       Just (ty_con,_) 	   = splitTyConApp_maybe ty
       all_cons        	   = tyConDataCons ty_con
       used_cons_as_id 	   = map (\ (ConPat d _ _ _ _) -> d) used_cons
       unused_cons     	   = uniqSetToList
		 (mkUniqSet all_cons `minusUniqSet` mkUniqSet used_cons_as_id) 


all_vars :: [TypecheckedPat] -> Bool
all_vars []              = True
all_vars (WildPat _:ps)  = all_vars ps
all_vars _               = False

remove_var :: EquationInfo -> EquationInfo
remove_var (EqnInfo n ctx (WildPat _:ps) result) = EqnInfo n ctx ps result
remove_var _                                     =
	 panic "Check.remove_var: equation does not begin with a variable"

is_con :: EquationInfo -> Bool
is_con (EqnInfo _ _ ((ConPat _ _ _ _ _):_) _) = True
is_con _                                      = False

is_lit :: EquationInfo -> Bool
is_lit (EqnInfo _ _ ((LitPat _ _):_) _) = True
is_lit (EqnInfo _ _ ((NPat _ _ _):_) _) = True
is_lit _                                = False

is_npat :: EquationInfo -> Bool
is_npat (EqnInfo _ _ ((NPat _ _ _):_) _) = True
is_npat _                                 = False

is_nplusk :: EquationInfo -> Bool
is_nplusk (EqnInfo _ _ ((NPlusKPat _ _ _ _ _):_) _) = True
is_nplusk _                                         = False

is_var :: EquationInfo -> Bool
is_var (EqnInfo _ _ ((WildPat _):_) _)  = True
is_var _                                = False

is_var_con :: DataCon -> EquationInfo -> Bool
is_var_con con (EqnInfo _ _ ((WildPat _):_)     _)                 = True
is_var_con con (EqnInfo _ _ ((ConPat id _ _ _ _):_) _) | id == con = True
is_var_con con _                                                   = False

is_var_lit :: HsLit -> EquationInfo -> Bool
is_var_lit lit (EqnInfo _ _ ((WildPat _):_)     _)               = True
is_var_lit lit (EqnInfo _ _ ((LitPat lit' _):_) _) | lit == lit' = True
is_var_lit lit (EqnInfo _ _ ((NPat lit' _ _):_) _) | lit == lit' = True
is_var_lit lit _                                                 = False
\end{code}

The difference beteewn @make_con@ and @make_whole_con@ is that
@make_wole_con@ creates a new constructor with all their arguments, and
@make_con@ takes a list of argumntes, creates the contructor getting their
arguments from the list. See where \fbox{\ ???\ } are used for details.

We need to reconstruct the patterns (make the constructors infix and
similar) at the same time that we create the constructors.

You can tell tuple constructors using
\begin{verbatim}
        Id.isTupleCon
\end{verbatim}
You can see if one constructor is infix with this clearer code :-))))))))))
\begin{verbatim}
        Lex.isLexConSym (Name.occNameString (Name.getOccName con))
\end{verbatim}

       Rather clumsy but it works. (Simon Peyton Jones)


We don't mind the @nilDataCon@ because it doesn't change the way to
print the messsage, we are searching only for things like: @[1,2,3]@,
not @x:xs@ ....

In @reconstruct_pat@ we want to ``undo'' the work
that we have done in @simplify_pat@.
In particular:
\begin{tabular}{lll}
	@((,) x y)@   & returns to be & @(x, y)@
\\      @((:) x xs)@  & returns to be & @(x:xs)@
\\      @(x:(...:[])@ & returns to be & @[x,...]@
\end{tabular}
%
The difficult case is the third one becouse we need to follow all the
contructors until the @[]@ to know that we need to use the second case,
not the second. \fbox{\ ???\ }
%
\begin{code}
isInfixCon con = isDataSymOcc (getOccName con)

is_nil (ConPatIn con []) = con == getName nilDataCon
is_nil _                 = False

is_list (ListPatIn _) = True
is_list _             = False

return_list id q = id == consDataCon && (is_nil q || is_list q) 

make_list p q | is_nil q   = ListPatIn [p]
make_list p (ListPatIn ps) = ListPatIn (p:ps)  
make_list _ _              = panic "Check.make_list: Invalid argument"

make_con :: TypecheckedPat -> ExhaustivePat -> ExhaustivePat           
make_con (ConPat id _ _ _ _) (p:q:ps, constraints) 
     | return_list id q = (make_list p q : ps, constraints)
     | isInfixCon id = ((ConOpPatIn p name fixity q) : ps, constraints) 
    where name   = getName id
          fixity = panic "Check.make_con: Guessing fixity"

make_con (ConPat id _ _ _ pats) (ps,constraints) 
      | isTupleTyCon tc = (TuplePatIn pats_con (tupleTyConBoxity tc) : rest_pats, constraints) 
      | otherwise       = (ConPatIn name pats_con		     : rest_pats, constraints)
    where num_args  = length pats
          name      = getName id
          pats_con  = take num_args ps
          rest_pats = drop num_args ps
	  tc	    = dataConTyCon id
	  

make_whole_con :: DataCon -> WarningPat
make_whole_con con | isInfixCon con = ConOpPatIn new_wild_pat name fixity new_wild_pat
                   | otherwise      = ConPatIn name pats
                where 
                  fixity = panic "Check.make_whole_con: Guessing fixity"
                  name   = getName con
                  arity  = dataConSourceArity con 
                  pats   = take arity (repeat new_wild_pat)


new_wild_pat :: WarningPat
new_wild_pat = WildPatIn
\end{code}

This equation makes the same thing as @tidy@ in @Match.lhs@, the
difference is that here we can do all the tidy in one place and in the
@Match@ tidy it must be done one column each time due to bookkeeping 
constraints.

\begin{code}

simplify_eqns :: [EquationInfo] -> [EquationInfo]
simplify_eqns []                               = []
simplify_eqns ((EqnInfo n ctx pats result):qs) = 
 (EqnInfo n ctx pats' result) : simplify_eqns qs
 where
  pats' = map simplify_pat pats

simplify_pat :: TypecheckedPat -> TypecheckedPat  

simplify_pat pat@(WildPat gt) = pat
simplify_pat (VarPat id)      = WildPat (idType id) 

simplify_pat (LazyPat p)    = simplify_pat p
simplify_pat (AsPat id p)   = simplify_pat p

simplify_pat (ConPat id ty tvs dicts ps) = ConPat id ty tvs dicts (map simplify_pat ps)

simplify_pat (ListPat ty ps) = foldr (\ x -> \y -> ConPat consDataCon list_ty [] [] [x, y])
	                             (ConPat nilDataCon list_ty [] [] [])
	                             (map simplify_pat ps)
                             where list_ty = mkListTy ty


simplify_pat (TuplePat ps boxity)
  = ConPat (tupleCon boxity arity)
	   (mkTupleTy boxity arity (map outPatType ps)) [] []
	   (map simplify_pat ps)
  where
    arity = length ps

simplify_pat (RecPat dc ty ex_tvs dicts [])   
  = ConPat dc ty ex_tvs dicts all_wild_pats
  where
    all_wild_pats = map WildPat con_arg_tys

      -- identical to machinations in Match.tidy1:
    (_, inst_tys, _) = splitAlgTyConApp ty
    con_arg_tys      = dataConArgTys dc (inst_tys ++ mkTyVarTys ex_tvs)

simplify_pat (RecPat dc ty ex_tvs dicts idps) 
  = ConPat dc ty ex_tvs dicts pats
  where
    pats = map (simplify_pat.snd) all_pats

     -- pad out all the missing fields with WildPats.
    field_pats = map (\ f -> (getName f, WildPat (panic "simplify_pat(RecPat-2)")))
		     (dataConFieldLabels dc)
    all_pats = 
      foldr
       ( \ (id,p,_) acc -> insertNm (getName id) p acc)
       field_pats
       idps
       
    insertNm nm p [] = [(nm,p)]
    insertNm nm p (x@(n,_):xs)
      | nm == n    = (nm,p):xs
      | otherwise  = x : insertNm nm p xs

simplify_pat pat@(LitPat lit lit_ty)        = tidyLitPat lit lit_ty pat
simplify_pat pat@(NPat   lit lit_ty hsexpr) = tidyLitPat lit lit_ty pat

simplify_pat (NPlusKPat	id hslit ty hsexpr1 hsexpr2) = 
     WildPat ty
   where ty = panic "Check.simplify_pat: Gessing ty"

simplify_pat (DictPat dicts methods) = 
    case num_of_d_and_ms of
       0 -> simplify_pat (TuplePat [] Boxed) 
       1 -> simplify_pat (head dict_and_method_pats) 
       _ -> simplify_pat (TuplePat dict_and_method_pats Boxed)
    where
       num_of_d_and_ms	 = length dicts + length methods
       dict_and_method_pats = map VarPat (dicts ++ methods)

\end{code}

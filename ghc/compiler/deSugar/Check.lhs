%
% (c) The GRASP/AQUA Project, Glasgow University, 1997
%
% Author: Juan J. Quintela    <quintela@dc.fi.udc.es>

\begin{code}


module Check ( check , ExhaustivePat, WarningPat, BoxedString(..) ) where


import {-# SOURCE #-} DsExpr  ( dsExpr  )
import {-# SOURCE #-} DsBinds ( dsBinds )

import HsSyn		
import TcHsSyn		( TypecheckedPat, 
                          TypecheckedMatch,
			  TypecheckedHsBinds, 
                          TypecheckedHsExpr	
                        )
import DsHsSyn		( outPatType ) 
import CoreSyn		

import DsMonad		( DsM, DsMatchContext(..),
			  DsMatchKind(..)
                        )
import DsUtils		( EquationInfo(..),
			  MatchResult(..),
			  EqnNo,
			  EqnSet,
			  CanItFail(..)
 			)
import Id		( idType,
			  Id,
			  idName,
                          isTupleCon,			   
                          getIdArity
			)
import IdInfo		( ArityInfo(..) )
import Lex              ( isLexConSym )
import Name             ( occNameString,
                          Name,
                          getName,
                          nameUnique,
                          getOccName,
                          getOccString
                        )
import Type		( Type, 
                          isUnboxedType, 
                          splitTyConApp_maybe
			)
import TyVar		( TyVar )
import TysPrim		( intPrimTy, 
                          charPrimTy, 
                          floatPrimTy, 
                          doublePrimTy,
			  addrPrimTy, 
                          wordPrimTy
			)
import TysWiredIn	( nilDataCon, consDataCon, 
                          mkTupleTy, tupleCon,
                          mkListTy, 
                          charTy, charDataCon, 
                          intTy, intDataCon,
			  floatTy, floatDataCon, 
                          doubleTy, doubleDataCon, 
                          addrTy, addrDataCon,
                          wordTy, wordDataCon
			)
import TyCon            ( tyConDataCons )
import UniqSet
import Unique		( Unique )
import Outputable

#include "HsVersions.h"
\end{code}

This module perfoms checks about if one list of equations are:
	- Overlapped
	- Non exhaustive

To discover that we go through the list of equations in a tree-like fashion.

If you like theory, a similar algoritm is described in:
	Two Tecniques for Compiling Lazy Pattern Matching
	Luc Maranguet
	INRIA Rocquencourt (RR-2385, 1994)

The algorithm is based in the first Technique, but there are somo diferences:
	- We don't generate code
	- We have constructors and literals (not only literals as in the article)
	- We don't use directions, we must select the columns from left-to-right

(By the wat the second technique is really similar to the one used in MAtch.lhs to generate code)


This function takes the equations of a pattern and returns:
  - The patterns that are not recognized
  - The equations that are not overlapped

It symplify the patterns and then call check' (the same semantics),and it needs to 
reconstruct the patterns again ....

The problem appear with things like:
  f [x,y]   = ....
  f (x:xs)  = .....

We want to put the two patterns with the same syntax, (prefix form) and then all the 
constructors are equal:
  f (: x (: y []))   = ....
  f (: x xs)         = .....

(more about that in symplify_eqns)

We would preffer to have a WarningPat of type String, but Strings and the 
Pretty Printer are not friends.
 
\begin{code}

newtype BoxedString = BS String

type WarningPat = InPat BoxedString --Name --String 
type ExhaustivePat = ([WarningPat], [(BoxedString, [HsLit])])


instance Outputable BoxedString where
    ppr (BS s) = text s


check :: [EquationInfo] -> ([ExhaustivePat],EqnSet)
check qs = check' (simplify_eqns qs)

\end{code}

This equation is the same that check, the only difference is that the
boring work is done, that woprk needs to be done only once, this is
the reason top have two funtions, check is the external interface,
check' is called recursively.

There are several cases:

\begin{item} 
\item There are no equations: Everything is okey. 
\item There are only one equation, that can fail, and all the patterns are
      variables. Then that equation is used and the same equation is 
      nonexhaustive.
\item All the patterns are variables, and the match can fail,therr are more equations 
      then the results is the result of the rest of equations and this equation is used also.

\item The general case, if all the patterns are variables (here the match can't fail) 
      then the result is that this equation is used and this equation doesn't generate 
      non-exustive cases.

\item In the general case, there can exist literals ,constructors or only vars in the 
      first column, we actuate in consecuence.

\end{item}


\begin{code}

check' :: [EquationInfo] -> ([ExhaustivePat],EqnSet)  
check' []                                              = ([([],[])],emptyUniqSet)

check' [EqnInfo n ctx ps (MatchResult CanFail _ _)] 
   | all_vars ps  = ([(take (length ps) (repeat new_wild_pat),[])],  unitUniqSet n)

check' qs@((EqnInfo n ctx ps (MatchResult CanFail _ _)):_) 
   | all_vars ps  = (pats,  addOneToUniqSet indexs n)
  where
    (pats,indexs) = check' (tail qs)

check' qs@((EqnInfo n ctx ps result):_) 
   | all_vars ps  = ([],  unitUniqSet n)
--   | nplusk       = panic "Check.check': Work in progress: nplusk"
--   | npat         = panic "Check.check': Work in progress: npat ?????"
   | literals     = split_by_literals qs
   | constructors = split_by_constructor qs
   | only_vars    = first_column_only_vars qs
   | otherwise    = panic "Check.check': Not implemented :-("
  where
    constructors = or (map is_con qs)
    literals     = or (map is_lit qs)    
--    npat         = or (map is_npat qs)
--    nplusk       = or (map is_nplusk qs)
    only_vars    = and (map is_var qs) 
\end{code}

Here begins the code to deal with literals, we need to split the matrix in diferent matrix 
begining by each literal and a last matrix with the rest of values.

\begin{code}
split_by_literals :: [EquationInfo] -> ([ExhaustivePat],EqnSet)
split_by_literals qs = process_literals used_lits qs
           where
             used_lits = get_used_lits qs
\end{code}

process_explicit_literals is a funtion taht process each literal that appears in
the column of the matrix. 

\begin{code}
process_explicit_literals :: [HsLit] -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
process_explicit_literals lits qs = (concat pats, unionManyUniqSets indexs)
    where                  
      pats_indexs   = map (\x -> construct_literal_matrix x qs) lits
      (pats,indexs) = unzip pats_indexs 

\end{code}


Process_literals calls process_explicit_literals to deal with the literals taht apears in 
the matrix and deal also sith ther rest of the cases. It must be one Variable to be complete.

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

Here we have selected the literal and we will select all the equations that begins for that 
literal and create a new matrix.

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

This function splits the equations @qs@ in groups that deal with the same constructor 

\begin{code}

split_by_constructor :: [EquationInfo] -> ([ExhaustivePat],EqnSet)

split_by_constructor qs | length unused_cons /= 0 = need_default_case used_cons unused_cons qs 
                        | otherwise               = no_need_default_case used_cons qs 
                       where 
                          used_cons   = get_used_cons qs 
                          unused_cons = get_unused_cons used_cons 

\end{code}

The first column of the patterns matrix only have vars, then there is nothing to do.

\begin{code}
first_column_only_vars :: [EquationInfo] -> ([ExhaustivePat],EqnSet)
first_column_only_vars qs = (map (\ (xs,ys) -> (WildPatIn:xs,ys)) pats,indexs)
                          where
                            (pats,indexs) = check' (map remove_var qs)
       
\end{code}

This equation takes a matrix of patterns and split the equations by constructor, using all
the constructors that appears in the first column of the pattern matching.

We can need a default clause or not ...., it depends if we used all the constructors or not
explicitily. The reasoning is similar to process_literals, the difference is that here
the default case is not allways needed.

\begin{code}
no_need_default_case :: [TypecheckedPat] -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
no_need_default_case cons qs = (concat pats, unionManyUniqSets indexs)
    where                  
      pats_indexs   = map (\x -> construct_matrix x qs) cons
      (pats,indexs) = unzip pats_indexs 

need_default_case :: [TypecheckedPat] -> [Id] -> [EquationInfo] -> ([ExhaustivePat],EqnSet)
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

Here remove first column is more difficult that with literals due to the fact that 
constructors can have arguments.

for instance, the matrix

 (: x xs) y
 z        y

is transformed in:

 x xs y
 _ _  y


\begin{code}
remove_first_column :: TypecheckedPat                -- Constructor 
                    -> [EquationInfo] 
                    -> [EquationInfo]
remove_first_column (ConPat con _ con_pats) qs = 
    map shift_var (filter (is_var_con con) qs)
  where
     new_wilds = [WildPat (outPatType arg_pat) | arg_pat <- con_pats]
     shift_var (EqnInfo n ctx (ConPat _ _ ps':ps) result) = 
                EqnInfo n ctx (ps'++ps)           result 
     shift_var (EqnInfo n ctx (WildPat _     :ps) result) = 
                EqnInfo n ctx (new_wilds ++   ps) result
     shift_var _                                          = panic "Check.Shift_var:No done"

make_row_vars :: [HsLit] -> EquationInfo -> ExhaustivePat
make_row_vars used_lits (EqnInfo _ _ pats _ ) = 
   (VarPatIn new_var:take (length (tail pats)) (repeat WildPatIn),[(new_var,used_lits)])
  where new_var = BS "#x"   

make_row_vars_for_constructor :: EquationInfo -> [WarningPat]
make_row_vars_for_constructor (EqnInfo _ _ pats _ ) = take (length (tail pats)) (repeat WildPatIn)

compare_cons :: TypecheckedPat -> TypecheckedPat -> Bool
compare_cons (ConPat id1 _ _) (ConPat id2 _ _) = id1 == id2  

remove_dups :: [TypecheckedPat] -> [TypecheckedPat]
remove_dups []     = []
remove_dups (x:xs) | or (map (\y -> compare_cons x y) xs) = remove_dups  xs
                   | otherwise                            = x : remove_dups xs

get_used_cons :: [EquationInfo] -> [TypecheckedPat]
get_used_cons qs = remove_dups [con | (EqnInfo _ _ (con@(ConPat _ _ _):_) _) <- qs]

remove_dups' :: [HsLit] -> [HsLit] 
remove_dups' []                   = []
remove_dups' (x:xs) | x `elem` xs = remove_dups' xs
                    | otherwise   = x : remove_dups' xs 


get_used_lits :: [EquationInfo] -> [HsLit]
get_used_lits qs = remove_dups' (get_used_lits' qs)

get_used_lits' :: [EquationInfo] -> [HsLit]
get_used_lits' []                                      = []
get_used_lits' ((EqnInfo _ _ ((LitPat lit _):_) _):qs) = lit : get_used_lits qs
get_used_lits' ((EqnInfo _ _ ((NPat lit _ _):_) _):qs) = lit : get_used_lits qs
get_used_lits' (q:qs)                                  =       get_used_lits qs

get_unused_cons :: [TypecheckedPat] -> [Id]
get_unused_cons used_cons = unused_cons
     where
       (ConPat _ ty _) = head used_cons
       Just (ty_con,_) = splitTyConApp_maybe ty
       all_cons        = tyConDataCons ty_con
       used_cons_as_id = map (\ (ConPat id _ _) -> id) used_cons
       unused_cons     = uniqSetToList (mkUniqSet all_cons `minusUniqSet` mkUniqSet used_cons_as_id) 

all_vars :: [TypecheckedPat] -> Bool
all_vars []              = True
all_vars (WildPat _:ps)  = all_vars ps
all_vars _               = False

remove_var :: EquationInfo -> EquationInfo
remove_var (EqnInfo n ctx (WildPat _:ps) result) = EqnInfo n ctx ps result
remove_var _                                     = panic "Check:remove_var: equation not begin with a variable"

is_con :: EquationInfo -> Bool
is_con (EqnInfo _ _ ((ConPat _ _ _):_) _) = True
is_con _                                  = False

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

is_var_con :: Id -> EquationInfo -> Bool
is_var_con con (EqnInfo _ _ ((WildPat _):_)     _)             = True
is_var_con con (EqnInfo _ _ ((ConPat id _ _):_) _) | id == con = True
is_var_con con _                                               = False

is_var_lit :: HsLit -> EquationInfo -> Bool
is_var_lit lit (EqnInfo _ _ ((WildPat _):_)     _)               = True
is_var_lit lit (EqnInfo _ _ ((LitPat lit' _):_) _) | lit == lit' = True
is_var_lit lit (EqnInfo _ _ ((NPat lit' _ _):_) _) | lit == lit' = True
is_var_lit lit _                                                 = False
\end{code}

The difference beteewn make_con and make_whole_con is that make_wole_con creates a new
constructor with all their arguments, and make_Con takes a list of argumntes, creates
the contructor geting thir argumnts from the list. See where are used for details.

We need to reconstruct the patterns (make the constructors infix and similar) at the 
same time that we create the constructors.

You can tell tuple constructors using

        Id.isTupleCon

You can see if one contructur is infix with this clearer code :-))))))))))

        Lex.isLexConSym (Name.occNameString (Name.getOccName con))

       Rather clumsy but it works. (Simon Peyton Jones)


We con't mind the nilDataCon because it doesn't change the way to print the messsage, 
we are searching only for things like: [1,2,3], not x:xs .... 


In recontruct_pat we want to "undo" the work taht we have done in simplify_pat
In particular:
	((,) x y)  returns to be (x, y)
        ((:) x xs) returns to be (x:xs)
        (x:(...:[]) returns to be [x,...]

The dificult case is the third one becouse we need to follow all the contructors until the []
to know taht we need to use the second case, not the second.

\begin{code}

isInfixCon con = isLexConSym (occNameString (getOccName con))

is_nil (ConPatIn (BS con) []) = con == getOccString nilDataCon
is_nil _                      = False

is_list (ListPatIn _) = True
is_list _             = False

return_list id q = id == consDataCon && (is_nil q || is_list q) 

make_list p q | is_nil q   = ListPatIn [p]
make_list p (ListPatIn ps) = ListPatIn (p:ps)  
make_list _ _              = panic "Check.make_list: Invalid argument"

make_con :: TypecheckedPat -> ExhaustivePat -> ExhaustivePat           
make_con (ConPat id ty pats) (p:q:ps, constraints) 
     | return_list id q = (make_list p q : ps, constraints)
     | isInfixCon id = (ParPatIn (ConOpPatIn p name fixity q) : ps, constraints) 
    where name   = BS (getOccString id)
          fixity = panic "Check.make_con: Guessing fixity"
make_con (ConPat id ty pats) (ps,constraints) 
      | isTupleCon id = (TuplePatIn pats_con : rest_pats,    constraints) 
      | otherwise     = (ConPatIn name pats_con : rest_pats, constraints)
    where num_args  = length pats
          name      = BS (getOccString id)
          pats_con  = (take num_args ps)
          rest_pats = drop num_args ps         

make_whole_con :: Id -> WarningPat
make_whole_con con | isInfixCon con = ParPatIn(ConOpPatIn new_wild_pat name fixity new_wild_pat)
                   | otherwise      = ConPatIn name pats
                where 
                  fixity = panic "Check.make_whole_con: Guessing fixity"
                  name   = BS (getOccString con)
                  arity  = get_int_arity con 
                  pats   = take arity (repeat new_wild_pat)


new_wild_pat :: WarningPat
new_wild_pat = WildPatIn

get_int_arity :: Id -> Int
get_int_arity id = arity_to_int (getIdArity id)
    where
      arity_to_int (ArityExactly n) = n
      arity_to_int _                = panic "getIntArity: Unknown arity"      

\end{code}

This equation makes the same thing that tidy in Match.lhs, the
diference is that here we can do all the tidy in one place and in the
Match tidy it must be done one column each time due to bookeping 
constraints.

\begin{code}

simplify_eqns :: [EquationInfo] -> [EquationInfo]
simplify_eqns []                               = []
simplify_eqns ((EqnInfo n ctx pats result):qs) = 
    (EqnInfo n ctx(map simplify_pat pats) result) : 
    simplify_eqns qs

simplify_pat :: TypecheckedPat -> TypecheckedPat  
simplify_pat (WildPat gt ) = WildPat gt	

simplify_pat (VarPat id)   = WildPat (idType id) 

simplify_pat (LazyPat p)   = simplify_pat p

simplify_pat (AsPat id p)  = simplify_pat p

simplify_pat (ConPat id ty ps) = ConPat id ty (map simplify_pat ps)

simplify_pat (ConOpPat p1 id p2 ty) = ConPat id ty (map simplify_pat [p1,p2])

simplify_pat (ListPat ty ps) = foldr (\ x -> \y -> ConPat consDataCon list_ty [x, y])
	                                            (ConPat nilDataCon  list_ty [])
	                                            (map simplify_pat ps)
                             where list_ty = mkListTy ty


simplify_pat (TuplePat ps) = ConPat (tupleCon arity)
	                             (mkTupleTy arity (map outPatType ps))
	                             (map simplify_pat ps)
                           where
                              arity = length ps

simplify_pat (RecPat id ty idps) = ConPat id ty pats
                                 where
                                   pats = map (\ (id,p,_)-> simplify_pat p) idps

simplify_pat pat@(LitPat lit lit_ty) 
  | isUnboxedType lit_ty = LitPat lit lit_ty

  | lit_ty == charTy = ConPat charDataCon charTy [LitPat (mk_char lit) charPrimTy]

  | otherwise = pprPanic "tidy1:LitPat:" (ppr pat)
  where
    mk_char (HsChar c)    = HsCharPrim c

simplify_pat (NPat lit lit_ty hsexpr) = better_pat
  where
    better_pat
      | lit_ty == charTy   = ConPat charDataCon   lit_ty [LitPat (mk_char lit)   charPrimTy]
      | lit_ty == intTy    = ConPat intDataCon    lit_ty [LitPat (mk_int lit)    intPrimTy]
      | lit_ty == wordTy   = ConPat wordDataCon   lit_ty [LitPat (mk_word lit)   wordPrimTy]
      | lit_ty == addrTy   = ConPat addrDataCon   lit_ty [LitPat (mk_addr lit)   addrPrimTy]
      | lit_ty == floatTy  = ConPat floatDataCon  lit_ty [LitPat (mk_float lit)  floatPrimTy]
      | lit_ty == doubleTy = ConPat doubleDataCon lit_ty [LitPat (mk_double lit) doublePrimTy]

		-- Convert the literal pattern "" to the constructor pattern [].
      | null_str_lit lit       = ConPat nilDataCon    lit_ty [] 

      | otherwise	       = NPat lit lit_ty hsexpr

    mk_int    (HsInt i)      = HsIntPrim i
    mk_int    l@(HsLitLit s) = l

    mk_char   (HsChar c)     = HsCharPrim c
    mk_char   l@(HsLitLit s) = l

    mk_word   l@(HsLitLit s) = l

    mk_addr   l@(HsLitLit s) = l

    mk_float  (HsInt i)      = HsFloatPrim (fromInteger i)
    mk_float  (HsFrac f)     = HsFloatPrim f
    mk_float  l@(HsLitLit s) = l

    mk_double (HsInt i)      = HsDoublePrim (fromInteger i)
    mk_double (HsFrac f)     = HsDoublePrim f
    mk_double l@(HsLitLit s) = l

    null_str_lit (HsString s) = _NULL_ s
    null_str_lit other_lit    = False

simplify_pat (NPlusKPat	id hslit ty hsexpr1 hsexpr2) = --NPlusKPat id hslit ty hsexpr1 hsexpr2 
     WildPat ty
   where ty = panic "Check.simplify_pat: Never used"

simplify_pat (DictPat dicts methods) = 
    case num_of_d_and_ms of
       0 -> simplify_pat (TuplePat []) 
       1 -> simplify_pat (head dict_and_method_pats) 
       _ -> simplify_pat (TuplePat dict_and_method_pats)
    where
       num_of_d_and_ms	 = length dicts + length methods
       dict_and_method_pats = map VarPat (dicts ++ methods)

\end{code}

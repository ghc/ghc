%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[MagicUFs]{Magic unfoldings that the simplifier knows about}

\begin{code}
#include "HsVersions.h"

module MagicUFs (
        MagicUnfoldingFun,  -- absolutely abstract

        mkMagicUnfoldingFun,
        applyMagicUnfoldingFun,
        
        CoreArg, PlainCoreArg(..), CoreAtom, PlainCoreAtom(..),
        CoreExpr, PlainCoreExpr(..), Id, Maybe, SimplEnv,
	SplitUniqSupply, TickType, UniType,
	SmplM(..), SimplCount
    ) where

IMPORT_Trace            -- ToDo: not sure why this is being used

import AbsPrel          ( foldlId, foldrId, buildId,
                          nilDataCon, consDataCon, mkListTy, mkFunTy,
                          unpackCStringAppendId
                        )
import AbsUniType       ( splitTypeWithDictsAsArgs, TyVarTemplate )
import BasicLit         ( BasicLit(..) )
import CmdLineOpts      ( SimplifierSwitch(..), switchIsOn, SwitchResult )
import Id
import IdInfo
import Maybes           ( Maybe(..), maybeToBool )
import Outputable
import PlainCore
import Pretty
import SimplEnv
import SimplMonad
import TaggedCore
import Util
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Types, etc., for magic-unfolding functions}
%*                                                                      *
%************************************************************************

\begin{code}
data MagicUnfoldingFun
  = MUF ( SimplEnv              -- state of play in simplifier...
                                -- (note: we can get simplifier switches
                                -- from the SimplEnv)
        -> [PlainCoreArg]       -- arguments
        -> SmplM (Maybe PlainCoreExpr))
                                -- Just result, or Nothing
\end{code}

Give us a string tag, we'll give you back the corresponding MUF.
\begin{code}
mkMagicUnfoldingFun :: FAST_STRING -> MagicUnfoldingFun

mkMagicUnfoldingFun tag
  = assoc ("mkMagicUnfoldingFun:"  ++ _UNPK_ tag) magic_UFs_table tag
\end{code}

Give us an MUF and stuff to apply it to, and we'll give you back the
answer.
\begin{code}
applyMagicUnfoldingFun
        :: MagicUnfoldingFun
        -> SimplEnv
        -> [PlainCoreArg]
        -> SmplM (Maybe PlainCoreExpr)

applyMagicUnfoldingFun (MUF fun) env args = fun env args
\end{code}

%************************************************************************
%*                                                                      *
\subsection{The table of actual magic unfoldings}
%*                                                                      *
%************************************************************************

\begin{code}
magic_UFs_table :: [(FAST_STRING, MagicUnfoldingFun)]

magic_UFs_table
  = [(SLIT("build"),  MUF build_fun),
     (SLIT("foldl"),  MUF foldl_fun),
     (SLIT("foldr"),  MUF foldr_fun) ]
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{Unfolding function for @append@}
%*                                                                      *
%************************************************************************

\begin{code}
-- First build, the way we express our lists.

build_fun :: SimplEnv
          -> [PlainCoreArg]
          -> SmplM (Maybe PlainCoreExpr)
build_fun env [TypeArg ty,ValArg (CoVarAtom e)] 
	| switchIsSet env SimplDoInlineFoldrBuild =
        let
                tyL     = mkListTy ty
                ourCons = mkCoTyApp (CoVar consDataCon) ty
                ourNil  = mkCoTyApp (CoVar nilDataCon) ty
        in
        newIds  [ ty `mkFunTy` (tyL `mkFunTy` tyL),
                  tyL ]                 `thenSmpl` \ [c,n] ->
        returnSmpl(Just (CoLet (CoNonRec c ourCons)
                        (CoLet (CoNonRec n ourNil)
                         (CoApp (CoApp (mkCoTyApp (CoVar e) tyL) (CoVarAtom c)) (CoVarAtom n)))))
-- ToDo: add `build' without an argument instance.
-- This is strange, because of g's type.
build_fun env _ = 
	ASSERT (not (switchIsSet env SimplDoInlineFoldrBuild))
	returnSmpl Nothing

-- Now foldr, the way we consume lists.

foldr_fun :: SimplEnv
          -> [PlainCoreArg]
          -> SmplM (Maybe PlainCoreExpr)
{-
foldr_fun env _ 
  | trace "HEHJDHF!" False = error "NEVER" 
-}
foldr_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:ValArg arg_z:rest_args)
  | isConsFun env arg_k && isNilForm env arg_z
  =     -- foldr (:) [] ==> id
	-- this transformation is *always* benificial
	-- cf.  foldr (:) [] (build g) == g (:) []
	-- with foldr (:) [] (build g) == build g
	-- after unfolding build, they are the same thing.
     tick FoldrConsNil			`thenSmpl_` 
     newId (mkListTy ty1) 		`thenSmpl` \ x ->
     returnSmpl({-trace "foldr (:) []"-} (Just (applyToArgs (CoLam [x] (CoVar x)) rest_args)))

foldr_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:ValArg arg_z:ValArg arg_list:rest_args)
 | do_fb_red && isNilForm env arg_list
  =     -- foldr f z [] = z
	-- again another short cut, helps with unroling of constant lists
    tick Foldr_Nil	`thenSmpl_`
    returnSmpl (Just (atomToExpr arg_z))

  | do_fb_red && arg_list_isBuildForm 
  =     -- foldr k z (build g) ==> g k z
	-- this next line *is* the foldr/build rule proper.
    tick FoldrBuild	`thenSmpl_`
    returnSmpl (Just (applyToArgs (CoVar g) (TypeArg ty2:ValArg arg_k:ValArg arg_z:rest_args)))

  | do_fb_red && arg_list_isAppendForm 
  =     -- foldr k z (foldr (:) ys xs) <args> ==> foldr k (foldr k z ys) xs <args>
	-- this unfolds foldr one into foldr
    tick FoldrFoldr	`thenSmpl_`
    newId ty2	`thenSmpl` \ other_foldr ->
	let
	    inner_foldr = applyToArgs (CoVar foldrId) 
			[TypeArg ty1,TypeArg ty2,
			 ValArg arg_k,ValArg arg_z,ValArg ys]
	    outer_foldr = applyToArgs (CoVar foldrId) 
			([TypeArg ty1,TypeArg ty2,
			 ValArg arg_k,ValArg (CoVarAtom other_foldr),ValArg xs]
			++ rest_args)
	in returnSmpl (Just (CoLet (CoNonRec other_foldr inner_foldr) outer_foldr))

 | do_fb_red && arg_list_isListForm
 =      -- foldr k z (a:b:c:rest) = 
	--	(\ f -> f a (f b (f c (foldr f z rest)))) k rest_args
	-- NB: 'k' is used just one by foldr, but 'f' is used many
	-- times inside the list structure. This means that
	-- 'f' needs to be inside a lambda, to make sure the simplifier
	-- realises this.
	-- 
	-- The structure of	
	--	 f a (f b (f c (foldr f z rest)))
	-- in core becomes:
	--	let ele_1 = foldr f z rest
	--	    ele_2 = f c ele_1
	--	    ele_3 = f b ele_2
	--	in f a ele_3
	--
  tick Foldr_List	`thenSmpl_`
  newIds ( 
		ty1 `mkFunTy` (ty2 `mkFunTy` ty2) :
		take (length the_list) (repeat ty2)
	)			`thenSmpl` \ (f_id:ele_id1:ele_ids) ->
  let
	fst_bind = CoNonRec 
			ele_id1 
			(applyToArgs (CoVar foldrId) 
				[TypeArg ty1,TypeArg ty2,
				 ValArg (CoVarAtom f_id),
				 ValArg arg_z,
				 ValArg the_tl])
	--ToDo: look for a zipWith that checks for the same length of a 3 lists
	rest_binds = zipWith3 
			 (\ e v e' -> CoNonRec e (mkRhs v e'))
			 ele_ids
			 (reverse (tail the_list))
			 (init (ele_id1:ele_ids))
	mkRhs v e = CoApp (CoApp (CoVar f_id) v) (CoVarAtom e)
	core_list = foldr
			CoLet 
			(mkRhs (head the_list) (last (ele_id1:ele_ids)))
			(fst_bind:rest_binds)
  in
 	returnSmpl (Just (applyToArgs (CoLam [f_id] core_list)
				      (ValArg arg_k:rest_args)))

 where
   do_fb_red		= switchIsSet env SimplDoFoldrBuild

   arg_list_isBuildForm = maybeToBool buildForm
   buildForm            = getBuildForm env arg_list
   (Just g)             = buildForm

   arg_list_isListForm      = maybeToBool listForm
   listForm                 = getListForm env arg_list
   (Just (the_list,the_tl)) = listForm

   arg_list_isAppendForm = maybeToBool appendForm
   appendForm            = getAppendForm env arg_list
   (Just (xs,ys))        = appendForm

foldr_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:rest_args)
{- OLD:
  | doing_inlining && isConsFun env arg_k 
  =    -- foldr (:) z xs = xs ++ z              
     tick FoldrCons	`thenSmpl_`
     newIds [ty2,mkListTy ty1] `thenSmpl` \ [z,x] ->
     returnSmpl (Just (applyToArgs 
                        (CoLam [z,x] (applyToArgs 
                                        (CoVar appendId) [
                                                TypeArg ty1,
                                                ValArg (CoVarAtom x),
                                                ValArg (CoVarAtom z)]))
                        rest_args))
-}
  | doing_inlining && (isInterestingArg env arg_k  
		       || isConsFun env arg_k)
  =   -- foldr k args =                         
      --        (\ f z xs ->
      --          letrec                                 
      --             h x = case x of
      --		    [] -> z
      --          	    (a:b) -> f a (h b)
      --          in
      --             h xs) k args
      --
     tick FoldrInline		`thenSmpl_`
     newIds [
                ty1,                    -- a :: t1
                mkListTy ty1,           -- b :: [t1]
                ty2,                    -- v :: t2
                mkListTy ty1,           -- x :: t1
                mkListTy ty1 `mkFunTy` ty2,
                                        -- h :: [t1] -> t2
                ty1 `mkFunTy` (ty2 `mkFunTy` ty2),
                                        -- f
                ty2,                    -- z
                mkListTy ty1            -- xs
                        ] `thenSmpl` \ [a,b,v,x,h,f,z,xs] ->
           let
             h_rhs = (CoLam [x] (CoCase (CoVar x)
                      (CoAlgAlts
                          [(nilDataCon,[],atomToExpr (CoVarAtom z)),
                           (consDataCon,[a,b],body)]
                       CoNoDefault)))
             body = CoLet (CoNonRec v (CoApp (CoVar h) (CoVarAtom b)))
                          (CoApp (CoApp (atomToExpr (CoVarAtom f))
                                                  (CoVarAtom a))
                                                    (CoVarAtom v))
           in
             returnSmpl (Just 
                     (applyToArgs
                         (CoLam [f,z,xs]
                          (CoLet (CoRec [(h,h_rhs)]) 
                                 (CoApp (CoVar h) (CoVarAtom xs))))
                     (ValArg arg_k:rest_args)))
   where
	doing_inlining = switchIsSet env SimplDoInlineFoldrBuild 
foldr_fun _ _ = returnSmpl Nothing

isConsFun :: SimplEnv -> PlainCoreAtom -> Bool
isConsFun env (CoVarAtom v) = 
    case lookupUnfolding env v of
        GeneralForm _ _ (CoLam [(x,_),(y,_)] 
                        (CoCon con tys [CoVarAtom x',CoVarAtom y'])) _
                        | con == consDataCon && x==x' && y==y'
          -> ASSERT ( length tys == 1 ) True
        _ -> False
isConsFun env _ = False

isNilForm :: SimplEnv -> PlainCoreAtom -> Bool
isNilForm env (CoVarAtom v) = 
    case lookupUnfolding env v of
        GeneralForm _ _ (CoTyApp (CoVar id) _) _
          | id == nilDataCon -> True
        ConstructorForm id _ _
          | id == nilDataCon   -> True
        LiteralForm (NoRepStr s) | _NULL_ s -> True
        _ -> False
isNilForm env _ = False

getBuildForm :: SimplEnv -> PlainCoreAtom -> Maybe Id
getBuildForm env (CoVarAtom v) = 
    case lookupUnfolding env v of
        GeneralForm False _ _ _ -> Nothing	-- not allowed to inline :-(
        GeneralForm _ _ (CoApp (CoTyApp (CoVar bld) _) (CoVarAtom g)) _
          | bld == buildId -> Just g
        _ -> Nothing
getBuildForm env _ = Nothing

getAppendForm :: SimplEnv -> PlainCoreAtom -> Maybe (CoreAtom Id,CoreAtom Id)
getAppendForm env (CoVarAtom v) = 
    case lookupUnfolding env v of
        GeneralForm False _ _ _ -> Nothing	-- not allowed to inline :-(
        GeneralForm _ _ (CoApp (CoApp (CoApp (CoTyApp (CoTyApp (CoVar fld) _) _) con) ys) xs) _
          | fld == foldrId && isConsFun env con -> Just (xs,ys)
        _ -> Nothing
getAppendForm env _ = Nothing

--
-- this gets a list of the form a : b : c : d and returns ([a,b,c],d)
-- it natuarally follows that [a,b,c] => ([a,b,c],e), where e = []
--

getListForm
	:: SimplEnv 
	-> PlainCoreAtom 
	-> Maybe ([PlainCoreAtom],PlainCoreAtom)
getListForm env (CoVarAtom v) = 
    case lookupUnfolding env v of
       ConstructorForm id _ [head,tail]
          | id == consDataCon -> 
		case getListForm env tail of
 		   Nothing -> Just ([head],tail)
		   Just (lst,new_tail) -> Just (head:lst,new_tail)
       _ -> Nothing
getListForm env _ = Nothing

isInterestingArg :: SimplEnv -> PlainCoreAtom -> Bool
isInterestingArg env (CoVarAtom v) = 
    case lookupUnfolding env v of
       GeneralForm False _ _ UnfoldNever -> False
       GeneralForm _ _ exp guide -> True
       _ -> False
isInterestingArg env _ = False

foldl_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:ValArg arg_z:ValArg arg_list:rest_args)
 | do_fb_red && isNilForm env arg_list
  =     -- foldl f z [] = z
	-- again another short cut, helps with unroling of constant lists
    tick Foldr_Nil	`thenSmpl_`
    returnSmpl (Just (atomToExpr arg_z))

  | do_fb_red && arg_list_isBuildForm 
  =     -- foldl t1 t2 k z (build t3 g) ==> 
	--		   let c {- INLINE -} = \ b g' a -> g' (f a b) 
	--		       n {- INLINE -} = \ a -> a
	--		   in g t1 c n z
	-- this next line *is* the foldr/build rule proper.
    tick FoldrBuild	`thenSmpl_`
	-- c :: t2 -> (t1 -> t1) -> t1 -> t1
	-- n :: t1 -> t1
    newIds [
	{- pre_c -}	ty2 `mkFunTy` ((ty1 `mkFunTy` ty1) `mkFunTy` (ty1 `mkFunTy` ty1)),
	{- pre_n -}	ty1 `mkFunTy` ty1,
	{- b -}		ty2,
	{- g' -}	ty1 `mkFunTy` ty1, 
	{- a -}		ty1,
	{- a' -}	ty1,	
	{- t -}		ty1
	]		`thenSmpl` \ [pre_c,
				      pre_n,
				      b,
				      g',
				      a,
				      a',
				      t] ->

    let
	c     = addIdUnfolding pre_c (iWantToBeINLINEd UnfoldAlways)
	c_rhs = CoLam [b,g',a]
	         (CoLet (CoNonRec t (CoApp (CoApp (atomToExpr arg_k) (CoVarAtom a)) (CoVarAtom b)))
		         (CoApp (CoVar g') (CoVarAtom t)))
	n     = addIdUnfolding pre_n (iWantToBeINLINEd UnfoldAlways) 
	n_rhs = CoLam [a'] (CoVar a')
    in
    returnSmpl (Just (CoLet (CoNonRec c c_rhs) (CoLet (CoNonRec n n_rhs) 
		  (applyToArgs (CoVar g) 
		      (TypeArg (ty1 `mkFunTy` ty1):ValArg (CoVarAtom c):ValArg (CoVarAtom n)
				:ValArg arg_z:rest_args)))))


  | do_fb_red && arg_list_isAppendForm 
  =     -- foldl k z (foldr (:) ys xs) <args> ==> foldl k (foldl k z xs) ys <args>
	-- be caseful with for order of xs / ys
    tick FoldrFoldr	`thenSmpl_`
    newId ty1	`thenSmpl` \ other_foldl ->
	let
	    inner_foldl = applyToArgs (CoVar foldlId) 
			[TypeArg ty1,TypeArg ty2,
			 ValArg arg_k,ValArg arg_z,ValArg xs]
	    outer_foldl = applyToArgs (CoVar foldlId) 
			([TypeArg ty1,TypeArg ty2,
			 ValArg arg_k,ValArg (CoVarAtom other_foldl),ValArg ys]
			++ rest_args)
	in returnSmpl (Just (CoLet (CoNonRec other_foldl inner_foldl) outer_foldl))

 | do_fb_red && arg_list_isListForm
 =      -- foldl k z (a:b:c:rest) = 
	--	(\ f -> foldl f (f (f (f z a) b) c) rest) k rest_args
	-- NB: 'k' is used just one by foldr, but 'f' is used many
	-- times inside the list structure. This means that
	-- 'f' needs to be inside a lambda, to make sure the simplifier
	-- realises this.
	-- 
	-- The structure of	
	-- 	 foldl f (f (f (f z a) b) c) rest
	--	 f a (f b (f c (foldr f z rest)))
	-- in core becomes:
	--	let ele_1 = f z a
	--	    ele_2 = f ele_1 b
	--	    ele_3 = f ele_2 c
	--	in foldl f ele_3 rest
	--
  tick Foldr_List	`thenSmpl_`
  newIds ( 
		ty1 `mkFunTy` (ty2 `mkFunTy` ty1) :
		take (length the_list) (repeat ty1)
	)			`thenSmpl` \ (f_id:ele_ids) ->
  let
	--ToDo: look for a zipWith that checks for the same length of a 3 lists
	rest_binds = zipWith3 
			 (\ e v e' -> CoNonRec e (mkRhs v e'))
			 ele_ids				-- :: [Id]
			 the_list				-- :: [PlainCoreAtom]
			 (init (arg_z:map CoVarAtom ele_ids))	-- :: [PlainCoreAtom]
	mkRhs v e = CoApp (CoApp (CoVar f_id) e) v

	last_bind = applyToArgs (CoVar foldlId) 
				[TypeArg ty1,TypeArg ty2,
				 ValArg (CoVarAtom f_id),
				 ValArg (CoVarAtom (last ele_ids)),
				 ValArg the_tl]
	core_list = foldr
			CoLet 
			last_bind
			rest_binds
  in
 	returnSmpl (Just (applyToArgs (CoLam [f_id] core_list)
				      (ValArg arg_k:rest_args)))

 where
   do_fb_red		= switchIsSet env SimplDoFoldrBuild

   arg_list_isBuildForm = maybeToBool buildForm
   buildForm            = getBuildForm env arg_list
   (Just g)             = buildForm

   arg_list_isListForm      = maybeToBool listForm
   listForm                 = getListForm env arg_list
   (Just (the_list,the_tl)) = listForm

   arg_list_isAppendForm = maybeToBool appendForm
   appendForm            = getAppendForm env arg_list
   (Just (xs,ys))        = appendForm

foldl_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:rest_args)
  | doing_inlining && (isInterestingArg env arg_k  
		       || isConsFun env arg_k)
  =   -- foldl k args =                         
      --        (\ f z xs ->
      --          letrec                                 
      --             h x r = case x of
      --		      []    -> r
      --          	      (a:b) -> h b (f r a)
      --          in
      --             h xs z) k args
      --
     tick FoldrInline				`thenSmpl_`
     newIds [
                ty2,                    -- a :: t1
                mkListTy ty2,           -- b :: [t1]
                ty1,                    -- v :: t2
                mkListTy ty2,           -- x :: t1
                mkListTy ty2 `mkFunTy` (ty1 `mkFunTy` ty1),
                                        -- h :: [t2] -> t1 -> t1
                ty1 `mkFunTy` (ty2 `mkFunTy` ty1),
                                        -- f
                ty1,                    -- z
                mkListTy ty2,           -- xs
		ty1			-- r
                        ] `thenSmpl` \ [a,b,v,x,h,f,z,xs,r] ->
           let
             h_rhs = (CoLam [x,r] (CoCase (CoVar x)
                      (CoAlgAlts
                          [(nilDataCon,[],atomToExpr (CoVarAtom r)),
                           (consDataCon,[a,b],body)]
                       CoNoDefault)))
             body = CoLet (CoNonRec v (CoApp (CoApp (CoVar f) (CoVarAtom r))
							      (CoVarAtom a)))
                          (CoApp (CoApp (atomToExpr (CoVarAtom h))
                                                  (CoVarAtom b))
                                                    (CoVarAtom v))
           in
             returnSmpl (Just 
                     (applyToArgs
                         (CoLam [f,z,xs]
                          (CoLet (CoRec [(h,h_rhs)]) 
                                 (CoApp (CoApp (CoVar h) (CoVarAtom xs)) 
							 (CoVarAtom z))))
                     (ValArg arg_k:rest_args)))
   where
	doing_inlining = switchIsSet env SimplDoInlineFoldrBuild 

foldl_fun env _ = returnSmpl Nothing
\end{code}


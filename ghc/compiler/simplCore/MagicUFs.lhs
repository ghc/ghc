%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[MagicUFs]{Magic unfoldings that the simplifier knows about}

\begin{code}
#include "HsVersions.h"

module MagicUFs (
	MagicUnfoldingFun,  -- absolutely abstract

	mkMagicUnfoldingFun,
	applyMagicUnfoldingFun
    ) where

IMP_Ubiq(){-uitous-}
IMPORT_DELOOPER(IdLoop)		-- paranoia checking

import CoreSyn
import SimplEnv		( SimplEnv )
import SimplMonad	( SYN_IE(SmplM), SimplCount )
import Type		( mkFunTys )
import TysWiredIn	( mkListTy )
import Unique		( Unique{-instances-} )
import Util		( assoc, zipWith3Equal, nOfThem, panic )
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
	-> [CoreArg]       -- arguments
	-> SmplM (Maybe CoreExpr))
				-- Just result, or Nothing
\end{code}

Give us a value's @Unique@, we'll give you back the corresponding MUF.
\begin{code}
mkMagicUnfoldingFun :: Unique -> MagicUnfoldingFun

mkMagicUnfoldingFun tag
  = assoc "mkMagicUnfoldingFun" magic_UFs_table tag

magic_UFs_table = panic "MagicUFs.magic_UFs_table:ToDo"
\end{code}

Give us an MUF and stuff to apply it to, and we'll give you back the
answer.
\begin{code}
applyMagicUnfoldingFun
	:: MagicUnfoldingFun
	-> SimplEnv
	-> [CoreArg]
	-> SmplM (Maybe CoreExpr)

applyMagicUnfoldingFun (MUF fun) env args = fun env args
\end{code}

%************************************************************************
%*                                                                      *
\subsection{The table of actual magic unfoldings}
%*                                                                      *
%************************************************************************

\begin{code}
{- LATER:

magic_UFs_table :: [(FAST_STRING, MagicUnfoldingFun)]

magic_UFs_table
  = [(SLIT("augment"), 		MUF augment_fun),
     (SLIT("build"),   		MUF build_fun),
     (SLIT("foldl"),   		MUF foldl_fun),
     (SLIT("foldr"),   		MUF foldr_fun),
     (SLIT("unpackFoldrPS__"),  MUF unpack_foldr_fun),
     (SLIT("unpackAppendPS__"),	MUF unpack_append_fun)]
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{Unfolding function for @append@}
%*                                                                      *
%************************************************************************

\begin{code}
-- First build, the way we express our lists.

build_fun :: SimplEnv
	  -> [CoreArg]
	  -> SmplM (Maybe CoreExpr)
build_fun env [TypeArg ty,ValArg (VarArg e)]
	| switchIsSet env SimplDoInlineFoldrBuild =
	let
		tyL     = mkListTy ty
		ourCons = CoTyApp (Var consDataCon) ty
		ourNil  = CoTyApp (Var nilDataCon) ty
	in
	newIds  [ mkFunTys [ty, tyL] tyL, tyL ]	`thenSmpl` \ [c,n] ->
	returnSmpl(Just (Let (NonRec c ourCons)
			(Let (NonRec n ourNil)
			 (App (App (CoTyApp (Var e) tyL) (VarArg c)) (VarArg n)))))
-- ToDo: add `build' without an argument instance.
-- This is strange, because of g's type.
build_fun env _ =
	ASSERT (not (switchIsSet env SimplDoInlineFoldrBuild))
	returnSmpl Nothing
\end{code}

\begin{code}
augment_fun :: SimplEnv
	  -> [CoreArg]
	  -> SmplM (Maybe CoreExpr)

augment_fun env [TypeArg ty,ValArg (VarArg e),ValArg nil]
	| switchIsSet env SimplDoInlineFoldrBuild =
	let
		tyL     = mkListTy ty
		ourCons = CoTyApp (Var consDataCon) ty
	in
	newId  (mkFunTys [ty, tyL] tyL)    `thenSmpl` \ c ->
	returnSmpl (Just (Let (NonRec c ourCons)
			 (App (App (CoTyApp (Var e) tyL) (VarArg c)) nil)))
-- ToDo: add `build' without an argument instance.
-- This is strange, because of g's type.
augment_fun env _ =
	ASSERT (not (switchIsSet env SimplDoInlineFoldrBuild))
	returnSmpl Nothing
\end{code}

Now foldr, the way we consume lists.

\begin{code}
foldr_fun :: SimplEnv
	  -> [CoreArg]
	  -> SmplM (Maybe CoreExpr)

foldr_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:ValArg arg_z:rest_args)
  | do_fb_red && isConsFun env arg_k && isNilForm env arg_z
  =     -- foldr (:) [] ==> id
	-- this transformation is *always* benificial
	-- cf.  foldr (:) [] (build g) == g (:) []
	-- with foldr (:) [] (build g) == build g
	-- after unfolding build, they are the same thing.
     tick Foldr_Cons_Nil		`thenSmpl_`
     newId (mkListTy ty1) 		`thenSmpl` \ x ->
     returnSmpl({-trace "foldr (:) []"-} (Just (mkGenApp (Lam x (Var x)) rest_args)))
 where
   do_fb_red		= switchIsSet env SimplDoFoldrBuild

foldr_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:ValArg arg_z:ValArg arg_list:rest_args)
 | do_fb_red && isNilForm env arg_list
  =     -- foldr f z [] = z
	-- again another short cut, helps with unroling of constant lists
    tick Foldr_Nil	`thenSmpl_`
    returnSmpl (Just (argToExpr arg_z))

  | do_fb_red && arg_list_isBuildForm
  =     -- foldr k z (build g) ==> g k z
	-- this next line *is* the foldr/build rule proper.
    tick FoldrBuild	`thenSmpl_`
    returnSmpl (Just (mkGenApp (Var g) (TypeArg ty2:ValArg arg_k:ValArg arg_z:rest_args)))

  | do_fb_red && arg_list_isAugmentForm
  =     -- foldr k z (augment g h) ==> let v = foldr k z h in g k v
	-- this next line *is* the foldr/augment rule proper.
    tick FoldrAugment	`thenSmpl_`
    newId ty2				`thenSmpl` \ v ->
    returnSmpl (Just
		(Let (NonRec v (mkGenApp (Var foldrId)
					[TypeArg ty1,TypeArg ty2,
					 ValArg arg_k,
					 ValArg arg_z,
					 ValArg h]))
		(mkGenApp (Var g') (TypeArg ty2:ValArg arg_k:ValArg (VarArg v):rest_args))))

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
		mkFunTys [ty1, ty2] ty2 :
		nOfThem (length the_list) ty2
	)			`thenSmpl` \ (f_id:ele_id1:ele_ids) ->
  let
	fst_bind = NonRec
			ele_id1
			(mkGenApp (Var foldrId)
				[TypeArg ty1,TypeArg ty2,
				 ValArg (VarArg f_id),
				 ValArg arg_z,
				 ValArg the_tl])
	rest_binds = zipWith3Equal "Foldr:rest_binds"
			 (\ e v e' -> NonRec e (mkRhs v e'))
			 ele_ids
			 (reverse (tail the_list))
			 (init (ele_id1:ele_ids))
	mkRhs v e = App (App (Var f_id) v) (VarArg e)
	core_list = foldr
			Let
			(mkRhs (head the_list) (last (ele_id1:ele_ids)))
			(fst_bind:rest_binds)
  in
 	returnSmpl (Just (mkGenApp (Lam f_id core_list)
				      (ValArg arg_k:rest_args)))


	--

 | do_fb_red && arg_list_isStringForm	-- ok, its a string!
	-- foldr f z "foo" => unpackFoldrPS__ f z "foo"#
   = tick Str_FoldrStr				`thenSmpl_`
     returnSmpl (Just (mkGenApp (Var unpackCStringFoldrId)
				(TypeArg ty2:
				 ValArg (LitArg (MachStr str_val)):
				 ValArg arg_k:
				 ValArg arg_z:
				 rest_args)))
 where
   do_fb_red		= switchIsSet env SimplDoFoldrBuild

   arg_list_isStringForm = maybeToBool stringForm
   stringForm		 = getStringForm env arg_list
   (Just str_val)	 = stringForm

   arg_list_isBuildForm = maybeToBool buildForm
   buildForm            = getBuildForm env arg_list
   (Just g)             = buildForm

   arg_list_isAugmentForm  = maybeToBool augmentForm
   augmentForm             = getAugmentForm env arg_list
   (Just (g',h))           = augmentForm

   arg_list_isListForm      = maybeToBool listForm
   listForm                 = getListForm env arg_list
   (Just (the_list,the_tl)) = listForm
{-
   arg_list_isAppendForm = maybeToBool appendForm
   appendForm            = getAppendForm env arg_list
   (Just (xs,ys))        = appendForm
-}

foldr_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:rest_args)
  | doing_inlining && isConsFun env arg_k && not dont_fold_back_append
  =    -- foldr (:) z xs = xs ++ z
     tick Foldr_Cons	`thenSmpl_`
     newIds [ty2,mkListTy ty1] `thenSmpl` \ [z,x] ->
     returnSmpl (Just (mkGenApp
			(Lam z (Lam x (mkGenApp
					(Var appendId) [
						TypeArg ty1,
						ValArg (VarArg x),
						ValArg (VarArg z)])))
			rest_args))
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
--     tick FoldrInline		`thenSmpl_`
     newIds [
		ty1,                    -- a :: t1
		mkListTy ty1,           -- b :: [t1]
		ty2,                    -- v :: t2
		mkListTy ty1,           -- x :: t1
		mkFunTys [mkListTy ty1] ty2,
					-- h :: [t1] -> t2
		mkFunTys [ty1, ty2] ty2,
					-- f
		ty2,                    -- z
		mkListTy ty1            -- xs
			] `thenSmpl` \ [a,b,v,x,h,f,z,xs] ->
	   let
	     h_rhs = (Lam x (Case (Var x)
		      (AlgAlts
			  [(nilDataCon,[],argToExpr (VarArg z)),
			   (consDataCon,[a,b],body)]
		       NoDefault)))
	     body = Let (NonRec v (App (Var h) (VarArg b)))
			  (App (App (argToExpr (VarArg f))
						  (VarArg a))
						    (VarArg v))
	   in
	     returnSmpl (Just
		     (mkGenApp
			 (Lam f (Lam z (Lam xs
			  (Let (Rec [(h,h_rhs)])
				 (App (Var h) (VarArg xs))))))
		     (ValArg arg_k:rest_args)))
   where
	doing_inlining = switchIsSet env SimplDoInlineFoldrBuild
	dont_fold_back_append = switchIsSet env SimplDontFoldBackAppend
foldr_fun _ _ = returnSmpl Nothing

isConsFun :: SimplEnv -> CoreArg -> Bool
isConsFun env (VarArg v)
  = case lookupUnfolding env v of
	GenForm _ (Lam (x,_) (Lam (y,_) (Con con tys [VarArg x',VarArg y']))) _
	  | con == consDataCon && x==x' && y==y'
	  -> ASSERT ( length tys == 1 ) True
	_ -> False
isConsFun env _ = False

isNilForm :: SimplEnv -> CoreArg -> Bool
isNilForm env (VarArg v)
  = case lookupUnfolding env v of
	GenForm _ (CoTyApp (Var id) _) _ | id == nilDataCon -> True
	GenForm _ (Lit (NoRepStr s))   _ | _NULL_ s	      -> True
	_ 						      -> False
isNilForm env _ = False

getBuildForm :: SimplEnv -> CoreArg -> Maybe Id
getBuildForm env (VarArg v)
  = case lookupUnfolding env v of
	GenForm False _ _ _ -> Nothing
					-- not allowed to inline :-(
	GenForm _ (App (CoTyApp (Var bld) _) (VarArg g)) _
	  | bld == buildId -> Just g
	GenForm _ (App (App (CoTyApp (Var bld) _)
					(VarArg g)) h) _
	  | bld == augmentId && isNilForm env h  -> Just g
	_ -> Nothing
getBuildForm env _ = Nothing



getAugmentForm :: SimplEnv -> CoreArg -> Maybe (Id,CoreArg)
getAugmentForm env (VarArg v)
  = case lookupUnfolding env v of
	GenForm False _ _ _ -> Nothing
				-- not allowed to inline :-(
	GenForm _ (App (App (CoTyApp (Var bld) _)
						(VarArg g)) h) _
	  | bld == augmentId -> Just (g,h)
	_ -> Nothing
getAugmentForm env _ = Nothing

getStringForm :: SimplEnv -> CoreArg -> Maybe FAST_STRING
getStringForm env (LitArg (NoRepStr str)) = Just str
getStringForm env _ = Nothing

{-
getAppendForm :: SimplEnv -> CoreArg -> Maybe (GenCoreAtom Id,GenCoreAtom Id)
getAppendForm env (VarArg v) =
    case lookupUnfolding env v of
	GenForm False _ _ _ -> Nothing	-- not allowed to inline :-(
	GenForm _ (App (App (App (CoTyApp (CoTyApp (Var fld) _) _) con) ys) xs) _
	  | fld == foldrId && isConsFun env con -> Just (xs,ys)
	_ -> Nothing
getAppendForm env _ = Nothing
-}

--
-- this gets a list of the form a : b : c : d and returns ([a,b,c],d)
-- it natuarally follows that [a,b,c] => ([a,b,c],e), where e = []
--

getListForm
	:: SimplEnv
	-> CoreArg
	-> Maybe ([CoreArg],CoreArg)
getListForm env (VarArg v)
  = case lookupUnfolding env v of
       GenForm _ (Con id [ty_arg,head,tail]) _
	  | id == consDataCon ->
		case getListForm env tail of
 		   Nothing -> Just ([head],tail)
		   Just (lst,new_tail) -> Just (head:lst,new_tail)
       _ -> Nothing
getListForm env _ = Nothing

isInterestingArg :: SimplEnv -> CoreArg -> Bool
isInterestingArg env (VarArg v)
  = case lookupUnfolding env v of
       GenForm False _ _ UnfoldNever -> False
       GenForm _ exp guide -> True
       _ -> False
isInterestingArg env _ = False

foldl_fun env (TypeArg ty1:TypeArg ty2:ValArg arg_k:ValArg arg_z:ValArg arg_list:rest_args)
 | do_fb_red && isNilForm env arg_list
  =     -- foldl f z [] = z
	-- again another short cut, helps with unroling of constant lists
    tick Foldl_Nil	`thenSmpl_`
    returnSmpl (Just (argToExpr arg_z))

  | do_fb_red && arg_list_isBuildForm
  =     -- foldl t1 t2 k z (build t3 g) ==>
	--		   let c {- INLINE -} = \ b g' a -> g' (f a b)
	--		       n {- INLINE -} = \ a -> a
	--		   in g t1 c n z
	-- this next line *is* the foldr/build rule proper.
    tick FoldlBuild	`thenSmpl_`
	-- c :: t2 -> (t1 -> t1) -> t1 -> t1
	-- n :: t1 -> t1
    newIds [
	{- pre_c -}	mkFunTys [ty2, mkFunTys [ty1] ty1, ty1]  ty1,
	{- pre_n -}	mkFunTys [ty1] ty1,
	{- b -}		ty2,
	{- g' -}	mkFunTys [ty1] ty1,
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
	c_rhs = Lam b (Lam g' (Lam a
		 (Let (NonRec t (App (App (argToExpr arg_k) (VarArg a)) (VarArg b)))
			 (App (Var g') (VarArg t)))))
	n     = addIdUnfolding pre_n (iWantToBeINLINEd UnfoldAlways)
	n_rhs = Lam a' (Var a')
    in
    returnSmpl (Just (Let (NonRec c c_rhs) (Let (NonRec n n_rhs)
		  (mkGenApp (Var g)
		      (TypeArg (mkFunTys [ty1] ty1):ValArg (VarArg c):ValArg (VarArg n)
				:ValArg arg_z:rest_args)))))

  | do_fb_red && arg_list_isAugmentForm
  =     -- foldl t1 t2 k z (augment t3 g h) ==>
	--		   let c {- INLINE -} = \ b g' a -> g' (f a b)
	--		       n {- INLINE -} = \ a -> a
	--		       r {- INLINE -} = foldr t2 (t1 -> t1) c n h
	--		   in g t1 c r z
	-- this next line *is* the foldr/build rule proper.
    tick FoldlAugment	`thenSmpl_`
	-- c :: t2 -> (t1 -> t1) -> t1 -> t1
	-- n :: t1 -> t1
    newIds [
	{- pre_c -}	mkFunTys [ty2, mkFunTys [ty1] ty1, ty1] ty1,
	{- pre_n -}	mkFunTys [ty1] ty1,
	{- pre_r -}	mkFunTys [ty1] ty1,
	{- b -}		ty2,
	{- g_ -}	mkFunTys [ty1] ty1,
	{- a -}		ty1,
	{- a' -}	ty1,
	{- t -}		ty1
	]		`thenSmpl` \ [pre_c,
				      pre_n,
				      pre_r,
				      b,
				      g_,
				      a,
				      a',
				      t] ->

    let
	c     = addIdUnfolding pre_c (iWantToBeINLINEd UnfoldAlways)
	c_rhs = Lam b (Lam g_ (Lam a
		 (Let (NonRec t (App (App (argToExpr arg_k) (VarArg a)) (VarArg b)))
			 (App (Var g_) (VarArg t)))))
	n     = addIdUnfolding pre_n (iWantToBeINLINEd UnfoldAlways)
	n_rhs = Lam a' (Var a')
	r     = addIdUnfolding pre_r (iWantToBeINLINEd UnfoldAlways)
	r_rhs = mkGenApp (Var foldrId)
					[TypeArg ty2,TypeArg (mkFunTys [ty1] ty1),
					 ValArg (VarArg c),
					 ValArg (VarArg n),
					 ValArg h]
    in
    returnSmpl (Just (Let (NonRec c c_rhs)
		     (Let (NonRec n n_rhs)
		     (Let (NonRec r r_rhs)
		  (mkGenApp (Var g')
		      (TypeArg (mkFunTys [ty1] ty1):ValArg (VarArg c):ValArg (VarArg r)
				:ValArg arg_z:rest_args))))))

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
  tick Foldl_List	`thenSmpl_`
  newIds (
		mkFunTys [ty1, ty2] ty1 :
		nOfThem (length the_list) ty1
	)			`thenSmpl` \ (f_id:ele_ids) ->
  let
	rest_binds = zipWith3Equal "foldl:rest_binds"
			 (\ e v e' -> NonRec e (mkRhs v e'))
			 ele_ids				-- :: [Id]
			 the_list				-- :: [CoreArg]
			 (init (arg_z:map VarArg ele_ids))	-- :: [CoreArg]
	mkRhs v e = App (App (Var f_id) e) v

	last_bind = mkGenApp (Var foldlId)
				[TypeArg ty1,TypeArg ty2,
				 ValArg (VarArg f_id),
				 ValArg (VarArg (last ele_ids)),
				 ValArg the_tl]
	core_list = foldr
			Let
			last_bind
			rest_binds
  in
 	returnSmpl (Just (mkGenApp (Lam f_id core_list)
				      (ValArg arg_k:rest_args)))

 where
   do_fb_red		= switchIsSet env SimplDoFoldrBuild

   arg_list_isAugmentForm  = maybeToBool augmentForm
   augmentForm             = getAugmentForm env arg_list
   (Just (g',h))           = augmentForm

   arg_list_isBuildForm = maybeToBool buildForm
   buildForm            = getBuildForm env arg_list
   (Just g)             = buildForm

   arg_list_isListForm      = maybeToBool listForm
   listForm                 = getListForm env arg_list
   (Just (the_list,the_tl)) = listForm

{-
   arg_list_isAppendForm = maybeToBool appendForm
   appendForm            = getAppendForm env arg_list
   (Just (xs,ys))        = appendForm
-}

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
--     tick FoldrInline				`thenSmpl_`
     newIds [
		ty2,                    -- a :: t1
		mkListTy ty2,           -- b :: [t1]
		ty1,                    -- v :: t2
		mkListTy ty2,           -- x :: t1
		mkFunTys [mkListTy ty2, ty1] ty1,
					-- h :: [t2] -> t1 -> t1
		mkFunTys [ty1, ty2] ty1,
					-- f
		ty1,                    -- z
		mkListTy ty2,           -- xs
		ty1			-- r
			] `thenSmpl` \ [a,b,v,x,h,f,z,xs,r] ->
	   let
	     h_rhs = (Lam x (Lam r (Case (Var x))
		      (AlgAlts
			  [(nilDataCon,[],argToExpr (VarArg r)),
			   (consDataCon,[a,b],body)]
		       NoDefault)))
	     body = Let (NonRec v (App (App (Var f) (VarArg r))
							      (VarArg a)))
			  (App (App (argToExpr (VarArg h))
						  (VarArg b))
						    (VarArg v))
	   in
	     returnSmpl (Just
		     (mkGenApp
			 (Lam f (Lam z (Lam xs
			  (Let (Rec [(h,h_rhs)])
				 (App (App (Var h) (VarArg xs))
							 (VarArg z))))))
		     (ValArg arg_k:rest_args)))
   where
	doing_inlining = switchIsSet env SimplDoInlineFoldrBuild

foldl_fun env _ = returnSmpl Nothing
\end{code}


\begin{code}
--
--  Foldr unpackFoldr "str"# (:) stuff ==> unpackAppend "str"#
--
unpack_foldr_fun env [TypeArg ty,ValArg str,ValArg arg_k,ValArg arg_z]
   | switchIsSet env SimplDoFoldrBuild && isConsFun env arg_k
   = tick Str_UnpackCons		`thenSmpl_`
     returnSmpl (Just (mkGenApp (Var unpackCStringAppendId)
				[ValArg str,
				 ValArg arg_z]))
unpack_foldr_fun env _ = returnSmpl Nothing

unpack_append_fun env
	[ValArg (LitArg (MachStr str_val)),ValArg arg_z]
   | switchIsSet env SimplDoFoldrBuild && isNilForm env arg_z
   = tick Str_UnpackNil		`thenSmpl_`
     returnSmpl (Just (Lit (NoRepStr str_val)))
unpack_append_fun env _ = returnSmpl Nothing
-}
\end{code}

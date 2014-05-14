\begin{code}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Handy functions for creating much Core syntax
module MkCore (
        -- * Constructing normal syntax
        mkCoreLet, mkCoreLets,
        mkCoreApp, mkCoreApps, mkCoreConApps,
        mkCoreLams, mkWildCase, mkIfThenElse,
        mkWildValBinder, mkWildEvBinder,
        sortQuantVars, castBottomExpr,
        
        -- * Constructing boxed literals
        mkWordExpr, mkWordExprWord,
        mkIntExpr, mkIntExprInt,
        mkIntegerExpr,
        mkFloatExpr, mkDoubleExpr,
        mkCharExpr, mkStringExpr, mkStringExprFS,

        -- * Floats
        FloatBind(..), wrapFloat,

        -- * Constructing equality evidence boxes
        mkEqBox,

        -- * Constructing general big tuples
        -- $big_tuples
        mkChunkified,
        
        -- * Constructing small tuples
        mkCoreVarTup, mkCoreVarTupTy, mkCoreTup, 
        
        -- * Constructing big tuples
        mkBigCoreVarTup, mkBigCoreVarTupTy,
        mkBigCoreTup, mkBigCoreTupTy,
        
        -- * Deconstructing small tuples
        mkSmallTupleSelector, mkSmallTupleCase,
        
        -- * Deconstructing big tuples
        mkTupleSelector, mkTupleCase,
        
        -- * Constructing list expressions
        mkNilExpr, mkConsExpr, mkListExpr, 
        mkFoldrExpr, mkBuildExpr,

    	-- * Error Ids 
    	mkRuntimeErrorApp, mkImpossibleExpr, errorIds,
    	rEC_CON_ERROR_ID, iRREFUT_PAT_ERROR_ID, rUNTIME_ERROR_ID,
    	nON_EXHAUSTIVE_GUARDS_ERROR_ID, nO_METHOD_BINDING_ERROR_ID,
    	pAT_ERROR_ID, eRROR_ID, rEC_SEL_ERROR_ID, aBSENT_ERROR_ID,
        uNDEFINED_ID, undefinedName
    ) where

#include "HsVersions.h"

import Id
import Var      ( EvVar, setTyVarUnique )

import CoreSyn
import CoreUtils        ( exprType, needsCaseBinding, bindNonRec )
import Literal
import HscTypes

import TysWiredIn
import PrelNames

import TcType		( mkSigmaTy )
import Type
import Coercion
import TysPrim
import DataCon          ( DataCon, dataConWorkId )
import IdInfo		( vanillaIdInfo, setStrictnessInfo, 
                          setArityInfo )
import Demand 
import Name      hiding ( varName )
import Outputable
import FastString
import UniqSupply
import BasicTypes
import Util
import Pair
import Constants
import DynFlags

import Data.Char        ( ord )
import Data.List
import Data.Ord
import Data.Word

infixl 4 `mkCoreApp`, `mkCoreApps`
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Basic CoreSyn construction}
%*                                                                      *
%************************************************************************

\begin{code}
sortQuantVars :: [Var] -> [Var]
-- Sort the variables (KindVars, TypeVars, and Ids) 
-- into order: Kind, then Type, then Id
sortQuantVars = sortBy (comparing withCategory)
  where
    withCategory v = (category v, v)
    category :: Var -> Int
    category v
     | isKindVar v = 1
     | isTyVar   v = 2
     | otherwise   = 3

-- | Bind a binding group over an expression, using a @let@ or @case@ as
-- appropriate (see "CoreSyn#let_app_invariant")
mkCoreLet :: CoreBind -> CoreExpr -> CoreExpr
mkCoreLet (NonRec bndr rhs) body        -- See Note [CoreSyn let/app invariant]
  | needsCaseBinding (idType bndr) rhs
  = Case rhs bndr (exprType body) [(DEFAULT,[],body)]
mkCoreLet bind body
  = Let bind body

-- | Bind a list of binding groups over an expression. The leftmost binding
-- group becomes the outermost group in the resulting expression
mkCoreLets :: [CoreBind] -> CoreExpr -> CoreExpr
mkCoreLets binds body = foldr mkCoreLet body binds

-- | Construct an expression which represents the application of one expression
-- to the other
mkCoreApp :: CoreExpr -> CoreExpr -> CoreExpr
-- Check the invariant that the arg of an App is ok-for-speculation if unlifted
-- See CoreSyn Note [CoreSyn let/app invariant]
mkCoreApp fun (Type ty) = App fun (Type ty)
mkCoreApp fun (Coercion co) = App fun (Coercion co)
mkCoreApp fun arg       = ASSERT2( isFunTy fun_ty, ppr fun $$ ppr arg )
                          mk_val_app fun arg arg_ty res_ty
                      where
                        fun_ty = exprType fun
                        (arg_ty, res_ty) = splitFunTy fun_ty

-- | Construct an expression which represents the application of a number of
-- expressions to another. The leftmost expression in the list is applied first
mkCoreApps :: CoreExpr -> [CoreExpr] -> CoreExpr
-- Slightly more efficient version of (foldl mkCoreApp)
mkCoreApps orig_fun orig_args
  = go orig_fun (exprType orig_fun) orig_args
  where
    go fun _      []               = fun
    go fun fun_ty (Type ty : args) = go (App fun (Type ty)) (applyTy fun_ty ty) args
    go fun fun_ty (Coercion co : args) = go (App fun (Coercion co)) (applyCo fun_ty co) args
    go fun fun_ty (arg     : args) = ASSERT2( isFunTy fun_ty, ppr fun_ty $$ ppr orig_fun $$ ppr orig_args )
                                     go (mk_val_app fun arg arg_ty res_ty) res_ty args
                                   where
                                     (arg_ty, res_ty) = splitFunTy fun_ty

-- | Construct an expression which represents the application of a number of
-- expressions to that of a data constructor expression. The leftmost expression
-- in the list is applied first
mkCoreConApps :: DataCon -> [CoreExpr] -> CoreExpr
mkCoreConApps con args = mkCoreApps (Var (dataConWorkId con)) args

-----------
mk_val_app :: CoreExpr -> CoreExpr -> Type -> Type -> CoreExpr
mk_val_app fun arg arg_ty _        -- See Note [CoreSyn let/app invariant]
  | not (needsCaseBinding arg_ty arg)
  = App fun arg                -- The vastly common case

mk_val_app fun arg arg_ty res_ty
  = Case arg arg_id res_ty [(DEFAULT,[],App fun (Var arg_id))]
  where
    arg_id = mkWildValBinder arg_ty    
	-- Lots of shadowing, but it doesn't matter,
        -- because 'fun ' should not have a free wild-id
	--
	-- This is Dangerous.  But this is the only place we play this 
	-- game, mk_val_app returns an expression that does not have
	-- have a free wild-id.  So the only thing that can go wrong
	-- is if you take apart this case expression, and pass a 
	-- fragmet of it as the fun part of a 'mk_val_app'.

mkWildEvBinder :: PredType -> EvVar
mkWildEvBinder pred = mkWildValBinder pred

-- | Make a /wildcard binder/. This is typically used when you need a binder 
-- that you expect to use only at a *binding* site.  Do not use it at
-- occurrence sites because it has a single, fixed unique, and it's very
-- easy to get into difficulties with shadowing.  That's why it is used so little.
-- See Note [WildCard binders] in SimplEnv
mkWildValBinder :: Type -> Id
mkWildValBinder ty = mkLocalId wildCardName ty

mkWildCase :: CoreExpr -> Type -> Type -> [CoreAlt] -> CoreExpr
-- Make a case expression whose case binder is unused
-- The alts should not have any occurrences of WildId
mkWildCase scrut scrut_ty res_ty alts 
  = Case scrut (mkWildValBinder scrut_ty) res_ty alts

mkIfThenElse :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
mkIfThenElse guard then_expr else_expr
-- Not going to be refining, so okay to take the type of the "then" clause
  = mkWildCase guard boolTy (exprType then_expr) 
	 [ (DataAlt falseDataCon, [], else_expr),	-- Increasing order of tag!
    	   (DataAlt trueDataCon,  [], then_expr) ]

castBottomExpr :: CoreExpr -> Type -> CoreExpr
-- (castBottomExpr e ty), assuming that 'e' diverges, 
-- return an expression of type 'ty'
-- See Note [Empty case alternatives] in CoreSyn
castBottomExpr e res_ty
  | e_ty `eqType` res_ty = e
  | otherwise            = Case e (mkWildValBinder e_ty) res_ty []
  where
    e_ty = exprType e
\end{code}

The functions from this point don't really do anything cleverer than
their counterparts in CoreSyn, but they are here for consistency

\begin{code}
-- | Create a lambda where the given expression has a number of variables
-- bound over it. The leftmost binder is that bound by the outermost
-- lambda in the result
mkCoreLams :: [CoreBndr] -> CoreExpr -> CoreExpr
mkCoreLams = mkLams
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Making literals}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Create a 'CoreExpr' which will evaluate to the given @Int@
mkIntExpr :: DynFlags -> Integer -> CoreExpr        -- Result = I# i :: Int
mkIntExpr dflags i = mkConApp intDataCon  [mkIntLit dflags i]

-- | Create a 'CoreExpr' which will evaluate to the given @Int@
mkIntExprInt :: DynFlags -> Int -> CoreExpr         -- Result = I# i :: Int
mkIntExprInt dflags i = mkConApp intDataCon  [mkIntLitInt dflags i]

-- | Create a 'CoreExpr' which will evaluate to the a @Word@ with the given value
mkWordExpr :: DynFlags -> Integer -> CoreExpr
mkWordExpr dflags w = mkConApp wordDataCon [mkWordLit dflags w]

-- | Create a 'CoreExpr' which will evaluate to the given @Word@
mkWordExprWord :: DynFlags -> Word -> CoreExpr
mkWordExprWord dflags w = mkConApp wordDataCon [mkWordLitWord dflags w]

-- | Create a 'CoreExpr' which will evaluate to the given @Integer@
mkIntegerExpr  :: MonadThings m => Integer -> m CoreExpr  -- Result :: Integer
mkIntegerExpr i = do t <- lookupTyCon integerTyConName
                     return (Lit (mkLitInteger i (mkTyConTy t)))

-- | Create a 'CoreExpr' which will evaluate to the given @Float@
mkFloatExpr :: Float -> CoreExpr
mkFloatExpr f = mkConApp floatDataCon [mkFloatLitFloat f]

-- | Create a 'CoreExpr' which will evaluate to the given @Double@
mkDoubleExpr :: Double -> CoreExpr
mkDoubleExpr d = mkConApp doubleDataCon [mkDoubleLitDouble d]


-- | Create a 'CoreExpr' which will evaluate to the given @Char@
mkCharExpr     :: Char             -> CoreExpr      -- Result = C# c :: Int
mkCharExpr c = mkConApp charDataCon [mkCharLit c]

-- | Create a 'CoreExpr' which will evaluate to the given @String@
mkStringExpr   :: MonadThings m => String     -> m CoreExpr  -- Result :: String
-- | Create a 'CoreExpr' which will evaluate to a string morally equivalent to the given @FastString@
mkStringExprFS :: MonadThings m => FastString -> m CoreExpr  -- Result :: String

mkStringExpr str = mkStringExprFS (mkFastString str)

mkStringExprFS str
  | nullFS str
  = return (mkNilExpr charTy)

  | lengthFS str == 1
  = do let the_char = mkCharExpr (headFS str)
       return (mkConsExpr charTy the_char (mkNilExpr charTy))

  | all safeChar chars
  = do unpack_id <- lookupId unpackCStringName
       return (App (Var unpack_id) (Lit (MachStr (fastStringToByteString str))))

  | otherwise
  = do unpack_id <- lookupId unpackCStringUtf8Name
       return (App (Var unpack_id) (Lit (MachStr (fastStringToByteString str))))

  where
    chars = unpackFS str
    safeChar c = ord c >= 1 && ord c <= 0x7F
\end{code}

\begin{code}

-- This take a ~# b (or a ~# R b) and returns a ~ b (or Coercible a b)
mkEqBox :: Coercion -> CoreExpr
mkEqBox co = ASSERT2( typeKind ty2 `eqKind` k, ppr co $$ ppr ty1 $$ ppr ty2 $$ ppr (typeKind ty1) $$ ppr (typeKind ty2) )
             Var (dataConWorkId datacon) `mkTyApps` [k, ty1, ty2] `App` Coercion co
  where Pair ty1 ty2 = coercionKind co
        k = typeKind ty1
        datacon = case coercionRole co of 
            Nominal ->          eqBoxDataCon
            Representational -> coercibleDataCon
            Phantom ->          pprPanic "mkEqBox does not support boxing phantom coercions"
                                         (ppr co)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Tuple constructors}
%*                                                                      *
%************************************************************************

\begin{code}

-- $big_tuples
-- #big_tuples#
--
-- GHCs built in tuples can only go up to 'mAX_TUPLE_SIZE' in arity, but
-- we might concievably want to build such a massive tuple as part of the
-- output of a desugaring stage (notably that for list comprehensions).
--
-- We call tuples above this size \"big tuples\", and emulate them by
-- creating and pattern matching on >nested< tuples that are expressible
-- by GHC.
--
-- Nesting policy: it's better to have a 2-tuple of 10-tuples (3 objects)
-- than a 10-tuple of 2-tuples (11 objects), so we want the leaves of any
-- construction to be big.
--
-- If you just use the 'mkBigCoreTup', 'mkBigCoreVarTupTy', 'mkTupleSelector'
-- and 'mkTupleCase' functions to do all your work with tuples you should be
-- fine, and not have to worry about the arity limitation at all.

-- | Lifts a \"small\" constructor into a \"big\" constructor by recursive decompositon
mkChunkified :: ([a] -> a)      -- ^ \"Small\" constructor function, of maximum input arity 'mAX_TUPLE_SIZE'
             -> [a]             -- ^ Possible \"big\" list of things to construct from
             -> a               -- ^ Constructed thing made possible by recursive decomposition
mkChunkified small_tuple as = mk_big_tuple (chunkify as)
  where
	-- Each sub-list is short enough to fit in a tuple
    mk_big_tuple [as] = small_tuple as
    mk_big_tuple as_s = mk_big_tuple (chunkify (map small_tuple as_s))

chunkify :: [a] -> [[a]]
-- ^ Split a list into lists that are small enough to have a corresponding
-- tuple arity. The sub-lists of the result all have length <= 'mAX_TUPLE_SIZE'
-- But there may be more than 'mAX_TUPLE_SIZE' sub-lists
chunkify xs
  | n_xs <= mAX_TUPLE_SIZE = [xs] 
  | otherwise		   = split xs
  where
    n_xs     = length xs
    split [] = []
    split xs = take mAX_TUPLE_SIZE xs : split (drop mAX_TUPLE_SIZE xs)
    
\end{code}

Creating tuples and their types for Core expressions 

@mkBigCoreVarTup@ builds a tuple; the inverse to @mkTupleSelector@.  

* If it has only one element, it is the identity function.

* If there are more elements than a big tuple can have, it nests 
  the tuples.  

\begin{code}

-- | Build a small tuple holding the specified variables
mkCoreVarTup :: [Id] -> CoreExpr
mkCoreVarTup ids = mkCoreTup (map Var ids)

-- | Bulid the type of a small tuple that holds the specified variables
mkCoreVarTupTy :: [Id] -> Type
mkCoreVarTupTy ids = mkBoxedTupleTy (map idType ids)

-- | Build a small tuple holding the specified expressions
mkCoreTup :: [CoreExpr] -> CoreExpr
mkCoreTup []  = Var unitDataConId
mkCoreTup [c] = c
mkCoreTup cs  = mkConApp (tupleCon BoxedTuple (length cs))
                         (map (Type . exprType) cs ++ cs)

-- | Build a big tuple holding the specified variables
mkBigCoreVarTup :: [Id] -> CoreExpr
mkBigCoreVarTup ids = mkBigCoreTup (map Var ids)

-- | Build the type of a big tuple that holds the specified variables
mkBigCoreVarTupTy :: [Id] -> Type
mkBigCoreVarTupTy ids = mkBigCoreTupTy (map idType ids)

-- | Build a big tuple holding the specified expressions
mkBigCoreTup :: [CoreExpr] -> CoreExpr
mkBigCoreTup = mkChunkified mkCoreTup

-- | Build the type of a big tuple that holds the specified type of thing
mkBigCoreTupTy :: [Type] -> Type
mkBigCoreTupTy = mkChunkified mkBoxedTupleTy
\end{code}


%************************************************************************
%*                                                                      *
                Floats
%*                                                                      *
%************************************************************************

\begin{code}
data FloatBind 
  = FloatLet  CoreBind
  | FloatCase CoreExpr Id AltCon [Var]       
      -- case e of y { C ys -> ... }
      -- See Note [Floating cases] in SetLevels

wrapFloat :: FloatBind -> CoreExpr -> CoreExpr
wrapFloat (FloatLet defns)       body = Let defns body
wrapFloat (FloatCase e b con bs) body = Case e b (exprType body) [(con, bs, body)]
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Tuple destructors}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Builds a selector which scrutises the given
-- expression and extracts the one name from the list given.
-- If you want the no-shadowing rule to apply, the caller
-- is responsible for making sure that none of these names
-- are in scope.
--
-- If there is just one 'Id' in the tuple, then the selector is
-- just the identity.
--
-- If necessary, we pattern match on a \"big\" tuple.
mkTupleSelector :: [Id]         -- ^ The 'Id's to pattern match the tuple against
                -> Id           -- ^ The 'Id' to select
                -> Id           -- ^ A variable of the same type as the scrutinee
                -> CoreExpr     -- ^ Scrutinee
                -> CoreExpr     -- ^ Selector expression

-- mkTupleSelector [a,b,c,d] b v e
--          = case e of v { 
--                (p,q) -> case p of p {
--                           (a,b) -> b }}
-- We use 'tpl' vars for the p,q, since shadowing does not matter.
--
-- In fact, it's more convenient to generate it innermost first, getting
--
--        case (case e of v 
--                (p,q) -> p) of p
--          (a,b) -> b
mkTupleSelector vars the_var scrut_var scrut
  = mk_tup_sel (chunkify vars) the_var
  where
    mk_tup_sel [vars] the_var = mkSmallTupleSelector vars the_var scrut_var scrut
    mk_tup_sel vars_s the_var = mkSmallTupleSelector group the_var tpl_v $
                                mk_tup_sel (chunkify tpl_vs) tpl_v
        where
          tpl_tys = [mkBoxedTupleTy (map idType gp) | gp <- vars_s]
          tpl_vs  = mkTemplateLocals tpl_tys
          [(tpl_v, group)] = [(tpl,gp) | (tpl,gp) <- zipEqual "mkTupleSelector" tpl_vs vars_s,
                                         the_var `elem` gp ]
\end{code}

\begin{code}
-- | Like 'mkTupleSelector' but for tuples that are guaranteed
-- never to be \"big\".
--
-- > mkSmallTupleSelector [x] x v e = [| e |]
-- > mkSmallTupleSelector [x,y,z] x v e = [| case e of v { (x,y,z) -> x } |]
mkSmallTupleSelector :: [Id]        -- The tuple args
          -> Id         -- The selected one
          -> Id         -- A variable of the same type as the scrutinee
          -> CoreExpr        -- Scrutinee
          -> CoreExpr
mkSmallTupleSelector [var] should_be_the_same_var _ scrut
  = ASSERT(var == should_be_the_same_var)
    scrut
mkSmallTupleSelector vars the_var scrut_var scrut
  = ASSERT( notNull vars )
    Case scrut scrut_var (idType the_var)
         [(DataAlt (tupleCon BoxedTuple (length vars)), vars, Var the_var)]
\end{code}

\begin{code}
-- | A generalization of 'mkTupleSelector', allowing the body
-- of the case to be an arbitrary expression.
--
-- To avoid shadowing, we use uniques to invent new variables.
--
-- If necessary we pattern match on a \"big\" tuple.
mkTupleCase :: UniqSupply       -- ^ For inventing names of intermediate variables
            -> [Id]             -- ^ The tuple identifiers to pattern match on
            -> CoreExpr         -- ^ Body of the case
            -> Id               -- ^ A variable of the same type as the scrutinee
            -> CoreExpr         -- ^ Scrutinee
            -> CoreExpr
-- ToDo: eliminate cases where none of the variables are needed.
--
--         mkTupleCase uniqs [a,b,c,d] body v e
--           = case e of v { (p,q) ->
--             case p of p { (a,b) ->
--             case q of q { (c,d) ->
--             body }}}
mkTupleCase uniqs vars body scrut_var scrut
  = mk_tuple_case uniqs (chunkify vars) body
  where
    -- This is the case where don't need any nesting
    mk_tuple_case _ [vars] body
      = mkSmallTupleCase vars body scrut_var scrut
      
    -- This is the case where we must make nest tuples at least once
    mk_tuple_case us vars_s body
      = let (us', vars', body') = foldr one_tuple_case (us, [], body) vars_s
            in mk_tuple_case us' (chunkify vars') body'
    
    one_tuple_case chunk_vars (us, vs, body)
      = let (uniq, us') = takeUniqFromSupply us
            scrut_var = mkSysLocal (fsLit "ds") uniq
              (mkBoxedTupleTy (map idType chunk_vars))
            body' = mkSmallTupleCase chunk_vars body scrut_var (Var scrut_var)
        in (us', scrut_var:vs, body')
\end{code}

\begin{code}
-- | As 'mkTupleCase', but for a tuple that is small enough to be guaranteed
-- not to need nesting.
mkSmallTupleCase
        :: [Id]         -- ^ The tuple args
        -> CoreExpr     -- ^ Body of the case
        -> Id           -- ^ A variable of the same type as the scrutinee
        -> CoreExpr     -- ^ Scrutinee
        -> CoreExpr

mkSmallTupleCase [var] body _scrut_var scrut
  = bindNonRec var scrut body
mkSmallTupleCase vars body scrut_var scrut
-- One branch no refinement?
  = Case scrut scrut_var (exprType body) [(DataAlt (tupleCon BoxedTuple (length vars)), vars, body)]
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Common list manipulation expressions}
%*                                                                      *
%************************************************************************

Call the constructor Ids when building explicit lists, so that they
interact well with rules.

\begin{code}
-- | Makes a list @[]@ for lists of the specified type
mkNilExpr :: Type -> CoreExpr
mkNilExpr ty = mkConApp nilDataCon [Type ty]

-- | Makes a list @(:)@ for lists of the specified type
mkConsExpr :: Type -> CoreExpr -> CoreExpr -> CoreExpr
mkConsExpr ty hd tl = mkConApp consDataCon [Type ty, hd, tl]

-- | Make a list containing the given expressions, where the list has the given type
mkListExpr :: Type -> [CoreExpr] -> CoreExpr
mkListExpr ty xs = foldr (mkConsExpr ty) (mkNilExpr ty) xs

-- | Make a fully applied 'foldr' expression
mkFoldrExpr :: MonadThings m
            => Type             -- ^ Element type of the list
            -> Type             -- ^ Fold result type
            -> CoreExpr         -- ^ "Cons" function expression for the fold
            -> CoreExpr         -- ^ "Nil" expression for the fold
            -> CoreExpr         -- ^ List expression being folded acress
            -> m CoreExpr
mkFoldrExpr elt_ty result_ty c n list = do
    foldr_id <- lookupId foldrName
    return (Var foldr_id `App` Type elt_ty 
           `App` Type result_ty
           `App` c
           `App` n
           `App` list)

-- | Make a 'build' expression applied to a locally-bound worker function
mkBuildExpr :: (MonadThings m, MonadUnique m)
            => Type                                     -- ^ Type of list elements to be built
            -> ((Id, Type) -> (Id, Type) -> m CoreExpr) -- ^ Function that, given information about the 'Id's
                                                        -- of the binders for the build worker function, returns
                                                        -- the body of that worker
            -> m CoreExpr
mkBuildExpr elt_ty mk_build_inside = do
    [n_tyvar] <- newTyVars [alphaTyVar]
    let n_ty = mkTyVarTy n_tyvar
        c_ty = mkFunTys [elt_ty, n_ty] n_ty
    [c, n] <- sequence [mkSysLocalM (fsLit "c") c_ty, mkSysLocalM (fsLit "n") n_ty]
    
    build_inside <- mk_build_inside (c, c_ty) (n, n_ty)
    
    build_id <- lookupId buildName
    return $ Var build_id `App` Type elt_ty `App` mkLams [n_tyvar, c, n] build_inside
  where
    newTyVars tyvar_tmpls = do
      uniqs <- getUniquesM
      return (zipWith setTyVarUnique tyvar_tmpls uniqs)
\end{code}


%************************************************************************
%*                                                                      *
                      Error expressions
%*                                                                      *
%************************************************************************

\begin{code}
mkRuntimeErrorApp 
        :: Id           -- Should be of type (forall a. Addr# -> a)
                        --      where Addr# points to a UTF8 encoded string
        -> Type         -- The type to instantiate 'a'
        -> String       -- The string to print
        -> CoreExpr

mkRuntimeErrorApp err_id res_ty err_msg 
  = mkApps (Var err_id) [Type res_ty, err_string]
  where
    err_string = Lit (mkMachString err_msg)

mkImpossibleExpr :: Type -> CoreExpr
mkImpossibleExpr res_ty
  = mkRuntimeErrorApp rUNTIME_ERROR_ID res_ty "Impossible case alternative"
\end{code}

%************************************************************************
%*                                                                      *
                     Error Ids
%*                                                                      *
%************************************************************************

GHC randomly injects these into the code.

@patError@ is just a version of @error@ for pattern-matching
failures.  It knows various ``codes'' which expand to longer
strings---this saves space!

@absentErr@ is a thing we put in for ``absent'' arguments.  They jolly
well shouldn't be yanked on, but if one is, then you will get a
friendly message from @absentErr@ (rather than a totally random
crash).

@parError@ is a special version of @error@ which the compiler does
not know to be a bottoming Id.  It is used in the @_par_@ and @_seq_@
templates, but we don't ever expect to generate code for it.

\begin{code}
errorIds :: [Id]
errorIds 
  = [ eRROR_ID,   -- This one isn't used anywhere else in the compiler
                  -- But we still need it in wiredInIds so that when GHC
                  -- compiles a program that mentions 'error' we don't
                  -- import its type from the interface file; we just get
                  -- the Id defined here.  Which has an 'open-tyvar' type.

      uNDEFINED_ID,   -- Ditto for 'undefined'. The big deal is to give it
                      -- an 'open-tyvar' type.

      rUNTIME_ERROR_ID,
      iRREFUT_PAT_ERROR_ID,
      nON_EXHAUSTIVE_GUARDS_ERROR_ID,
      nO_METHOD_BINDING_ERROR_ID,
      pAT_ERROR_ID,
      rEC_CON_ERROR_ID,
      rEC_SEL_ERROR_ID,
      aBSENT_ERROR_ID ]

recSelErrorName, runtimeErrorName, absentErrorName :: Name
irrefutPatErrorName, recConErrorName, patErrorName :: Name
nonExhaustiveGuardsErrorName, noMethodBindingErrorName :: Name

recSelErrorName     = err_nm "recSelError"     recSelErrorIdKey     rEC_SEL_ERROR_ID
absentErrorName     = err_nm "absentError"     absentErrorIdKey     aBSENT_ERROR_ID
runtimeErrorName    = err_nm "runtimeError"    runtimeErrorIdKey    rUNTIME_ERROR_ID
irrefutPatErrorName = err_nm "irrefutPatError" irrefutPatErrorIdKey iRREFUT_PAT_ERROR_ID
recConErrorName     = err_nm "recConError"     recConErrorIdKey     rEC_CON_ERROR_ID
patErrorName        = err_nm "patError"        patErrorIdKey        pAT_ERROR_ID

noMethodBindingErrorName     = err_nm "noMethodBindingError"
                                  noMethodBindingErrorIdKey nO_METHOD_BINDING_ERROR_ID
nonExhaustiveGuardsErrorName = err_nm "nonExhaustiveGuardsError" 
                                  nonExhaustiveGuardsErrorIdKey nON_EXHAUSTIVE_GUARDS_ERROR_ID

err_nm :: String -> Unique -> Id -> Name
err_nm str uniq id = mkWiredInIdName cONTROL_EXCEPTION_BASE (fsLit str) uniq id

rEC_SEL_ERROR_ID, rUNTIME_ERROR_ID, iRREFUT_PAT_ERROR_ID, rEC_CON_ERROR_ID :: Id
pAT_ERROR_ID, nO_METHOD_BINDING_ERROR_ID, nON_EXHAUSTIVE_GUARDS_ERROR_ID :: Id
aBSENT_ERROR_ID :: Id
rEC_SEL_ERROR_ID                = mkRuntimeErrorId recSelErrorName
rUNTIME_ERROR_ID                = mkRuntimeErrorId runtimeErrorName
iRREFUT_PAT_ERROR_ID            = mkRuntimeErrorId irrefutPatErrorName
rEC_CON_ERROR_ID                = mkRuntimeErrorId recConErrorName
pAT_ERROR_ID                    = mkRuntimeErrorId patErrorName
nO_METHOD_BINDING_ERROR_ID      = mkRuntimeErrorId noMethodBindingErrorName
nON_EXHAUSTIVE_GUARDS_ERROR_ID  = mkRuntimeErrorId nonExhaustiveGuardsErrorName
aBSENT_ERROR_ID                 = mkRuntimeErrorId absentErrorName

mkRuntimeErrorId :: Name -> Id
mkRuntimeErrorId name = pc_bottoming_Id1 name runtimeErrorTy

runtimeErrorTy :: Type
-- The runtime error Ids take a UTF8-encoded string as argument
runtimeErrorTy = mkSigmaTy [openAlphaTyVar] [] (mkFunTy addrPrimTy openAlphaTy)
\end{code}

\begin{code}
errorName :: Name
errorName = mkWiredInIdName gHC_ERR (fsLit "error") errorIdKey eRROR_ID

eRROR_ID :: Id
eRROR_ID = pc_bottoming_Id1 errorName errorTy

errorTy  :: Type   -- See Note [Error and friends have an "open-tyvar" forall]
errorTy  = mkSigmaTy [openAlphaTyVar] [] (mkFunTys [mkListTy charTy] openAlphaTy)

undefinedName :: Name
undefinedName = mkWiredInIdName gHC_ERR (fsLit "undefined") undefinedKey uNDEFINED_ID

uNDEFINED_ID :: Id
uNDEFINED_ID = pc_bottoming_Id0 undefinedName undefinedTy

undefinedTy  :: Type   -- See Note [Error and friends have an "open-tyvar" forall]
undefinedTy  = mkSigmaTy [openAlphaTyVar] [] openAlphaTy
\end{code}

Note [Error and friends have an "open-tyvar" forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'error' and 'undefined' have types 
        error     :: forall (a::OpenKind). String -> a
        undefined :: forall (a::OpenKind). a
Notice the 'OpenKind' (manifested as openAlphaTyVar in the code). This ensures that
"error" can be instantiated at 
  * unboxed as well as boxed types
  * polymorphic types
This is OK because it never returns, so the return type is irrelevant.
See Note [OpenTypeKind accepts foralls] in TcUnify.


%************************************************************************
%*                                                                      *
\subsection{Utilities}
%*                                                                      *
%************************************************************************

\begin{code}
pc_bottoming_Id1 :: Name -> Type -> Id
-- Function of arity 1, which diverges after being given one argument
pc_bottoming_Id1 name ty
 = mkVanillaGlobalWithInfo name ty bottoming_info
 where
    bottoming_info = vanillaIdInfo `setStrictnessInfo`    strict_sig
				   `setArityInfo`         1
			-- Make arity and strictness agree

        -- Do *not* mark them as NoCafRefs, because they can indeed have
        -- CAF refs.  For example, pAT_ERROR_ID calls GHC.Err.untangle,
        -- which has some CAFs
        -- In due course we may arrange that these error-y things are
        -- regarded by the GC as permanently live, in which case we
        -- can give them NoCaf info.  As it is, any function that calls
        -- any pc_bottoming_Id will itself have CafRefs, which bloats
        -- SRTs.

    strict_sig = mkClosedStrictSig [evalDmd] botRes
    -- These "bottom" out, no matter what their arguments

pc_bottoming_Id0 :: Name -> Type -> Id
-- Same but arity zero
pc_bottoming_Id0 name ty
 = mkVanillaGlobalWithInfo name ty bottoming_info
 where
    bottoming_info = vanillaIdInfo `setStrictnessInfo` strict_sig
    strict_sig = mkClosedStrictSig [] botRes
\end{code}


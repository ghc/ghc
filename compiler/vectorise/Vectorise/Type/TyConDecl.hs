module Vectorise.Type.TyConDecl (
  vectTyConDecls
) where

import Vectorise.Type.Type
import Vectorise.Monad
import BuildTyCl
import Class
import Type
import TyCon
import DataCon
import BasicTypes
import Var
import Name
import Outputable
import Util
import Control.Monad


-- |Vectorise some (possibly recursively defined) type constructors.
--
vectTyConDecls :: [TyCon] -> VM [TyCon]
vectTyConDecls tcs = fixV $ \tcs' ->
  do
    mapM_ (uncurry defTyCon) (zipLazy tcs tcs')
    mapM vectTyConDecl tcs

-- | Vectorise a single type construcrtor.
vectTyConDecl :: TyCon -> VM TyCon
vectTyConDecl tycon
    -- a type class constructor.
    -- TODO: check for no stupid theta, fds, assoc types. 
    | isClassTyCon tycon
    , Just cls		<- tyConClass_maybe tycon

    = do    -- make the name of the vectorised class tycon.
	    name'	<- cloneName mkVectTyConOcc (tyConName tycon)

            -- vectorise right of definition.
            rhs'        <- vectAlgTyConRhs tycon (algTyConRhs tycon)

            -- vectorise method selectors.
            -- This also adds a mapping between the original and vectorised method selector
            -- to the state.
            methods'	<- mapM vectMethod
			$  [(id, defMethSpecOfDefMeth meth) 
				| (id, meth)	<- classOpItems cls]

            -- keep the original recursiveness flag.
            let rec_flag = boolToRecFlag (isRecursiveTyCon tycon)
	
	    -- Calling buildclass here attaches new quantifiers and dictionaries to the method types.
            cls'     <- liftDs 
             	    $  buildClass
                             False               -- include unfoldings on dictionary selectors.
                             name'               -- new name  V_T:Class
                             (tyConTyVars tycon) -- keep original type vars
                             []                  -- no stupid theta
                             []                  -- no functional dependencies
                             []                  -- no associated types
                             methods'            -- method info
                             rec_flag            -- whether recursive

            let tycon'  = mkClassTyCon name'
			     (tyConKind tycon)
			     (tyConTyVars tycon)
			     rhs'
			     cls'
			     rec_flag

            return $ tycon'
			
    -- a regular algebraic type constructor.
    -- TODO: check for stupid theta, generaics, GADTS etc
    | isAlgTyCon tycon
    = do    name'	<- cloneName mkVectTyConOcc (tyConName tycon)
            rhs'	<- vectAlgTyConRhs tycon (algTyConRhs tycon)
            let rec_flag =  boolToRecFlag (isRecursiveTyCon tycon)

            liftDs $ buildAlgTyCon 
                            name'               -- new name
                            (tyConTyVars tycon) -- keep original type vars.
                            []                  -- no stupid theta.
                            rhs'                -- new constructor defs.
                            rec_flag            -- FIXME: is this ok?
                            False               -- not GADT syntax
                            NoParentTyCon
                            Nothing             -- not a family instance

    -- some other crazy thing that we don't handle.
    | otherwise
    = cantVectorise "Can't vectorise type constructor: " (ppr tycon)


-- | Vectorise a class method.
vectMethod :: (Id, DefMethSpec) -> VM (Name, DefMethSpec, Type)
vectMethod (id, defMeth)
 = do	
	-- Vectorise the method type.
	typ'	<- vectType (varType id)

	-- Create a name for the vectorised method.
	id'	<- cloneId mkVectOcc id typ'
	defGlobalVar id id'

	-- When we call buildClass in vectTyConDecl, it adds foralls and dictionaries
	-- to the types of each method. However, the types we get back from vectType
	-- above already already have these, so we need to chop them off here otherwise
	-- we'll get two copies in the final version.
	let (_tyvars, tyBody) = splitForAllTys typ'
	let (_dict,   tyRest) = splitFunTy tyBody

	return	(Var.varName id', defMeth, tyRest)


-- | Vectorise the RHS of an algebraic type.
vectAlgTyConRhs :: TyCon -> AlgTyConRhs -> VM AlgTyConRhs
vectAlgTyConRhs _ (DataTyCon { data_cons = data_cons
                             , is_enum   = is_enum
                             })
  = do
      data_cons' <- mapM vectDataCon data_cons
      zipWithM_ defDataCon data_cons data_cons'
      return $ DataTyCon { data_cons = data_cons'
                         , is_enum   = is_enum
                         }

vectAlgTyConRhs tc _ 
	= cantVectorise "Can't vectorise type definition:" (ppr tc)


-- | Vectorise a data constructor.
--   Vectorises its argument and return types.
vectDataCon :: DataCon -> VM DataCon
vectDataCon dc
  | not . null $ dataConExTyVars dc
  = cantVectorise "Can't vectorise constructor (existentials):" (ppr dc)

  | not . null $ dataConEqSpec   dc
  = cantVectorise "Can't vectorise constructor (eq spec):" (ppr dc)

  | otherwise
  = do
      name'    <- cloneName mkVectDataConOcc name
      tycon'   <- vectTyCon tycon
      arg_tys  <- mapM vectType rep_arg_tys

      liftDs $ buildDataCon 
		name'
                False                          -- not infix
                (map (const HsNoBang) arg_tys) -- strictness annots on args.
                []                             -- no labelled fields
                univ_tvs                       -- universally quantified vars
                []                             -- no existential tvs for now
                []                             -- no eq spec for now
                []                             -- no context
                arg_tys                        -- argument types
		(mkFamilyTyConApp tycon' (mkTyVarTys univ_tvs)) -- return type
                tycon'                         -- representation tycon
  where
    name        = dataConName dc
    univ_tvs    = dataConUnivTyVars dc
    rep_arg_tys = dataConRepArgTys dc
    tycon       = dataConTyCon dc

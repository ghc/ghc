
module Vectorise.Type.TyConDecl (
  vectTyConDecls
) where

import Vectorise.Type.Type
import Vectorise.Monad
import Vectorise.Env( GlobalEnv( global_fam_inst_env ) )
import BuildTyCl
import Class
import Type
import TyCon
import DataCon
import BasicTypes
import DynFlags
import Var
import Name
import Outputable
import Util
import Control.Monad


-- |Vectorise some (possibly recursively defined) type constructors.
--
vectTyConDecls :: [TyCon] -> VM [TyCon]
vectTyConDecls tcs = fixV $ \tcs' ->
  do { names' <- mapM (mkLocalisedName mkVectTyConOcc . tyConName) tcs
     ; mapM_ (uncurry (uncurry defTyConName)) (tcs `zip` names' `zipLazy` tcs')
     ; zipWithM vectTyConDecl tcs names'
     }

-- |Vectorise a single type constructor.
--
vectTyConDecl :: TyCon -> Name -> VM TyCon
vectTyConDecl tycon name'

      -- Type constructor representing a type class
  | Just cls <- tyConClass_maybe tycon
  = do { unless (null $ classATs cls) $
           do dflags <- getDynFlags
              cantVectorise dflags "Associated types are not yet supported" (ppr cls)

           -- vectorise superclass constraint (types)
       ; theta' <- mapM vectType (classSCTheta cls)

           -- vectorise method selectors
       ; let opItems      = classOpItems cls
             Just datacon = tyConSingleDataCon_maybe tycon
             argTys       = dataConRepArgTys datacon                      -- all selector types
             opTys        = drop (length argTys - length opItems) argTys  -- only method types
       ; methods' <- sequence [ vectMethod id meth ty | ((id, meth), ty) <- zip opItems opTys]

           -- keep the original recursiveness flag
       ; let rec_flag = boolToRecFlag (isRecursiveTyCon tycon)

           -- construct the vectorised class (this also creates the class type constructors and its
           -- data constructor)
           --
           -- NB: 'buildClass' attaches new quantifiers and dictionaries to the method types
       ; cls' <- liftDs $
                   buildClass
                     False                      -- include unfoldings on dictionary selectors
                     name'                      -- new name: "V:Class"
                     (tyConTyVars tycon)        -- keep original type vars
                     (map (const Nominal) (tyConRoles tycon)) -- all role are N for safety
                     theta'                     -- superclasses
                     (snd . classTvsFds $ cls)  -- keep the original functional dependencies
                     []                         -- no associated types (for the moment)
                     methods'                   -- method info
                     rec_flag                   -- whether recursive

           -- the original dictionary constructor must map to the vectorised one
       ; let tycon'        = classTyCon cls'
             Just datacon  = tyConSingleDataCon_maybe tycon
             Just datacon' = tyConSingleDataCon_maybe tycon'
       ; defDataCon datacon datacon'

           -- the original superclass and methods selectors must map to the vectorised ones
       ; let selIds  = classAllSelIds cls
             selIds' = classAllSelIds cls'
       ; zipWithM_ defGlobalVar selIds selIds'

           -- return the type constructor of the vectorised class
       ; return tycon'
       }
                      
       -- Regular algebraic type constructor — for now, Haskell 2011-style only
  | isAlgTyCon tycon
  = do { unless (all isVanillaDataCon (tyConDataCons tycon)) $
           do dflags <- getDynFlags
              cantVectorise dflags "Currently only Haskell 2011 datatypes are supported" (ppr tycon)
  
           -- vectorise the data constructor of the class tycon
       ; rhs' <- vectAlgTyConRhs tycon (algTyConRhs tycon)

           -- keep the original recursiveness and GADT flags
       ; let rec_flag  = boolToRecFlag (isRecursiveTyCon tycon)
             gadt_flag = isGadtSyntaxTyCon tycon

           -- build the vectorised type constructor
       ; return $ buildAlgTyCon 
                    name'                   -- new name
                    (tyConTyVars tycon)     -- keep original type vars
                    (map (const Nominal) (tyConRoles tycon)) -- all roles are N for safety
                    Nothing
                    []                      -- no stupid theta
                    rhs'                    -- new constructor defs
                    rec_flag                -- whether recursive
                    False                   -- Not promotable
                    gadt_flag               -- whether in GADT syntax
                    NoParentTyCon           
       }

  -- some other crazy thing that we don't handle
  | otherwise
  = do dflags <- getDynFlags
       cantVectorise dflags "Can't vectorise exotic type constructor" (ppr tycon)

-- |Vectorise a class method.  (Don't enter it into the vectorisation map yet.)
--
vectMethod :: Id -> DefMeth -> Type -> VM (Name, DefMethSpec, Type)
vectMethod id defMeth ty
 = do {   -- Vectorise the method type.
      ; ty' <- vectType ty

          -- Create a name for the vectorised method.
      ; id' <- mkVectId id ty'

      ; return  (Var.varName id', defMethSpecOfDefMeth defMeth, ty')
      }

-- |Vectorise the RHS of an algebraic type.
--
vectAlgTyConRhs :: TyCon -> AlgTyConRhs -> VM AlgTyConRhs
vectAlgTyConRhs tc (AbstractTyCon {})
  = do dflags <- getDynFlags
       cantVectorise dflags "Can't vectorise imported abstract type" (ppr tc)
vectAlgTyConRhs _tc DataFamilyTyCon
  = return DataFamilyTyCon
vectAlgTyConRhs _tc (DataTyCon { data_cons = data_cons
                               , is_enum   = is_enum
                               })
  = do { data_cons' <- mapM vectDataCon data_cons
       ; zipWithM_ defDataCon data_cons data_cons'
       ; return $ DataTyCon { data_cons = data_cons'
                            , is_enum   = is_enum
                            }
       }
vectAlgTyConRhs tc (NewTyCon {})
  = do dflags <- getDynFlags
       cantVectorise dflags noNewtypeErr (ppr tc)
  where
    noNewtypeErr = "Vectorisation of newtypes not supported yet; please use a 'data' declaration"

-- |Vectorise a data constructor by vectorising its argument and return types..
--
vectDataCon :: DataCon -> VM DataCon
vectDataCon dc
  | not . null $ ex_tvs
  = do dflags <- getDynFlags
       cantVectorise dflags "Can't vectorise constructor with existential type variables yet" (ppr dc)
  | not . null $ eq_spec
  = do dflags <- getDynFlags
       cantVectorise dflags "Can't vectorise constructor with equality context yet" (ppr dc)
  | not . null $ dataConFieldLabels dc
  = do dflags <- getDynFlags
       cantVectorise dflags "Can't vectorise constructor with labelled fields yet" (ppr dc)
  | not . null $ theta
  = do dflags <- getDynFlags
       cantVectorise dflags "Can't vectorise constructor with constraint context yet" (ppr dc)
  | otherwise
  = do { name'   <- mkLocalisedName mkVectDataConOcc name
       ; tycon'  <- vectTyCon tycon
       ; arg_tys <- mapM vectType rep_arg_tys
       ; let ret_ty = mkFamilyTyConApp tycon' (mkTyVarTys univ_tvs)
       ; fam_envs  <- readGEnv global_fam_inst_env
       ; liftDs $ buildDataCon fam_envs
                    name'
                    (dataConIsInfix dc)            -- infix if the original is
                    (dataConStrictMarks dc)        -- strictness as original constructor
                    []                             -- no labelled fields for now
                    univ_tvs                       -- universally quantified vars
                    []                             -- no existential tvs for now
                    []                             -- no equalities for now
                    []                             -- no context for now
                    arg_tys                        -- argument types
                    ret_ty                         -- return type
                    tycon'                         -- representation tycon
       }
  where
    name        = dataConName dc
    rep_arg_tys = dataConRepArgTys dc
    tycon       = dataConTyCon dc
    (univ_tvs, ex_tvs, eq_spec, theta, _arg_tys, _res_ty) = dataConFullSig dc

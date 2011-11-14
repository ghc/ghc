{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

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
  do { mapM_ (uncurry defTyCon) (zipLazy tcs tcs')
     ; mapM vectTyConDecl tcs
     }

-- |Vectorise a single type constructor.
--
vectTyConDecl :: TyCon -> VM TyCon
vectTyConDecl tycon

      -- Type constructor representing a type class
  | Just cls <- tyConClass_maybe tycon
  = do { unless (null $ classATs cls) $
           cantVectorise "Associated types are not yet supported" (ppr cls)

           -- make the name of the vectorised class tycon: "Class" --> "V:Class"
       ; name' <- mkLocalisedName mkVectTyConOcc (tyConName tycon)
       
           -- vectorise superclass constraint (types)
       ; theta' <- mapM vectType (classSCTheta cls)

           -- vectorise method selectors
       ; methods' <- sequence [ vectMethod id meth | (id, meth) <- classOpItems cls]

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
           cantVectorise "Currently only Haskell 2011 datatypes are supported" (ppr tycon)
  
           -- make the name of the vectorised class tycon
       ; name' <- mkLocalisedName mkVectTyConOcc (tyConName tycon)

           -- vectorise the data constructor of the class tycon
       ; rhs' <- vectAlgTyConRhs tycon (algTyConRhs tycon)

           -- keep the original recursiveness and GADT flags
       ; let rec_flag  = boolToRecFlag (isRecursiveTyCon tycon)
             gadt_flag = isGadtSyntaxTyCon tycon

           -- build the vectorised type constructor
       ; liftDs $ buildAlgTyCon 
                    name'                   -- new name
                    (tyConTyVars tycon)     -- keep original type vars
                    []                      -- no stupid theta
                    rhs'                    -- new constructor defs
                    rec_flag                -- whether recursive
                    gadt_flag               -- whether in GADT syntax
                    NoParentTyCon           
                    Nothing                 -- not a family instance
       }

  -- some other crazy thing that we don't handle
  | otherwise
  = cantVectorise "Can't vectorise exotic type constructor" (ppr tycon)

-- |Vectorise a class method.  (Don't enter into the vectorisation map yet.)
--
vectMethod :: Id -> DefMeth -> VM (Name, DefMethSpec, Type)
vectMethod id defMeth
 = do {   -- Vectorise the method type.
      ; typ' <- vectType (varType id)

          -- Create a name for the vectorised method.
      ; id' <- mkVectId id typ'

          -- When we call buildClass in vectTyConDecl, it adds foralls and dictionaries
          -- to the types of each method. However, the types we get back from vectType
          -- above already already have these, so we need to chop them off here otherwise
          -- we'll get two copies in the final version.
      ; let (_tyvars, tyBody) = splitForAllTys typ'
      ; let (_dict,   tyRest) = splitFunTy tyBody

      ; return  (Var.varName id', defMethSpecOfDefMeth defMeth, tyRest)
      }

-- |Vectorise the RHS of an algebraic type.
--
vectAlgTyConRhs :: TyCon -> AlgTyConRhs -> VM AlgTyConRhs
vectAlgTyConRhs tc (AbstractTyCon {})
  = cantVectorise "Can't vectorise imported abstract type" (ppr tc)
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
  = cantVectorise noNewtypeErr (ppr tc)
  where
    noNewtypeErr = "Vectorisation of newtypes not supported yet; please use a 'data' declaration"

-- |Vectorise a data constructor by vectorising its argument and return types..
--
vectDataCon :: DataCon -> VM DataCon
vectDataCon dc
  | not . null $ ex_tvs
  = cantVectorise "Can't vectorise constructor with existential type variables yet" (ppr dc)
  | not . null $ eq_spec
  = cantVectorise "Can't vectorise constructor with equality context yet" (ppr dc)
  | not . null $ dataConFieldLabels dc
  = cantVectorise "Can't vectorise constructor with labelled fields yet" (ppr dc)
  | not . null $ theta
  = cantVectorise "Can't vectorise constructor with constraint context yet" (ppr dc)
  | otherwise
  = do { name'   <- mkLocalisedName mkVectDataConOcc name
       ; tycon'  <- vectTyCon tycon
       ; arg_tys <- mapM vectType rep_arg_tys
       ; let ret_ty = mkFamilyTyConApp tycon' (mkTyVarTys univ_tvs)
       ; liftDs $ buildDataCon
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

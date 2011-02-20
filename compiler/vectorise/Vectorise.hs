{-# OPTIONS -fno-warn-missing-signatures #-}

module Vectorise ( vectorise )
where

import Vectorise.Type.Env
import Vectorise.Type.Type
import Vectorise.Convert
import Vectorise.Utils.Hoisting
import Vectorise.Exp
import Vectorise.Vect
import Vectorise.Env
import Vectorise.Monad

import HscTypes hiding      ( MonadThings(..) )
import CoreUnfold           ( mkInlineUnfolding )
import CoreFVs
import PprCore
import CoreSyn
import CoreMonad            ( CoreM, getHscEnv )
import Type
import Var
import Id
import OccName
import DynFlags
import BasicTypes           ( isLoopBreaker )
import Outputable
import Util                 ( zipLazy )
import MonadUtils

import Control.Monad


-- | Vectorise a single module.
--
vectorise :: ModGuts -> CoreM ModGuts
vectorise guts
 = do { hsc_env <- getHscEnv
      ; liftIO $ vectoriseIO hsc_env guts
      }

-- | Vectorise a single monad, given the dynamic compiler flags and HscEnv.
--
vectoriseIO :: HscEnv -> ModGuts -> IO ModGuts
vectoriseIO hsc_env guts
 = do {   -- Get information about currently loaded external packages.
      ; eps <- hscEPS hsc_env

          -- Combine vectorisation info from the current module, and external ones.
      ; let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps

          -- Run the main VM computation.
      ; Just (info', guts') <- initV hsc_env guts info (vectModule guts)
      ; return (guts' { mg_vect_info = info' })
      }

-- | Vectorise a single module, in the VM monad.
--
vectModule :: ModGuts -> VM ModGuts
vectModule guts@(ModGuts { mg_types     = types
                         , mg_binds     = binds
                         , mg_fam_insts = fam_insts
                         })
 = do { dumpOptVt Opt_D_dump_vt_trace "Before vectorisation" $ 
          pprCoreBindings binds
 
          -- Vectorise the type environment.
          -- This may add new TyCons and DataCons.
      ; (types', new_fam_insts, tc_binds) <- vectTypeEnv types

      ; (_, fam_inst_env) <- readGEnv global_fam_inst_env

      -- dicts   <- mapM buildPADict pa_insts
      -- workers <- mapM vectDataConWorkers pa_insts

          -- Vectorise all the top level bindings.
      ; binds'  <- mapM vectTopBind binds

      ; return $ guts { mg_types        = types'
                      , mg_binds        = Rec tc_binds : binds'
                      , mg_fam_inst_env = fam_inst_env
                      , mg_fam_insts    = fam_insts ++ new_fam_insts
                      }
      }

-- | Try to vectorise a top-level binding.
--   If it doesn't vectorise then return it unharmed.
--
--   For example, for the binding 
--
--   @  
--      foo :: Int -> Int
--      foo = \x -> x + x
--   @
--  
--   we get
--   @
--      foo  :: Int -> Int
--      foo  = \x -> vfoo $: x                  
-- 
--      v_foo :: Closure void vfoo lfoo
--      v_foo = closure vfoo lfoo void        
-- 
--      vfoo :: Void -> Int -> Int
--      vfoo = ...
--
--      lfoo :: PData Void -> PData Int -> PData Int
--      lfoo = ...
--   @ 
--
--   @vfoo@ is the "vectorised", or scalar, version that does the same as the original
--   function foo, but takes an explicit environment.
-- 
--   @lfoo@ is the "lifted" version that works on arrays.
--
--   @v_foo@ combines both of these into a `Closure` that also contains the
--   environment.
--
--   The original binding @foo@ is rewritten to call the vectorised version
--   present in the closure.
--
vectTopBind :: CoreBind -> VM CoreBind
vectTopBind b@(NonRec var expr)
 = do
      (inline, _, expr') 	<- vectTopRhs [] var expr
      var' <- vectTopBinder var inline expr'

      -- Vectorising the body may create other top-level bindings.
      hs <- takeHoisted

      -- To get the same functionality as the original body we project
      -- out its vectorised version from the closure.
      cexpr <- tryConvert var var' expr

      return . Rec $ (var, cexpr) : (var', expr') : hs
  `orElseV`
    return b

vectTopBind b@(Rec bs)
 = do
      (vars', _, exprs') 
        <- fixV $ \ ~(_, inlines, rhss) ->
            do vars' <- sequence [vectTopBinder var inline rhs
                                      | (var, ~(inline, rhs)) <- zipLazy vars (zip inlines rhss)]
               (inlines', areScalars', exprs') 
                     <- mapAndUnzip3M (uncurry $ vectTopRhs vars) bs
               if  (and areScalars') || (length bs <= 1)
                  then do
                    return (vars', inlines', exprs')
                  else do
                    _ <- mapM deleteGlobalScalar vars
                    (inlines'', _, exprs'')  <- mapAndUnzip3M (uncurry $ vectTopRhs []) bs
                    return (vars', inlines'', exprs'')
                      
      hs     <- takeHoisted
      cexprs <- sequence $ zipWith3 tryConvert vars vars' exprs
      return . Rec $ zip vars cexprs ++ zip vars' exprs' ++ hs
  `orElseV`
    return b
  where
    (vars, exprs) = unzip bs
    
-- | Make the vectorised version of this top level binder, and add the mapping
--   between it and the original to the state. For some binder @foo@ the vectorised
--   version is @$v_foo@
--
--   NOTE: vectTopBinder *MUST* be lazy in inline and expr because of how it is
--   used inside of fixV in vectTopBind
--
vectTopBinder :: Var      -- ^ Name of the binding.
              -> Inline   -- ^ Whether it should be inlined, used to annotate it.
              -> CoreExpr -- ^ RHS of binding, used to set the 'Unfolding' of the returned 'Var'.
              -> VM Var   -- ^ Name of the vectorised binding.
vectTopBinder var inline expr
 = do {   -- Vectorise the type attached to the var.
      ; vty  <- vectType (idType var)
      
          -- If there is a vectorisation declartion for this binding, make sure that its type
          --  matches
      ; vectDecl <- lookupVectDecl var
      ; case vectDecl of
          Nothing                 -> return ()
          Just (vdty, _) 
            | coreEqType vty vdty -> return ()
            | otherwise           -> 
              cantVectorise ("Type mismatch in vectorisation pragma for " ++ show var) $
                (text "Expected type" <+> ppr vty)
                $$
                (text "Inferred type" <+> ppr vdty)

          -- Make the vectorised version of binding's name, and set the unfolding used for inlining
      ; var' <- liftM (`setIdUnfoldingLazily` unfolding) 
                $  cloneId mkVectOcc var vty

          -- Add the mapping between the plain and vectorised name to the state.
      ; defGlobalVar var var'

      ; return var'
    }
  where
    unfolding = case inline of
                  Inline arity -> mkInlineUnfolding (Just arity) expr
                  DontInline   -> noUnfolding

-- | Vectorise the RHS of a top-level binding, in an empty local environment.
--
-- We need to distinguish three cases:
--
-- (1) We have a (non-scalar) vectorisation declaration for the variable (which explicitly provides
--     vectorised code implemented by the user)
--     => no automatic vectorisation & instead use the user-supplied code
-- 
-- (2) We have a scalar vectorisation declaration for the variable
--     => generate vectorised code that uses a scalar 'map'/'zipWith' to lift the computation
-- 
-- (3) There is no vectorisation declaration for the variable
--     => perform automatic vectorisation of the RHS
--
vectTopRhs :: [Var]           -- ^ Names of all functions in the rec block
           -> Var             -- ^ Name of the binding.
           -> CoreExpr        -- ^ Body of the binding.
           -> VM ( Inline     -- (1) inline specification for the binding
                 , Bool       -- (2) whether the right-hand side is a scalar computation
                 , CoreExpr)  -- (3) the vectorised right-hand side
vectTopRhs recFs var expr
  = closedV
  $ do { traceVt ("vectTopRhs of " ++ show var) $ ppr expr
  
       ; globalScalar <- isGlobalScalar var
       ; vectDecl     <- lookupVectDecl var
       ; rhs globalScalar vectDecl
       }
  where
    rhs _globalScalar (Just (_, expr'))               -- Case (1)
      = return (inlineMe, False, expr')
    rhs True          _vectDecl                       -- Case (2)
      = return (inlineMe, True, scalarRHS)
                          -- FIXME: that True is not enough to register scalarness
    rhs False         _vectDecl                       -- Case (3)
      = do { let fvs = freeVars expr
           ; (inline, isScalar, vexpr) <- inBind var $
                                            vectPolyExpr (isLoopBreaker $ idOccInfo var) recFs fvs
           ; if isScalar 
             then addGlobalScalar var
             else deleteGlobalScalar var
           ; return (inline, isScalar, vectorised vexpr)
           }
      
    -- For scalar right-hand sides, we know that the original binding will remain unaltered
    -- (hence, we can refer to it without risk of cycles) - cf, 'tryConvert'.
    scalarRHS = panic "Vectorise.scalarRHS: not implemented yet"

-- | Project out the vectorised version of a binding from some closure,
--   or return the original body if that doesn't work or the binding is scalar. 
--
tryConvert :: Var       -- ^ Name of the original binding (eg @foo@)
           -> Var       -- ^ Name of vectorised version of binding (eg @$vfoo@)
           -> CoreExpr  -- ^ The original body of the binding.
           -> VM CoreExpr
tryConvert var vect_var rhs
  = do { globalScalar <- isGlobalScalar var
       ; if globalScalar
         then
           return rhs
         else
           fromVect (idType var) (Var vect_var) `orElseV` return rhs
       }

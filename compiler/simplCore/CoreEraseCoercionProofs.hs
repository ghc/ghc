module CoreEraseCoercionProofs (eraseCoercionProgram,coreProgramEraseCoercionProofs) where

import GhcPrelude

import CoreSyn
import HscTypes         ( ModGuts(..) )
import Coercion
import CoreMonad        ( CoreM )
import DynFlags
import TyCoRep (Coercion(ErasedCoercion))
{-
Top-level interface function, @eraseCoercionProgram@.

-}

eraseCoercionProgram :: ModGuts -> CoreM ModGuts
eraseCoercionProgram pgm@(ModGuts { mg_binds = binds })
  = do { dflags <- getDynFlags ;
         return (pgm { mg_binds =  coreProgramEraseCoercionProofs dflags binds })
        }

coreProgramEraseCoercionProofs :: DynFlags ->CoreProgram -> CoreProgram
coreProgramEraseCoercionProofs dflags topLevelBindings =
    if not (gopt Opt_DoCoreLinting dflags) then
     flip  map  topLevelBindings $  \ x -> case  x of
          NonRec v expr -> NonRec v $  coreExprEraseProof expr
          Rec bindings -> Rec $ map (\(v,expr)-> (v,coreExprEraseProof expr)) bindings
      else topLevelBindings

coreExprEraseProof :: CoreExpr -> CoreExpr
coreExprEraseProof e@(Var   _) = e
coreExprEraseProof e@(Lit   _) = e
coreExprEraseProof (App   f  e)  = App (coreExprEraseProof f) (coreExprEraseProof e)
coreExprEraseProof (Lam   v e) =  Lam v $ coreExprEraseProof e
coreExprEraseProof (Let   binder bod) = Let (eraseBinders binder) (coreExprEraseProof bod)
coreExprEraseProof (Case  scrut v ty alts  )=
    Case (coreExprEraseProof scrut) v ty (map eraseAltPfs alts)
coreExprEraseProof (Cast  e co ) = Cast (coreExprEraseProof e) (ErasedCoercion role lty rty )
  where
    (Pair lty rty,role) = coercionKindRole
coreExprEraseProof (Tick  tick e)= Tick tick (coreExprEraseProof e)
coreExprEraseProof (Type  t) = Type t
coreExprEraseProof (Coercion _)= Coercion ErasedCoercion

eraseAltPfs :: CoreAlt -> CoreAlt
eraseAltPfs (con, vars, body) = (con,vars,coreExprEraseProof body)

eraseBinders :: CoreBind -> CoreBind
eraseBinders(NonRec var rhs)=  NonRec var $ coreExprEraseProof rhs
eraseBinders (Rec  binders) = Rec $ map (\(v,e)-> (v,coreExprEraseProof e)) binders
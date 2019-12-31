module CoreEraseCoercionProofs (eraseCoercionProgram,coreProgramEraseCoercionProofs) where

import GhcPrelude

import CoreSyn
import HscTypes         ( ModGuts(..) )

import CoreMonad        ( CoreM )
import DynFlags

{-
Top-level interface function, @eraseCoercionProgram@.

-}

eraseCoercionProgram :: ModGuts -> CoreM ModGuts
eraseCoercionProgram pgm@(ModGuts { mg_binds = binds })
  = do { dflags <- getDynFlags
         return (pgm { mg_binds = map (coreProgramEraseCoercionProofs dflags) binds })
        }

coreProgramEraseCoercionProofs :: DynFlags ->CoreProgram -> CoreProgram
coreProgramEraseCoercionProofs dflags topLevelBindings =
    if not (gopt Opt_DoCoreLinting dflags) then
      case  topLevelBindings of
          NonRec v expr -> NonRec v $  coreExprEraseProof expr
          Rec bindings -> Rec $ map (\(v,expr)-> (v,coreExprEraseProof expr)) bindings
      else topLevelBindings

coreExprEraseProof :: Expr b -> Expr b
coreExprEraseProof e@(Var   Id) = e
coreExprEraseProof e@(Lit   Literal) = e
coreExprEraseProof (App   f  e)  = App (coreExprEraseProof f) (coreExprEraseProof)
coreExprEraseProof (Lam   v e) =  Lam v $ coreExprEraseProof e
coreExprEraseProof (Let   binders bod) = Let (eraseBinders binder) (coreExprEraseProof bod)
coreExprEraseProof (Case  scrut v ty alts  )=
    Case (coreExprEraseProof scrut) v ty (map eraseAltPfs alts)
coreExprEraseProof (Cast  e _) = Cast (coreExprEraseProof e) ErasedCoercion
coreExprEraseProof (Tick  tick e)= Tick tick (coreExprEraseProof e)
coreExprEraseProof (Type  t) = Type t
coreExprEraseProof (Coercion _)= Coercion ErasedCoercion

eraseAltPfs :: Alt b -> Alt b
eraseAltPfs (con, vars, body) = (con,vars,coreExprEraseProof body)

eraseBinders :: Bind b -> Bind b
eraseBinders(NonRec var rhs)=  NonRec var $ coreExprEraseProof rhs
eraseBinders (Rec  binders) = Rec $ map (\(v,e)-> (v,coreExprEraseProof e)) binders
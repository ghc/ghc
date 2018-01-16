module Environment
       (Env, emptyEnv, extendLocal, extendGlobal,
        makeEnv, unmakeEnv, lookupEnv, domEnv, freeTVarEnv)
       where

import Shows
import Parse
import Term           (VarId, readsId)
import Type           (TVarId, TConId, MonoType, PolyType (All), freeTVarPoly)
import FiniteMap      (FM, emptyFM, lookupFM, extendFM, makeFM, unmakeFM,
                       mapFM, domFM, ranFM)
data  Env             =   MkEnv (FM VarId PolyType)
rep                   ::  Env -> FM VarId PolyType
rep (MkEnv f)         =   f
emptyEnv              ::  Env
emptyEnv              =   MkEnv emptyFM
extendLocal           ::  Env -> VarId -> MonoType -> Env
extendLocal env x t   =   MkEnv (extendFM (rep env) x (All [] t))
extendGlobal          ::  Env -> VarId -> PolyType -> Env
extendGlobal env x t  =   MkEnv (extendFM (rep env) x t)
makeEnv               ::  [(VarId, PolyType)] -> Env
makeEnv               =   MkEnv . makeFM
unmakeEnv             ::  Env -> [(VarId, PolyType)]
unmakeEnv             =   unmakeFM . rep
lookupEnv             ::  Env -> VarId -> PolyType
lookupEnv env x       =   lookupFM (rep env) x
domEnv                ::  Env -> [VarId]
domEnv env            =   domFM (rep env)
freeTVarEnv           ::  Env -> [TVarId]
freeTVarEnv env       =   concat (map freeTVarPoly (ranFM (rep env)))
instance  Read Env  where
      readsPrec d  =  readsEnv
instance  Show Env  where
      showsPrec d  =  showsEnv
readsEnv              :: Parses Env
readsEnv              =  listP readsPair `eachP` makeEnv
readsPair             :: Parses (VarId, PolyType)
readsPair             =       readsId         `thenP` (\x ->
                              lexP ":"        `thenP` (\_ ->
                              reads           `thenP` (\t ->
                                              returnP (x,t))))
showsEnv              :: Shows Env
showsEnv              =  showsSurround "[" (showsStarSep ",\n " showsPair) "]"
                      .  unmakeEnv
showsPair             :: Shows (VarId, PolyType)
showsPair (x,t)       =  showsString x . showsString " : " . shows t

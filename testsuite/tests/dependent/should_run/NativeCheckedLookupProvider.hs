{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs #-}
#ifdef HIDE_ENV
module NativeCheckedLookupProvider (Nat(..), Env, Fin(..), lookupEnv) where
#else
module NativeCheckedLookupProvider where
#endif

data Nat = Z | S Nat

data Env (n :: Nat) where
    Nil :: Env 'Z
    Cons :: Bool -> Env n -> Env ('S n)

data Fin (n :: Nat) where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)

lookupEnv :: Env n -> Fin n -> Bool
lookupEnv (Cons x _) FZ = x
#ifdef WRONG_BODY
lookupEnv (Cons x _) (FS _) = x
#else
lookupEnv (Cons _ e) (FS i) = lookupEnv e i
#endif

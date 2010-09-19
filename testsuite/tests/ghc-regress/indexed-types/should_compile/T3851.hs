{-# LANGUAGE GADTs, TypeFamilies #-}

module T3851 where

type family TF a :: * -> *
type instance TF () = App (Equ ())

data Equ ix ix' where Refl :: Equ ix ix
data App f x = App (f x)

-- does not typecheck in 6.12.1 (but works in 6.10.4)
bar :: TF () () -> ()
bar (App Refl) = ()

-- does typecheck in 6.12.1 and 6.10.4
ar :: App (Equ ()) () -> ()
ar (App Refl) = ()

------------------
data family DF a :: * -> *
data instance DF () a = D (App (Equ ()) a)

bar_df :: DF () () -> ()
bar_df (D (App Refl)) = ()

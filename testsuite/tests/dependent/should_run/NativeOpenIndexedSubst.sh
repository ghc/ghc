#!/usr/bin/env bash
set -euo pipefail
hc=("$1" "-B$2" -v0 -package-env - -i -dcore-lint -dstg-lint
    -fno-write-if-simplified-core -fdiagnostics-color=never)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

cat >"$work/Syntax.hs" <<'HS'
{-# LANGUAGE DataKinds, GADTs, LambdaCase, RankNTypes #-}
module Syntax where

import Data.Type.Equality ((:~:) (Refl))

data Sort = Num | Fun Sort Sort
data Ctx = Empty | Extend Ctx Sort

data Name (g :: Ctx) (a :: Sort) where
    Here :: Name ('Extend g a) a
    There :: Name g a -> Name ('Extend g b) a

data Term (g :: Ctx) (a :: Sort) where
    Use :: Name g a -> Term g a
    Bind :: Term ('Extend g a) b -> Term g ('Fun a b)
    Lit :: Term g 'Num

liftRename :: (forall a. Name g a -> Name d a)
           -> Name ('Extend g b) a -> Name ('Extend d b) a
liftRename _ Here = Here
liftRename rename (There x) = There (rename x)

renameTerm :: (forall a. Name g a -> Name d a) -> Term g b -> Term d b
renameTerm rename (Use x) = Use (rename x)
renameTerm rename (Bind body) = Bind (renameTerm (liftRename rename) body)
renameTerm _ Lit = Lit

liftSubst :: (forall a. Name g a -> Term d a)
          -> Name ('Extend g b) a -> Term ('Extend d b) a
liftSubst _ Here = Use Here
liftSubst sub (There x) = renameTerm There (sub x)

substTerm :: (forall a. Name g a -> Term d a) -> Term g b -> Term d b
substTerm sub (Use x) = sub x
substTerm sub (Bind body) = Bind (substTerm (liftSubst sub) body)
substTerm _ Lit = Lit

substOne :: Term ('Extend g a) b -> Term g a -> Term g b
substOne body value = substTerm (\case Here -> value; There x -> Use x) body

data SomeTerm (g :: Ctx) (a :: Sort) where
    Pack :: (x :~: a) -> Term g x -> SomeTerm g a

packTerm :: Name g a -> Term g a -> SomeTerm g a
packTerm _ term = Pack Refl term

unpackTerm :: Name g a -> SomeTerm g a -> Term g a
unpackTerm _ (Pack Refl term) = term

unpackOne :: SomeTerm g a -> Term g a
unpackOne (Pack Refl term) = term
HS

cat >"$work/Reader.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeOperators #-}
module Reader where

import Data.Type.Equality ((:~:) (Refl))
import Syntax (Sort (..), Ctx (..), Name, Term, SomeTerm)
import qualified Syntax as S

leaf :: forall (g :: Ctx) (a :: Sort) (x :: Term g a).
        S.substOne ('S.Use 'S.Here) x :~: x
leaf = Refl

#ifdef RIGID
underBinder :: forall (g :: Ctx) (x :: Term g 'Num).
#else
underBinder :: forall (g :: Ctx) (x :: Name g 'Num).
#endif
               S.substOne
                 ('S.Bind ('S.Use ('S.There 'S.Here))
                   :: Term ('Extend g 'Num) ('Fun 'Num 'Num))
#ifdef RIGID
                 x
#else
                 ('S.Use x)
#endif
#if defined(WRONG) || defined(RIGID)
               :~: 'S.Bind ('S.Use 'S.Here)
#else
               :~: 'S.Bind ('S.Use ('S.There x))
#endif
underBinder = Refl

packExistential :: forall (x :: Term ('Extend 'Empty 'Num) 'Num)
                          (y :: Term ('Extend 'Empty 'Num) 'Num).
                   S.packTerm ('S.Here :: Name ('Extend 'Empty 'Num) 'Num) x
#ifdef WRONG_EXISTENTIAL
                   :~: 'S.Pack 'Refl y
#else
                   :~: 'S.Pack 'Refl x
#endif
packExistential = Refl

unpackExistential :: forall (n :: Name ('Extend 'Empty 'Num) 'Num)
                            (x :: Term ('Extend 'Empty 'Num) 'Num).
                     S.unpackTerm n ('S.Pack 'Refl x) :~: x
unpackExistential = Refl

unpackUnary :: forall (x :: Term ('Extend 'Empty 'Num) 'Num)
                      (y :: Term ('Extend 'Empty 'Num) 'Num).
               S.unpackOne ('S.Pack 'Refl x :: SomeTerm ('Extend 'Empty 'Num) 'Num)
#ifdef WRONG_UNARY
                 :~: y
#else
                 :~: x
#endif
unpackUnary = Refl

#ifdef RULE
{-# RULES "replace/substitution" forall (x :: Term 'Empty 'Num).
    S.substOne (S.Use S.Here) x = S.Lit #-}
#endif
HS

# A retained declaration uses the native pipeline, which can keep a
# multiplicity cast on the constructor callback in the optimized interface.
for variant in 0 2 2-retained; do
    opt=${variant%%-*}
    mkdir -p "$work/$variant/"{provider,reader}
    cd "$work/$variant/provider"
    cp "$work/Syntax.hs" Plain.hs
    if [[ $variant == 2-retained ]]; then
        sed -i '1i{-# LANGUAGE ExperimentalDependentTypes #-}' Plain.hs
        cat >>Plain.hs <<'HS'

retained :: foreach (x :: Term 'Empty 'Num) -> x :~: x
retained _ = Refl
HS
    fi
    cp Plain.hs Syntax.hs
    provider=(-O"$opt" -fno-omit-interface-pragmas -fexpose-all-unfoldings)
    "${hc[@]}" "${provider[@]}" -c Syntax.hs
    cp Syntax.hi ../reader/
    cd ../reader
    cp "$work/Reader.hs" .
    reader=(-O"$opt" -i. -fno-code -fwrite-interface -fomit-interface-pragmas -fforce-recomp)
    "${hc[@]}" "${reader[@]}" -c Reader.hs
    test ! -e Reader.o && test ! -e Syntax.o && test ! -e Syntax.hs
    reject() {
        local diagnostic=$1
        shift
        if "${hc[@]}" "${reader[@]}" "$@" -c Reader.hs >wrong.log 2>&1 ||
           ! grep -Fq "error: [$diagnostic]" wrong.log; then
            cat wrong.log >&2
            exit 1
        fi
    }
    reject GHC-83865 -DWRONG
    reject GHC-25897 -DWRONG_EXISTENTIAL
    reject GHC-25897 -DWRONG_UNARY
    reject GHC-70566 -DRIGID
    grep -Fq 'case on the rigid argument x' wrong.log
    reject GHC-83605 -DRULE -fenable-rewrite-rules

    cd ../provider
    sed 's/= substTerm (/= value `seq` substTerm (/' Plain.hs >Syntax.hs
    "${hc[@]}" "${provider[@]}" -fforce-recomp -c Syntax.hs
    cp Syntax.hi ../reader/
    cd ../reader
    reject GHC-70566
    grep -Fq 'case on the rigid argument x' wrong.log
done

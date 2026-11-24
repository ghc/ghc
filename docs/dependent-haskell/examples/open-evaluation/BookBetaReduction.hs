{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, EmptyCase, GADTs, PolyKinds #-}
{-# LANGUAGE Modifiers, RankNTypes, StandaloneKindSignatures, TypeAbstractions, TypeApplications, TypeOperators #-}
-- | Progress and capture-avoiding beta reduction for the PLFH DeBruijn terms.
module BookBetaReduction (Step(..), Progress(..), progress, progressExamples,
                          captureAvoided, freeIndexAfterBeta, replacementOrder,
                          weakenUnderLambda, substUnderLambda, weakenSubstUnderLambda) where

import BookDeBruijn
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)

-- | One call-by-value reduction. The endpoints have the same context and type.
type Step :: forall (g :: Ctx) (a :: Ty). Term g a -> Term g a -> Type
data Step m n where
    XiApp1 :: Step l l' -> Step (App l m) (App l' m)
    XiApp2 :: Value l -> Step m m' -> Step (App l m) (App l m')
    XiSuc  :: Step m m' -> Step (Suc m) (Suc m')
    XiCase :: Step l l' -> Step (Case l m n) (Case l' m n)

    BetaLam :: forall g a b (n :: Term (CExt g a) b) (w :: Term g a).
               Value w -> Step (App (Lam n) w) (substSingle n w)
    BetaZero :: forall g a (m :: Term g a) (n :: Term (CExt g TNat) a).
                Step (Case Zero m n) m
    BetaSuc :: forall g a (m :: Term g a) (n :: Term (CExt g TNat) a)
                          (v :: Term g TNat).
               Value v -> Step (Case (Suc v) m n) (substSingle n v)
    BetaMu :: forall g a (n :: Term (CExt g a) a).
              Step (Mu n) (substSingle n (Mu n))

-- | A value witness or a step to another term of the same type.
type Progress :: forall (a :: Ty). Term CEmpty a -> Type
data Progress m where
    PStep :: Step m n -> Progress m
    PDone :: Value m -> Progress m

-- | Prove progress for a closed term by inspecting its outer constructor.
progress :: forall (a :: Ty). foreach (m :: Term CEmpty a) -> Progress m
progress t = case t of
    Var x -> case x of {}
    Lam _ -> PDone VLam
    App l m -> case progress l of
        PStep s -> appLeft s
        PDone v -> case progress m of
            PStep s -> appRight v s
            PDone w -> appBeta v w
    Zero -> PDone VZero
    Suc n -> case progress n of
        PStep s -> sucStep s
        PDone v -> PDone (VSuc v)
    Case l m n -> case progress l of
        PStep s -> caseStep @a @l @_ @m @n s
        PDone v -> caseBeta @a @l @m @n v
    Mu n -> muStep @a @n

-- | Reduce the function position first.
appLeft :: forall a b (l :: Term CEmpty (TArr a b))
                     (l' :: Term CEmpty (TArr a b)) (m :: Term CEmpty a).
           Step l l' -> Progress (App l m)
appLeft s = PStep (XiApp1 s)

-- | Reduce the argument once the function is a value.
appRight :: forall a b (l :: Term CEmpty (TArr a b))
                      (m :: Term CEmpty a) (m' :: Term CEmpty a).
            Value l -> Step m m' -> Progress (App l m)
appRight v s = PStep (XiApp2 v s)

-- | A function value is a lambda, so substitute its argument.
appBeta :: forall a b (l :: Term CEmpty (TArr a b)) (m :: Term CEmpty a).
           Value l -> Value m -> Progress (App l m)
appBeta (VLam @n) w = PStep (BetaLam @CEmpty @a @b @n @m w)

-- | Lift a step through the successor constructor.
sucStep :: forall (n :: Term CEmpty TNat) (n' :: Term CEmpty TNat).
           Step n n' -> Progress (Suc n)
sucStep s = PStep (XiSuc s)

-- | Reduce the scrutinee before selecting a case branch.
caseStep :: forall a (l :: Term CEmpty TNat) (l' :: Term CEmpty TNat)
                     (m :: Term CEmpty a) (n :: Term (CExt CEmpty TNat) a).
            Step l l' -> Progress (Case l m n)
caseStep s = PStep (XiCase s)

-- | Select the branch for zero or substitute the predecessor.
caseBeta :: forall a (l :: Term CEmpty TNat) (m :: Term CEmpty a)
                     (n :: Term (CExt CEmpty TNat) a).
            Value l -> Progress (Case l m n)
caseBeta VZero = PStep (BetaZero @CEmpty @a @m @n)
caseBeta (VSuc @v w) = PStep (BetaSuc @CEmpty @a @m @n @v w)

-- | Unfold the fixed point by substituting it into its body.
muStep :: forall a (n :: Term (CExt CEmpty a) a). Progress (Mu n)
muStep = PStep (BetaMu @CEmpty @a @n)

-- | The scrutinee and the zero branch select different variables of the same type.
replacementOrder :: foreach (older :: Term CEmpty TNat) -> foreach (newer :: Term CEmpty TNat)
                 -> substDouble (Case (Var Z) (Var (S Z)) (Var Z)) older newer
                    :~: Case newer older (Var Z)
replacementOrder _ _ = Refl

type Outer = CExt CEmpty TNat

type Renaming g d = forall a. Lookup g a %U -> Lookup d a

-- | Renaming keeps the lambda-bound variable and renames the outer variable.
renameUnderLambda :: forall g d (r :: Renaming g d) (v :: Lookup g TNat).
    rename r (Lam (Case (Var Z) (Var (S v)) Zero) :: Term g (TArr TNat TNat))
    :~: (Lam (Case (Var Z) (Var (S (r v))) Zero) :: Term d (TArr TNat TNat))
renameUnderLambda = Refl

type Shift :: forall (a :: Ty). @U Lookup Outer a %U -> Lookup (CExt Outer TNat) a
type Shift x = S x

-- | Add one outer variable. The lookup bound by the lambda remains zero.
weakenUnderLambda ::
    rename Shift (Lam (Case (Var Z) (Var (S Z)) Zero) :: Term Outer (TArr TNat TNat))
    :~: (Lam (Case (Var Z) (Var (S (S Z))) Zero) :: Term (CExt Outer TNat) (TArr TNat TNat))
weakenUnderLambda = renameUnderLambda @Outer @(CExt Outer TNat) @Shift @Z

type Substitution g d = forall a. Lookup g a %U -> Term d a

-- | Open substitution keeps the lambda-bound variable and weakens the outer image.
substUnderLambda :: forall g d (s :: Substitution g d) (v :: Lookup g TNat).
    subst s (Lam (Case (Var Z) (Var (S v)) Zero) :: Term g (TArr TNat TNat))
    :~: (Lam (Case (Var Z) (rename (weakenLookup @d @TNat) (s v)) Zero) :: Term d (TArr TNat TNat))
substUnderLambda = Refl

type WeakenTerm :: forall (a :: Ty). @U Lookup Outer a %U -> Term (CExt Outer TNat) a
type WeakenTerm x = Var (S x)

-- | Substitute an open variable under a lambda and shift its outer lookup.
weakenSubstUnderLambda ::
    subst WeakenTerm (Lam (Case (Var Z) (Var (S Z)) Zero) :: Term Outer (TArr TNat TNat))
    :~: (Lam (Case (Var Z) (Var (S (S Z))) Zero) :: Term (CExt Outer TNat) (TArr TNat TNat))
weakenSubstUnderLambda = substUnderLambda @Outer @(CExt Outer TNat) @WeakenTerm @Z

type Fn = TArr TNat TNat
type Argument = Lam (Var (S Z)) :: Term Outer Fn
type Body = Lam (Var (S Z)) :: Term (CExt Outer Fn) (TArr TNat Fn)

-- | The free variable in the argument crosses one extra binder.
captureAvoided :: Step (App (Lam Body) Argument) (Lam (Lam (Var (S (S Z)))))
captureAvoided = BetaLam @Outer @Fn @(TArr TNat Fn) @Body @Argument VLam

-- | Read the index of the variable in the ordinary substitution result.
freeIndexAfterBeta :: Maybe Int
freeIndexAfterBeta = case substSingle body argument of
    Lam (Lam (Var x)) -> Just (distance x)
    _ -> Nothing
  where
    body :: Term (CExt Outer Fn) (TArr TNat Fn)
    body = Lam (Var (S Z))
    argument :: Term Outer Fn
    argument = Lam (Var (S Z))
    distance :: Lookup g a -> Int
    distance Z = 0
    distance (S i) = 1 + distance i

-- | Observe the progress proof for four closed natural-number terms.
progressExamples :: [String]
progressExamples = map rule [App (Lam (Var Z)) Zero, Case (Suc Zero) Zero (Var Z),
                             Mu (Var Z), Zero]
  where
    rule :: Term CEmpty TNat -> String
    rule t = case progress t of
        PDone _         -> "value"
        PStep XiApp1{}  -> "step function"
        PStep XiApp2{}  -> "step argument"
        PStep XiSuc{}   -> "step successor"
        PStep XiCase{}  -> "step scrutinee"
        PStep BetaLam{} -> "beta"
        PStep BetaZero  -> "case zero"
        PStep BetaSuc{} -> "case successor"
        PStep BetaMu    -> "unfold"

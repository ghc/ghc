(* Proof StoreT forms a comonad -- Russell O'Connor *)

Set Implict Arguments.
Unset Strict Implicit.

Require Import FunctionalExtensionality.

Record Comonad (w : Type -> Type) : Type :=
 { extract : forall a, w a -> a
 ; extend : forall a b, (w a -> b) -> w a -> w b
 ; law1 : forall a x, extend _ _ (extract a) x = x
 ; law2 : forall a b f x, extract b (extend a _ f x) = f x
 ; law3 : forall a b c f g x, extend b c f (extend a b g x) = extend a c (fun y => f (extend a b g y)) x
 }.

Section StoreT.

Variables (s : Type) (w:Type -> Type).
Hypothesis wH : Comonad w.

Definition map a b f x := extend _ wH a b (fun y => f (extract _ wH _ y)) x.

Lemma map_extend : forall a b c f g x, map b c f (extend _ wH a b g x) = extend _ wH _ _ (fun y => f (g y)) x.
Proof.
intros a b c f g x.
unfold map.
rewrite law3.
apply equal_f.
apply f_equal.
extensionality y.
rewrite law2.
reflexivity.
Qed.

Record StoreT (a:Type): Type := mkStoreT
  {store : w (s -> a)
  ;loc   : s}.

Definition extractST a (x:StoreT a) : a := 
 extract _ wH _ (store _ x) (loc _ x).

Definition mapST a b (f:a -> b) (x:StoreT a) : StoreT b :=
 mkStoreT _ (map _ _ (fun g x => f (g x)) (store _ x)) (loc _ x).

Definition duplicateST a (x:StoreT a) : StoreT (StoreT a) :=
 mkStoreT _ (extend _ wH _ _ (mkStoreT _) (store _ x)) (loc _ x).

Let extendST := fun a b f x => mapST _ b f (duplicateST a x).

Lemma law1ST : forall a x, extendST _ _ (extractST a) x = x.
Proof.
intros a [v b].
unfold extractST, extendST, duplicateST, mapST.
simpl.
rewrite map_extend.
simpl.
replace (fun (y : w (s -> a)) (x : s) => extract w wH (s -> a) y x)
 with (extract w wH (s -> a)).
 rewrite law1.
 reflexivity.
extensionality y.
extensionality x.
reflexivity.
Qed.

Lemma law2ST : forall a b f x, extractST b (extendST a _ f x) = f x.
Proof.
intros a b f [v c].
unfold extendST, mapST, extractST.
simpl.
rewrite map_extend.
rewrite law2.
reflexivity.
Qed.

Lemma law3ST : forall a b c f g x, extendST b c f (extendST a b g x) = extendST a c (fun y => f (extendST a b g y)) x.
Proof.
intros a b c f g [v d].
unfold extendST, mapST, extractST.
simpl.
repeat rewrite map_extend.
rewrite law3.
repeat (apply equal_f||apply f_equal).
extensionality y.
extensionality x.
rewrite map_extend.
reflexivity.
Qed.

Definition StoreTComonad : Comonad StoreT :=
 Build_Comonad _ _ _ law1ST law2ST law3ST.

End StoreT.

Check StoreTComonad.




type kind = 
    ARROW of kind * kind
  | TYP

type tycon = 
  | TyVar of int
  | FUN
  | LIST
  | STRING

type typ = 
    TyForall of kind * typ
  | TyApp of tycon * typ list

type exp = 
  | AbsTm of typ * exp
  | Var of int
  | App of exp * exp
  | String of string
  | AbsTy of kind * exp
  | AppTy of exp * typ

type ttyp =
  | TTyFun of ttyp * ttyp
  | TTyList of ttyp
  | TTyString
  | TTyAny
  | TTyVar of int
  | TTyForall of ttyp

type texp =
  | TAbsTm of  ttyp * texp
  | TVar of int
  | TApp of texp * texp
  | TString of string
  | TLetTy of texp * texp
  | TCast of texp * ttyp

  | TAppTy of texp * ttyp
  | TAbsTy of texp

 
let (-->) x y = TyApp (FUN, [x;y])
let (--->) x y = TTyFun (x,y)

let rec trans_kind = function
    ARROW (k1,k2) -> (trans_kind k1 ---> trans_kind k2)
  | TYP -> (TTyForall TANY ---> TTyAny)

let rec trans_typ_arg_aux = function
    (* TyForall (k,ty) -> TAbsTm (trans_kind k, TAbsTy (trans_typ ty)) ??? *)
  | TyApp (TyVar tv, args) -> failwith "unreduced"
  | ty -> TAbsTm (trans_kind k, TAbsTy (trans_typ ty))failwith "unreduced"
  | 
let rec trans_typ_arg env = function
  | TyApp (FUN, []) -> 
      TAbsTm 
	(trans_kind TYP, 
	 TLetTy (TVar 0, 
		 TAbsTm 
		   (trans_kind TYP, 
		    TLetTy (TVar 0, 
			    TAbsTm 
			      (TTyForall TANY, 
			       TAppTy (TVar 0, TTyFun (TTyVar 0, TTyVar 1)))))))
  | TyApp (TyVar tv, args) -> 
      try List.assoc (tv,args) env 
      with Not_found -> failwith "trans_typ: unreduced type variable"
  | ty -> TAbsTm (TTyForall TANY, TAppTy (TVar 0, trans_typ env ty))
(*
  | TyApp (STRING, []) -> TAbsTm (TTyForall TANY, TAppTy (TVar 0, TTyString))
  | TyApp (FUN, [l;r]) -> TAbsTm (TTyForall TANY, TAppTy (TVar 0, TTyFun (trans_typ l, trans_typ r)))
*)


let rec trans_typ env = function
    TyForall (k,ty) -> (trans_kind k ---> TTyAny)
  | TyApp (TyVar tv, args) -> 
      try List.assoc (tv,args) env 
      with Not_found -> failwith "trans_typ: unreduced type variable"
  | TyApp (FUN, [l;r]) -> TTyFun (trans_typ env l, trans_typ env r)
  | TyApp (STRING, []) -> TTyString
  | _ -> failwith "trans_typ: badly formed input type"


let rec trans_exp env = function
  | AbsTm (ty,e) -> TAbsTm(trans_typ ty, trans_exp e)
  | Var n -> TVar n
  | App (l,r) -> TApp(trans_exp l, trans_exp r)
  | String s -> TString s
  | AbsTy (k,e) -> TAbsTm(trans_kind k, reduce env e)
  | AppTy (tm,ty) -> TAppTy(trans_exp tm, trans_typ_arg env ty)


open Format;;


let rec pp_print_exp pps = function
    L e -> fprintf pps "\

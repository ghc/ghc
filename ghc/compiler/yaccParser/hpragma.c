

#include "hspincl.h"
#include "yaccParser/hpragma.h"

Thpragma thpragma(t)
 hpragma t;
{
	return(t -> tag);
}


/************** no_pragma ******************/

hpragma mkno_pragma(void)
{
	register struct Sno_pragma *pp =
		(struct Sno_pragma *) malloc(sizeof(struct Sno_pragma));
	pp -> tag = no_pragma;
	return((hpragma)pp);
}

/************** idata_pragma ******************/

hpragma mkidata_pragma(PPgprag_data_constrs, PPgprag_data_specs)
 list PPgprag_data_constrs;
 list PPgprag_data_specs;
{
	register struct Sidata_pragma *pp =
		(struct Sidata_pragma *) malloc(sizeof(struct Sidata_pragma));
	pp -> tag = idata_pragma;
	pp -> Xgprag_data_constrs = PPgprag_data_constrs;
	pp -> Xgprag_data_specs = PPgprag_data_specs;
	return((hpragma)pp);
}

list *Rgprag_data_constrs(t)
 struct Sidata_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != idata_pragma)
		fprintf(stderr,"gprag_data_constrs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_data_constrs);
}

list *Rgprag_data_specs(t)
 struct Sidata_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != idata_pragma)
		fprintf(stderr,"gprag_data_specs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_data_specs);
}

/************** itype_pragma ******************/

hpragma mkitype_pragma(void)
{
	register struct Sitype_pragma *pp =
		(struct Sitype_pragma *) malloc(sizeof(struct Sitype_pragma));
	pp -> tag = itype_pragma;
	return((hpragma)pp);
}

/************** iclas_pragma ******************/

hpragma mkiclas_pragma(PPgprag_clas)
 list PPgprag_clas;
{
	register struct Siclas_pragma *pp =
		(struct Siclas_pragma *) malloc(sizeof(struct Siclas_pragma));
	pp -> tag = iclas_pragma;
	pp -> Xgprag_clas = PPgprag_clas;
	return((hpragma)pp);
}

list *Rgprag_clas(t)
 struct Siclas_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iclas_pragma)
		fprintf(stderr,"gprag_clas: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_clas);
}

/************** iclasop_pragma ******************/

hpragma mkiclasop_pragma(PPgprag_dsel, PPgprag_defm)
 hpragma PPgprag_dsel;
 hpragma PPgprag_defm;
{
	register struct Siclasop_pragma *pp =
		(struct Siclasop_pragma *) malloc(sizeof(struct Siclasop_pragma));
	pp -> tag = iclasop_pragma;
	pp -> Xgprag_dsel = PPgprag_dsel;
	pp -> Xgprag_defm = PPgprag_defm;
	return((hpragma)pp);
}

hpragma *Rgprag_dsel(t)
 struct Siclasop_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iclasop_pragma)
		fprintf(stderr,"gprag_dsel: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_dsel);
}

hpragma *Rgprag_defm(t)
 struct Siclasop_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iclasop_pragma)
		fprintf(stderr,"gprag_defm: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_defm);
}

/************** iinst_simpl_pragma ******************/

hpragma mkiinst_simpl_pragma(PPgprag_imod_simpl, PPgprag_dfun_simpl)
 stringId PPgprag_imod_simpl;
 hpragma PPgprag_dfun_simpl;
{
	register struct Siinst_simpl_pragma *pp =
		(struct Siinst_simpl_pragma *) malloc(sizeof(struct Siinst_simpl_pragma));
	pp -> tag = iinst_simpl_pragma;
	pp -> Xgprag_imod_simpl = PPgprag_imod_simpl;
	pp -> Xgprag_dfun_simpl = PPgprag_dfun_simpl;
	return((hpragma)pp);
}

stringId *Rgprag_imod_simpl(t)
 struct Siinst_simpl_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_simpl_pragma)
		fprintf(stderr,"gprag_imod_simpl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_imod_simpl);
}

hpragma *Rgprag_dfun_simpl(t)
 struct Siinst_simpl_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_simpl_pragma)
		fprintf(stderr,"gprag_dfun_simpl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_dfun_simpl);
}

/************** iinst_const_pragma ******************/

hpragma mkiinst_const_pragma(PPgprag_imod_const, PPgprag_dfun_const, PPgprag_constms)
 stringId PPgprag_imod_const;
 hpragma PPgprag_dfun_const;
 list PPgprag_constms;
{
	register struct Siinst_const_pragma *pp =
		(struct Siinst_const_pragma *) malloc(sizeof(struct Siinst_const_pragma));
	pp -> tag = iinst_const_pragma;
	pp -> Xgprag_imod_const = PPgprag_imod_const;
	pp -> Xgprag_dfun_const = PPgprag_dfun_const;
	pp -> Xgprag_constms = PPgprag_constms;
	return((hpragma)pp);
}

stringId *Rgprag_imod_const(t)
 struct Siinst_const_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_const_pragma)
		fprintf(stderr,"gprag_imod_const: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_imod_const);
}

hpragma *Rgprag_dfun_const(t)
 struct Siinst_const_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_const_pragma)
		fprintf(stderr,"gprag_dfun_const: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_dfun_const);
}

list *Rgprag_constms(t)
 struct Siinst_const_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_const_pragma)
		fprintf(stderr,"gprag_constms: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_constms);
}

/************** igen_pragma ******************/

hpragma mkigen_pragma(PPgprag_arity, PPgprag_update, PPgprag_deforest, PPgprag_strictness, PPgprag_unfolding, PPgprag_specs)
 hpragma PPgprag_arity;
 hpragma PPgprag_update;
 hpragma PPgprag_deforest;
 hpragma PPgprag_strictness;
 hpragma PPgprag_unfolding;
 list PPgprag_specs;
{
	register struct Sigen_pragma *pp =
		(struct Sigen_pragma *) malloc(sizeof(struct Sigen_pragma));
	pp -> tag = igen_pragma;
	pp -> Xgprag_arity = PPgprag_arity;
	pp -> Xgprag_update = PPgprag_update;
	pp -> Xgprag_deforest = PPgprag_deforest;
	pp -> Xgprag_strictness = PPgprag_strictness;
	pp -> Xgprag_unfolding = PPgprag_unfolding;
	pp -> Xgprag_specs = PPgprag_specs;
	return((hpragma)pp);
}

hpragma *Rgprag_arity(t)
 struct Sigen_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_arity: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_arity);
}

hpragma *Rgprag_update(t)
 struct Sigen_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_update: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_update);
}

hpragma *Rgprag_deforest(t)
 struct Sigen_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_deforest: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_deforest);
}

hpragma *Rgprag_strictness(t)
 struct Sigen_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_strictness: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_strictness);
}

hpragma *Rgprag_unfolding(t)
 struct Sigen_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_unfolding: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfolding);
}

list *Rgprag_specs(t)
 struct Sigen_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_specs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_specs);
}

/************** iarity_pragma ******************/

hpragma mkiarity_pragma(PPgprag_arity_val)
 numId PPgprag_arity_val;
{
	register struct Siarity_pragma *pp =
		(struct Siarity_pragma *) malloc(sizeof(struct Siarity_pragma));
	pp -> tag = iarity_pragma;
	pp -> Xgprag_arity_val = PPgprag_arity_val;
	return((hpragma)pp);
}

numId *Rgprag_arity_val(t)
 struct Siarity_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iarity_pragma)
		fprintf(stderr,"gprag_arity_val: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_arity_val);
}

/************** iupdate_pragma ******************/

hpragma mkiupdate_pragma(PPgprag_update_val)
 stringId PPgprag_update_val;
{
	register struct Siupdate_pragma *pp =
		(struct Siupdate_pragma *) malloc(sizeof(struct Siupdate_pragma));
	pp -> tag = iupdate_pragma;
	pp -> Xgprag_update_val = PPgprag_update_val;
	return((hpragma)pp);
}

stringId *Rgprag_update_val(t)
 struct Siupdate_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iupdate_pragma)
		fprintf(stderr,"gprag_update_val: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_update_val);
}

/************** ideforest_pragma ******************/

hpragma mkideforest_pragma(void)
{
	register struct Sideforest_pragma *pp =
		(struct Sideforest_pragma *) malloc(sizeof(struct Sideforest_pragma));
	pp -> tag = ideforest_pragma;
	return((hpragma)pp);
}

/************** istrictness_pragma ******************/

hpragma mkistrictness_pragma(PPgprag_strict_spec, PPgprag_strict_wrkr)
 hstring PPgprag_strict_spec;
 hpragma PPgprag_strict_wrkr;
{
	register struct Sistrictness_pragma *pp =
		(struct Sistrictness_pragma *) malloc(sizeof(struct Sistrictness_pragma));
	pp -> tag = istrictness_pragma;
	pp -> Xgprag_strict_spec = PPgprag_strict_spec;
	pp -> Xgprag_strict_wrkr = PPgprag_strict_wrkr;
	return((hpragma)pp);
}

hstring *Rgprag_strict_spec(t)
 struct Sistrictness_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != istrictness_pragma)
		fprintf(stderr,"gprag_strict_spec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_strict_spec);
}

hpragma *Rgprag_strict_wrkr(t)
 struct Sistrictness_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != istrictness_pragma)
		fprintf(stderr,"gprag_strict_wrkr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_strict_wrkr);
}

/************** imagic_unfolding_pragma ******************/

hpragma mkimagic_unfolding_pragma(PPgprag_magic_str)
 stringId PPgprag_magic_str;
{
	register struct Simagic_unfolding_pragma *pp =
		(struct Simagic_unfolding_pragma *) malloc(sizeof(struct Simagic_unfolding_pragma));
	pp -> tag = imagic_unfolding_pragma;
	pp -> Xgprag_magic_str = PPgprag_magic_str;
	return((hpragma)pp);
}

stringId *Rgprag_magic_str(t)
 struct Simagic_unfolding_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != imagic_unfolding_pragma)
		fprintf(stderr,"gprag_magic_str: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_magic_str);
}

/************** iunfolding_pragma ******************/

hpragma mkiunfolding_pragma(PPgprag_unfold_guide, PPgprag_unfold_core)
 hpragma PPgprag_unfold_guide;
 coresyn PPgprag_unfold_core;
{
	register struct Siunfolding_pragma *pp =
		(struct Siunfolding_pragma *) malloc(sizeof(struct Siunfolding_pragma));
	pp -> tag = iunfolding_pragma;
	pp -> Xgprag_unfold_guide = PPgprag_unfold_guide;
	pp -> Xgprag_unfold_core = PPgprag_unfold_core;
	return((hpragma)pp);
}

hpragma *Rgprag_unfold_guide(t)
 struct Siunfolding_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfolding_pragma)
		fprintf(stderr,"gprag_unfold_guide: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_guide);
}

coresyn *Rgprag_unfold_core(t)
 struct Siunfolding_pragma *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfolding_pragma)
		fprintf(stderr,"gprag_unfold_core: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_core);
}

/************** iunfold_always ******************/

hpragma mkiunfold_always(void)
{
	register struct Siunfold_always *pp =
		(struct Siunfold_always *) malloc(sizeof(struct Siunfold_always));
	pp -> tag = iunfold_always;
	return((hpragma)pp);
}

/************** iunfold_if_args ******************/

hpragma mkiunfold_if_args(PPgprag_unfold_if_t_args, PPgprag_unfold_if_v_args, PPgprag_unfold_if_con_args, PPgprag_unfold_if_size)
 numId PPgprag_unfold_if_t_args;
 numId PPgprag_unfold_if_v_args;
 stringId PPgprag_unfold_if_con_args;
 numId PPgprag_unfold_if_size;
{
	register struct Siunfold_if_args *pp =
		(struct Siunfold_if_args *) malloc(sizeof(struct Siunfold_if_args));
	pp -> tag = iunfold_if_args;
	pp -> Xgprag_unfold_if_t_args = PPgprag_unfold_if_t_args;
	pp -> Xgprag_unfold_if_v_args = PPgprag_unfold_if_v_args;
	pp -> Xgprag_unfold_if_con_args = PPgprag_unfold_if_con_args;
	pp -> Xgprag_unfold_if_size = PPgprag_unfold_if_size;
	return((hpragma)pp);
}

numId *Rgprag_unfold_if_t_args(t)
 struct Siunfold_if_args *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfold_if_args)
		fprintf(stderr,"gprag_unfold_if_t_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_if_t_args);
}

numId *Rgprag_unfold_if_v_args(t)
 struct Siunfold_if_args *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfold_if_args)
		fprintf(stderr,"gprag_unfold_if_v_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_if_v_args);
}

stringId *Rgprag_unfold_if_con_args(t)
 struct Siunfold_if_args *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfold_if_args)
		fprintf(stderr,"gprag_unfold_if_con_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_if_con_args);
}

numId *Rgprag_unfold_if_size(t)
 struct Siunfold_if_args *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfold_if_args)
		fprintf(stderr,"gprag_unfold_if_size: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_if_size);
}

/************** iname_pragma_pr ******************/

hpragma mkiname_pragma_pr(PPgprag_name_pr1, PPgprag_name_pr2)
 unkId PPgprag_name_pr1;
 hpragma PPgprag_name_pr2;
{
	register struct Siname_pragma_pr *pp =
		(struct Siname_pragma_pr *) malloc(sizeof(struct Siname_pragma_pr));
	pp -> tag = iname_pragma_pr;
	pp -> Xgprag_name_pr1 = PPgprag_name_pr1;
	pp -> Xgprag_name_pr2 = PPgprag_name_pr2;
	return((hpragma)pp);
}

unkId *Rgprag_name_pr1(t)
 struct Siname_pragma_pr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iname_pragma_pr)
		fprintf(stderr,"gprag_name_pr1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_name_pr1);
}

hpragma *Rgprag_name_pr2(t)
 struct Siname_pragma_pr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != iname_pragma_pr)
		fprintf(stderr,"gprag_name_pr2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_name_pr2);
}

/************** itype_pragma_pr ******************/

hpragma mkitype_pragma_pr(PPgprag_type_pr1, PPgprag_type_pr2, PPgprag_type_pr3)
 list PPgprag_type_pr1;
 numId PPgprag_type_pr2;
 hpragma PPgprag_type_pr3;
{
	register struct Sitype_pragma_pr *pp =
		(struct Sitype_pragma_pr *) malloc(sizeof(struct Sitype_pragma_pr));
	pp -> tag = itype_pragma_pr;
	pp -> Xgprag_type_pr1 = PPgprag_type_pr1;
	pp -> Xgprag_type_pr2 = PPgprag_type_pr2;
	pp -> Xgprag_type_pr3 = PPgprag_type_pr3;
	return((hpragma)pp);
}

list *Rgprag_type_pr1(t)
 struct Sitype_pragma_pr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != itype_pragma_pr)
		fprintf(stderr,"gprag_type_pr1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_type_pr1);
}

numId *Rgprag_type_pr2(t)
 struct Sitype_pragma_pr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != itype_pragma_pr)
		fprintf(stderr,"gprag_type_pr2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_type_pr2);
}

hpragma *Rgprag_type_pr3(t)
 struct Sitype_pragma_pr *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != itype_pragma_pr)
		fprintf(stderr,"gprag_type_pr3: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_type_pr3);
}

/************** idata_pragma_4s ******************/

hpragma mkidata_pragma_4s(PPgprag_data_spec)
 list PPgprag_data_spec;
{
	register struct Sidata_pragma_4s *pp =
		(struct Sidata_pragma_4s *) malloc(sizeof(struct Sidata_pragma_4s));
	pp -> tag = idata_pragma_4s;
	pp -> Xgprag_data_spec = PPgprag_data_spec;
	return((hpragma)pp);
}

list *Rgprag_data_spec(t)
 struct Sidata_pragma_4s *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != idata_pragma_4s)
		fprintf(stderr,"gprag_data_spec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_data_spec);
}

#ifndef hpragma_defined
#define hpragma_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	no_pragma,
	idata_pragma,
	itype_pragma,
	iclas_pragma,
	iclasop_pragma,
	iinst_simpl_pragma,
	iinst_const_pragma,
	iinst_spec_pragma,
	igen_pragma,
	iarity_pragma,
	iupdate_pragma,
	ideforest_pragma,
	istrictness_pragma,
	imagic_unfolding_pragma,
	iunfolding_pragma,
	iunfold_always,
	iunfold_if_args,
	iname_pragma_pr,
	itype_pragma_pr,
	iinst_pragma_3s,
	idata_pragma_4s
} Thpragma;

typedef struct { Thpragma tag; } *hpragma;

#ifdef __GNUC__
Thpragma thpragma(hpragma t);
extern __inline__ Thpragma thpragma(hpragma t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Thpragma thpragma PROTO((hpragma));
#endif /* ! __GNUC__ */

struct Sno_pragma {
	Thpragma tag;
};

struct Sidata_pragma {
	Thpragma tag;
	list Xgprag_data_constrs;
	list Xgprag_data_specs;
};

struct Sitype_pragma {
	Thpragma tag;
};

struct Siclas_pragma {
	Thpragma tag;
	list Xgprag_clas;
};

struct Siclasop_pragma {
	Thpragma tag;
	hpragma Xgprag_dsel;
	hpragma Xgprag_defm;
};

struct Siinst_simpl_pragma {
	Thpragma tag;
	stringId Xgprag_imod_simpl;
	hpragma Xgprag_dfun_simpl;
};

struct Siinst_const_pragma {
	Thpragma tag;
	stringId Xgprag_imod_const;
	hpragma Xgprag_dfun_const;
	list Xgprag_constms;
};

struct Siinst_spec_pragma {
	Thpragma tag;
	stringId Xgprag_imod_spec;
	hpragma Xgprag_dfun_spec;
	list Xgprag_inst_specs;
};

struct Sigen_pragma {
	Thpragma tag;
	hpragma Xgprag_arity;
	hpragma Xgprag_update;
	hpragma Xgprag_deforest;
	hpragma Xgprag_strictness;
	hpragma Xgprag_unfolding;
	list Xgprag_specs;
};

struct Siarity_pragma {
	Thpragma tag;
	numId Xgprag_arity_val;
};

struct Siupdate_pragma {
	Thpragma tag;
	stringId Xgprag_update_val;
};

struct Sideforest_pragma {
	Thpragma tag;
};

struct Sistrictness_pragma {
	Thpragma tag;
	hstring Xgprag_strict_spec;
	hpragma Xgprag_strict_wrkr;
};

struct Simagic_unfolding_pragma {
	Thpragma tag;
	stringId Xgprag_magic_str;
};

struct Siunfolding_pragma {
	Thpragma tag;
	hpragma Xgprag_unfold_guide;
	coresyn Xgprag_unfold_core;
};

struct Siunfold_always {
	Thpragma tag;
};

struct Siunfold_if_args {
	Thpragma tag;
	numId Xgprag_unfold_if_t_args;
	numId Xgprag_unfold_if_v_args;
	stringId Xgprag_unfold_if_con_args;
	numId Xgprag_unfold_if_size;
};

struct Siname_pragma_pr {
	Thpragma tag;
	unkId Xgprag_name_pr1;
	hpragma Xgprag_name_pr2;
};

struct Sitype_pragma_pr {
	Thpragma tag;
	list Xgprag_type_pr1;
	numId Xgprag_type_pr2;
	hpragma Xgprag_type_pr3;
};

struct Siinst_pragma_3s {
	Thpragma tag;
	list Xgprag_inst_pt1;
	numId Xgprag_inst_pt2;
	hpragma Xgprag_inst_pt3;
	list Xgprag_inst_pt4;
};

struct Sidata_pragma_4s {
	Thpragma tag;
	list Xgprag_data_spec;
};

extern hpragma mkno_pragma PROTO((void));

extern hpragma mkidata_pragma PROTO((list, list));
#ifdef __GNUC__

list *Rgprag_data_constrs PROTO((struct Sidata_pragma *));

extern __inline__ list *Rgprag_data_constrs(struct Sidata_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != idata_pragma)
		fprintf(stderr,"gprag_data_constrs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_data_constrs);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_data_constrs PROTO((struct Sidata_pragma *));
#endif /* ! __GNUC__ */

#define gprag_data_constrs(xyzxyz) (*Rgprag_data_constrs((struct Sidata_pragma *) (xyzxyz)))
#ifdef __GNUC__

list *Rgprag_data_specs PROTO((struct Sidata_pragma *));

extern __inline__ list *Rgprag_data_specs(struct Sidata_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != idata_pragma)
		fprintf(stderr,"gprag_data_specs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_data_specs);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_data_specs PROTO((struct Sidata_pragma *));
#endif /* ! __GNUC__ */

#define gprag_data_specs(xyzxyz) (*Rgprag_data_specs((struct Sidata_pragma *) (xyzxyz)))

extern hpragma mkitype_pragma PROTO((void));

extern hpragma mkiclas_pragma PROTO((list));
#ifdef __GNUC__

list *Rgprag_clas PROTO((struct Siclas_pragma *));

extern __inline__ list *Rgprag_clas(struct Siclas_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iclas_pragma)
		fprintf(stderr,"gprag_clas: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_clas);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_clas PROTO((struct Siclas_pragma *));
#endif /* ! __GNUC__ */

#define gprag_clas(xyzxyz) (*Rgprag_clas((struct Siclas_pragma *) (xyzxyz)))

extern hpragma mkiclasop_pragma PROTO((hpragma, hpragma));
#ifdef __GNUC__

hpragma *Rgprag_dsel PROTO((struct Siclasop_pragma *));

extern __inline__ hpragma *Rgprag_dsel(struct Siclasop_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iclasop_pragma)
		fprintf(stderr,"gprag_dsel: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_dsel);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_dsel PROTO((struct Siclasop_pragma *));
#endif /* ! __GNUC__ */

#define gprag_dsel(xyzxyz) (*Rgprag_dsel((struct Siclasop_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_defm PROTO((struct Siclasop_pragma *));

extern __inline__ hpragma *Rgprag_defm(struct Siclasop_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iclasop_pragma)
		fprintf(stderr,"gprag_defm: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_defm);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_defm PROTO((struct Siclasop_pragma *));
#endif /* ! __GNUC__ */

#define gprag_defm(xyzxyz) (*Rgprag_defm((struct Siclasop_pragma *) (xyzxyz)))

extern hpragma mkiinst_simpl_pragma PROTO((stringId, hpragma));
#ifdef __GNUC__

stringId *Rgprag_imod_simpl PROTO((struct Siinst_simpl_pragma *));

extern __inline__ stringId *Rgprag_imod_simpl(struct Siinst_simpl_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_simpl_pragma)
		fprintf(stderr,"gprag_imod_simpl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_imod_simpl);
}
#else  /* ! __GNUC__ */
extern stringId *Rgprag_imod_simpl PROTO((struct Siinst_simpl_pragma *));
#endif /* ! __GNUC__ */

#define gprag_imod_simpl(xyzxyz) (*Rgprag_imod_simpl((struct Siinst_simpl_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_dfun_simpl PROTO((struct Siinst_simpl_pragma *));

extern __inline__ hpragma *Rgprag_dfun_simpl(struct Siinst_simpl_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_simpl_pragma)
		fprintf(stderr,"gprag_dfun_simpl: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_dfun_simpl);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_dfun_simpl PROTO((struct Siinst_simpl_pragma *));
#endif /* ! __GNUC__ */

#define gprag_dfun_simpl(xyzxyz) (*Rgprag_dfun_simpl((struct Siinst_simpl_pragma *) (xyzxyz)))

extern hpragma mkiinst_const_pragma PROTO((stringId, hpragma, list));
#ifdef __GNUC__

stringId *Rgprag_imod_const PROTO((struct Siinst_const_pragma *));

extern __inline__ stringId *Rgprag_imod_const(struct Siinst_const_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_const_pragma)
		fprintf(stderr,"gprag_imod_const: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_imod_const);
}
#else  /* ! __GNUC__ */
extern stringId *Rgprag_imod_const PROTO((struct Siinst_const_pragma *));
#endif /* ! __GNUC__ */

#define gprag_imod_const(xyzxyz) (*Rgprag_imod_const((struct Siinst_const_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_dfun_const PROTO((struct Siinst_const_pragma *));

extern __inline__ hpragma *Rgprag_dfun_const(struct Siinst_const_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_const_pragma)
		fprintf(stderr,"gprag_dfun_const: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_dfun_const);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_dfun_const PROTO((struct Siinst_const_pragma *));
#endif /* ! __GNUC__ */

#define gprag_dfun_const(xyzxyz) (*Rgprag_dfun_const((struct Siinst_const_pragma *) (xyzxyz)))
#ifdef __GNUC__

list *Rgprag_constms PROTO((struct Siinst_const_pragma *));

extern __inline__ list *Rgprag_constms(struct Siinst_const_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_const_pragma)
		fprintf(stderr,"gprag_constms: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_constms);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_constms PROTO((struct Siinst_const_pragma *));
#endif /* ! __GNUC__ */

#define gprag_constms(xyzxyz) (*Rgprag_constms((struct Siinst_const_pragma *) (xyzxyz)))

extern hpragma mkiinst_spec_pragma PROTO((stringId, hpragma, list));
#ifdef __GNUC__

stringId *Rgprag_imod_spec PROTO((struct Siinst_spec_pragma *));

extern __inline__ stringId *Rgprag_imod_spec(struct Siinst_spec_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_spec_pragma)
		fprintf(stderr,"gprag_imod_spec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_imod_spec);
}
#else  /* ! __GNUC__ */
extern stringId *Rgprag_imod_spec PROTO((struct Siinst_spec_pragma *));
#endif /* ! __GNUC__ */

#define gprag_imod_spec(xyzxyz) (*Rgprag_imod_spec((struct Siinst_spec_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_dfun_spec PROTO((struct Siinst_spec_pragma *));

extern __inline__ hpragma *Rgprag_dfun_spec(struct Siinst_spec_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_spec_pragma)
		fprintf(stderr,"gprag_dfun_spec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_dfun_spec);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_dfun_spec PROTO((struct Siinst_spec_pragma *));
#endif /* ! __GNUC__ */

#define gprag_dfun_spec(xyzxyz) (*Rgprag_dfun_spec((struct Siinst_spec_pragma *) (xyzxyz)))
#ifdef __GNUC__

list *Rgprag_inst_specs PROTO((struct Siinst_spec_pragma *));

extern __inline__ list *Rgprag_inst_specs(struct Siinst_spec_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_spec_pragma)
		fprintf(stderr,"gprag_inst_specs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_inst_specs);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_inst_specs PROTO((struct Siinst_spec_pragma *));
#endif /* ! __GNUC__ */

#define gprag_inst_specs(xyzxyz) (*Rgprag_inst_specs((struct Siinst_spec_pragma *) (xyzxyz)))

extern hpragma mkigen_pragma PROTO((hpragma, hpragma, hpragma, hpragma, hpragma, list));
#ifdef __GNUC__

hpragma *Rgprag_arity PROTO((struct Sigen_pragma *));

extern __inline__ hpragma *Rgprag_arity(struct Sigen_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_arity: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_arity);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_arity PROTO((struct Sigen_pragma *));
#endif /* ! __GNUC__ */

#define gprag_arity(xyzxyz) (*Rgprag_arity((struct Sigen_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_update PROTO((struct Sigen_pragma *));

extern __inline__ hpragma *Rgprag_update(struct Sigen_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_update: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_update);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_update PROTO((struct Sigen_pragma *));
#endif /* ! __GNUC__ */

#define gprag_update(xyzxyz) (*Rgprag_update((struct Sigen_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_deforest PROTO((struct Sigen_pragma *));

extern __inline__ hpragma *Rgprag_deforest(struct Sigen_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_deforest: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_deforest);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_deforest PROTO((struct Sigen_pragma *));
#endif /* ! __GNUC__ */

#define gprag_deforest(xyzxyz) (*Rgprag_deforest((struct Sigen_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_strictness PROTO((struct Sigen_pragma *));

extern __inline__ hpragma *Rgprag_strictness(struct Sigen_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_strictness: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_strictness);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_strictness PROTO((struct Sigen_pragma *));
#endif /* ! __GNUC__ */

#define gprag_strictness(xyzxyz) (*Rgprag_strictness((struct Sigen_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_unfolding PROTO((struct Sigen_pragma *));

extern __inline__ hpragma *Rgprag_unfolding(struct Sigen_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_unfolding: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfolding);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_unfolding PROTO((struct Sigen_pragma *));
#endif /* ! __GNUC__ */

#define gprag_unfolding(xyzxyz) (*Rgprag_unfolding((struct Sigen_pragma *) (xyzxyz)))
#ifdef __GNUC__

list *Rgprag_specs PROTO((struct Sigen_pragma *));

extern __inline__ list *Rgprag_specs(struct Sigen_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != igen_pragma)
		fprintf(stderr,"gprag_specs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_specs);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_specs PROTO((struct Sigen_pragma *));
#endif /* ! __GNUC__ */

#define gprag_specs(xyzxyz) (*Rgprag_specs((struct Sigen_pragma *) (xyzxyz)))

extern hpragma mkiarity_pragma PROTO((numId));
#ifdef __GNUC__

numId *Rgprag_arity_val PROTO((struct Siarity_pragma *));

extern __inline__ numId *Rgprag_arity_val(struct Siarity_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iarity_pragma)
		fprintf(stderr,"gprag_arity_val: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_arity_val);
}
#else  /* ! __GNUC__ */
extern numId *Rgprag_arity_val PROTO((struct Siarity_pragma *));
#endif /* ! __GNUC__ */

#define gprag_arity_val(xyzxyz) (*Rgprag_arity_val((struct Siarity_pragma *) (xyzxyz)))

extern hpragma mkiupdate_pragma PROTO((stringId));
#ifdef __GNUC__

stringId *Rgprag_update_val PROTO((struct Siupdate_pragma *));

extern __inline__ stringId *Rgprag_update_val(struct Siupdate_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iupdate_pragma)
		fprintf(stderr,"gprag_update_val: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_update_val);
}
#else  /* ! __GNUC__ */
extern stringId *Rgprag_update_val PROTO((struct Siupdate_pragma *));
#endif /* ! __GNUC__ */

#define gprag_update_val(xyzxyz) (*Rgprag_update_val((struct Siupdate_pragma *) (xyzxyz)))

extern hpragma mkideforest_pragma PROTO((void));

extern hpragma mkistrictness_pragma PROTO((hstring, hpragma));
#ifdef __GNUC__

hstring *Rgprag_strict_spec PROTO((struct Sistrictness_pragma *));

extern __inline__ hstring *Rgprag_strict_spec(struct Sistrictness_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != istrictness_pragma)
		fprintf(stderr,"gprag_strict_spec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_strict_spec);
}
#else  /* ! __GNUC__ */
extern hstring *Rgprag_strict_spec PROTO((struct Sistrictness_pragma *));
#endif /* ! __GNUC__ */

#define gprag_strict_spec(xyzxyz) (*Rgprag_strict_spec((struct Sistrictness_pragma *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_strict_wrkr PROTO((struct Sistrictness_pragma *));

extern __inline__ hpragma *Rgprag_strict_wrkr(struct Sistrictness_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != istrictness_pragma)
		fprintf(stderr,"gprag_strict_wrkr: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_strict_wrkr);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_strict_wrkr PROTO((struct Sistrictness_pragma *));
#endif /* ! __GNUC__ */

#define gprag_strict_wrkr(xyzxyz) (*Rgprag_strict_wrkr((struct Sistrictness_pragma *) (xyzxyz)))

extern hpragma mkimagic_unfolding_pragma PROTO((stringId));
#ifdef __GNUC__

stringId *Rgprag_magic_str PROTO((struct Simagic_unfolding_pragma *));

extern __inline__ stringId *Rgprag_magic_str(struct Simagic_unfolding_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != imagic_unfolding_pragma)
		fprintf(stderr,"gprag_magic_str: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_magic_str);
}
#else  /* ! __GNUC__ */
extern stringId *Rgprag_magic_str PROTO((struct Simagic_unfolding_pragma *));
#endif /* ! __GNUC__ */

#define gprag_magic_str(xyzxyz) (*Rgprag_magic_str((struct Simagic_unfolding_pragma *) (xyzxyz)))

extern hpragma mkiunfolding_pragma PROTO((hpragma, coresyn));
#ifdef __GNUC__

hpragma *Rgprag_unfold_guide PROTO((struct Siunfolding_pragma *));

extern __inline__ hpragma *Rgprag_unfold_guide(struct Siunfolding_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfolding_pragma)
		fprintf(stderr,"gprag_unfold_guide: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_guide);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_unfold_guide PROTO((struct Siunfolding_pragma *));
#endif /* ! __GNUC__ */

#define gprag_unfold_guide(xyzxyz) (*Rgprag_unfold_guide((struct Siunfolding_pragma *) (xyzxyz)))
#ifdef __GNUC__

coresyn *Rgprag_unfold_core PROTO((struct Siunfolding_pragma *));

extern __inline__ coresyn *Rgprag_unfold_core(struct Siunfolding_pragma *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfolding_pragma)
		fprintf(stderr,"gprag_unfold_core: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_core);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgprag_unfold_core PROTO((struct Siunfolding_pragma *));
#endif /* ! __GNUC__ */

#define gprag_unfold_core(xyzxyz) (*Rgprag_unfold_core((struct Siunfolding_pragma *) (xyzxyz)))

extern hpragma mkiunfold_always PROTO((void));

extern hpragma mkiunfold_if_args PROTO((numId, numId, stringId, numId));
#ifdef __GNUC__

numId *Rgprag_unfold_if_t_args PROTO((struct Siunfold_if_args *));

extern __inline__ numId *Rgprag_unfold_if_t_args(struct Siunfold_if_args *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfold_if_args)
		fprintf(stderr,"gprag_unfold_if_t_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_if_t_args);
}
#else  /* ! __GNUC__ */
extern numId *Rgprag_unfold_if_t_args PROTO((struct Siunfold_if_args *));
#endif /* ! __GNUC__ */

#define gprag_unfold_if_t_args(xyzxyz) (*Rgprag_unfold_if_t_args((struct Siunfold_if_args *) (xyzxyz)))
#ifdef __GNUC__

numId *Rgprag_unfold_if_v_args PROTO((struct Siunfold_if_args *));

extern __inline__ numId *Rgprag_unfold_if_v_args(struct Siunfold_if_args *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfold_if_args)
		fprintf(stderr,"gprag_unfold_if_v_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_if_v_args);
}
#else  /* ! __GNUC__ */
extern numId *Rgprag_unfold_if_v_args PROTO((struct Siunfold_if_args *));
#endif /* ! __GNUC__ */

#define gprag_unfold_if_v_args(xyzxyz) (*Rgprag_unfold_if_v_args((struct Siunfold_if_args *) (xyzxyz)))
#ifdef __GNUC__

stringId *Rgprag_unfold_if_con_args PROTO((struct Siunfold_if_args *));

extern __inline__ stringId *Rgprag_unfold_if_con_args(struct Siunfold_if_args *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfold_if_args)
		fprintf(stderr,"gprag_unfold_if_con_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_if_con_args);
}
#else  /* ! __GNUC__ */
extern stringId *Rgprag_unfold_if_con_args PROTO((struct Siunfold_if_args *));
#endif /* ! __GNUC__ */

#define gprag_unfold_if_con_args(xyzxyz) (*Rgprag_unfold_if_con_args((struct Siunfold_if_args *) (xyzxyz)))
#ifdef __GNUC__

numId *Rgprag_unfold_if_size PROTO((struct Siunfold_if_args *));

extern __inline__ numId *Rgprag_unfold_if_size(struct Siunfold_if_args *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iunfold_if_args)
		fprintf(stderr,"gprag_unfold_if_size: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_unfold_if_size);
}
#else  /* ! __GNUC__ */
extern numId *Rgprag_unfold_if_size PROTO((struct Siunfold_if_args *));
#endif /* ! __GNUC__ */

#define gprag_unfold_if_size(xyzxyz) (*Rgprag_unfold_if_size((struct Siunfold_if_args *) (xyzxyz)))

extern hpragma mkiname_pragma_pr PROTO((unkId, hpragma));
#ifdef __GNUC__

unkId *Rgprag_name_pr1 PROTO((struct Siname_pragma_pr *));

extern __inline__ unkId *Rgprag_name_pr1(struct Siname_pragma_pr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iname_pragma_pr)
		fprintf(stderr,"gprag_name_pr1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_name_pr1);
}
#else  /* ! __GNUC__ */
extern unkId *Rgprag_name_pr1 PROTO((struct Siname_pragma_pr *));
#endif /* ! __GNUC__ */

#define gprag_name_pr1(xyzxyz) (*Rgprag_name_pr1((struct Siname_pragma_pr *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_name_pr2 PROTO((struct Siname_pragma_pr *));

extern __inline__ hpragma *Rgprag_name_pr2(struct Siname_pragma_pr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iname_pragma_pr)
		fprintf(stderr,"gprag_name_pr2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_name_pr2);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_name_pr2 PROTO((struct Siname_pragma_pr *));
#endif /* ! __GNUC__ */

#define gprag_name_pr2(xyzxyz) (*Rgprag_name_pr2((struct Siname_pragma_pr *) (xyzxyz)))

extern hpragma mkitype_pragma_pr PROTO((list, numId, hpragma));
#ifdef __GNUC__

list *Rgprag_type_pr1 PROTO((struct Sitype_pragma_pr *));

extern __inline__ list *Rgprag_type_pr1(struct Sitype_pragma_pr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != itype_pragma_pr)
		fprintf(stderr,"gprag_type_pr1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_type_pr1);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_type_pr1 PROTO((struct Sitype_pragma_pr *));
#endif /* ! __GNUC__ */

#define gprag_type_pr1(xyzxyz) (*Rgprag_type_pr1((struct Sitype_pragma_pr *) (xyzxyz)))
#ifdef __GNUC__

numId *Rgprag_type_pr2 PROTO((struct Sitype_pragma_pr *));

extern __inline__ numId *Rgprag_type_pr2(struct Sitype_pragma_pr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != itype_pragma_pr)
		fprintf(stderr,"gprag_type_pr2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_type_pr2);
}
#else  /* ! __GNUC__ */
extern numId *Rgprag_type_pr2 PROTO((struct Sitype_pragma_pr *));
#endif /* ! __GNUC__ */

#define gprag_type_pr2(xyzxyz) (*Rgprag_type_pr2((struct Sitype_pragma_pr *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_type_pr3 PROTO((struct Sitype_pragma_pr *));

extern __inline__ hpragma *Rgprag_type_pr3(struct Sitype_pragma_pr *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != itype_pragma_pr)
		fprintf(stderr,"gprag_type_pr3: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_type_pr3);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_type_pr3 PROTO((struct Sitype_pragma_pr *));
#endif /* ! __GNUC__ */

#define gprag_type_pr3(xyzxyz) (*Rgprag_type_pr3((struct Sitype_pragma_pr *) (xyzxyz)))

extern hpragma mkiinst_pragma_3s PROTO((list, numId, hpragma, list));
#ifdef __GNUC__

list *Rgprag_inst_pt1 PROTO((struct Siinst_pragma_3s *));

extern __inline__ list *Rgprag_inst_pt1(struct Siinst_pragma_3s *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_pragma_3s)
		fprintf(stderr,"gprag_inst_pt1: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_inst_pt1);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_inst_pt1 PROTO((struct Siinst_pragma_3s *));
#endif /* ! __GNUC__ */

#define gprag_inst_pt1(xyzxyz) (*Rgprag_inst_pt1((struct Siinst_pragma_3s *) (xyzxyz)))
#ifdef __GNUC__

numId *Rgprag_inst_pt2 PROTO((struct Siinst_pragma_3s *));

extern __inline__ numId *Rgprag_inst_pt2(struct Siinst_pragma_3s *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_pragma_3s)
		fprintf(stderr,"gprag_inst_pt2: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_inst_pt2);
}
#else  /* ! __GNUC__ */
extern numId *Rgprag_inst_pt2 PROTO((struct Siinst_pragma_3s *));
#endif /* ! __GNUC__ */

#define gprag_inst_pt2(xyzxyz) (*Rgprag_inst_pt2((struct Siinst_pragma_3s *) (xyzxyz)))
#ifdef __GNUC__

hpragma *Rgprag_inst_pt3 PROTO((struct Siinst_pragma_3s *));

extern __inline__ hpragma *Rgprag_inst_pt3(struct Siinst_pragma_3s *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_pragma_3s)
		fprintf(stderr,"gprag_inst_pt3: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_inst_pt3);
}
#else  /* ! __GNUC__ */
extern hpragma *Rgprag_inst_pt3 PROTO((struct Siinst_pragma_3s *));
#endif /* ! __GNUC__ */

#define gprag_inst_pt3(xyzxyz) (*Rgprag_inst_pt3((struct Siinst_pragma_3s *) (xyzxyz)))
#ifdef __GNUC__

list *Rgprag_inst_pt4 PROTO((struct Siinst_pragma_3s *));

extern __inline__ list *Rgprag_inst_pt4(struct Siinst_pragma_3s *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != iinst_pragma_3s)
		fprintf(stderr,"gprag_inst_pt4: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_inst_pt4);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_inst_pt4 PROTO((struct Siinst_pragma_3s *));
#endif /* ! __GNUC__ */

#define gprag_inst_pt4(xyzxyz) (*Rgprag_inst_pt4((struct Siinst_pragma_3s *) (xyzxyz)))

extern hpragma mkidata_pragma_4s PROTO((list));
#ifdef __GNUC__

list *Rgprag_data_spec PROTO((struct Sidata_pragma_4s *));

extern __inline__ list *Rgprag_data_spec(struct Sidata_pragma_4s *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != idata_pragma_4s)
		fprintf(stderr,"gprag_data_spec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgprag_data_spec);
}
#else  /* ! __GNUC__ */
extern list *Rgprag_data_spec PROTO((struct Sidata_pragma_4s *));
#endif /* ! __GNUC__ */

#define gprag_data_spec(xyzxyz) (*Rgprag_data_spec((struct Sidata_pragma_4s *) (xyzxyz)))

#endif

#ifndef coresyn_defined
#define coresyn_defined

#include <stdio.h>

#ifndef PROTO
#ifdef __STDC__
#define PROTO(x) x
#else
#define PROTO(x) /**/
#endif
#endif

typedef enum {
	cobinder,
	colit,
	colocal,
	cononrec,
	corec,
	corec_pair,
	covar,
	coliteral,
	cocon,
	coprim,
	colam,
	cotylam,
	coapp,
	cotyapp,
	cocase,
	colet,
	coscc,
	coalg_alts,
	coalg_alt,
	coprim_alts,
	coprim_alt,
	conodeflt,
	cobinddeflt,
	co_primop,
	co_ccall,
	co_casm,
	co_preludedictscc,
	co_alldictscc,
	co_usercc,
	co_autocc,
	co_dictcc,
	co_scc_noncaf,
	co_scc_caf,
	co_scc_nondupd,
	co_scc_dupd,
	co_id,
	co_orig_id,
	co_sdselid,
	co_classopid,
	co_defmid,
	co_dfunid,
	co_constmid,
	co_specid,
	co_wrkrid
} Tcoresyn;

typedef struct { Tcoresyn tag; } *coresyn;

#ifdef __GNUC__
extern __inline__ Tcoresyn tcoresyn(coresyn t)
{
	return(t -> tag);
}
#else  /* ! __GNUC__ */
extern Tcoresyn tcoresyn PROTO((coresyn));
#endif /* ! __GNUC__ */

struct Scobinder {
	Tcoresyn tag;
	unkId Xgcobinder_v;
	ttype Xgcobinder_ty;
};

struct Scolit {
	Tcoresyn tag;
	literal Xgcolit;
};

struct Scolocal {
	Tcoresyn tag;
	coresyn Xgcolocal_v;
};

struct Scononrec {
	Tcoresyn tag;
	coresyn Xgcononrec_b;
	coresyn Xgcononrec_rhs;
};

struct Scorec {
	Tcoresyn tag;
	list Xgcorec;
};

struct Scorec_pair {
	Tcoresyn tag;
	coresyn Xgcorec_b;
	coresyn Xgcorec_rhs;
};

struct Scovar {
	Tcoresyn tag;
	coresyn Xgcovar;
};

struct Scoliteral {
	Tcoresyn tag;
	literal Xgcoliteral;
};

struct Scocon {
	Tcoresyn tag;
	coresyn Xgcocon_con;
	list Xgcocon_tys;
	list Xgcocon_args;
};

struct Scoprim {
	Tcoresyn tag;
	coresyn Xgcoprim_op;
	list Xgcoprim_tys;
	list Xgcoprim_args;
};

struct Scolam {
	Tcoresyn tag;
	list Xgcolam_vars;
	coresyn Xgcolam_body;
};

struct Scotylam {
	Tcoresyn tag;
	list Xgcotylam_tvs;
	coresyn Xgcotylam_body;
};

struct Scoapp {
	Tcoresyn tag;
	coresyn Xgcoapp_fun;
	list Xgcoapp_args;
};

struct Scotyapp {
	Tcoresyn tag;
	coresyn Xgcotyapp_e;
	ttype Xgcotyapp_t;
};

struct Scocase {
	Tcoresyn tag;
	coresyn Xgcocase_s;
	coresyn Xgcocase_alts;
};

struct Scolet {
	Tcoresyn tag;
	coresyn Xgcolet_bind;
	coresyn Xgcolet_body;
};

struct Scoscc {
	Tcoresyn tag;
	coresyn Xgcoscc_scc;
	coresyn Xgcoscc_body;
};

struct Scoalg_alts {
	Tcoresyn tag;
	list Xgcoalg_alts;
	coresyn Xgcoalg_deflt;
};

struct Scoalg_alt {
	Tcoresyn tag;
	coresyn Xgcoalg_con;
	list Xgcoalg_bs;
	coresyn Xgcoalg_rhs;
};

struct Scoprim_alts {
	Tcoresyn tag;
	list Xgcoprim_alts;
	coresyn Xgcoprim_deflt;
};

struct Scoprim_alt {
	Tcoresyn tag;
	literal Xgcoprim_lit;
	coresyn Xgcoprim_rhs;
};

struct Sconodeflt {
	Tcoresyn tag;
};

struct Scobinddeflt {
	Tcoresyn tag;
	coresyn Xgcobinddeflt_v;
	coresyn Xgcobinddeflt_rhs;
};

struct Sco_primop {
	Tcoresyn tag;
	stringId Xgco_primop;
};

struct Sco_ccall {
	Tcoresyn tag;
	stringId Xgco_ccall;
	long Xgco_ccall_may_gc;
	list Xgco_ccall_arg_tys;
	ttype Xgco_ccall_res_ty;
};

struct Sco_casm {
	Tcoresyn tag;
	literal Xgco_casm;
	long Xgco_casm_may_gc;
	list Xgco_casm_arg_tys;
	ttype Xgco_casm_res_ty;
};

struct Sco_preludedictscc {
	Tcoresyn tag;
	coresyn Xgco_preludedictscc_dupd;
};

struct Sco_alldictscc {
	Tcoresyn tag;
	hstring Xgco_alldictscc_m;
	hstring Xgco_alldictscc_g;
	coresyn Xgco_alldictscc_dupd;
};

struct Sco_usercc {
	Tcoresyn tag;
	hstring Xgco_usercc_n;
	hstring Xgco_usercc_m;
	hstring Xgco_usercc_g;
	coresyn Xgco_usercc_dupd;
	coresyn Xgco_usercc_cafd;
};

struct Sco_autocc {
	Tcoresyn tag;
	coresyn Xgco_autocc_i;
	hstring Xgco_autocc_m;
	hstring Xgco_autocc_g;
	coresyn Xgco_autocc_dupd;
	coresyn Xgco_autocc_cafd;
};

struct Sco_dictcc {
	Tcoresyn tag;
	coresyn Xgco_dictcc_i;
	hstring Xgco_dictcc_m;
	hstring Xgco_dictcc_g;
	coresyn Xgco_dictcc_dupd;
	coresyn Xgco_dictcc_cafd;
};

struct Sco_scc_noncaf {
	Tcoresyn tag;
};

struct Sco_scc_caf {
	Tcoresyn tag;
};

struct Sco_scc_nondupd {
	Tcoresyn tag;
};

struct Sco_scc_dupd {
	Tcoresyn tag;
};

struct Sco_id {
	Tcoresyn tag;
	stringId Xgco_id;
};

struct Sco_orig_id {
	Tcoresyn tag;
	stringId Xgco_orig_id_m;
	stringId Xgco_orig_id_n;
};

struct Sco_sdselid {
	Tcoresyn tag;
	unkId Xgco_sdselid_c;
	unkId Xgco_sdselid_sc;
};

struct Sco_classopid {
	Tcoresyn tag;
	unkId Xgco_classopid_c;
	unkId Xgco_classopid_o;
};

struct Sco_defmid {
	Tcoresyn tag;
	unkId Xgco_defmid_c;
	unkId Xgco_defmid_op;
};

struct Sco_dfunid {
	Tcoresyn tag;
	unkId Xgco_dfunid_c;
	ttype Xgco_dfunid_ty;
};

struct Sco_constmid {
	Tcoresyn tag;
	unkId Xgco_constmid_c;
	unkId Xgco_constmid_op;
	ttype Xgco_constmid_ty;
};

struct Sco_specid {
	Tcoresyn tag;
	coresyn Xgco_specid_un;
	list Xgco_specid_tys;
};

struct Sco_wrkrid {
	Tcoresyn tag;
	coresyn Xgco_wrkrid_un;
};

extern coresyn mkcobinder PROTO((unkId, ttype));
#ifdef __GNUC__

extern __inline__ unkId *Rgcobinder_v(struct Scobinder *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cobinder)
		fprintf(stderr,"gcobinder_v: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcobinder_v);
}
#else  /* ! __GNUC__ */
extern unkId *Rgcobinder_v PROTO((struct Scobinder *));
#endif /* ! __GNUC__ */

#define gcobinder_v(xyzxyz) (*Rgcobinder_v((struct Scobinder *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgcobinder_ty(struct Scobinder *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cobinder)
		fprintf(stderr,"gcobinder_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcobinder_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rgcobinder_ty PROTO((struct Scobinder *));
#endif /* ! __GNUC__ */

#define gcobinder_ty(xyzxyz) (*Rgcobinder_ty((struct Scobinder *) (xyzxyz)))

extern coresyn mkcolit PROTO((literal));
#ifdef __GNUC__

extern __inline__ literal *Rgcolit(struct Scolit *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != colit)
		fprintf(stderr,"gcolit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolit);
}
#else  /* ! __GNUC__ */
extern literal *Rgcolit PROTO((struct Scolit *));
#endif /* ! __GNUC__ */

#define gcolit(xyzxyz) (*Rgcolit((struct Scolit *) (xyzxyz)))

extern coresyn mkcolocal PROTO((coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcolocal_v(struct Scolocal *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != colocal)
		fprintf(stderr,"gcolocal_v: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolocal_v);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcolocal_v PROTO((struct Scolocal *));
#endif /* ! __GNUC__ */

#define gcolocal_v(xyzxyz) (*Rgcolocal_v((struct Scolocal *) (xyzxyz)))

extern coresyn mkcononrec PROTO((coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcononrec_b(struct Scononrec *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cononrec)
		fprintf(stderr,"gcononrec_b: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcononrec_b);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcononrec_b PROTO((struct Scononrec *));
#endif /* ! __GNUC__ */

#define gcononrec_b(xyzxyz) (*Rgcononrec_b((struct Scononrec *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcononrec_rhs(struct Scononrec *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cononrec)
		fprintf(stderr,"gcononrec_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcononrec_rhs);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcononrec_rhs PROTO((struct Scononrec *));
#endif /* ! __GNUC__ */

#define gcononrec_rhs(xyzxyz) (*Rgcononrec_rhs((struct Scononrec *) (xyzxyz)))

extern coresyn mkcorec PROTO((list));
#ifdef __GNUC__

extern __inline__ list *Rgcorec(struct Scorec *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != corec)
		fprintf(stderr,"gcorec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcorec);
}
#else  /* ! __GNUC__ */
extern list *Rgcorec PROTO((struct Scorec *));
#endif /* ! __GNUC__ */

#define gcorec(xyzxyz) (*Rgcorec((struct Scorec *) (xyzxyz)))

extern coresyn mkcorec_pair PROTO((coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcorec_b(struct Scorec_pair *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != corec_pair)
		fprintf(stderr,"gcorec_b: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcorec_b);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcorec_b PROTO((struct Scorec_pair *));
#endif /* ! __GNUC__ */

#define gcorec_b(xyzxyz) (*Rgcorec_b((struct Scorec_pair *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcorec_rhs(struct Scorec_pair *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != corec_pair)
		fprintf(stderr,"gcorec_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcorec_rhs);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcorec_rhs PROTO((struct Scorec_pair *));
#endif /* ! __GNUC__ */

#define gcorec_rhs(xyzxyz) (*Rgcorec_rhs((struct Scorec_pair *) (xyzxyz)))

extern coresyn mkcovar PROTO((coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcovar(struct Scovar *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != covar)
		fprintf(stderr,"gcovar: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcovar);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcovar PROTO((struct Scovar *));
#endif /* ! __GNUC__ */

#define gcovar(xyzxyz) (*Rgcovar((struct Scovar *) (xyzxyz)))

extern coresyn mkcoliteral PROTO((literal));
#ifdef __GNUC__

extern __inline__ literal *Rgcoliteral(struct Scoliteral *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coliteral)
		fprintf(stderr,"gcoliteral: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoliteral);
}
#else  /* ! __GNUC__ */
extern literal *Rgcoliteral PROTO((struct Scoliteral *));
#endif /* ! __GNUC__ */

#define gcoliteral(xyzxyz) (*Rgcoliteral((struct Scoliteral *) (xyzxyz)))

extern coresyn mkcocon PROTO((coresyn, list, list));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcocon_con(struct Scocon *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocon)
		fprintf(stderr,"gcocon_con: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocon_con);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcocon_con PROTO((struct Scocon *));
#endif /* ! __GNUC__ */

#define gcocon_con(xyzxyz) (*Rgcocon_con((struct Scocon *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgcocon_tys(struct Scocon *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocon)
		fprintf(stderr,"gcocon_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocon_tys);
}
#else  /* ! __GNUC__ */
extern list *Rgcocon_tys PROTO((struct Scocon *));
#endif /* ! __GNUC__ */

#define gcocon_tys(xyzxyz) (*Rgcocon_tys((struct Scocon *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgcocon_args(struct Scocon *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocon)
		fprintf(stderr,"gcocon_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocon_args);
}
#else  /* ! __GNUC__ */
extern list *Rgcocon_args PROTO((struct Scocon *));
#endif /* ! __GNUC__ */

#define gcocon_args(xyzxyz) (*Rgcocon_args((struct Scocon *) (xyzxyz)))

extern coresyn mkcoprim PROTO((coresyn, list, list));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoprim_op(struct Scoprim *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim)
		fprintf(stderr,"gcoprim_op: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_op);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoprim_op PROTO((struct Scoprim *));
#endif /* ! __GNUC__ */

#define gcoprim_op(xyzxyz) (*Rgcoprim_op((struct Scoprim *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgcoprim_tys(struct Scoprim *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim)
		fprintf(stderr,"gcoprim_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_tys);
}
#else  /* ! __GNUC__ */
extern list *Rgcoprim_tys PROTO((struct Scoprim *));
#endif /* ! __GNUC__ */

#define gcoprim_tys(xyzxyz) (*Rgcoprim_tys((struct Scoprim *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgcoprim_args(struct Scoprim *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim)
		fprintf(stderr,"gcoprim_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_args);
}
#else  /* ! __GNUC__ */
extern list *Rgcoprim_args PROTO((struct Scoprim *));
#endif /* ! __GNUC__ */

#define gcoprim_args(xyzxyz) (*Rgcoprim_args((struct Scoprim *) (xyzxyz)))

extern coresyn mkcolam PROTO((list, coresyn));
#ifdef __GNUC__

extern __inline__ list *Rgcolam_vars(struct Scolam *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != colam)
		fprintf(stderr,"gcolam_vars: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolam_vars);
}
#else  /* ! __GNUC__ */
extern list *Rgcolam_vars PROTO((struct Scolam *));
#endif /* ! __GNUC__ */

#define gcolam_vars(xyzxyz) (*Rgcolam_vars((struct Scolam *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcolam_body(struct Scolam *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != colam)
		fprintf(stderr,"gcolam_body: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolam_body);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcolam_body PROTO((struct Scolam *));
#endif /* ! __GNUC__ */

#define gcolam_body(xyzxyz) (*Rgcolam_body((struct Scolam *) (xyzxyz)))

extern coresyn mkcotylam PROTO((list, coresyn));
#ifdef __GNUC__

extern __inline__ list *Rgcotylam_tvs(struct Scotylam *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cotylam)
		fprintf(stderr,"gcotylam_tvs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcotylam_tvs);
}
#else  /* ! __GNUC__ */
extern list *Rgcotylam_tvs PROTO((struct Scotylam *));
#endif /* ! __GNUC__ */

#define gcotylam_tvs(xyzxyz) (*Rgcotylam_tvs((struct Scotylam *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcotylam_body(struct Scotylam *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cotylam)
		fprintf(stderr,"gcotylam_body: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcotylam_body);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcotylam_body PROTO((struct Scotylam *));
#endif /* ! __GNUC__ */

#define gcotylam_body(xyzxyz) (*Rgcotylam_body((struct Scotylam *) (xyzxyz)))

extern coresyn mkcoapp PROTO((coresyn, list));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoapp_fun(struct Scoapp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coapp)
		fprintf(stderr,"gcoapp_fun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoapp_fun);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoapp_fun PROTO((struct Scoapp *));
#endif /* ! __GNUC__ */

#define gcoapp_fun(xyzxyz) (*Rgcoapp_fun((struct Scoapp *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgcoapp_args(struct Scoapp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coapp)
		fprintf(stderr,"gcoapp_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoapp_args);
}
#else  /* ! __GNUC__ */
extern list *Rgcoapp_args PROTO((struct Scoapp *));
#endif /* ! __GNUC__ */

#define gcoapp_args(xyzxyz) (*Rgcoapp_args((struct Scoapp *) (xyzxyz)))

extern coresyn mkcotyapp PROTO((coresyn, ttype));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcotyapp_e(struct Scotyapp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cotyapp)
		fprintf(stderr,"gcotyapp_e: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcotyapp_e);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcotyapp_e PROTO((struct Scotyapp *));
#endif /* ! __GNUC__ */

#define gcotyapp_e(xyzxyz) (*Rgcotyapp_e((struct Scotyapp *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgcotyapp_t(struct Scotyapp *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cotyapp)
		fprintf(stderr,"gcotyapp_t: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcotyapp_t);
}
#else  /* ! __GNUC__ */
extern ttype *Rgcotyapp_t PROTO((struct Scotyapp *));
#endif /* ! __GNUC__ */

#define gcotyapp_t(xyzxyz) (*Rgcotyapp_t((struct Scotyapp *) (xyzxyz)))

extern coresyn mkcocase PROTO((coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcocase_s(struct Scocase *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocase)
		fprintf(stderr,"gcocase_s: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocase_s);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcocase_s PROTO((struct Scocase *));
#endif /* ! __GNUC__ */

#define gcocase_s(xyzxyz) (*Rgcocase_s((struct Scocase *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcocase_alts(struct Scocase *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocase)
		fprintf(stderr,"gcocase_alts: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocase_alts);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcocase_alts PROTO((struct Scocase *));
#endif /* ! __GNUC__ */

#define gcocase_alts(xyzxyz) (*Rgcocase_alts((struct Scocase *) (xyzxyz)))

extern coresyn mkcolet PROTO((coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcolet_bind(struct Scolet *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != colet)
		fprintf(stderr,"gcolet_bind: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolet_bind);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcolet_bind PROTO((struct Scolet *));
#endif /* ! __GNUC__ */

#define gcolet_bind(xyzxyz) (*Rgcolet_bind((struct Scolet *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcolet_body(struct Scolet *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != colet)
		fprintf(stderr,"gcolet_body: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolet_body);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcolet_body PROTO((struct Scolet *));
#endif /* ! __GNUC__ */

#define gcolet_body(xyzxyz) (*Rgcolet_body((struct Scolet *) (xyzxyz)))

extern coresyn mkcoscc PROTO((coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoscc_scc(struct Scoscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coscc)
		fprintf(stderr,"gcoscc_scc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoscc_scc);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoscc_scc PROTO((struct Scoscc *));
#endif /* ! __GNUC__ */

#define gcoscc_scc(xyzxyz) (*Rgcoscc_scc((struct Scoscc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoscc_body(struct Scoscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coscc)
		fprintf(stderr,"gcoscc_body: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoscc_body);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoscc_body PROTO((struct Scoscc *));
#endif /* ! __GNUC__ */

#define gcoscc_body(xyzxyz) (*Rgcoscc_body((struct Scoscc *) (xyzxyz)))

extern coresyn mkcoalg_alts PROTO((list, coresyn));
#ifdef __GNUC__

extern __inline__ list *Rgcoalg_alts(struct Scoalg_alts *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alts)
		fprintf(stderr,"gcoalg_alts: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_alts);
}
#else  /* ! __GNUC__ */
extern list *Rgcoalg_alts PROTO((struct Scoalg_alts *));
#endif /* ! __GNUC__ */

#define gcoalg_alts(xyzxyz) (*Rgcoalg_alts((struct Scoalg_alts *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoalg_deflt(struct Scoalg_alts *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alts)
		fprintf(stderr,"gcoalg_deflt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_deflt);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoalg_deflt PROTO((struct Scoalg_alts *));
#endif /* ! __GNUC__ */

#define gcoalg_deflt(xyzxyz) (*Rgcoalg_deflt((struct Scoalg_alts *) (xyzxyz)))

extern coresyn mkcoalg_alt PROTO((coresyn, list, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoalg_con(struct Scoalg_alt *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alt)
		fprintf(stderr,"gcoalg_con: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_con);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoalg_con PROTO((struct Scoalg_alt *));
#endif /* ! __GNUC__ */

#define gcoalg_con(xyzxyz) (*Rgcoalg_con((struct Scoalg_alt *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgcoalg_bs(struct Scoalg_alt *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alt)
		fprintf(stderr,"gcoalg_bs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_bs);
}
#else  /* ! __GNUC__ */
extern list *Rgcoalg_bs PROTO((struct Scoalg_alt *));
#endif /* ! __GNUC__ */

#define gcoalg_bs(xyzxyz) (*Rgcoalg_bs((struct Scoalg_alt *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoalg_rhs(struct Scoalg_alt *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alt)
		fprintf(stderr,"gcoalg_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_rhs);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoalg_rhs PROTO((struct Scoalg_alt *));
#endif /* ! __GNUC__ */

#define gcoalg_rhs(xyzxyz) (*Rgcoalg_rhs((struct Scoalg_alt *) (xyzxyz)))

extern coresyn mkcoprim_alts PROTO((list, coresyn));
#ifdef __GNUC__

extern __inline__ list *Rgcoprim_alts(struct Scoprim_alts *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim_alts)
		fprintf(stderr,"gcoprim_alts: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_alts);
}
#else  /* ! __GNUC__ */
extern list *Rgcoprim_alts PROTO((struct Scoprim_alts *));
#endif /* ! __GNUC__ */

#define gcoprim_alts(xyzxyz) (*Rgcoprim_alts((struct Scoprim_alts *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoprim_deflt(struct Scoprim_alts *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim_alts)
		fprintf(stderr,"gcoprim_deflt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_deflt);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoprim_deflt PROTO((struct Scoprim_alts *));
#endif /* ! __GNUC__ */

#define gcoprim_deflt(xyzxyz) (*Rgcoprim_deflt((struct Scoprim_alts *) (xyzxyz)))

extern coresyn mkcoprim_alt PROTO((literal, coresyn));
#ifdef __GNUC__

extern __inline__ literal *Rgcoprim_lit(struct Scoprim_alt *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim_alt)
		fprintf(stderr,"gcoprim_lit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_lit);
}
#else  /* ! __GNUC__ */
extern literal *Rgcoprim_lit PROTO((struct Scoprim_alt *));
#endif /* ! __GNUC__ */

#define gcoprim_lit(xyzxyz) (*Rgcoprim_lit((struct Scoprim_alt *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcoprim_rhs(struct Scoprim_alt *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim_alt)
		fprintf(stderr,"gcoprim_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_rhs);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcoprim_rhs PROTO((struct Scoprim_alt *));
#endif /* ! __GNUC__ */

#define gcoprim_rhs(xyzxyz) (*Rgcoprim_rhs((struct Scoprim_alt *) (xyzxyz)))

extern coresyn mkconodeflt PROTO(());

extern coresyn mkcobinddeflt PROTO((coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgcobinddeflt_v(struct Scobinddeflt *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cobinddeflt)
		fprintf(stderr,"gcobinddeflt_v: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcobinddeflt_v);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcobinddeflt_v PROTO((struct Scobinddeflt *));
#endif /* ! __GNUC__ */

#define gcobinddeflt_v(xyzxyz) (*Rgcobinddeflt_v((struct Scobinddeflt *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgcobinddeflt_rhs(struct Scobinddeflt *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != cobinddeflt)
		fprintf(stderr,"gcobinddeflt_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcobinddeflt_rhs);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgcobinddeflt_rhs PROTO((struct Scobinddeflt *));
#endif /* ! __GNUC__ */

#define gcobinddeflt_rhs(xyzxyz) (*Rgcobinddeflt_rhs((struct Scobinddeflt *) (xyzxyz)))

extern coresyn mkco_primop PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgco_primop(struct Sco_primop *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_primop)
		fprintf(stderr,"gco_primop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_primop);
}
#else  /* ! __GNUC__ */
extern stringId *Rgco_primop PROTO((struct Sco_primop *));
#endif /* ! __GNUC__ */

#define gco_primop(xyzxyz) (*Rgco_primop((struct Sco_primop *) (xyzxyz)))

extern coresyn mkco_ccall PROTO((stringId, long, list, ttype));
#ifdef __GNUC__

extern __inline__ stringId *Rgco_ccall(struct Sco_ccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_ccall)
		fprintf(stderr,"gco_ccall: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_ccall);
}
#else  /* ! __GNUC__ */
extern stringId *Rgco_ccall PROTO((struct Sco_ccall *));
#endif /* ! __GNUC__ */

#define gco_ccall(xyzxyz) (*Rgco_ccall((struct Sco_ccall *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ long *Rgco_ccall_may_gc(struct Sco_ccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_ccall)
		fprintf(stderr,"gco_ccall_may_gc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_ccall_may_gc);
}
#else  /* ! __GNUC__ */
extern long *Rgco_ccall_may_gc PROTO((struct Sco_ccall *));
#endif /* ! __GNUC__ */

#define gco_ccall_may_gc(xyzxyz) (*Rgco_ccall_may_gc((struct Sco_ccall *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgco_ccall_arg_tys(struct Sco_ccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_ccall)
		fprintf(stderr,"gco_ccall_arg_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_ccall_arg_tys);
}
#else  /* ! __GNUC__ */
extern list *Rgco_ccall_arg_tys PROTO((struct Sco_ccall *));
#endif /* ! __GNUC__ */

#define gco_ccall_arg_tys(xyzxyz) (*Rgco_ccall_arg_tys((struct Sco_ccall *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgco_ccall_res_ty(struct Sco_ccall *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_ccall)
		fprintf(stderr,"gco_ccall_res_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_ccall_res_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rgco_ccall_res_ty PROTO((struct Sco_ccall *));
#endif /* ! __GNUC__ */

#define gco_ccall_res_ty(xyzxyz) (*Rgco_ccall_res_ty((struct Sco_ccall *) (xyzxyz)))

extern coresyn mkco_casm PROTO((literal, long, list, ttype));
#ifdef __GNUC__

extern __inline__ literal *Rgco_casm(struct Sco_casm *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_casm)
		fprintf(stderr,"gco_casm: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_casm);
}
#else  /* ! __GNUC__ */
extern literal *Rgco_casm PROTO((struct Sco_casm *));
#endif /* ! __GNUC__ */

#define gco_casm(xyzxyz) (*Rgco_casm((struct Sco_casm *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ long *Rgco_casm_may_gc(struct Sco_casm *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_casm)
		fprintf(stderr,"gco_casm_may_gc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_casm_may_gc);
}
#else  /* ! __GNUC__ */
extern long *Rgco_casm_may_gc PROTO((struct Sco_casm *));
#endif /* ! __GNUC__ */

#define gco_casm_may_gc(xyzxyz) (*Rgco_casm_may_gc((struct Sco_casm *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgco_casm_arg_tys(struct Sco_casm *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_casm)
		fprintf(stderr,"gco_casm_arg_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_casm_arg_tys);
}
#else  /* ! __GNUC__ */
extern list *Rgco_casm_arg_tys PROTO((struct Sco_casm *));
#endif /* ! __GNUC__ */

#define gco_casm_arg_tys(xyzxyz) (*Rgco_casm_arg_tys((struct Sco_casm *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgco_casm_res_ty(struct Sco_casm *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_casm)
		fprintf(stderr,"gco_casm_res_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_casm_res_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rgco_casm_res_ty PROTO((struct Sco_casm *));
#endif /* ! __GNUC__ */

#define gco_casm_res_ty(xyzxyz) (*Rgco_casm_res_ty((struct Sco_casm *) (xyzxyz)))

extern coresyn mkco_preludedictscc PROTO((coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_preludedictscc_dupd(struct Sco_preludedictscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_preludedictscc)
		fprintf(stderr,"gco_preludedictscc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_preludedictscc_dupd);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_preludedictscc_dupd PROTO((struct Sco_preludedictscc *));
#endif /* ! __GNUC__ */

#define gco_preludedictscc_dupd(xyzxyz) (*Rgco_preludedictscc_dupd((struct Sco_preludedictscc *) (xyzxyz)))

extern coresyn mkco_alldictscc PROTO((hstring, hstring, coresyn));
#ifdef __GNUC__

extern __inline__ hstring *Rgco_alldictscc_m(struct Sco_alldictscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_alldictscc)
		fprintf(stderr,"gco_alldictscc_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_alldictscc_m);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_alldictscc_m PROTO((struct Sco_alldictscc *));
#endif /* ! __GNUC__ */

#define gco_alldictscc_m(xyzxyz) (*Rgco_alldictscc_m((struct Sco_alldictscc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ hstring *Rgco_alldictscc_g(struct Sco_alldictscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_alldictscc)
		fprintf(stderr,"gco_alldictscc_g: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_alldictscc_g);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_alldictscc_g PROTO((struct Sco_alldictscc *));
#endif /* ! __GNUC__ */

#define gco_alldictscc_g(xyzxyz) (*Rgco_alldictscc_g((struct Sco_alldictscc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_alldictscc_dupd(struct Sco_alldictscc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_alldictscc)
		fprintf(stderr,"gco_alldictscc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_alldictscc_dupd);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_alldictscc_dupd PROTO((struct Sco_alldictscc *));
#endif /* ! __GNUC__ */

#define gco_alldictscc_dupd(xyzxyz) (*Rgco_alldictscc_dupd((struct Sco_alldictscc *) (xyzxyz)))

extern coresyn mkco_usercc PROTO((hstring, hstring, hstring, coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ hstring *Rgco_usercc_n(struct Sco_usercc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_n: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_n);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_usercc_n PROTO((struct Sco_usercc *));
#endif /* ! __GNUC__ */

#define gco_usercc_n(xyzxyz) (*Rgco_usercc_n((struct Sco_usercc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ hstring *Rgco_usercc_m(struct Sco_usercc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_m);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_usercc_m PROTO((struct Sco_usercc *));
#endif /* ! __GNUC__ */

#define gco_usercc_m(xyzxyz) (*Rgco_usercc_m((struct Sco_usercc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ hstring *Rgco_usercc_g(struct Sco_usercc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_g: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_g);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_usercc_g PROTO((struct Sco_usercc *));
#endif /* ! __GNUC__ */

#define gco_usercc_g(xyzxyz) (*Rgco_usercc_g((struct Sco_usercc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_usercc_dupd(struct Sco_usercc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_dupd);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_usercc_dupd PROTO((struct Sco_usercc *));
#endif /* ! __GNUC__ */

#define gco_usercc_dupd(xyzxyz) (*Rgco_usercc_dupd((struct Sco_usercc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_usercc_cafd(struct Sco_usercc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_cafd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_cafd);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_usercc_cafd PROTO((struct Sco_usercc *));
#endif /* ! __GNUC__ */

#define gco_usercc_cafd(xyzxyz) (*Rgco_usercc_cafd((struct Sco_usercc *) (xyzxyz)))

extern coresyn mkco_autocc PROTO((coresyn, hstring, hstring, coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_autocc_i(struct Sco_autocc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_i: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_i);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_autocc_i PROTO((struct Sco_autocc *));
#endif /* ! __GNUC__ */

#define gco_autocc_i(xyzxyz) (*Rgco_autocc_i((struct Sco_autocc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ hstring *Rgco_autocc_m(struct Sco_autocc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_m);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_autocc_m PROTO((struct Sco_autocc *));
#endif /* ! __GNUC__ */

#define gco_autocc_m(xyzxyz) (*Rgco_autocc_m((struct Sco_autocc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ hstring *Rgco_autocc_g(struct Sco_autocc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_g: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_g);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_autocc_g PROTO((struct Sco_autocc *));
#endif /* ! __GNUC__ */

#define gco_autocc_g(xyzxyz) (*Rgco_autocc_g((struct Sco_autocc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_autocc_dupd(struct Sco_autocc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_dupd);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_autocc_dupd PROTO((struct Sco_autocc *));
#endif /* ! __GNUC__ */

#define gco_autocc_dupd(xyzxyz) (*Rgco_autocc_dupd((struct Sco_autocc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_autocc_cafd(struct Sco_autocc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_cafd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_cafd);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_autocc_cafd PROTO((struct Sco_autocc *));
#endif /* ! __GNUC__ */

#define gco_autocc_cafd(xyzxyz) (*Rgco_autocc_cafd((struct Sco_autocc *) (xyzxyz)))

extern coresyn mkco_dictcc PROTO((coresyn, hstring, hstring, coresyn, coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_dictcc_i(struct Sco_dictcc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_i: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_i);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_dictcc_i PROTO((struct Sco_dictcc *));
#endif /* ! __GNUC__ */

#define gco_dictcc_i(xyzxyz) (*Rgco_dictcc_i((struct Sco_dictcc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ hstring *Rgco_dictcc_m(struct Sco_dictcc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_m);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_dictcc_m PROTO((struct Sco_dictcc *));
#endif /* ! __GNUC__ */

#define gco_dictcc_m(xyzxyz) (*Rgco_dictcc_m((struct Sco_dictcc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ hstring *Rgco_dictcc_g(struct Sco_dictcc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_g: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_g);
}
#else  /* ! __GNUC__ */
extern hstring *Rgco_dictcc_g PROTO((struct Sco_dictcc *));
#endif /* ! __GNUC__ */

#define gco_dictcc_g(xyzxyz) (*Rgco_dictcc_g((struct Sco_dictcc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_dictcc_dupd(struct Sco_dictcc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_dupd);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_dictcc_dupd PROTO((struct Sco_dictcc *));
#endif /* ! __GNUC__ */

#define gco_dictcc_dupd(xyzxyz) (*Rgco_dictcc_dupd((struct Sco_dictcc *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_dictcc_cafd(struct Sco_dictcc *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_cafd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_cafd);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_dictcc_cafd PROTO((struct Sco_dictcc *));
#endif /* ! __GNUC__ */

#define gco_dictcc_cafd(xyzxyz) (*Rgco_dictcc_cafd((struct Sco_dictcc *) (xyzxyz)))

extern coresyn mkco_scc_noncaf PROTO(());

extern coresyn mkco_scc_caf PROTO(());

extern coresyn mkco_scc_nondupd PROTO(());

extern coresyn mkco_scc_dupd PROTO(());

extern coresyn mkco_id PROTO((stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgco_id(struct Sco_id *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_id)
		fprintf(stderr,"gco_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_id);
}
#else  /* ! __GNUC__ */
extern stringId *Rgco_id PROTO((struct Sco_id *));
#endif /* ! __GNUC__ */

#define gco_id(xyzxyz) (*Rgco_id((struct Sco_id *) (xyzxyz)))

extern coresyn mkco_orig_id PROTO((stringId, stringId));
#ifdef __GNUC__

extern __inline__ stringId *Rgco_orig_id_m(struct Sco_orig_id *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_orig_id)
		fprintf(stderr,"gco_orig_id_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_orig_id_m);
}
#else  /* ! __GNUC__ */
extern stringId *Rgco_orig_id_m PROTO((struct Sco_orig_id *));
#endif /* ! __GNUC__ */

#define gco_orig_id_m(xyzxyz) (*Rgco_orig_id_m((struct Sco_orig_id *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ stringId *Rgco_orig_id_n(struct Sco_orig_id *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_orig_id)
		fprintf(stderr,"gco_orig_id_n: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_orig_id_n);
}
#else  /* ! __GNUC__ */
extern stringId *Rgco_orig_id_n PROTO((struct Sco_orig_id *));
#endif /* ! __GNUC__ */

#define gco_orig_id_n(xyzxyz) (*Rgco_orig_id_n((struct Sco_orig_id *) (xyzxyz)))

extern coresyn mkco_sdselid PROTO((unkId, unkId));
#ifdef __GNUC__

extern __inline__ unkId *Rgco_sdselid_c(struct Sco_sdselid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_sdselid)
		fprintf(stderr,"gco_sdselid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_sdselid_c);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_sdselid_c PROTO((struct Sco_sdselid *));
#endif /* ! __GNUC__ */

#define gco_sdselid_c(xyzxyz) (*Rgco_sdselid_c((struct Sco_sdselid *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ unkId *Rgco_sdselid_sc(struct Sco_sdselid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_sdselid)
		fprintf(stderr,"gco_sdselid_sc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_sdselid_sc);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_sdselid_sc PROTO((struct Sco_sdselid *));
#endif /* ! __GNUC__ */

#define gco_sdselid_sc(xyzxyz) (*Rgco_sdselid_sc((struct Sco_sdselid *) (xyzxyz)))

extern coresyn mkco_classopid PROTO((unkId, unkId));
#ifdef __GNUC__

extern __inline__ unkId *Rgco_classopid_c(struct Sco_classopid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_classopid)
		fprintf(stderr,"gco_classopid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_classopid_c);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_classopid_c PROTO((struct Sco_classopid *));
#endif /* ! __GNUC__ */

#define gco_classopid_c(xyzxyz) (*Rgco_classopid_c((struct Sco_classopid *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ unkId *Rgco_classopid_o(struct Sco_classopid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_classopid)
		fprintf(stderr,"gco_classopid_o: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_classopid_o);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_classopid_o PROTO((struct Sco_classopid *));
#endif /* ! __GNUC__ */

#define gco_classopid_o(xyzxyz) (*Rgco_classopid_o((struct Sco_classopid *) (xyzxyz)))

extern coresyn mkco_defmid PROTO((unkId, unkId));
#ifdef __GNUC__

extern __inline__ unkId *Rgco_defmid_c(struct Sco_defmid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_defmid)
		fprintf(stderr,"gco_defmid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_defmid_c);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_defmid_c PROTO((struct Sco_defmid *));
#endif /* ! __GNUC__ */

#define gco_defmid_c(xyzxyz) (*Rgco_defmid_c((struct Sco_defmid *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ unkId *Rgco_defmid_op(struct Sco_defmid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_defmid)
		fprintf(stderr,"gco_defmid_op: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_defmid_op);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_defmid_op PROTO((struct Sco_defmid *));
#endif /* ! __GNUC__ */

#define gco_defmid_op(xyzxyz) (*Rgco_defmid_op((struct Sco_defmid *) (xyzxyz)))

extern coresyn mkco_dfunid PROTO((unkId, ttype));
#ifdef __GNUC__

extern __inline__ unkId *Rgco_dfunid_c(struct Sco_dfunid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dfunid)
		fprintf(stderr,"gco_dfunid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dfunid_c);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_dfunid_c PROTO((struct Sco_dfunid *));
#endif /* ! __GNUC__ */

#define gco_dfunid_c(xyzxyz) (*Rgco_dfunid_c((struct Sco_dfunid *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgco_dfunid_ty(struct Sco_dfunid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dfunid)
		fprintf(stderr,"gco_dfunid_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dfunid_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rgco_dfunid_ty PROTO((struct Sco_dfunid *));
#endif /* ! __GNUC__ */

#define gco_dfunid_ty(xyzxyz) (*Rgco_dfunid_ty((struct Sco_dfunid *) (xyzxyz)))

extern coresyn mkco_constmid PROTO((unkId, unkId, ttype));
#ifdef __GNUC__

extern __inline__ unkId *Rgco_constmid_c(struct Sco_constmid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_constmid)
		fprintf(stderr,"gco_constmid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_constmid_c);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_constmid_c PROTO((struct Sco_constmid *));
#endif /* ! __GNUC__ */

#define gco_constmid_c(xyzxyz) (*Rgco_constmid_c((struct Sco_constmid *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ unkId *Rgco_constmid_op(struct Sco_constmid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_constmid)
		fprintf(stderr,"gco_constmid_op: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_constmid_op);
}
#else  /* ! __GNUC__ */
extern unkId *Rgco_constmid_op PROTO((struct Sco_constmid *));
#endif /* ! __GNUC__ */

#define gco_constmid_op(xyzxyz) (*Rgco_constmid_op((struct Sco_constmid *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ ttype *Rgco_constmid_ty(struct Sco_constmid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_constmid)
		fprintf(stderr,"gco_constmid_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_constmid_ty);
}
#else  /* ! __GNUC__ */
extern ttype *Rgco_constmid_ty PROTO((struct Sco_constmid *));
#endif /* ! __GNUC__ */

#define gco_constmid_ty(xyzxyz) (*Rgco_constmid_ty((struct Sco_constmid *) (xyzxyz)))

extern coresyn mkco_specid PROTO((coresyn, list));
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_specid_un(struct Sco_specid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_specid)
		fprintf(stderr,"gco_specid_un: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_specid_un);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_specid_un PROTO((struct Sco_specid *));
#endif /* ! __GNUC__ */

#define gco_specid_un(xyzxyz) (*Rgco_specid_un((struct Sco_specid *) (xyzxyz)))
#ifdef __GNUC__

extern __inline__ list *Rgco_specid_tys(struct Sco_specid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_specid)
		fprintf(stderr,"gco_specid_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_specid_tys);
}
#else  /* ! __GNUC__ */
extern list *Rgco_specid_tys PROTO((struct Sco_specid *));
#endif /* ! __GNUC__ */

#define gco_specid_tys(xyzxyz) (*Rgco_specid_tys((struct Sco_specid *) (xyzxyz)))

extern coresyn mkco_wrkrid PROTO((coresyn));
#ifdef __GNUC__

extern __inline__ coresyn *Rgco_wrkrid_un(struct Sco_wrkrid *t)
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_wrkrid)
		fprintf(stderr,"gco_wrkrid_un: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_wrkrid_un);
}
#else  /* ! __GNUC__ */
extern coresyn *Rgco_wrkrid_un PROTO((struct Sco_wrkrid *));
#endif /* ! __GNUC__ */

#define gco_wrkrid_un(xyzxyz) (*Rgco_wrkrid_un((struct Sco_wrkrid *) (xyzxyz)))

#endif

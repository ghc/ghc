

#include "hspincl.h"
#include "yaccParser/coresyn.h"

Tcoresyn tcoresyn(t)
 coresyn t;
{
	return(t -> tag);
}


/************** cobinder ******************/

coresyn mkcobinder(PPgcobinder_v, PPgcobinder_ty)
 unkId PPgcobinder_v;
 ttype PPgcobinder_ty;
{
	register struct Scobinder *pp =
		(struct Scobinder *) malloc(sizeof(struct Scobinder));
	pp -> tag = cobinder;
	pp -> Xgcobinder_v = PPgcobinder_v;
	pp -> Xgcobinder_ty = PPgcobinder_ty;
	return((coresyn)pp);
}

unkId *Rgcobinder_v(t)
 struct Scobinder *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cobinder)
		fprintf(stderr,"gcobinder_v: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcobinder_v);
}

ttype *Rgcobinder_ty(t)
 struct Scobinder *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cobinder)
		fprintf(stderr,"gcobinder_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcobinder_ty);
}

/************** colit ******************/

coresyn mkcolit(PPgcolit)
 literal PPgcolit;
{
	register struct Scolit *pp =
		(struct Scolit *) malloc(sizeof(struct Scolit));
	pp -> tag = colit;
	pp -> Xgcolit = PPgcolit;
	return((coresyn)pp);
}

literal *Rgcolit(t)
 struct Scolit *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != colit)
		fprintf(stderr,"gcolit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolit);
}

/************** colocal ******************/

coresyn mkcolocal(PPgcolocal_v)
 coresyn PPgcolocal_v;
{
	register struct Scolocal *pp =
		(struct Scolocal *) malloc(sizeof(struct Scolocal));
	pp -> tag = colocal;
	pp -> Xgcolocal_v = PPgcolocal_v;
	return((coresyn)pp);
}

coresyn *Rgcolocal_v(t)
 struct Scolocal *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != colocal)
		fprintf(stderr,"gcolocal_v: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolocal_v);
}

/************** cononrec ******************/

coresyn mkcononrec(PPgcononrec_b, PPgcononrec_rhs)
 coresyn PPgcononrec_b;
 coresyn PPgcononrec_rhs;
{
	register struct Scononrec *pp =
		(struct Scononrec *) malloc(sizeof(struct Scononrec));
	pp -> tag = cononrec;
	pp -> Xgcononrec_b = PPgcononrec_b;
	pp -> Xgcononrec_rhs = PPgcononrec_rhs;
	return((coresyn)pp);
}

coresyn *Rgcononrec_b(t)
 struct Scononrec *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cononrec)
		fprintf(stderr,"gcononrec_b: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcononrec_b);
}

coresyn *Rgcononrec_rhs(t)
 struct Scononrec *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cononrec)
		fprintf(stderr,"gcononrec_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcononrec_rhs);
}

/************** corec ******************/

coresyn mkcorec(PPgcorec)
 list PPgcorec;
{
	register struct Scorec *pp =
		(struct Scorec *) malloc(sizeof(struct Scorec));
	pp -> tag = corec;
	pp -> Xgcorec = PPgcorec;
	return((coresyn)pp);
}

list *Rgcorec(t)
 struct Scorec *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != corec)
		fprintf(stderr,"gcorec: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcorec);
}

/************** corec_pair ******************/

coresyn mkcorec_pair(PPgcorec_b, PPgcorec_rhs)
 coresyn PPgcorec_b;
 coresyn PPgcorec_rhs;
{
	register struct Scorec_pair *pp =
		(struct Scorec_pair *) malloc(sizeof(struct Scorec_pair));
	pp -> tag = corec_pair;
	pp -> Xgcorec_b = PPgcorec_b;
	pp -> Xgcorec_rhs = PPgcorec_rhs;
	return((coresyn)pp);
}

coresyn *Rgcorec_b(t)
 struct Scorec_pair *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != corec_pair)
		fprintf(stderr,"gcorec_b: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcorec_b);
}

coresyn *Rgcorec_rhs(t)
 struct Scorec_pair *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != corec_pair)
		fprintf(stderr,"gcorec_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcorec_rhs);
}

/************** covar ******************/

coresyn mkcovar(PPgcovar)
 coresyn PPgcovar;
{
	register struct Scovar *pp =
		(struct Scovar *) malloc(sizeof(struct Scovar));
	pp -> tag = covar;
	pp -> Xgcovar = PPgcovar;
	return((coresyn)pp);
}

coresyn *Rgcovar(t)
 struct Scovar *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != covar)
		fprintf(stderr,"gcovar: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcovar);
}

/************** coliteral ******************/

coresyn mkcoliteral(PPgcoliteral)
 literal PPgcoliteral;
{
	register struct Scoliteral *pp =
		(struct Scoliteral *) malloc(sizeof(struct Scoliteral));
	pp -> tag = coliteral;
	pp -> Xgcoliteral = PPgcoliteral;
	return((coresyn)pp);
}

literal *Rgcoliteral(t)
 struct Scoliteral *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coliteral)
		fprintf(stderr,"gcoliteral: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoliteral);
}

/************** cocon ******************/

coresyn mkcocon(PPgcocon_con, PPgcocon_tys, PPgcocon_args)
 coresyn PPgcocon_con;
 list PPgcocon_tys;
 list PPgcocon_args;
{
	register struct Scocon *pp =
		(struct Scocon *) malloc(sizeof(struct Scocon));
	pp -> tag = cocon;
	pp -> Xgcocon_con = PPgcocon_con;
	pp -> Xgcocon_tys = PPgcocon_tys;
	pp -> Xgcocon_args = PPgcocon_args;
	return((coresyn)pp);
}

coresyn *Rgcocon_con(t)
 struct Scocon *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocon)
		fprintf(stderr,"gcocon_con: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocon_con);
}

list *Rgcocon_tys(t)
 struct Scocon *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocon)
		fprintf(stderr,"gcocon_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocon_tys);
}

list *Rgcocon_args(t)
 struct Scocon *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocon)
		fprintf(stderr,"gcocon_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocon_args);
}

/************** coprim ******************/

coresyn mkcoprim(PPgcoprim_op, PPgcoprim_tys, PPgcoprim_args)
 coresyn PPgcoprim_op;
 list PPgcoprim_tys;
 list PPgcoprim_args;
{
	register struct Scoprim *pp =
		(struct Scoprim *) malloc(sizeof(struct Scoprim));
	pp -> tag = coprim;
	pp -> Xgcoprim_op = PPgcoprim_op;
	pp -> Xgcoprim_tys = PPgcoprim_tys;
	pp -> Xgcoprim_args = PPgcoprim_args;
	return((coresyn)pp);
}

coresyn *Rgcoprim_op(t)
 struct Scoprim *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim)
		fprintf(stderr,"gcoprim_op: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_op);
}

list *Rgcoprim_tys(t)
 struct Scoprim *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim)
		fprintf(stderr,"gcoprim_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_tys);
}

list *Rgcoprim_args(t)
 struct Scoprim *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim)
		fprintf(stderr,"gcoprim_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_args);
}

/************** colam ******************/

coresyn mkcolam(PPgcolam_vars, PPgcolam_body)
 list PPgcolam_vars;
 coresyn PPgcolam_body;
{
	register struct Scolam *pp =
		(struct Scolam *) malloc(sizeof(struct Scolam));
	pp -> tag = colam;
	pp -> Xgcolam_vars = PPgcolam_vars;
	pp -> Xgcolam_body = PPgcolam_body;
	return((coresyn)pp);
}

list *Rgcolam_vars(t)
 struct Scolam *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != colam)
		fprintf(stderr,"gcolam_vars: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolam_vars);
}

coresyn *Rgcolam_body(t)
 struct Scolam *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != colam)
		fprintf(stderr,"gcolam_body: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolam_body);
}

/************** cotylam ******************/

coresyn mkcotylam(PPgcotylam_tvs, PPgcotylam_body)
 list PPgcotylam_tvs;
 coresyn PPgcotylam_body;
{
	register struct Scotylam *pp =
		(struct Scotylam *) malloc(sizeof(struct Scotylam));
	pp -> tag = cotylam;
	pp -> Xgcotylam_tvs = PPgcotylam_tvs;
	pp -> Xgcotylam_body = PPgcotylam_body;
	return((coresyn)pp);
}

list *Rgcotylam_tvs(t)
 struct Scotylam *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cotylam)
		fprintf(stderr,"gcotylam_tvs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcotylam_tvs);
}

coresyn *Rgcotylam_body(t)
 struct Scotylam *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cotylam)
		fprintf(stderr,"gcotylam_body: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcotylam_body);
}

/************** coapp ******************/

coresyn mkcoapp(PPgcoapp_fun, PPgcoapp_args)
 coresyn PPgcoapp_fun;
 list PPgcoapp_args;
{
	register struct Scoapp *pp =
		(struct Scoapp *) malloc(sizeof(struct Scoapp));
	pp -> tag = coapp;
	pp -> Xgcoapp_fun = PPgcoapp_fun;
	pp -> Xgcoapp_args = PPgcoapp_args;
	return((coresyn)pp);
}

coresyn *Rgcoapp_fun(t)
 struct Scoapp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coapp)
		fprintf(stderr,"gcoapp_fun: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoapp_fun);
}

list *Rgcoapp_args(t)
 struct Scoapp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coapp)
		fprintf(stderr,"gcoapp_args: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoapp_args);
}

/************** cotyapp ******************/

coresyn mkcotyapp(PPgcotyapp_e, PPgcotyapp_t)
 coresyn PPgcotyapp_e;
 ttype PPgcotyapp_t;
{
	register struct Scotyapp *pp =
		(struct Scotyapp *) malloc(sizeof(struct Scotyapp));
	pp -> tag = cotyapp;
	pp -> Xgcotyapp_e = PPgcotyapp_e;
	pp -> Xgcotyapp_t = PPgcotyapp_t;
	return((coresyn)pp);
}

coresyn *Rgcotyapp_e(t)
 struct Scotyapp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cotyapp)
		fprintf(stderr,"gcotyapp_e: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcotyapp_e);
}

ttype *Rgcotyapp_t(t)
 struct Scotyapp *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cotyapp)
		fprintf(stderr,"gcotyapp_t: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcotyapp_t);
}

/************** cocase ******************/

coresyn mkcocase(PPgcocase_s, PPgcocase_alts)
 coresyn PPgcocase_s;
 coresyn PPgcocase_alts;
{
	register struct Scocase *pp =
		(struct Scocase *) malloc(sizeof(struct Scocase));
	pp -> tag = cocase;
	pp -> Xgcocase_s = PPgcocase_s;
	pp -> Xgcocase_alts = PPgcocase_alts;
	return((coresyn)pp);
}

coresyn *Rgcocase_s(t)
 struct Scocase *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocase)
		fprintf(stderr,"gcocase_s: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocase_s);
}

coresyn *Rgcocase_alts(t)
 struct Scocase *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cocase)
		fprintf(stderr,"gcocase_alts: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcocase_alts);
}

/************** colet ******************/

coresyn mkcolet(PPgcolet_bind, PPgcolet_body)
 coresyn PPgcolet_bind;
 coresyn PPgcolet_body;
{
	register struct Scolet *pp =
		(struct Scolet *) malloc(sizeof(struct Scolet));
	pp -> tag = colet;
	pp -> Xgcolet_bind = PPgcolet_bind;
	pp -> Xgcolet_body = PPgcolet_body;
	return((coresyn)pp);
}

coresyn *Rgcolet_bind(t)
 struct Scolet *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != colet)
		fprintf(stderr,"gcolet_bind: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolet_bind);
}

coresyn *Rgcolet_body(t)
 struct Scolet *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != colet)
		fprintf(stderr,"gcolet_body: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcolet_body);
}

/************** coscc ******************/

coresyn mkcoscc(PPgcoscc_scc, PPgcoscc_body)
 coresyn PPgcoscc_scc;
 coresyn PPgcoscc_body;
{
	register struct Scoscc *pp =
		(struct Scoscc *) malloc(sizeof(struct Scoscc));
	pp -> tag = coscc;
	pp -> Xgcoscc_scc = PPgcoscc_scc;
	pp -> Xgcoscc_body = PPgcoscc_body;
	return((coresyn)pp);
}

coresyn *Rgcoscc_scc(t)
 struct Scoscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coscc)
		fprintf(stderr,"gcoscc_scc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoscc_scc);
}

coresyn *Rgcoscc_body(t)
 struct Scoscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coscc)
		fprintf(stderr,"gcoscc_body: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoscc_body);
}

/************** coalg_alts ******************/

coresyn mkcoalg_alts(PPgcoalg_alts, PPgcoalg_deflt)
 list PPgcoalg_alts;
 coresyn PPgcoalg_deflt;
{
	register struct Scoalg_alts *pp =
		(struct Scoalg_alts *) malloc(sizeof(struct Scoalg_alts));
	pp -> tag = coalg_alts;
	pp -> Xgcoalg_alts = PPgcoalg_alts;
	pp -> Xgcoalg_deflt = PPgcoalg_deflt;
	return((coresyn)pp);
}

list *Rgcoalg_alts(t)
 struct Scoalg_alts *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alts)
		fprintf(stderr,"gcoalg_alts: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_alts);
}

coresyn *Rgcoalg_deflt(t)
 struct Scoalg_alts *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alts)
		fprintf(stderr,"gcoalg_deflt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_deflt);
}

/************** coalg_alt ******************/

coresyn mkcoalg_alt(PPgcoalg_con, PPgcoalg_bs, PPgcoalg_rhs)
 coresyn PPgcoalg_con;
 list PPgcoalg_bs;
 coresyn PPgcoalg_rhs;
{
	register struct Scoalg_alt *pp =
		(struct Scoalg_alt *) malloc(sizeof(struct Scoalg_alt));
	pp -> tag = coalg_alt;
	pp -> Xgcoalg_con = PPgcoalg_con;
	pp -> Xgcoalg_bs = PPgcoalg_bs;
	pp -> Xgcoalg_rhs = PPgcoalg_rhs;
	return((coresyn)pp);
}

coresyn *Rgcoalg_con(t)
 struct Scoalg_alt *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alt)
		fprintf(stderr,"gcoalg_con: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_con);
}

list *Rgcoalg_bs(t)
 struct Scoalg_alt *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alt)
		fprintf(stderr,"gcoalg_bs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_bs);
}

coresyn *Rgcoalg_rhs(t)
 struct Scoalg_alt *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coalg_alt)
		fprintf(stderr,"gcoalg_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoalg_rhs);
}

/************** coprim_alts ******************/

coresyn mkcoprim_alts(PPgcoprim_alts, PPgcoprim_deflt)
 list PPgcoprim_alts;
 coresyn PPgcoprim_deflt;
{
	register struct Scoprim_alts *pp =
		(struct Scoprim_alts *) malloc(sizeof(struct Scoprim_alts));
	pp -> tag = coprim_alts;
	pp -> Xgcoprim_alts = PPgcoprim_alts;
	pp -> Xgcoprim_deflt = PPgcoprim_deflt;
	return((coresyn)pp);
}

list *Rgcoprim_alts(t)
 struct Scoprim_alts *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim_alts)
		fprintf(stderr,"gcoprim_alts: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_alts);
}

coresyn *Rgcoprim_deflt(t)
 struct Scoprim_alts *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim_alts)
		fprintf(stderr,"gcoprim_deflt: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_deflt);
}

/************** coprim_alt ******************/

coresyn mkcoprim_alt(PPgcoprim_lit, PPgcoprim_rhs)
 literal PPgcoprim_lit;
 coresyn PPgcoprim_rhs;
{
	register struct Scoprim_alt *pp =
		(struct Scoprim_alt *) malloc(sizeof(struct Scoprim_alt));
	pp -> tag = coprim_alt;
	pp -> Xgcoprim_lit = PPgcoprim_lit;
	pp -> Xgcoprim_rhs = PPgcoprim_rhs;
	return((coresyn)pp);
}

literal *Rgcoprim_lit(t)
 struct Scoprim_alt *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim_alt)
		fprintf(stderr,"gcoprim_lit: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_lit);
}

coresyn *Rgcoprim_rhs(t)
 struct Scoprim_alt *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != coprim_alt)
		fprintf(stderr,"gcoprim_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcoprim_rhs);
}

/************** conodeflt ******************/

coresyn mkconodeflt(void)
{
	register struct Sconodeflt *pp =
		(struct Sconodeflt *) malloc(sizeof(struct Sconodeflt));
	pp -> tag = conodeflt;
	return((coresyn)pp);
}

/************** cobinddeflt ******************/

coresyn mkcobinddeflt(PPgcobinddeflt_v, PPgcobinddeflt_rhs)
 coresyn PPgcobinddeflt_v;
 coresyn PPgcobinddeflt_rhs;
{
	register struct Scobinddeflt *pp =
		(struct Scobinddeflt *) malloc(sizeof(struct Scobinddeflt));
	pp -> tag = cobinddeflt;
	pp -> Xgcobinddeflt_v = PPgcobinddeflt_v;
	pp -> Xgcobinddeflt_rhs = PPgcobinddeflt_rhs;
	return((coresyn)pp);
}

coresyn *Rgcobinddeflt_v(t)
 struct Scobinddeflt *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cobinddeflt)
		fprintf(stderr,"gcobinddeflt_v: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcobinddeflt_v);
}

coresyn *Rgcobinddeflt_rhs(t)
 struct Scobinddeflt *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != cobinddeflt)
		fprintf(stderr,"gcobinddeflt_rhs: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgcobinddeflt_rhs);
}

/************** co_primop ******************/

coresyn mkco_primop(PPgco_primop)
 stringId PPgco_primop;
{
	register struct Sco_primop *pp =
		(struct Sco_primop *) malloc(sizeof(struct Sco_primop));
	pp -> tag = co_primop;
	pp -> Xgco_primop = PPgco_primop;
	return((coresyn)pp);
}

stringId *Rgco_primop(t)
 struct Sco_primop *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_primop)
		fprintf(stderr,"gco_primop: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_primop);
}

/************** co_ccall ******************/

coresyn mkco_ccall(PPgco_ccall, PPgco_ccall_may_gc, PPgco_ccall_arg_tys, PPgco_ccall_res_ty)
 stringId PPgco_ccall;
 long PPgco_ccall_may_gc;
 list PPgco_ccall_arg_tys;
 ttype PPgco_ccall_res_ty;
{
	register struct Sco_ccall *pp =
		(struct Sco_ccall *) malloc(sizeof(struct Sco_ccall));
	pp -> tag = co_ccall;
	pp -> Xgco_ccall = PPgco_ccall;
	pp -> Xgco_ccall_may_gc = PPgco_ccall_may_gc;
	pp -> Xgco_ccall_arg_tys = PPgco_ccall_arg_tys;
	pp -> Xgco_ccall_res_ty = PPgco_ccall_res_ty;
	return((coresyn)pp);
}

stringId *Rgco_ccall(t)
 struct Sco_ccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_ccall)
		fprintf(stderr,"gco_ccall: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_ccall);
}

long *Rgco_ccall_may_gc(t)
 struct Sco_ccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_ccall)
		fprintf(stderr,"gco_ccall_may_gc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_ccall_may_gc);
}

list *Rgco_ccall_arg_tys(t)
 struct Sco_ccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_ccall)
		fprintf(stderr,"gco_ccall_arg_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_ccall_arg_tys);
}

ttype *Rgco_ccall_res_ty(t)
 struct Sco_ccall *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_ccall)
		fprintf(stderr,"gco_ccall_res_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_ccall_res_ty);
}

/************** co_casm ******************/

coresyn mkco_casm(PPgco_casm, PPgco_casm_may_gc, PPgco_casm_arg_tys, PPgco_casm_res_ty)
 literal PPgco_casm;
 long PPgco_casm_may_gc;
 list PPgco_casm_arg_tys;
 ttype PPgco_casm_res_ty;
{
	register struct Sco_casm *pp =
		(struct Sco_casm *) malloc(sizeof(struct Sco_casm));
	pp -> tag = co_casm;
	pp -> Xgco_casm = PPgco_casm;
	pp -> Xgco_casm_may_gc = PPgco_casm_may_gc;
	pp -> Xgco_casm_arg_tys = PPgco_casm_arg_tys;
	pp -> Xgco_casm_res_ty = PPgco_casm_res_ty;
	return((coresyn)pp);
}

literal *Rgco_casm(t)
 struct Sco_casm *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_casm)
		fprintf(stderr,"gco_casm: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_casm);
}

long *Rgco_casm_may_gc(t)
 struct Sco_casm *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_casm)
		fprintf(stderr,"gco_casm_may_gc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_casm_may_gc);
}

list *Rgco_casm_arg_tys(t)
 struct Sco_casm *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_casm)
		fprintf(stderr,"gco_casm_arg_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_casm_arg_tys);
}

ttype *Rgco_casm_res_ty(t)
 struct Sco_casm *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_casm)
		fprintf(stderr,"gco_casm_res_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_casm_res_ty);
}

/************** co_preludedictscc ******************/

coresyn mkco_preludedictscc(PPgco_preludedictscc_dupd)
 coresyn PPgco_preludedictscc_dupd;
{
	register struct Sco_preludedictscc *pp =
		(struct Sco_preludedictscc *) malloc(sizeof(struct Sco_preludedictscc));
	pp -> tag = co_preludedictscc;
	pp -> Xgco_preludedictscc_dupd = PPgco_preludedictscc_dupd;
	return((coresyn)pp);
}

coresyn *Rgco_preludedictscc_dupd(t)
 struct Sco_preludedictscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_preludedictscc)
		fprintf(stderr,"gco_preludedictscc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_preludedictscc_dupd);
}

/************** co_alldictscc ******************/

coresyn mkco_alldictscc(PPgco_alldictscc_m, PPgco_alldictscc_g, PPgco_alldictscc_dupd)
 hstring PPgco_alldictscc_m;
 hstring PPgco_alldictscc_g;
 coresyn PPgco_alldictscc_dupd;
{
	register struct Sco_alldictscc *pp =
		(struct Sco_alldictscc *) malloc(sizeof(struct Sco_alldictscc));
	pp -> tag = co_alldictscc;
	pp -> Xgco_alldictscc_m = PPgco_alldictscc_m;
	pp -> Xgco_alldictscc_g = PPgco_alldictscc_g;
	pp -> Xgco_alldictscc_dupd = PPgco_alldictscc_dupd;
	return((coresyn)pp);
}

hstring *Rgco_alldictscc_m(t)
 struct Sco_alldictscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_alldictscc)
		fprintf(stderr,"gco_alldictscc_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_alldictscc_m);
}

hstring *Rgco_alldictscc_g(t)
 struct Sco_alldictscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_alldictscc)
		fprintf(stderr,"gco_alldictscc_g: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_alldictscc_g);
}

coresyn *Rgco_alldictscc_dupd(t)
 struct Sco_alldictscc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_alldictscc)
		fprintf(stderr,"gco_alldictscc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_alldictscc_dupd);
}

/************** co_usercc ******************/

coresyn mkco_usercc(PPgco_usercc_n, PPgco_usercc_m, PPgco_usercc_g, PPgco_usercc_dupd, PPgco_usercc_cafd)
 hstring PPgco_usercc_n;
 hstring PPgco_usercc_m;
 hstring PPgco_usercc_g;
 coresyn PPgco_usercc_dupd;
 coresyn PPgco_usercc_cafd;
{
	register struct Sco_usercc *pp =
		(struct Sco_usercc *) malloc(sizeof(struct Sco_usercc));
	pp -> tag = co_usercc;
	pp -> Xgco_usercc_n = PPgco_usercc_n;
	pp -> Xgco_usercc_m = PPgco_usercc_m;
	pp -> Xgco_usercc_g = PPgco_usercc_g;
	pp -> Xgco_usercc_dupd = PPgco_usercc_dupd;
	pp -> Xgco_usercc_cafd = PPgco_usercc_cafd;
	return((coresyn)pp);
}

hstring *Rgco_usercc_n(t)
 struct Sco_usercc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_n: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_n);
}

hstring *Rgco_usercc_m(t)
 struct Sco_usercc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_m);
}

hstring *Rgco_usercc_g(t)
 struct Sco_usercc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_g: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_g);
}

coresyn *Rgco_usercc_dupd(t)
 struct Sco_usercc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_dupd);
}

coresyn *Rgco_usercc_cafd(t)
 struct Sco_usercc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_usercc)
		fprintf(stderr,"gco_usercc_cafd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_usercc_cafd);
}

/************** co_autocc ******************/

coresyn mkco_autocc(PPgco_autocc_i, PPgco_autocc_m, PPgco_autocc_g, PPgco_autocc_dupd, PPgco_autocc_cafd)
 coresyn PPgco_autocc_i;
 hstring PPgco_autocc_m;
 hstring PPgco_autocc_g;
 coresyn PPgco_autocc_dupd;
 coresyn PPgco_autocc_cafd;
{
	register struct Sco_autocc *pp =
		(struct Sco_autocc *) malloc(sizeof(struct Sco_autocc));
	pp -> tag = co_autocc;
	pp -> Xgco_autocc_i = PPgco_autocc_i;
	pp -> Xgco_autocc_m = PPgco_autocc_m;
	pp -> Xgco_autocc_g = PPgco_autocc_g;
	pp -> Xgco_autocc_dupd = PPgco_autocc_dupd;
	pp -> Xgco_autocc_cafd = PPgco_autocc_cafd;
	return((coresyn)pp);
}

coresyn *Rgco_autocc_i(t)
 struct Sco_autocc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_i: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_i);
}

hstring *Rgco_autocc_m(t)
 struct Sco_autocc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_m);
}

hstring *Rgco_autocc_g(t)
 struct Sco_autocc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_g: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_g);
}

coresyn *Rgco_autocc_dupd(t)
 struct Sco_autocc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_dupd);
}

coresyn *Rgco_autocc_cafd(t)
 struct Sco_autocc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_autocc)
		fprintf(stderr,"gco_autocc_cafd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_autocc_cafd);
}

/************** co_dictcc ******************/

coresyn mkco_dictcc(PPgco_dictcc_i, PPgco_dictcc_m, PPgco_dictcc_g, PPgco_dictcc_dupd, PPgco_dictcc_cafd)
 coresyn PPgco_dictcc_i;
 hstring PPgco_dictcc_m;
 hstring PPgco_dictcc_g;
 coresyn PPgco_dictcc_dupd;
 coresyn PPgco_dictcc_cafd;
{
	register struct Sco_dictcc *pp =
		(struct Sco_dictcc *) malloc(sizeof(struct Sco_dictcc));
	pp -> tag = co_dictcc;
	pp -> Xgco_dictcc_i = PPgco_dictcc_i;
	pp -> Xgco_dictcc_m = PPgco_dictcc_m;
	pp -> Xgco_dictcc_g = PPgco_dictcc_g;
	pp -> Xgco_dictcc_dupd = PPgco_dictcc_dupd;
	pp -> Xgco_dictcc_cafd = PPgco_dictcc_cafd;
	return((coresyn)pp);
}

coresyn *Rgco_dictcc_i(t)
 struct Sco_dictcc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_i: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_i);
}

hstring *Rgco_dictcc_m(t)
 struct Sco_dictcc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_m);
}

hstring *Rgco_dictcc_g(t)
 struct Sco_dictcc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_g: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_g);
}

coresyn *Rgco_dictcc_dupd(t)
 struct Sco_dictcc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_dupd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_dupd);
}

coresyn *Rgco_dictcc_cafd(t)
 struct Sco_dictcc *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dictcc)
		fprintf(stderr,"gco_dictcc_cafd: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dictcc_cafd);
}

/************** co_scc_noncaf ******************/

coresyn mkco_scc_noncaf(void)
{
	register struct Sco_scc_noncaf *pp =
		(struct Sco_scc_noncaf *) malloc(sizeof(struct Sco_scc_noncaf));
	pp -> tag = co_scc_noncaf;
	return((coresyn)pp);
}

/************** co_scc_caf ******************/

coresyn mkco_scc_caf(void)
{
	register struct Sco_scc_caf *pp =
		(struct Sco_scc_caf *) malloc(sizeof(struct Sco_scc_caf));
	pp -> tag = co_scc_caf;
	return((coresyn)pp);
}

/************** co_scc_nondupd ******************/

coresyn mkco_scc_nondupd(void)
{
	register struct Sco_scc_nondupd *pp =
		(struct Sco_scc_nondupd *) malloc(sizeof(struct Sco_scc_nondupd));
	pp -> tag = co_scc_nondupd;
	return((coresyn)pp);
}

/************** co_scc_dupd ******************/

coresyn mkco_scc_dupd(void)
{
	register struct Sco_scc_dupd *pp =
		(struct Sco_scc_dupd *) malloc(sizeof(struct Sco_scc_dupd));
	pp -> tag = co_scc_dupd;
	return((coresyn)pp);
}

/************** co_id ******************/

coresyn mkco_id(PPgco_id)
 stringId PPgco_id;
{
	register struct Sco_id *pp =
		(struct Sco_id *) malloc(sizeof(struct Sco_id));
	pp -> tag = co_id;
	pp -> Xgco_id = PPgco_id;
	return((coresyn)pp);
}

stringId *Rgco_id(t)
 struct Sco_id *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_id)
		fprintf(stderr,"gco_id: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_id);
}

/************** co_orig_id ******************/

coresyn mkco_orig_id(PPgco_orig_id_m, PPgco_orig_id_n)
 stringId PPgco_orig_id_m;
 stringId PPgco_orig_id_n;
{
	register struct Sco_orig_id *pp =
		(struct Sco_orig_id *) malloc(sizeof(struct Sco_orig_id));
	pp -> tag = co_orig_id;
	pp -> Xgco_orig_id_m = PPgco_orig_id_m;
	pp -> Xgco_orig_id_n = PPgco_orig_id_n;
	return((coresyn)pp);
}

stringId *Rgco_orig_id_m(t)
 struct Sco_orig_id *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_orig_id)
		fprintf(stderr,"gco_orig_id_m: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_orig_id_m);
}

stringId *Rgco_orig_id_n(t)
 struct Sco_orig_id *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_orig_id)
		fprintf(stderr,"gco_orig_id_n: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_orig_id_n);
}

/************** co_sdselid ******************/

coresyn mkco_sdselid(PPgco_sdselid_c, PPgco_sdselid_sc)
 unkId PPgco_sdselid_c;
 unkId PPgco_sdselid_sc;
{
	register struct Sco_sdselid *pp =
		(struct Sco_sdselid *) malloc(sizeof(struct Sco_sdselid));
	pp -> tag = co_sdselid;
	pp -> Xgco_sdselid_c = PPgco_sdselid_c;
	pp -> Xgco_sdselid_sc = PPgco_sdselid_sc;
	return((coresyn)pp);
}

unkId *Rgco_sdselid_c(t)
 struct Sco_sdselid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_sdselid)
		fprintf(stderr,"gco_sdselid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_sdselid_c);
}

unkId *Rgco_sdselid_sc(t)
 struct Sco_sdselid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_sdselid)
		fprintf(stderr,"gco_sdselid_sc: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_sdselid_sc);
}

/************** co_classopid ******************/

coresyn mkco_classopid(PPgco_classopid_c, PPgco_classopid_o)
 unkId PPgco_classopid_c;
 unkId PPgco_classopid_o;
{
	register struct Sco_classopid *pp =
		(struct Sco_classopid *) malloc(sizeof(struct Sco_classopid));
	pp -> tag = co_classopid;
	pp -> Xgco_classopid_c = PPgco_classopid_c;
	pp -> Xgco_classopid_o = PPgco_classopid_o;
	return((coresyn)pp);
}

unkId *Rgco_classopid_c(t)
 struct Sco_classopid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_classopid)
		fprintf(stderr,"gco_classopid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_classopid_c);
}

unkId *Rgco_classopid_o(t)
 struct Sco_classopid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_classopid)
		fprintf(stderr,"gco_classopid_o: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_classopid_o);
}

/************** co_defmid ******************/

coresyn mkco_defmid(PPgco_defmid_c, PPgco_defmid_op)
 unkId PPgco_defmid_c;
 unkId PPgco_defmid_op;
{
	register struct Sco_defmid *pp =
		(struct Sco_defmid *) malloc(sizeof(struct Sco_defmid));
	pp -> tag = co_defmid;
	pp -> Xgco_defmid_c = PPgco_defmid_c;
	pp -> Xgco_defmid_op = PPgco_defmid_op;
	return((coresyn)pp);
}

unkId *Rgco_defmid_c(t)
 struct Sco_defmid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_defmid)
		fprintf(stderr,"gco_defmid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_defmid_c);
}

unkId *Rgco_defmid_op(t)
 struct Sco_defmid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_defmid)
		fprintf(stderr,"gco_defmid_op: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_defmid_op);
}

/************** co_dfunid ******************/

coresyn mkco_dfunid(PPgco_dfunid_c, PPgco_dfunid_ty)
 unkId PPgco_dfunid_c;
 ttype PPgco_dfunid_ty;
{
	register struct Sco_dfunid *pp =
		(struct Sco_dfunid *) malloc(sizeof(struct Sco_dfunid));
	pp -> tag = co_dfunid;
	pp -> Xgco_dfunid_c = PPgco_dfunid_c;
	pp -> Xgco_dfunid_ty = PPgco_dfunid_ty;
	return((coresyn)pp);
}

unkId *Rgco_dfunid_c(t)
 struct Sco_dfunid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dfunid)
		fprintf(stderr,"gco_dfunid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dfunid_c);
}

ttype *Rgco_dfunid_ty(t)
 struct Sco_dfunid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_dfunid)
		fprintf(stderr,"gco_dfunid_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_dfunid_ty);
}

/************** co_constmid ******************/

coresyn mkco_constmid(PPgco_constmid_c, PPgco_constmid_op, PPgco_constmid_ty)
 unkId PPgco_constmid_c;
 unkId PPgco_constmid_op;
 ttype PPgco_constmid_ty;
{
	register struct Sco_constmid *pp =
		(struct Sco_constmid *) malloc(sizeof(struct Sco_constmid));
	pp -> tag = co_constmid;
	pp -> Xgco_constmid_c = PPgco_constmid_c;
	pp -> Xgco_constmid_op = PPgco_constmid_op;
	pp -> Xgco_constmid_ty = PPgco_constmid_ty;
	return((coresyn)pp);
}

unkId *Rgco_constmid_c(t)
 struct Sco_constmid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_constmid)
		fprintf(stderr,"gco_constmid_c: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_constmid_c);
}

unkId *Rgco_constmid_op(t)
 struct Sco_constmid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_constmid)
		fprintf(stderr,"gco_constmid_op: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_constmid_op);
}

ttype *Rgco_constmid_ty(t)
 struct Sco_constmid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_constmid)
		fprintf(stderr,"gco_constmid_ty: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_constmid_ty);
}

/************** co_specid ******************/

coresyn mkco_specid(PPgco_specid_un, PPgco_specid_tys)
 coresyn PPgco_specid_un;
 list PPgco_specid_tys;
{
	register struct Sco_specid *pp =
		(struct Sco_specid *) malloc(sizeof(struct Sco_specid));
	pp -> tag = co_specid;
	pp -> Xgco_specid_un = PPgco_specid_un;
	pp -> Xgco_specid_tys = PPgco_specid_tys;
	return((coresyn)pp);
}

coresyn *Rgco_specid_un(t)
 struct Sco_specid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_specid)
		fprintf(stderr,"gco_specid_un: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_specid_un);
}

list *Rgco_specid_tys(t)
 struct Sco_specid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_specid)
		fprintf(stderr,"gco_specid_tys: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_specid_tys);
}

/************** co_wrkrid ******************/

coresyn mkco_wrkrid(PPgco_wrkrid_un)
 coresyn PPgco_wrkrid_un;
{
	register struct Sco_wrkrid *pp =
		(struct Sco_wrkrid *) malloc(sizeof(struct Sco_wrkrid));
	pp -> tag = co_wrkrid;
	pp -> Xgco_wrkrid_un = PPgco_wrkrid_un;
	return((coresyn)pp);
}

coresyn *Rgco_wrkrid_un(t)
 struct Sco_wrkrid *t;
{
#ifdef UGEN_DEBUG
	if(t -> tag != co_wrkrid)
		fprintf(stderr,"gco_wrkrid_un: illegal selection; was %d\n", t -> tag);
#endif /* UGEN_DEBUG */
	return(& t -> Xgco_wrkrid_un);
}

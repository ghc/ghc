/* fwd decls*/
extern void g_consels PROTO((tree, id));
extern void g_tagfun PROTO((id));
extern void g_typconsel PROTO((tree, id));
extern void ge_typdef PROTO((tree));
extern void ge_typlist PROTO((tree));
extern void gencons PROTO((id, tree));
extern void genmkfillin PROTO((tree));
extern void genmkparamdekl PROTO((tree));
extern void genmkparamlist PROTO((tree));
extern void genmkprotodekl PROTO((tree));
extern void gensels PROTO((id, id, tree));
extern void gentype PROTO((tree));

extern void gs_def PROTO((tree, id));
extern void gs_itemlist PROTO((tree));
extern void gs_typlist PROTO((tree, id));

extern void hs_def PROTO((tree));
extern void hs_itemlist PROTO((tree));
extern void hs_typlist PROTO((tree));
extern void gen_hs_reader PROTO((id, tree));
extern void gen_hs_rdalts PROTO((id, tree));
extern void gen_hs_rdalt  PROTO((id, tree));
extern void gen_hs_rdcomponents PROTO((id, id, tree));
extern void gen_hs_retcomponents PROTO((id, id, tree));

extern id   installid PROTO((char *));

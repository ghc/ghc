/* fwd decls*/
void g_consels PROTO((tree, id));
void g_tagfun PROTO((id));
void g_typconsel PROTO((tree, id));
void ge_typdef PROTO((tree));
void ge_typlist PROTO((tree));
void gencons PROTO((id, tree));
void genmkfillin PROTO((tree));
void genmkparamdekl PROTO((tree));
void genmkparamlist PROTO((tree));
void genmkprotodekl PROTO((tree));
void gensels PROTO((id, id, tree));
void gentype PROTO((tree));

void gs_def PROTO((tree, id));
void gs_itemlist PROTO((tree));
void gs_typlist PROTO((tree, id));

void hs_def PROTO((tree));
void hs_itemlist PROTO((tree));
void hs_typlist PROTO((tree));
void gen_hs_reader PROTO((id, tree));
void gen_hs_rdalts PROTO((id, tree));
void gen_hs_rdalt  PROTO((id, tree));
void gen_hs_rdcomponents PROTO((id, id, tree));
void gen_hs_retcomponents PROTO((id, id, tree));

id   installid PROTO((char *));

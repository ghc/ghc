typedef union {
	tree utree;
	list ulist;
	ttype uttype;
	atype uatype;
	binding ubinding;
	pbinding upbinding;
	finfot ufinfo;
	entidt uentid;
	id uid;
	literal uliteral;
	int uint;
	float ufloat;
	char *ustring;
	hstring uhstring;
	hpragma uhpragma;
	coresyn ucoresyn;
} YYSTYPE;
#define	VARID	258
#define	CONID	259
#define	VARSYM	260
#define	CONSYM	261
#define	MINUS	262
#define	INTEGER	263
#define	FLOAT	264
#define	CHAR	265
#define	STRING	266
#define	CHARPRIM	267
#define	STRINGPRIM	268
#define	INTPRIM	269
#define	FLOATPRIM	270
#define	DOUBLEPRIM	271
#define	CLITLIT	272
#define	OCURLY	273
#define	CCURLY	274
#define	VCCURLY	275
#define	SEMI	276
#define	OBRACK	277
#define	CBRACK	278
#define	OPAREN	279
#define	CPAREN	280
#define	COMMA	281
#define	BQUOTE	282
#define	RARROW	283
#define	VBAR	284
#define	EQUAL	285
#define	DARROW	286
#define	DOTDOT	287
#define	DCOLON	288
#define	LARROW	289
#define	WILDCARD	290
#define	AT	291
#define	LAZY	292
#define	LAMBDA	293
#define	LET	294
#define	IN	295
#define	WHERE	296
#define	CASE	297
#define	OF	298
#define	TYPE	299
#define	DATA	300
#define	CLASS	301
#define	INSTANCE	302
#define	DEFAULT	303
#define	INFIX	304
#define	INFIXL	305
#define	INFIXR	306
#define	MODULE	307
#define	IMPORT	308
#define	INTERFACE	309
#define	HIDING	310
#define	CCALL	311
#define	CCALL_GC	312
#define	CASM	313
#define	CASM_GC	314
#define	SCC	315
#define	IF	316
#define	THEN	317
#define	ELSE	318
#define	RENAMING	319
#define	DERIVING	320
#define	TO	321
#define	LEOF	322
#define	GHC_PRAGMA	323
#define	END_PRAGMA	324
#define	NO_PRAGMA	325
#define	NOINFO_PRAGMA	326
#define	ABSTRACT_PRAGMA	327
#define	SPECIALISE_PRAGMA	328
#define	MODNAME_PRAGMA	329
#define	ARITY_PRAGMA	330
#define	UPDATE_PRAGMA	331
#define	STRICTNESS_PRAGMA	332
#define	KIND_PRAGMA	333
#define	UNFOLDING_PRAGMA	334
#define	MAGIC_UNFOLDING_PRAGMA	335
#define	DEFOREST_PRAGMA	336
#define	SPECIALISE_UPRAGMA	337
#define	INLINE_UPRAGMA	338
#define	MAGIC_UNFOLDING_UPRAGMA	339
#define	ABSTRACT_UPRAGMA	340
#define	DEFOREST_UPRAGMA	341
#define	END_UPRAGMA	342
#define	TYLAMBDA	343
#define	COCON	344
#define	COPRIM	345
#define	COAPP	346
#define	COTYAPP	347
#define	FORALL	348
#define	TYVAR_TEMPLATE_ID	349
#define	CO_ALG_ALTS	350
#define	CO_PRIM_ALTS	351
#define	CO_NO_DEFAULT	352
#define	CO_LETREC	353
#define	CO_SDSEL_ID	354
#define	CO_METH_ID	355
#define	CO_DEFM_ID	356
#define	CO_DFUN_ID	357
#define	CO_CONSTM_ID	358
#define	CO_SPEC_ID	359
#define	CO_WRKR_ID	360
#define	CO_ORIG_NM	361
#define	UNFOLD_ALWAYS	362
#define	UNFOLD_IF_ARGS	363
#define	NOREP_INTEGER	364
#define	NOREP_RATIONAL	365
#define	NOREP_STRING	366
#define	CO_PRELUDE_DICTS_CC	367
#define	CO_ALL_DICTS_CC	368
#define	CO_USER_CC	369
#define	CO_AUTO_CC	370
#define	CO_DICT_CC	371
#define	CO_CAF_CC	372
#define	CO_DUPD_CC	373
#define	PLUS	374


extern YYSTYPE yylval;

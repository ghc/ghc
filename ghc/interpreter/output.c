
/* --------------------------------------------------------------------------
 * Unparse expressions and types - for use in error messages, type checker
 * and for debugging.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: output.c,v $
 * $Revision: 1.17 $
 * $Date: 2000/03/23 14:54:21 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <ctype.h>

#define DEPTH_LIMIT     15

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local put            ( Int,Cell );
static Void local putFlds        ( Cell,List );
static Void local putComp        ( Cell,List );
static Void local putQual        ( Cell );
static Bool local isDictVal      ( Cell );
static Cell local maySkipDict    ( Cell );
static Void local putAp          ( Int,Cell );
static Void local putOverInfix   ( Int,Text,Syntax,Cell );
static Void local putInfix       ( Int,Text,Syntax,Cell,Cell );
static Void local putSimpleAp    ( Cell,Int );
static Void local putTuple       ( Int,Cell );
static Int  local unusedTups     ( Int,Cell );
static Void local unlexOp        ( Text );

static Void local putSigType     ( Cell );
static Void local putContext     ( List,List,Int );
static Void local putPred        ( Cell,Int );
static Void local putType        ( Cell,Int,Int );
static Void local putTyVar       ( Int );
static Bool local putTupleType   ( Cell,Int );
static Void local putApType      ( Type,Int,Int );

static Void local putKind        ( Kind );
static Void local putKinds       ( Kinds );


/* --------------------------------------------------------------------------
 * Basic output routines:
 * ------------------------------------------------------------------------*/

FILE *outputStream;                    /* current output stream            */
Int  outColumn = 0;                    /* current output column number     */
                                                                       
#define OPEN(b)    if (b) putChr('(');                                 
#define CLOSE(b)   if (b) putChr(')');                                 
                                                                       
Void putChr(c)                         /* print single character           */
Int c; {                                                               
    Putc(c,outputStream);                                              
    outColumn++;                                                       
}                                                                      
                                                                       
Void putStr(s)                        /* print string                     */
String s; {                                                            
    for (; *s; s++) {                                                  
        Putc(*s,outputStream);                                         
        outColumn++;                                                   
    }                                                                  
}                                                                      
                                                                       
Void putInt(n)                        /* print integer                    */
Int n; {
    static char intBuf[16];
    sprintf(intBuf,"%d",n);
    putStr(intBuf);
}

Void putPtr(p)                        /* print pointer                    */
Ptr p; {
    static char intBuf[16];
    sprintf(intBuf,"%p",p);
    putStr(intBuf);
}

/* --------------------------------------------------------------------------
 * Precedence values (See Haskell 1.3 report, p.12):
 * ------------------------------------------------------------------------*/

#define ALWAYS      FUN_PREC           /* Always use parens (unless atomic)*/
                                       /* User defined operators have prec */
                                       /* in the range MIN_PREC..MAX_PREC  */
#define ARROW_PREC  MAX_PREC           /* for printing -> in type exprs    */
#define COCO_PREC   (MIN_PREC-1)       /* :: is left assoc, low precedence */
#define COND_PREC   (MIN_PREC-2)       /* conditional expressions          */
#define WHERE_PREC  (MIN_PREC-3)       /* where expressions                */
#define LAM_PREC    (MIN_PREC-4)       /* lambda abstraction               */
#define NEVER       LAM_PREC           /* Never use parentheses            */


/* --------------------------------------------------------------------------
 * Print an expression (used to display context of type errors):
 * ------------------------------------------------------------------------*/

static Int putDepth = 0;               /* limits depth of printing DBG     */

static Void local put(d,e)             /* print expression e in context of */
Int  d;                                /* operator of precedence d         */
Cell e; {
    List xs;

    if (putDepth>DEPTH_LIMIT) {
        putStr("...");
        return;
    }
    else
        putDepth++;

    switch (whatIs(e)) {
        case FINLIST    : putChr('[');
                          xs = snd(e);
                          if (nonNull(xs)) {
                              put(NEVER,hd(xs));
                              while (nonNull(xs=tl(xs))) {
                                  putChr(',');
                                  put(NEVER,hd(xs));
                              }
                          }
                          putChr(']');
                          break;

        case AP         : putAp(d,e);
                          break;

        case NAME       : unlexVar(name(e).text);
                          break;

        case VARIDCELL  :
        case VAROPCELL  :
        case DICTVAR    :
        case CONIDCELL  :
        case CONOPCELL  : unlexVar(textOf(e));
                          break;

#if IPARAM
	case IPVAR	: putChr('?');
			  unlexVar(textOf(e));
			  break;

	case WITHEXP	: OPEN(d>WHERE_PREC);
			  putStr("dlet {...} in ");
			  put(WHERE_PREC+1,fst(snd(e)));
			  CLOSE(d>WHERE_PREC);
			  break;
#endif

#if TREX
        case RECSEL     : putChr('#');
                          unlexVar(extText(snd(e)));
                          break;
#endif

        case FREECELL   : putStr("{free!}");
                          break;

        case TUPLE      : putTuple(tupleOf(e),e);
                          break;

        case WILDCARD   : putChr('_');
                          break;

        case ASPAT      : put(NEVER,fst(snd(e)));
                          putChr('@');
                          put(ALWAYS,snd(snd(e)));
                          break;

        case LAZYPAT    : putChr('~');
                          put(ALWAYS,snd(e));
                          break;

        case DOCOMP     : putStr("do {...}");
                          break;

        case COMP       : putComp(fst(snd(e)),snd(snd(e)));
                          break;

        case MONADCOMP  : putComp(fst(snd(snd(e))),snd(snd(snd(e))));
                          break;

        case CHARCELL   : unlexCharConst(charOf(e));
                          break;

        case INTCELL    : {   Int i = intOf(e);
                              if (i<0 && d>=UMINUS_PREC) putChr('(');
                              putInt(i);
                              if (i<0 && d>=UMINUS_PREC) putChr(')');
                          }
                          break;

        case FLOATCELL  : {   Float f = floatOf(e);
                              if (f<0 && d>=UMINUS_PREC) putChr('(');
                              putStr(floatToString(e));
                              if (f<0 && d>=UMINUS_PREC) putChr(')');
                          }
                          break;

        case STRCELL    : unlexStrConst(textOf(e));
                          break;

        case LETREC     : OPEN(d>WHERE_PREC);
#if 0
                          putStr("let {");
                          put(NEVER,fst(snd(e)));
                          putStr("} in ");
#else
                          putStr("let {...} in ");
#endif
                          put(WHERE_PREC+1,snd(snd(e)));
                          CLOSE(d>WHERE_PREC);
                          break;

        case COND       : OPEN(d>COND_PREC);
                          putStr("if ");
                          put(COND_PREC+1,fst3(snd(e)));
                          putStr(" then ");
                          put(COND_PREC+1,snd3(snd(e)));
                          putStr(" else ");
                          put(COND_PREC+1,thd3(snd(e)));
                          CLOSE(d>COND_PREC);
                          break;

        case LAMBDA     : xs = fst(snd(e));
                          if (whatIs(xs)==BIGLAM)
                              xs = snd(snd(xs));
                          while (nonNull(xs) && isDictVal(hd(xs)))
                              xs = tl(xs);
                          if (isNull(xs)) {
                              put(d,snd(snd(snd(e))));
                              break;
                          }
                          OPEN(d>LAM_PREC);
                          putChr('\\');
                          if (nonNull(xs)) {
                              put(ALWAYS,hd(xs));
                              while (nonNull(xs=tl(xs))) {
                                  putChr(' ');
                                  put(ALWAYS,hd(xs));
                              }
                          }
                          putStr(" -> ");
                          put(LAM_PREC,snd(snd(snd(e))));
                          CLOSE(d>LAM_PREC);
                          break;

        case ESIGN      : OPEN(d>COCO_PREC);
                          put(COCO_PREC,fst(snd(e)));
                          putStr(" :: ");
                          putSigType(snd(snd(e)));
                          CLOSE(d>COCO_PREC);
                          break;

        case BIGLAM     : put(d,snd(snd(e)));
                          break;

        case CASE       : putStr("case ");
                          put(NEVER,fst(snd(e)));
#if 0
                          putStr(" of {");
                          put(NEVER,snd(snd(e)));
                          putChr('}');
#else
                          putStr(" of {...}");
#endif
                          break;

        case CONFLDS    : putFlds(fst(snd(e)),snd(snd(e)));
                          break;

        case UPDFLDS    : putFlds(fst3(snd(e)),thd3(snd(e)));
                          break;

        default         : /*internal("put");*/
                          putChr('$');
                          putInt(e);
                          break;
    }
    putDepth--;
}

static Void local putFlds(exp,fs)       /* Output exp using labelled fields*/
Cell exp;
List fs; {
    put(ALWAYS,exp);
    putChr('{');
    for (; nonNull(fs); fs=tl(fs)) {
        Cell v = hd(fs);
        if (isVar(v))
            put(NEVER,v);
        else {
            Cell f = fst(v);
            Cell e = snd(v);
            Text t = isName(f) ? name(f).text :
                     isVar(f)  ? textOf(f)    : inventText();
            Text s = isName(e) ? name(e).text :
                     isVar(e)  ? textOf(e)    : inventText();

            put(NEVER,f);
            if (haskell98 || s!=t) {
                putStr(" = ");
                put(NEVER,e);
            }
        }
        if (nonNull(tl(fs)))
            putStr(", ");
    }
    putChr('}');
}

static Void local putComp(e,qs)         /* print comprehension             */
Cell e;
List qs; {
    putStr("[ ");
    put(NEVER,e);
    if (nonNull(qs)) {
        putStr(" | ");
        putQual(hd(qs));
        while (nonNull(qs=tl(qs))) {
            putStr(", ");
            putQual(hd(qs));
        }
    }
    putStr(" ]");
}

static Void local putQual(q)            /* print list comp qualifier       */
Cell q; {
    switch (whatIs(q)) {
        case BOOLQUAL : put(NEVER,snd(q));
                        return;

        case QWHERE   : putStr("let {...}");
                        return;

        case FROMQUAL : put(ALWAYS,fst(snd(q)));
                        putStr("<-");
                        put(NEVER,snd(snd(q)));
                        return;
    }
}

static Bool local isDictVal(e)          /* Look for dictionary value       */
Cell e; {
#if 0   /* was !DEBUG_CODE -- is it necessary? */
    Cell h = getHead(e);
    switch (whatIs(h)) {
        case DICTVAR : return TRUE;
        case NAME    : return isDfun(h);
    }
#endif
    return FALSE;
}

static Cell local maySkipDict(e)        /* descend function application,   */
Cell e; {                               /* ignoring dict aps               */
    while (isAp(e) && isDictVal(arg(e)))
        e = fun(e);
    return e;
}

static Void local putAp(d,e)            /* print application (args>=1)     */
Int  d;
Cell e; {
    Cell   h;
    Text   t = 0;                       /* bogus init to keep gcc -O happy */
    Syntax sy;
    Int    args = 0;

    for (h=e; isAp(h); h=fun(h))        /* find head of expression, looking*/
        if (!isDictVal(arg(h)))         /* for dictionary arguments        */
            args++;

    if (args==0) {                      /* Special case when *all* args    */
        put(d,h);                       /* are dictionary values           */
        return;
    }

    switch (whatIs(h)) {
        case ADDPAT     : if (args==1)
                              putInfix(d,textPlus,syntaxOf(namePlus),
                                         arg(e),mkInt(intValOf(fun(e))));
                          else
                              putStr("ADDPAT");
                          return;

        case TUPLE      : OPEN(args>tupleOf(h) && d>=FUN_PREC);
                          putTuple(tupleOf(h),e);
                          CLOSE(args>tupleOf(h) && d>=FUN_PREC);
                          return;

        case NAME       : if (args==1 &&
                              ((h==nameFromInt     && isInt(arg(e)))    ||
                               (h==nameFromDouble  && isFloat(arg(e))))) {
                              put(d,arg(e));
                              return;
                          }
                          t  = name(h).text;
                          sy = syntaxOf(h);
                          break;

        case VARIDCELL  :
        case VAROPCELL  :
        case DICTVAR    :
        case CONIDCELL  :
        case CONOPCELL  : sy = defaultSyntax(t = textOf(h));
                          break;

#if TREX
        case EXT        : if (args==2) {
                              String punc = "(";
                              do {
                                  putStr(punc);
                                  punc = ", ";
                                  putStr(textToStr(extText(h)));
                                  putStr("=");
                                  put(NEVER,extField(e));
                                  args = 0;
                                  e    = extRow(e);
                                  for (h=e; isAp(h); h=fun(h))
                                      if (!isDictVal(arg(h)))
                                          args++;
                              } while (isExt(h) && args==2);
                              if (e!=nameNoRec) {
                                  putStr(" | ");
                                  put(NEVER,e);
                              }
                              putChr(')');
                              return;
                          }
                          else if (args<2)
                              internal("putExt");
                          else
                              args-=2;
                          break;
#endif

        default         : sy = APPLIC;
                          break;
    }

    e = maySkipDict(e);

    if (sy==APPLIC) {                   /* print simple application        */
        OPEN(d>=FUN_PREC);
        putSimpleAp(e,args);
        CLOSE(d>=FUN_PREC);
        return;
    }
    else if (args==1) {                 /* print section of the form (e+)  */
        putChr('(');
        put(FUN_PREC-1,arg(e));
        putChr(' ');
        unlexOp(t);
        putChr(')');
    }
    else if (args==2)                  /* infix expr of the form e1 + e2   */
        putInfix(d,t,sy,arg(maySkipDict(fun(e))),arg(e));
    else {                             /* o/w (e1 + e2) e3 ... en   (n>=3) */
        OPEN(d>=FUN_PREC);
        putOverInfix(args,t,sy,e);
        CLOSE(d>=FUN_PREC);
    }
}

static Void local putOverInfix(args,t,sy,e)
Int    args;                           /* infix applied to >= 3 arguments  */
Text   t;
Syntax sy;
Cell   e; {
    if (args>2) {
        putOverInfix(args-1,t,sy,maySkipDict(fun(e)));
        putChr(' ');
        put(FUN_PREC,arg(e));
    }
    else
        putInfix(ALWAYS,t,sy,arg(maySkipDict(fun(e))),arg(e));
}

static Void local putInfix(d,t,sy,e,f)  /* print infix expression          */
Int    d;
Text   t;                               /* Infix operator symbol           */
Syntax sy;                              /* with name t, syntax s           */
Cell   e, f; {                          /* Left and right operands         */
    Syntax a = assocOf(sy);
    Int    p = precOf(sy);

    OPEN(d>p);
    put((a==LEFT_ASS ? p : 1+p), e);
    putChr(' ');
    unlexOp(t);
    putChr(' ');
    put((a==RIGHT_ASS ? p : 1+p), f);
    CLOSE(d>p);
}

static Void local putSimpleAp(e,n)      /* print application e0 e1 ... en  */
Cell e; 
Int  n; {
    if (n>0) {
        putSimpleAp(maySkipDict(fun(e)),n-1);
        putChr(' ');
        put(FUN_PREC,arg(e));
    }
    else
        put(FUN_PREC,e);
}

static Void local putTuple(ts,e)        /* Print tuple expression, allowing*/
Int  ts;                                /* for possibility of either too   */
Cell e; {                               /* few or too many args to constr  */
    Int i;
    putChr('(');
    if ((i=unusedTups(ts,e))>0) {
        while (--i>0)
            putChr(',');
        putChr(')');
    }
}

static Int local unusedTups(ts,e)       /* print first part of tuple expr  */
Int  ts;                                /* returning number of constructor */
Cell e; {                               /* args not yet printed ...        */
    if (isAp(e)) {
        if ((ts=unusedTups(ts,fun(e))-1)>=0) {
            put(NEVER,arg(e));
            putChr(ts>0?',':')');
        }
        else {
            putChr(' ');
            put(FUN_PREC,arg(e));
        }
    }
    return ts;
}

Void unlexVarStr(s)
String s; {
    if ((isascii((int)(s[0])) && isalpha((int)(s[0]))) 
        || s[0]=='_' || s[0]=='[' || s[0]=='('
        || s[0]=='$'
        || (s[0]==':' && s[1]=='D')
       )
        putStr(s);
    else {
        putChr('(');
        putStr(s);
        putChr(')');
    }
}

Void unlexVar(t)                       /* print text as a variable name    */
Text t; {                              /* operator symbols must be enclosed*/
    unlexVarStr(textToStr(t));         /* in parentheses... except [] ...  */
}

static Void local unlexOp(t)           /* print text as operator name      */
Text t; {                              /* alpha numeric symbols must be    */
    String s = textToStr(t);           /* enclosed by backquotes           */

    if (isascii((int)(s[0])) && isalpha((int)(s[0]))) {
        putChr('`');
        putStr(s);
        putChr('`');
    }
    else
        putStr(s);
}

Void unlexCharConst(c)
Cell c; {
    putChr('\'');
    putStr(unlexChar(c,'\''));
    putChr('\'');
}

Void unlexStrConst(t)
Text t; {
    String s            = textToStr(t);
    static Char SO      = 14;          /* ASCII code for '\SO'             */
    Bool   lastWasSO    = FALSE;
    Bool   lastWasDigit = FALSE;
    Bool   lastWasEsc   = FALSE;

    putChr('\"');
    for (; *s; s++) {
        String ch = unlexChar(*s,'\"');
        Char   c  = ' ';

        if ((lastWasSO && *ch=='H') ||
                (lastWasEsc && lastWasDigit 
                 && isascii((int)(*ch)) && isdigit((int)(*ch))))
            putStr("\\&");

        lastWasEsc   = (*ch=='\\');
        lastWasSO    = (*s==SO);
        for (; *ch; c = *ch++)
            putChr(*ch);
        lastWasDigit = (isascii(c) && isdigit(c));
    }
    putChr('\"');
}

/* --------------------------------------------------------------------------
 * Print type expression:
 * ------------------------------------------------------------------------*/

static Void local putSigType(t)         /* print (possibly) generic type   */
Cell t; {
    Int fr = 0;
    if (isPolyType(t)) {
        Kinds ks = polySigOf(t);
        for (; isAp(ks); ks=tl(ks))
            fr++;
        t = monotypeOf(t);
    }

    putType(t,NEVER,fr);                /* Finally, print rest of type ... */
}

static Void local putContext(ps,qs,fr)  /* print context list              */
List ps;
List qs;
Int  fr; {
    Int len = length(ps) + length(qs);
    Int c   = len;
#if IPARAM
    Bool useParens = len!=1 || isIP(fun(hd(ps)));
#else
    Bool useParens = len!=1;
#endif
    if (useParens)
        putChr('(');
    for (; nonNull(ps); ps=tl(ps)) {
        putPred(hd(ps),fr);
        if (--c > 0) {
            putStr(", ");
        }
    }
    for (; nonNull(qs); qs=tl(qs)) {
        putPred(hd(qs),fr);
        if (--c > 0) {
            putStr(", ");
        }
    }
    if (useParens)
        putChr(')');
}

static Void local putPred(pi,fr)        /* Output predicate                */
Cell pi;
Int  fr; {
    if (isAp(pi)) {
#if TREX
        if (isExt(fun(pi))) {
            putType(arg(pi),ALWAYS,fr);
            putChr('\\');
            putStr(textToStr(extText(fun(pi))));
            return;
        }
#endif
#if IPARAM
	if (whatIs(fun(pi)) == IPCELL) {
	    putChr('?');
	    putPred(fun(pi),fr);
	    putStr(" :: ");
	    putType(arg(pi),NEVER,fr);
	    return;
	}
#endif
        putPred(fun(pi),fr);
        putChr(' ');
        putType(arg(pi),ALWAYS,fr);
    }
    else if (isClass(pi))
        putStr(textToStr(cclass(pi).text));
    else if (isCon(pi))
        putStr(textToStr(textOf(pi)));
#if IPARAM
    else if (whatIs(pi) == IPCELL)
        unlexVar(textOf(pi));
#endif
    else
        putStr("<unknownPredicate>");
}

static Void local putType(t,prec,fr)    /* print nongeneric type expression*/
Cell t;
Int  prec;
Int  fr; {
    switch(whatIs(t)) {
        case TYCON     : putStr(textToStr(tycon(t).text));
                         break;

        case TUPLE     : {   Int n = tupleOf(t);
                             putChr('(');
                             while (--n > 0)
                                 putChr(',');
                             putChr(')');
                         }
                         break;

        case POLYTYPE  : {   Kinds ks = polySigOf(t);
                             OPEN(prec>=ARROW_PREC);
                             putStr("forall ");
                             for (; isAp(ks); ks=tl(ks)) {
                                 putTyVar(fr++);
                                 if (isAp(tl(ks)))
                                     putChr(' ');
                             }
                             putStr(". ");
                             putType(monotypeOf(t),NEVER,fr);
                             CLOSE(prec>=ARROW_PREC);
                         }
                         break;

        case CDICTS    :
        case QUAL      : OPEN(prec>=ARROW_PREC);
                         if (whatIs(snd(snd(t)))==CDICTS) {
                             putContext(fst(snd(t)),fst(snd(snd(snd(t)))),fr);
                             putStr(" => ");
                             putType(snd(snd(snd(snd(t)))),NEVER,fr);
                         } else {
                             putContext(fst(snd(t)),NIL,fr);
                             putStr(" => ");
                             putType(snd(snd(t)),NEVER,fr);
                         }
                         CLOSE(prec>=ARROW_PREC);
                         break;

        case EXIST     :
        case RANK2     : putType(snd(snd(t)),prec,fr);
                         break;

        case OFFSET    : putTyVar(offsetOf(t));
                         break;

        case VARIDCELL :
        case VAROPCELL : putChr('_');
                         unlexVar(textOf(t));
                         break;

        case INTCELL   : putChr('_');
                         putInt(intOf(t));
                         break;

        case AP       : {   Cell typeHead = getHead(t);
                            Bool brackets = (argCount!=0 && prec>=ALWAYS);
                            Int  args    = argCount;

                            if (typeHead==typeList) {
                                if (argCount==1) {
                                    putChr('[');
                                    putType(arg(t),NEVER,fr);
                                    putChr(']');
                                    return;
                                }
                            }
                            else if (typeHead==typeArrow) {
                                if (argCount==2) {
                                    OPEN(prec>=ARROW_PREC);
                                    putType(arg(fun(t)),ARROW_PREC,fr);
                                    putStr(" -> ");
                                    putType(arg(t),NEVER,fr);
                                    CLOSE(prec>=ARROW_PREC);
                                    return;
                                }
#if 0
                                else if (argCount==1) {
                                    putChr('(');
                                    putType(arg(t),ARROW_PREC,fr);
                                    putStr("->)");
                                    return;
                                }
#endif
                            }
                            else if (isTuple(typeHead)) {
                                if (argCount==tupleOf(typeHead)) {
                                    putChr('(');
                                    putTupleType(t,fr);
                                    putChr(')');
                                    return;
                                }
                            }
#if TREX
                            else if (isExt(typeHead)) {
                                if (args==2) {
                                    String punc = "(";
                                    do {
                                        putStr(punc);
                                        punc = ", ";
                                        putStr(textToStr(extText(typeHead)));
                                        putStr(" :: ");
                                        putType(extField(t),NEVER,fr);
                                        t        = extRow(t);
                                        typeHead = getHead(t);
                                    } while (isExt(typeHead) && argCount==2);
                                    if (t!=typeNoRow) {
                                        putStr(" | ");
                                        putType(t,NEVER,fr);
                                    }
                                    putChr(')');
                                    return;
                                }
                                else if (args<2)
                                    internal("putExt");
                                else
                                    args-=2;
                            }
#endif
                            OPEN(brackets);
                            putApType(t,args,fr);
                            CLOSE(brackets);
                        }
                        break;

        default       : putStr("(bad type)");
    }
}

static Void local putTyVar(n)           /* print type variable             */
Int n; {
    static String alphabet              /* for the benefit of EBCDIC :-)   */
                ="abcdefghijklmnopqrstuvwxyz";
    putChr(alphabet[n%26]);
    if (n /= 26)                        /* just in case there are > 26 vars*/
        putInt(n);
}

static Bool local putTupleType(e,fr)    /* print tuple of types, returning */
Cell e;                                 /* TRUE if something was printed,  */
Int  fr; {                              /* FALSE otherwise; used to control*/
    if (isAp(e)) {                      /* printing of intermed. commas    */
        if (putTupleType(fun(e),fr))
            putChr(',');
        putType(arg(e),NEVER,fr);
        return TRUE;
    }
    return FALSE;
}

static Void local putApType(t,n,fr)     /* print type application          */
Cell t;
Int  n;
Int  fr; {
    if (n>0) {
        putApType(fun(t),n-1,fr);
        putChr(' ');
        putType(arg(t),ALWAYS,fr);
    }
    else
        putType(t,ALWAYS,fr);
}

/* --------------------------------------------------------------------------
 * Print kind expression:
 * ------------------------------------------------------------------------*/

static Void local putKind(k)            /* print kind expression           */
Kind k; {
    switch (whatIs(k)) {
        case AP      : if (isAp(fst(k))) {
                           putChr('(');
                           putKind(fst(k));
                           putChr(')');
                       }
                       else
                           putKind(fst(k));
                       putStr(" -> ");
                       putKind(snd(k));
                       break;

#if TREX
        case ROW     : putStr("row");
                       break;
#endif

        case STAR    : putChr('*');
                       break;

        case OFFSET  : putTyVar(offsetOf(k));
                       break;

        case INTCELL : putChr('_');
                       putInt(intOf(k));
                       break;

        default      : putStr("(bad kind)");
    }
}

static Void local putKinds(ks)          /* Print list of kinds             */
Kinds ks; {
    if (isNull(ks))
        putStr("()");
    else if (nonNull(tl(ks))) {
        putChr('(');
        putKind(hd(ks));
        while (nonNull(ks=tl(ks))) {
            putChr(',');
            putKind(hd(ks));
        }
        putChr(')');
    }
    else
        putKind(hd(ks));
}

/* --------------------------------------------------------------------------
 * Main drivers:
 * ------------------------------------------------------------------------*/

FILE *mystdout ( Void ) {
  /* We use this from the gdb command line when debugging */
  return stdout;
}

Void printExp(fp,e)                     /* print expr on specified stream  */
FILE *fp;
Cell e; {
    outputStream = fp;
    putDepth     = 0;
    put(NEVER,e);
}

Void printType(fp,t)                    /* print type on specified stream  */
FILE *fp;
Cell t; {
    outputStream = fp;
    putSigType(t);
}

Void printContext(fp,qs)                /* print context on spec. stream   */
FILE *fp;
List qs; {
    outputStream = fp;
    putContext(qs,NIL,0);
}

Void printPred(fp,pi)                   /* print predicate pi on stream    */
FILE *fp;
Cell pi; {
    outputStream = fp;
    putPred(pi,0);
}

Void printKind(fp,k)                    /* print kind k on stream          */
FILE *fp;
Kind k; {
    outputStream = fp;
    putKind(k);
}

Void printKinds(fp,ks)                  /* print list of kinds on stream   */
FILE  *fp;
Kinds ks; {
    outputStream = fp;
    putKinds(ks);
}

Void printFD(fp,fd)			/* print functional dependency	   */
FILE* fp;
Pair  fd; {
    List us;
    outputStream = fp;
    for (us=fst(fd); nonNull(us); us=tl(us)) {
        putTyVar(offsetOf(hd(us)));
	if (nonNull(tl(us))) {
	    putChr(' ');
	}
    }
    putStr(" -> ");
    for (us=snd(fd); nonNull(us); us=tl(us)) {
	putTyVar(offsetOf(hd(us)));
	if (nonNull(tl(us))) {
	    putChr(' ');
	}
    }
}
  
/*-------------------------------------------------------------------------*/

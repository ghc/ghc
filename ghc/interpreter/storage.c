
/* --------------------------------------------------------------------------
 * Primitives for manipulating global data structures
 *
 * Hugs 98 is Copyright (c) Mark P Jones, Alastair Reid and the Yale
 * Haskell Group 1994-99, and is distributed as Open Source software
 * under the Artistic License; see the file "Artistic" that is included
 * in the distribution for details.
 *
 * $RCSfile: storage.c,v $
 * $Revision: 1.4 $
 * $Date: 1999/03/01 14:46:54 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"
#include <setjmp.h>

/*#define DEBUG_SHOWUSE*/

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Int  local hash                  Args((String));
static Int  local saveText              Args((Text));
#if !IGNORE_MODULES
static Module local findQualifier       Args((Text));
#endif
static List local insertTycon           Args((Tycon,List));
static List local insertName            Args((Name,List));
static Void local patternError          Args((String));
static Bool local stringMatch           Args((String,String));
static Bool local typeInvolves          Args((Type,Type));
static Cell local markCell              Args((Cell));
static Void local markSnd               Args((Cell));
static Cell local lowLevelLastIn        Args((Cell));
static Cell local lowLevelLastOut       Args((Cell));
/* from STG */
       Module local moduleOfScript      Args((Script));
       Script local scriptThisFile      Args((Text));
/* from 98 */
#if IO_HANDLES
static Void local freeHandle            Args((Int));
#endif
#if GC_STABLEPTRS
static Void local resetStablePtrs       Args((Void));
#endif
/* end */

/* --------------------------------------------------------------------------
 * Text storage:
 *
 * provides storage for the characters making up identifier and symbol
 * names, string literals, character constants etc...
 *
 * All character strings are stored in a large character array, with textHw
 * pointing to the next free position.  Lookup in the array is improved using
 * a hash table.  Internally, text strings are represented by integer offsets
 * from the beginning of the array to the string in question.
 *
 * Where memory permits, the use of multiple hashtables gives a significant
 * increase in performance, particularly when large source files are used.
 *
 * Each string in the array is terminated by a zero byte.  No string is
 * stored more than once, so that it is safe to test equality of strings by
 * comparing the corresponding offsets.
 *
 * Special text values (beyond the range of the text array table) are used
 * to generate unique `new variable names' as required.
 *
 * The same text storage is also used to hold text values stored in a saved
 * expression.  This grows downwards from the top of the text table (and is
 * not included in the hash table).
 * ------------------------------------------------------------------------*/

#define TEXTHSZ 512                     /* Size of Text hash table         */
#define NOTEXT  ((Text)(~0))            /* Empty bucket in Text hash table */
static  Text    textHw;                 /* Next unused position            */
static  Text    savedText = NUM_TEXT;   /* Start of saved portion of text  */
static  Text    nextNewText;            /* Next new text value             */
static  Text    nextNewDText;           /* Next new dict text value        */
static  char    DEFTABLE(text,NUM_TEXT);/* Storage of character strings    */
static  Text    textHash[TEXTHSZ][NUM_TEXTH]; /* Hash table storage        */

String textToStr(t)                    /* find string corresp to given Text*/
Text t; {
    static char newVar[16];

    if (0<=t && t<NUM_TEXT)                     /* standard char string    */
        return text + t;
    if (t<0)
        sprintf(newVar,"d%d",-t);               /* dictionary variable     */
    else
        sprintf(newVar,"v%d",t-NUM_TEXT);       /* normal variable         */
    return newVar;
}

String identToStr(v) /*find string corresp to given ident or qualified name*/
Cell v; {
    if (!isPair(v)) {
        internal("identToStr");
    }
    switch (fst(v)) {
        case VARIDCELL  :
        case VAROPCELL  : 
        case CONIDCELL  :
        case CONOPCELL  : return text+textOf(v);

        case QUALIDENT  : {   Text pos = textHw;
                              Text t   = qmodOf(v);
                              while (pos+1 < savedText && text[t]!=0) {
                                  text[pos++] = text[t++];
                              }
                              if (pos+1 < savedText) {
                                  text[pos++] = '.';
                              }
                              t = qtextOf(v);
                              while (pos+1 < savedText && text[t]!=0) {
                                  text[pos++] = text[t++];
                              }
                              text[pos] = '\0';
                              return text+textHw;
                          }
    }
    internal("identToStr2");
    assert(0); return 0; /* NOTREACHED */
}

Text inventText()     {                 /* return new unused variable name */
    return nextNewText++;
}

Text inventDictText() {                 /* return new unused dictvar name  */
    return nextNewDText--;
}

Bool inventedText(t)                    /* Signal TRUE if text has been    */
Text t; {                               /* generated internally            */
    return (t<0 || t>=NUM_TEXT);
}

static Int local hash(s)                /* Simple hash function on strings */
String s; {
    int v, j = 3;

    for (v=((int)(*s))*8; *s; s++)
        v += ((int)(*s))*(j++);
    if (v<0)
        v = (-v);
    return(v%TEXTHSZ);
}

Text findText(s)                       /* Locate string in Text array      */
String s; {
    int    h       = hash(s);
    int    hashno  = 0;
    Text   textPos = textHash[h][hashno];

#define TryMatch        {   Text   originalTextPos = textPos;              \
                            String t;                                      \
                            for (t=s; *t==text[textPos]; textPos++,t++)    \
                                if (*t=='\0')                              \
                                    return originalTextPos;                \
                        }
#define Skip            while (text[textPos++]) ;

    while (textPos!=NOTEXT) {
        TryMatch
        if (++hashno<NUM_TEXTH)         /* look in next hashtable entry    */
            textPos = textHash[h][hashno];
        else {
            Skip
            while (textPos < textHw) {
                TryMatch
                Skip
            }
            break;
        }
    }

#undef TryMatch
#undef Skip

    textPos = textHw;                  /* if not found, save in array      */
    if (textHw + (Int)strlen(s) + 1 > savedText) {
        ERRMSG(0) "Character string storage space exhausted"
        EEND;
    }
    while ((text[textHw++] = *s++) != 0) {
    }
    if (hashno<NUM_TEXTH) {            /* updating hash table as necessary */
        textHash[h][hashno] = textPos;
        if (hashno<NUM_TEXTH-1)
            textHash[h][hashno+1] = NOTEXT;
    }

    return textPos;
}

static Int local saveText(t)            /* Save text value in buffer       */
Text t; {                               /* at top of text table            */
    String s = textToStr(t);
    Int    l = strlen(s);

    if (textHw + l + 1 > savedText) {
        ERRMSG(0) "Character string storage space exhausted"
        EEND;
    }
    savedText -= l+1;
    strcpy(text+savedText,s);
    return savedText;
}


/* --------------------------------------------------------------------------
 * Ext storage:
 *
 * Currently, the only attributes that we store for each Ext value is the
 * corresponding Text label.  At some later stage, we may decide to cache
 * types, predicates, etc. here as a space saving gesture.  Given that Text
 * comparison is cheap, and that this is an experimental implementation, we
 * will use a straightforward linear search to locate Ext values from their
 * corresponding Text labels; a hashing scheme can be introduced later if
 * this turns out to be a problem.
 * ------------------------------------------------------------------------*/

#if TREX
Text  DEFTABLE(tabExt,NUM_EXT);         /* Storage for Ext names           */
Ext   extHw;

Ext mkExt(t)                            /* Allocate or find an Ext value   */
Text t; {
    Ext e = EXTMIN;
    for (; e<extHw; e++)
        if (t==extText(e))
            return e;
    if (extHw-EXTMIN >= NUM_EXT) {
        ERRMSG(0) "Ext storage space exhausted"
        EEND;
    }
    extText(extHw) = t;
    return extHw++;
}
#endif

/* --------------------------------------------------------------------------
 * Tycon storage:
 *
 * A Tycon represents a user defined type constructor.  Tycons are indexed
 * by Text values ... a very simple hash function is used to improve lookup
 * times.  Tycon entries with the same hash code are chained together, with
 * the most recent entry at the front of the list.
 * ------------------------------------------------------------------------*/

        Tycon    tyconHw;                       /* next unused Tycon       */
struct  strTycon DEFTABLE(tabTycon,NUM_TYCON);  /* Tycon storage           */

Tycon newTycon(t)                       /* add new tycon to tycon table    */
Text t; {
    if (tyconHw-TYCMIN >= NUM_TYCON) {
        ERRMSG(0) "Type constructor storage space exhausted"
        EEND;
    }
    tycon(tyconHw).text          = t;   /* clear new tycon record          */
    tycon(tyconHw).kind          = NIL;
    tycon(tyconHw).defn          = NIL;
    tycon(tyconHw).what          = NIL;
    tycon(tyconHw).conToTag      = NIL;
    tycon(tyconHw).tagToCon      = NIL;
#if !IGNORE_MODULES
    tycon(tyconHw).mod           = currentModule;
    module(currentModule).tycons = cons(tyconHw,module(currentModule).tycons);
#endif
    return tyconHw++;
}

Tycon findTycon ( Text t )
{
   int n;
   for (n = TYCMIN; n < tyconHw; n++)
      if (tycon(n).text == t) return n;
   return NIL;
}

Tycon addTycon(tc)  /* Insert Tycon in tycon table - if no clash is caused */
Tycon tc; {
    Tycon oldtc = findTycon(tycon(tc).text);
    if (isNull(oldtc)) {
      //        hashTycon(tc);
#if !IGNORE_MODULES
        module(currentModule).tycons=cons(tc,module(currentModule).tycons);
#endif
        return tc;
    } else
        return oldtc;
}

Tycon findQualTycon(id) /*locate (possibly qualified) Tycon in tycon table */
Cell id; {
    if (!isPair(id)) internal("findQualTycon");
    switch (fst(id)) {
        case CONIDCELL :
        case CONOPCELL :
            return findTycon(textOf(id));
        case QUALIDENT : {
#if IGNORE_MODULES
            return findTycon(qtextOf(id));
#else /* !IGNORE_MODULES */
            Text   t  = qtextOf(id);
            Module m  = findQualifier(qmodOf(id));
            List   es = NIL;
            if (isNull(m)) return NIL;
            for(es=module(m).exports; nonNull(es); es=tl(es)) {
                Cell e = hd(es);
                if (isPair(e) && isTycon(fst(e)) && tycon(fst(e)).text==t) 
                    return fst(e);
            }
            return NIL;
#endif /* !IGNORE_MODULES */
        }
        default : internal("findQualTycon2");
    }
    assert(0); return 0; /* NOTREACHED */
}

Tycon addPrimTycon(t,kind,ar,what,defn) /* add new primitive type constr   */
Text t;
Kind kind;
Int  ar;
Cell what;
Cell defn; {
    Tycon tc        = newTycon(t);
    tycon(tc).line  = 0;
    tycon(tc).kind  = kind;
    tycon(tc).what  = what;
    tycon(tc).defn  = defn;
    tycon(tc).arity = ar;
    return tc;
}

static List local insertTycon(tc,ts)    /* insert tycon tc into sorted list*/
Tycon tc;                               /* ts                              */
List  ts; {
    Cell   prev = NIL;
    Cell   curr = ts;
    String s    = textToStr(tycon(tc).text);

    while (nonNull(curr) && strCompare(s,textToStr(tycon(hd(curr)).text))>=0) {
        if (hd(curr)==tc)               /* just in case we get duplicates! */
            return ts;
        prev = curr;
        curr = tl(curr);
    }
    if (nonNull(prev)) {
        tl(prev) = cons(tc,curr);
        return ts;
    }
    else
        return cons(tc,curr);
}

List addTyconsMatching(pat,ts)          /* Add tycons matching pattern pat */
String pat;                             /* to list of Tycons ts            */
List   ts; {                            /* Null pattern matches every tycon*/
    Tycon tc;                           /* (Tycons with NIL kind excluded) */
    for (tc=TYCMIN; tc<tyconHw; ++tc)
        if (!pat || stringMatch(pat,textToStr(tycon(tc).text)))
            if (nonNull(tycon(tc).kind))
                ts = insertTycon(tc,ts);
    return ts;
}

/* --------------------------------------------------------------------------
 * Name storage:
 *
 * A Name represents a top level binding of a value to an identifier.
 * Such values may be a constructor function, a member function in a
 * class, a user-defined or primitive value/function.
 *
 * Names are indexed by Text values ... a very simple hash functions speeds
 * access to the table of Names and Name entries with the same hash value
 * are chained together, with the most recent entry at the front of the
 * list.
 * ------------------------------------------------------------------------*/

#define NAMEHSZ  256                            /* Size of Name hash table */
#define nHash(x) ((x)%NAMEHSZ)                  /* hash fn :: Text->Int    */
        Name     nameHw;                        /* next unused name        */
static  Name     DEFTABLE(nameHash,NAMEHSZ);    /* Hash table storage      */
struct  strName  DEFTABLE(tabName,NUM_NAME);    /* Name table storage      */

Name newName(t,parent)                  /* Add new name to name table      */
Text t; 
Cell parent; {
    //Int h = nHash(t);

    if (nameHw-NAMEMIN >= NUM_NAME) {
        ERRMSG(0) "Name storage space exhausted"
        EEND;
    }
    name(nameHw).text         = t;      /* clear new name record           */
    name(nameHw).line         = 0;
    name(nameHw).syntax       = NO_SYNTAX;
    name(nameHw).parent       = parent;
    name(nameHw).arity        = 0;
    name(nameHw).number       = EXECNAME;
    name(nameHw).defn         = NIL;
    name(nameHw).stgVar       = NIL;
    name(nameHw).type         = NIL;
    name(nameHw).primop       = 0;
    name(nameHw).mod          = currentModule;
    module(currentModule).names=cons(nameHw,module(currentModule).names);
    return nameHw++;
}

Name findName ( Text t )
{
   int n;
   for (n = NAMEMIN; n < nameHw; n++)
      if (name(n).text == t) return n;
   return NIL;
}



Name addName(nm)                        /* Insert Name in name table - if  */
Name nm; {                              /* no clash is caused              */
    Name oldnm = findName(name(nm).text);
    if (isNull(oldnm)) {
      //        hashName(nm);
#if !IGNORE_MODULES
        module(currentModule).names=cons(nm,module(currentModule).names);
#endif
        return nm;
    } else
        return oldnm;
}

Name findQualName(id)              /* Locate (possibly qualified) name*/
Cell id; {                         /* in name table                   */
    if (!isPair(id))
        internal("findQualName");
    switch (fst(id)) {
        case VARIDCELL :
        case VAROPCELL :
        case CONIDCELL :
        case CONOPCELL :
            return findName(textOf(id));
        case QUALIDENT : {
#if IGNORE_MODULES
            return findName(qtextOf(id));
#else /* !IGNORE_MODULES */
            Text   t  = qtextOf(id);
            Module m  = findQualifier(qmodOf(id));
            List   es = NIL;
            if (isNull(m)) return NIL;
            if (m==currentModule) {
                /* The Haskell report (rightly) forbids this.
                 * We added it to let the Prelude refer to itself
                 * without having to import itself.
	         */
                return findName(t);
            }
            for(es=module(m).exports; nonNull(es); es=tl(es)) {
                Cell e = hd(es);
                if (isName(e) && name(e).text==t) 
                    return e;
                else if (isPair(e) && DOTDOT==snd(e)) {
                    List subentities = NIL;
                    Cell c = fst(e);
                    if (isTycon(c)
                        && (tycon(c).what==DATATYPE || tycon(c).what==NEWTYPE))
                        subentities = tycon(c).defn;
                    else if (isClass(c))
                        subentities = cclass(c).members;
                    for(; nonNull(subentities); subentities=tl(subentities)) {
                        assert(isName(hd(subentities)));
                        if (name(hd(subentities)).text == t)
                            return hd(subentities);
                    }
                }
            }
            return NIL;
#endif /* !IGNORE_MODULES */
        }
        default : internal("findQualName2");
    }
    assert(0); return 0; /* NOTREACHED */
}

/* --------------------------------------------------------------------------
 * Primitive functions:
 * ------------------------------------------------------------------------*/

Name addPrimCfunREP(t,arity,no,rep)     /* add primitive constructor func  */
Text t;                                 /* sets rep, not type              */
Int  arity;
Int  no;
Int  rep; { /* Really AsmRep */
    Name n          = newName(t,NIL);
    name(n).arity   = arity;
    name(n).number  = cfunNo(no);
    name(n).type    = NIL;
    name(n).primop  = (void*)rep;
    return n;
}


Name addPrimCfun(t,arity,no,type)       /* add primitive constructor func  */
Text t;
Int  arity;
Int  no;
Cell type; {
    Name n         = newName(t,NIL);
    name(n).arity  = arity;
    name(n).number = cfunNo(no);
    name(n).type   = type;
    return n;
}


Int sfunPos(s,c)                        /* Find position of field with     */
Name s;                                 /* selector s in constructor c.    */
Name c; {
    List cns;
    cns = name(s).defn;
    for (; nonNull(cns); cns=tl(cns))
        if (fst(hd(cns))==c)
            return intOf(snd(hd(cns)));
    internal("sfunPos");
    return 0;/* NOTREACHED */
}

static List local insertName(nm,ns)     /* insert name nm into sorted list */
Name nm;                                /* ns                              */
List ns; {
    Cell   prev = NIL;
    Cell   curr = ns;
    String s    = textToStr(name(nm).text);

    while (nonNull(curr) && strCompare(s,textToStr(name(hd(curr)).text))>=0) {
        if (hd(curr)==nm)               /* just in case we get duplicates! */
            return ns;
        prev = curr;
        curr = tl(curr);
    }
    if (nonNull(prev)) {
        tl(prev) = cons(nm,curr);
        return ns;
    }
    else
        return cons(nm,curr);
}

List addNamesMatching(pat,ns)           /* Add names matching pattern pat  */
String pat;                             /* to list of names ns             */
List   ns; {                            /* Null pattern matches every name */
    Name nm;                            /* (Names with NIL type, or hidden */
#if 1
    for (nm=NAMEMIN; nm<nameHw; ++nm)   /* or invented names are excluded) */
        if (!inventedText(name(nm).text) && nonNull(name(nm).type)) {
            String str = textToStr(name(nm).text);
            if (str[0]!='_' && (!pat || stringMatch(pat,str)))
                ns = insertName(nm,ns);
        }
    return ns;
#else
    List mns = module(currentModule).names;
    for(; nonNull(mns); mns=tl(mns)) {
        Name nm = hd(mns);
        if (!inventedText(name(nm).text)) {
            String str = textToStr(name(nm).text);
            if (str[0]!='_' && (!pat || stringMatch(pat,str)))
                ns = insertName(nm,ns);
        }
    }
    return ns;
#endif
}

/* --------------------------------------------------------------------------
 * A simple string matching routine
 *     `*'    matches any sequence of zero or more characters
 *     `?'    matches any single character exactly 
 *     `@str' matches the string str exactly (ignoring any special chars)
 *     `\c'   matches the character c only (ignoring special chars)
 *     c      matches the character c only
 * ------------------------------------------------------------------------*/

static Void local patternError(s)       /* report error in pattern         */
String s; {
    ERRMSG(0) "%s in pattern", s
    EEND;
}

static Bool local stringMatch(pat,str)  /* match string against pattern    */
String pat;
String str; {

    for (;;)
        switch (*pat) {
            case '\0' : return (*str=='\0');

            case '*'  : do {
                            if (stringMatch(pat+1,str))
                                return TRUE;
                        } while (*str++);
                        return FALSE;

            case '?'  : if (*str++=='\0')
                            return FALSE;
                        pat++;
                        break;

            case '['  : {   Bool found = FALSE;
                            while (*++pat!='\0' && *pat!=']')
                                if (!found && ( pat[0] == *str  ||
                                               (pat[1] == '-'   &&
                                                pat[2] != ']'   &&
                                                pat[2] != '\0'  &&
                                                pat[0] <= *str  &&
                                                pat[2] >= *str)))

                                    found = TRUE;
                            if (*pat != ']')
                                patternError("missing `]'");
                            if (!found)
                                return FALSE;
                            pat++;
                            str++;
                        }
                        break;

            case '\\' : if (*++pat == '\0')
                            patternError("extra trailing `\\'");
                        /*fallthru!*/
            default   : if (*pat++ != *str++)
                            return FALSE;
                        break;
        }
}

/* --------------------------------------------------------------------------
 * Storage of type classes, instances etc...:
 * ------------------------------------------------------------------------*/

static Class classHw;                  /* next unused class                */
static List  classes;                  /* list of classes in current scope */
static Inst  instHw;                   /* next unused instance record      */

struct strClass DEFTABLE(tabClass,NUM_CLASSES); /* table of class records  */
struct strInst far *tabInst;           /* (pointer to) table of instances  */

Class newClass(t)                      /* add new class to class table     */
Text t; {
    if (classHw-CLASSMIN >= NUM_CLASSES) {
        ERRMSG(0) "Class storage space exhausted"
        EEND;
    }
    cclass(classHw).text      = t;
    cclass(classHw).arity     = 0;
    cclass(classHw).kinds     = NIL;
    cclass(classHw).head      = NIL;
    cclass(classHw).dcon      = NIL;
    cclass(classHw).supers    = NIL;
    cclass(classHw).dsels     = NIL;
    cclass(classHw).members   = NIL;
    cclass(classHw).dbuild    = NIL;
    cclass(classHw).defaults  = NIL;
    cclass(classHw).instances = NIL;
    classes=cons(classHw,classes);
#if !IGNORE_MODULES
    cclass(classHw).mod       = currentModule;
    module(currentModule).classes=cons(classHw,module(currentModule).classes);
#endif
    return classHw++;
}

Class classMax() {                      /* Return max Class in use ...     */
    return classHw;                     /* This is a bit ugly, but it's not*/
}                                       /* worth a lot of effort right now */

Class findClass(t)                     /* look for named class in table    */
Text t; {
    Class cl;
    List cs;
    for (cs=classes; nonNull(cs); cs=tl(cs)) {
        cl=hd(cs);
        if (cclass(cl).text==t)
	    return cl;
    }
    return NIL;
}

Class addClass(c)                       /* Insert Class in class list      */
Class c; {                              /*  - if no clash caused           */
    Class oldc = findClass(cclass(c).text);
    if (isNull(oldc)) {
        classes=cons(c,classes);
#if !IGNORE_MODULES
        module(currentModule).classes=cons(c,module(currentModule).classes);
#endif
        return c;
    }
    else
        return oldc;
}

Class findQualClass(c)                  /* Look for (possibly qualified)   */
Cell c; {                               /* class in class list             */
    if (!isQualIdent(c)) {
        return findClass(textOf(c));
    } else {
#if IGNORE_MODULES
        return findClass(qtextOf(c));
#else /* !IGNORE_MODULES */
        Text   t  = qtextOf(c);
        Module m  = findQualifier(qmodOf(c));
        List   es = NIL;
        if (isNull(m))
            return NIL;
        for (es=module(m).exports; nonNull(es); es=tl(es)) {
            Cell e = hd(es);
            if (isPair(e) && isClass(fst(e)) && cclass(fst(e)).text==t) 
                return fst(e);
        }
#endif
    }
    return NIL;
}

Inst newInst() {                       /* Add new instance to table        */
    if (instHw-INSTMIN >= NUM_INSTS) {
        ERRMSG(0) "Instance storage space exhausted"
        EEND;
    }
    inst(instHw).kinds      = NIL;
    inst(instHw).head       = NIL;
    inst(instHw).specifics  = NIL;
    inst(instHw).implements = NIL;
    inst(instHw).builder    = NIL;
    /* from STG */ inst(instHw).mod        = currentModule;

    return instHw++;
}

#ifdef DEBUG_DICTS
extern Void printInst Args((Inst));

Void printInst(in)
Inst in; {
    Class cl = inst(in).c;
    Printf("%s-", textToStr(cclass(cl).text));
    printType(stdout,inst(in).t);
}
#endif /* DEBUG_DICTS */

Inst findFirstInst(tc)                  /* look for 1st instance involving */
Tycon tc; {                             /* the type constructor tc         */
    return findNextInst(tc,INSTMIN-1);
}

Inst findNextInst(tc,in)                /* look for next instance involving*/
Tycon tc;                               /* the type constructor tc         */
Inst  in; {                             /* starting after instance in      */
    while (++in < instHw) {
        Cell pi = inst(in).head;
        for (; isAp(pi); pi=fun(pi))
            if (typeInvolves(arg(pi),tc))
                return in;
    }
    return NIL;
}

static Bool local typeInvolves(ty,tc)   /* Test to see if type ty involves */
Type ty;                                /* type constructor/tuple tc.      */
Type tc; {
    return (ty==tc)
        || (isAp(ty) && (typeInvolves(fun(ty),tc)
                         || typeInvolves(arg(ty),tc)));
}

/* --------------------------------------------------------------------------
 * Control stack:
 *
 * Various parts of the system use a stack of cells.  Most of the stack
 * operations are defined as macros, expanded inline.
 * ------------------------------------------------------------------------*/

Cell DEFTABLE(cellStack,NUM_STACK); /* Storage for cells on stack          */
StackPtr sp;                        /* stack pointer                       */

#if GIMME_STACK_DUMPS

#define UPPER_DISP  5               /* # display entries on top of stack   */
#define LOWER_DISP  5               /* # display entries on bottom of stack*/

Void hugsStackOverflow() {          /* Report stack overflow               */
    extern Int  rootsp;
    extern Cell evalRoots[];

    ERRMSG(0) "Control stack overflow" ETHEN
    if (rootsp>=0) {
        Int i;
        if (rootsp>=UPPER_DISP+LOWER_DISP) {
            for (i=0; i<UPPER_DISP; i++) {
                ERRTEXT "\nwhile evaluating: " ETHEN
                ERREXPR(evalRoots[rootsp-i]);
            }
            ERRTEXT "\n..." ETHEN
            for (i=LOWER_DISP-1; i>=0; i--) {
                ERRTEXT "\nwhile evaluating: " ETHEN
                ERREXPR(evalRoots[i]);
            }
        }
        else {
            for (i=rootsp; i>=0; i--) {
                ERRTEXT "\nwhile evaluating: " ETHEN
                ERREXPR(evalRoots[i]);
            }
        }
    }
    ERRTEXT "\n"
    EEND;
}

#else /* !GIMME_STACK_DUMPS */

Void hugsStackOverflow() {          /* Report stack overflow               */
    ERRMSG(0) "Control stack overflow"
    EEND;
}

#endif /* !GIMME_STACK_DUMPS */

/* --------------------------------------------------------------------------
 * Module storage:
 *
 * A Module represents a user defined module.  
 *
 * Note: there are now two lookup mechanisms in the system:
 *
 * 1) The exports from a module are stored in a big list.
 *    We resolve qualified names, and import lists by linearly scanning
 *    through this list.
 *
 * 2) Unqualified imports and local definitions for the current module
 *    are stored in hash tables (tyconHash and nameHash) or linear lists
 *    (classes).
 *
 * ------------------------------------------------------------------------*/

#if !IGNORE_MODULES
static  Module   moduleHw;              /* next unused Module              */
struct  Module   DEFTABLE(tabModule,NUM_MODULE); /* Module storage         */
Module  currentModule;                  /* Module currently being processed*/

Bool isValidModule(m)                  /* is m a legitimate module id?     */
Module m; {
    return (MODMIN <= m && m < moduleHw);
}

Module newModule(t)                     /* add new module to module table  */
Text t; {
    if (moduleHw-MODMIN >= NUM_MODULE) {
        ERRMSG(0) "Module storage space exhausted"
        EEND;
    }
    module(moduleHw).text          = t; /* clear new module record         */
    module(moduleHw).qualImports   = NIL;
    module(moduleHw).exports       = NIL;
    module(moduleHw).tycons        = NIL;
    module(moduleHw).names         = NIL;
    module(moduleHw).classes       = NIL;
    module(moduleHw).objectFile    = 0;
    return moduleHw++;
}

Module findModule(t)                    /* locate Module in module table  */
Text t; {
    Module m;
    for(m=MODMIN; m<moduleHw; ++m) {
        if (module(m).text==t)
            return m;
    }
    return NIL;
}

Module findModid(c)                    /* Find module by name or filename  */
Cell c; {
    switch (whatIs(c)) {
        case STRCELL   : { Script s = scriptThisFile(snd(c));
                           return (s==-1) ? NIL : moduleOfScript(s);
                         }
        case CONIDCELL : return findModule(textOf(c));
        default        : internal("findModid");
    }
    assert(0); return 0; /* NOTREACHED */
}

static local Module findQualifier(t)    /* locate Module in import list   */
Text t; {
    Module ms;
    ////if (t==module(modulePreludeHugs).text) {
    if (t==module(modulePrelude).text) {
        /* The Haskell report (rightly) forbids this.
         * We added it to let the Prelude refer to itself
         * without having to import itself.
         */
         ////return modulePreludeHugs;
         return modulePrelude;
    }
    for (ms=module(currentModule).qualImports; nonNull(ms); ms=tl(ms)) {
        if (textOf(fst(hd(ms)))==t)
            return snd(hd(ms));
    }
#if 1 /* mpj */
    if (module(currentModule).text==t)
        return currentModule;
#endif
    return NIL;
}

Void setCurrModule(m)              /* set lookup tables for current module */
Module m; {
    //Int i;
    if (m!=currentModule) {
        currentModule = m; /* This is the only assignment to currentModule */
#if 0
        for (i=0; i<TYCONHSZ; ++i)
            tyconHash[i] = NIL;
        mapProc(hashTycon,module(m).tycons);
        for (i=0; i<NAMEHSZ; ++i)
            nameHash[i] = NIL;
        mapProc(hashName,module(m).names);
#endif
        classes = module(m).classes;
    }
}
#endif /* !IGNORE_MODULES */

/* --------------------------------------------------------------------------
 * Script file storage:
 *
 * script files are read into the system one after another.  The state of
 * the stored data structures (except the garbage-collected heap) is recorded
 * before reading a new script.  In the event of being unable to read the
 * script, or if otherwise requested, the system can be restored to its
 * original state immediately before the file was read.
 * ------------------------------------------------------------------------*/

typedef struct {                       /* record of storage state prior to */
    Text  file;                        /* reading script/module            */
    Text  textHw;
    Text  nextNewText;
    Text  nextNewDText;
#if !IGNORE_MODULES
    Module moduleHw;
#endif
    Tycon tyconHw;
    Name  nameHw;
    Class classHw;
    Inst  instHw;
#if TREX
    Ext   extHw;
#endif
} script;

#ifdef  DEBUG_SHOWUSE
static Void local showUse(msg,val,mx)
String msg;
Int val, mx; {
    Printf("%6s : %d of %d (%d%%)\n",msg,val,mx,(100*val)/mx);
}
#endif

static Script scriptHw;                 /* next unused script number       */
static script scripts[NUM_SCRIPTS];     /* storage for script records      */

Script startNewScript(f)                /* start new script, keeping record */
String f; {                             /* of status for later restoration  */
    if (scriptHw >= NUM_SCRIPTS) {
        ERRMSG(0) "Too many script files in use"
        EEND;
    }
#ifdef DEBUG_SHOWUSE
    showUse("Text",   textHw,           NUM_TEXT);
#if !IGNORE_MODULES
    showUse("Module", moduleHw-MODMIN,  NUM_MODULE);
#endif
    showUse("Tycon",  tyconHw-TYCMIN,   NUM_TYCON);
    showUse("Name",   nameHw-NAMEMIN,   NUM_NAME);
    showUse("Class",  classHw-CLASSMIN, NUM_CLASSES);
    showUse("Inst",   instHw-INSTMIN,   NUM_INSTS);
#if TREX
    showUse("Ext",    extHw-EXTMIN,     NUM_EXT);
#endif
#endif

    scripts[scriptHw].file         = findText( f ? f : "<nofile>" );
    scripts[scriptHw].textHw       = textHw;
    scripts[scriptHw].nextNewText  = nextNewText;
    scripts[scriptHw].nextNewDText = nextNewDText;
#if !IGNORE_MODULES
    scripts[scriptHw].moduleHw     = moduleHw;
#endif
    scripts[scriptHw].tyconHw      = tyconHw;
    scripts[scriptHw].nameHw       = nameHw;
    scripts[scriptHw].classHw      = classHw;
    scripts[scriptHw].instHw       = instHw;
#if TREX
    scripts[scriptHw].extHw        = extHw;
#endif
    return scriptHw++;
}

Bool isPreludeScript() {                /* Test whether this is the Prelude*/
    return (scriptHw==0
	    /*ToDo: jrs hack*/ || scriptHw==1
           );
}

#if !IGNORE_MODULES
Bool moduleThisScript(m)                /* Test if given module is defined */
Module m; {                             /* in current script file          */
    return scriptHw<1 || m>=scripts[scriptHw-1].moduleHw;
}

Module lastModule() {              /* Return module in current script file */
    return (moduleHw>MODMIN ? moduleHw-1 : modulePrelude);
}
#endif /* !IGNORE_MODULES */

#define scriptThis(nm,t,tag)            Script nm(x)                       \
                                        t x; {                             \
                                            Script s=0;                    \
                                            while (s<scriptHw              \
                                                   && x>=scripts[s].tag)   \
                                                s++;                       \
                                            return s;                      \
                                        }
scriptThis(scriptThisName,Name,nameHw)
scriptThis(scriptThisTycon,Tycon,tyconHw)
scriptThis(scriptThisInst,Inst,instHw)
scriptThis(scriptThisClass,Class,classHw)
#undef scriptThis

Module moduleOfScript(s)
Script s; {
    return (s==0) ? modulePrelude : scripts[s-1].moduleHw;
}

#if !IGNORE_MODULES
String fileOfModule(m)
Module m; {
    Script s;
    if (m == modulePrelude) {
        return STD_PRELUDE;
    }
    for(s=0; s<scriptHw; ++s) {
        if (scripts[s].moduleHw == m) {
            return textToStr(scripts[s].file);
        }
    }
    return 0;
}
#endif

Script scriptThisFile(f)
Text f; {
    Script s;
    for (s=0; s < scriptHw; ++s) {
        if (scripts[s].file == f) {
            return s+1;
        }
    }
    if (f == findText(STD_PRELUDE)) {
        return 0;
    }
    return (-1);
}

Void dropScriptsFrom(sno)               /* Restore storage to state prior  */
Script sno; {                           /* to reading script sno           */
    if (sno<scriptHw) {                 /* is there anything to restore?   */
        int i;
        textHw       = scripts[sno].textHw;
        nextNewText  = scripts[sno].nextNewText;
        nextNewDText = scripts[sno].nextNewDText;
#if !IGNORE_MODULES
        moduleHw     = scripts[sno].moduleHw;
#endif
        tyconHw      = scripts[sno].tyconHw;
        nameHw       = scripts[sno].nameHw;
        classHw      = scripts[sno].classHw;
        instHw       = scripts[sno].instHw;
#if USE_DICTHW
        dictHw       = scripts[sno].dictHw;
#endif
#if TREX
        extHw        = scripts[sno].extHw;
#endif

        for (i=moduleHw; i >= scripts[sno].moduleHw; --i) {
            if (module(i).objectFile) {
                printf("[bogus] closing objectFile for module %d\n",i);
                /*dlclose(module(i).objectFile);*/
            }
        }
        moduleHw = scripts[sno].moduleHw;

        for (i=0; i<TEXTHSZ; ++i) {
            int j = 0;
            while (j<NUM_TEXTH && textHash[i][j]!=NOTEXT
                               && textHash[i][j]<textHw)
                ++j;
            if (j<NUM_TEXTH)
                textHash[i][j] = NOTEXT;
        }

#if IGNORE_MODULES
        for (i=0; i<TYCONHSZ; ++i) {
            Tycon tc = tyconHash[i];
            while (nonNull(tc) && tc>=tyconHw)
                tc = tycon(tc).nextTyconHash;
            tyconHash[i] = tc;
        }

        for (i=0; i<NAMEHSZ; ++i) {
            Name n = nameHash[i];
            while (nonNull(n) && n>=nameHw)
                n = name(n).nextNameHash;
            nameHash[i] = n;
        }
#else /* !IGNORE_MODULES */
        currentModule=NIL;
#if 0
        for (i=0; i<TYCONHSZ; ++i) {
            tyconHash[i] = NIL;
        }
        for (i=0; i<NAMEHSZ; ++i) {
            nameHash[i] = NIL;
        }
#endif
#endif /* !IGNORE_MODULES */

        for (i=CLASSMIN; i<classHw; i++) {
            List ins = cclass(i).instances;
            List is  = NIL;

            while (nonNull(ins)) {
                List temp = tl(ins);
                if (hd(ins)<instHw) {
                    tl(ins) = is;
                    is      = ins;
                }
                ins = temp;
            }
            cclass(i).instances = rev(is);
        }

        scriptHw = sno;
    }
}

/* --------------------------------------------------------------------------
 * Heap storage:
 *
 * Provides a garbage collectable heap for storage of expressions etc.
 *
 * Now incorporates a flat resource:  A two-space collected extension of
 * the heap that provides storage for contiguous arrays of Cell storage,
 * cooperating with the garbage collection mechanisms for the main heap.
 * ------------------------------------------------------------------------*/

Int     heapSize = DEFAULTHEAP;         /* number of cells in heap         */
Heap    heapFst;                        /* array of fst component of pairs */
Heap    heapSnd;                        /* array of snd component of pairs */
#ifndef GLOBALfst
Heap    heapTopFst;
#endif
#ifndef GLOBALsnd
Heap    heapTopSnd;
#endif
Bool    consGC = TRUE;                  /* Set to FALSE to turn off gc from*/
                                        /* C stack; use with extreme care! */
#if     PROFILING
Heap    heapThd, heapTopThd;            /* to keep record of producers     */
Int     sysCount;                       /* record unattached cells         */
Name    producer;                       /* current producer, if any        */
Bool    profiling = FALSE;              /* should profiling be performed   */
Int     profInterval = MAXPOSINT;       /* interval between samples        */
FILE    *profile = 0;                   /* pointer to profiler log, if any */
#endif
Long    numCells;
Int     numGcs;                         /* number of garbage collections   */
Int     cellsRecovered;                 /* number of cells recovered       */

static  Cell freeList;                  /* free list of unused cells       */
static  Cell lsave, rsave;              /* save components of pair         */

#if GC_WEAKPTRS
static List weakPtrs;                   /* list of weak ptrs               */
                                        /* reconstructed during every GC   */
List   finalizers = NIL;
List   liveWeakPtrs = NIL;
#endif

#if GC_STATISTICS

static Int markCount, stackRoots;

#define initStackRoots() stackRoots = 0
#define recordStackRoot() stackRoots++

#define startGC()       \
    if (gcMessages) {   \
        Printf("\n");   \
        fflush(stdout); \
    }
#define endGC()         \
    if (gcMessages) {   \
        Printf("\n");   \
        fflush(stdout); \
    }

#define start()      markCount = 0
#define end(thing,rs) \
    if (gcMessages) { \
        Printf("GC: %-18s: %4d cells, %4d roots.\n", thing, markCount, rs); \
        fflush(stdout); \
    }
#define recordMark() markCount++

#else /* !GC_STATISTICS */

#define startGC()
#define endGC()

#define initStackRoots()
#define recordStackRoot()

#define start()   
#define end(thing,root) 
#define recordMark() 

#endif /* !GC_STATISTICS */

Cell pair(l,r)                          /* Allocate pair (l, r) from       */
Cell l, r; {                            /* heap, garbage collecting first  */
    Cell c = freeList;                  /* if necessary ...                */

    if (isNull(c)) {
        lsave = l;
        rsave = r;
        garbageCollect();
        l     = lsave;
        lsave = NIL;
        r     = rsave;
        rsave = NIL;
        c     = freeList;
    }
    freeList = snd(freeList);
    fst(c)   = l;
    snd(c)   = r;
#if PROFILING
    thd(c)   = producer;
#endif
    numCells++;
    return c;
}

Void overwrite(dst,src)                 /* overwrite dst cell with src cell*/
Cell dst, src; {                        /* both *MUST* be pairs            */
    if (isPair(dst) && isPair(src)) {
        fst(dst) = fst(src);
        snd(dst) = snd(src);
    }
    else
        internal("overwrite");
}

static Int *marks;
static Int marksSize;

Cell markExpr(c)                        /* External interface to markCell  */
Cell c; {
    return isGenPair(c) ? markCell(c) : c;
}

static Cell local markCell(c)           /* Traverse part of graph marking  */
Cell c; {                               /* cells reachable from given root */
                                        /* markCell(c) is only called if c */
                                        /* is a pair                       */
    {   register int place = placeInSet(c);
        register int mask  = maskInSet(c);
        if (marks[place]&mask)
            return c;
        else {
            marks[place] |= mask;
            recordMark();
        }
    }

    if (isGenPair(fst(c))) {
        fst(c) = markCell(fst(c));
        markSnd(c);
    }
    else if (isNull(fst(c)) || fst(c)>=BCSTAG) {
        markSnd(c);
    }

    return c;
}

static Void local markSnd(c)            /* Variant of markCell used to     */
Cell c; {                               /* update snd component of cell    */
    Cell t;                             /* using tail recursion            */

ma: t = c;                              /* Keep pointer to original pair   */
    c = snd(c);
    if (!isPair(c))
        return;

    {   register int place = placeInSet(c);
        register int mask  = maskInSet(c);
        if (marks[place]&mask)
            return;
        else {
            marks[place] |= mask;
            recordMark();
        }
    }

    if (isGenPair(fst(c))) {
        fst(c) = markCell(fst(c));
        goto ma;
    }
    else if (isNull(fst(c)) || fst(c)>=BCSTAG)
        goto ma;
    return;
}

Void markWithoutMove(n)                 /* Garbage collect cell at n, as if*/
Cell n; {                               /* it was a cell ref, but don't    */
                                        /* move cell so we don't have      */
                                        /* to modify the stored value of n */
    if (isGenPair(n)) {
        recordStackRoot();
        markCell(n); 
    }
}

Void garbageCollect()     {             /* Run garbage collector ...       */
    Bool breakStat = breakOn(FALSE);    /* disable break checking          */
    Int i,j;
    register Int mask;
    register Int place;
    Int      recovered;

    jmp_buf  regs;                      /* save registers on stack         */
printf("\n\n$$$$$$$$$$$ GARBAGE COLLECTION; aborting\n\n");
exit(1);
    setjmp(regs);

    gcStarted();
    for (i=0; i<marksSize; ++i)         /* initialise mark set to empty    */
        marks[i] = 0;
#if GC_WEAKPTRS
    weakPtrs = NIL;                     /* clear list of weak pointers     */
#endif
    everybody(MARK);                    /* Mark all components of system   */

#if IO_HANDLES
    for (i=0; i<NUM_HANDLES; ++i)       /* release any unused handles      */
        if (nonNull(handles[i].hcell)) {
            register place = placeInSet(handles[i].hcell);
            register mask  = maskInSet(handles[i].hcell);
            if ((marks[place]&mask)==0)
                freeHandle(i);
        }
#endif
#if GC_MALLOCPTRS
    for (i=0; i<NUM_MALLOCPTRS; ++i)    /* release any unused mallocptrs   */
        if (isPair(mallocPtrs[i].mpcell)) {
            register place = placeInSet(mallocPtrs[i].mpcell);
            register mask  = maskInSet(mallocPtrs[i].mpcell);
            if ((marks[place]&mask)==0)
                incMallocPtrRefCnt(i,-1);
        }
#endif /* GC_MALLOCPTRS */
#if GC_WEAKPTRS
    /* After GC completes, we scan the list of weak pointers that are
     * still live and zap their contents unless the contents are still
     * live (by some other means).
     * Note that this means the contents must itself be heap allocated.
     * This means it can't be a nullary constructor or an Int or a Name
     * or lots of other things - hope this doesn't bite too hard.
     */
    for (; nonNull(weakPtrs); weakPtrs=nextWeakPtr(weakPtrs)) {
        Cell ptr = derefWeakPtr(weakPtrs);
        if (isGenPair(ptr)) {
            Int  place = placeInSet(ptr);
            Int  mask  = maskInSet(ptr);
            if ((marks[place]&mask)==0) {
                /* printf("Zapping weak pointer %d\n", ptr); */
                derefWeakPtr(weakPtrs) = NIL;
            } else {
                /* printf("Keeping weak pointer %d\n", ptr); */
            }
        } else if (nonNull(ptr)) {
            printf("Weak ptr contains object which isn't heap allocated %d\n", ptr);
        }
    }

    if (nonNull(liveWeakPtrs) || nonNull(finalizers)) {
        Bool anyMarked;                 /* Weak pointers with finalizers   */
        List wps;
        List newFins = NIL;

        /* Step 1: iterate until we've found out what is reachable         */
        do {
            anyMarked = FALSE;
            for (wps=liveWeakPtrs; nonNull(wps); wps=tl(wps)) {
                Cell wp = hd(wps);
                Cell k  = fst(snd(wp));
                if (isNull(k)) {
                    internal("bad weak ptr");
                }
                if (isMarked(k)) {
                    Cell vf = snd(snd(wp));
                    if (!isMarked(fst(vf)) || !isMarked(snd(vf))) {
                        mark(fst(vf));
                        mark(snd(vf));
                        anyMarked = TRUE;
                    }
                }
            }
        } while (anyMarked);

        /* Step 2: Now we know which weak pointers will die, so we can     */
        /* remove them from the live set and gather their finalizers.  But */
        /* note that we mustn't mark *anything* at this stage or we will   */
        /* corrupt our view of what's alive, and what's dead.              */
        wps = NIL;
        while (nonNull(liveWeakPtrs)) {
            Cell wp = hd(liveWeakPtrs);
            List nx = tl(liveWeakPtrs);
            Cell k  = fst(snd(wp));
            if (!isMarked(k)) {                 /* If the key is dead, then*/
                Cell vf      = snd(snd(wp));    /* stomp on weak pointer   */
                fst(vf)      = snd(vf);
                snd(vf)      = newFins;
                newFins      = vf;              /* reuse because we can't  */
                fst(snd(wp)) = NIL;             /* reallocate here ...     */
                snd(snd(wp)) = NIL;
                snd(wp)      = NIL;
                liveWeakPtrs = nx;
            } else {
                tl(liveWeakPtrs) = wps;         /* Otherwise, weak pointer */
                wps              = liveWeakPtrs;/* survives to face another*/
                liveWeakPtrs     = nx;          /* garbage collection      */
            }
        }

        /* Step 3: Now we've identified the live cells and the newly       */
        /* scheduled finalizers, but we had better make sure that they are */
        /* all marked now, including any internal structure, to ensure that*/
        /* they make it to the other side of gc.                           */
        for (liveWeakPtrs=wps; nonNull(wps); wps=tl(wps)) {
            mark(snd(hd(wps)));
        }
        mark(liveWeakPtrs);
        mark(newFins);
        finalizers = revOnto(newFins,finalizers);
    }

#endif /* GC_WEAKPTRS */
    gcScanning();                       /* scan mark set                   */
    mask      = 1;
    place     = 0;
    recovered = 0;
    j         = 0;
#if PROFILING
    if (profile) {
        sysCount = 0;
        for (i=NAMEMIN; i<nameHw; i++)
            name(i).count = 0;
    }
#endif
    freeList = NIL;
    for (i=1; i<=heapSize; i++) {
        if ((marks[place] & mask) == 0) {
            snd(-i)  = freeList;
            fst(-i)  = FREECELL;
            freeList = -i;
            recovered++;
        }
#if PROFILING
        else if (nonNull(thd(-i)))
            name(thd(-i)).count++;
        else
            sysCount++;
#endif
        mask <<= 1;
        if (++j == bitsPerWord) {
            place++;
            mask = 1;
            j    = 0;
        }
    }

    gcRecovered(recovered);
    breakOn(breakStat);                 /* restore break trapping if nec.  */

#if PROFILING
    if (profile) {
        fprintf(profile,"BEGIN_SAMPLE %ld.00\n",numReductions);
/* For the time being, we won't include the system count in the output:
        if (sysCount>0)
            fprintf(profile,"  SYSTEM %d\n",sysCount);
*/
        /* Accumulate costs in top level objects */
        for (i=NAMEMIN; i<nameHw; i++) {
            Name cc = i;
            /* Use of "while" instead of "if" is pure paranoia - ADR */
            while (isName(name(cc).parent)) 
                cc = name(cc).parent;
            if (i != cc) {
                name(cc).count += name(i).count;
                name(i).count = 0;
            }
        }
        for (i=NAMEMIN; i<nameHw; i++)
            if (name(i).count>0) 
                if (isPair(name(i).parent)) {
                    Pair p = name(i).parent;
                    Cell f = fst(p);
                    fprintf(profile,"  ");
                    if (isClass(f))
                        fprintf(profile,"%s",textToStr(cclass(f).text));
                    else {
                        fprintf(profile,"%s_",textToStr(cclass(inst(f).c).text));
                        /* Will hp2ps accept the spaces produced by this? */
                        printPred(profile,inst(f).head);
                    }
                    fprintf(profile,"_%s %d\n",
                            textToStr(name(snd(p)).text),
                            name(i).count);
                } else {
                    fprintf(profile,"  %s %d\n",
                            textToStr(name(i).text),
                            name(i).count);
                }
        fprintf(profile,"END_SAMPLE %ld.00\n",numReductions);
    }
#endif

    /* can only return if freeList is nonempty on return. */
    if (recovered<minRecovery || isNull(freeList)) {
        ERRMSG(0) "Garbage collection fails to reclaim sufficient space"
        EEND;
    }
    cellsRecovered = recovered;
}

#if PROFILING
Void profilerLog(s)                     /* turn heap profiling on, saving log*/
String s; {                             /* in specified file                 */
    if ((profile=fopen(s,"w")) != NULL) {
        fprintf(profile,"JOB \"Hugs Heap Profile\"\n");
        fprintf(profile,"DATE \"%s\"\n",timeString());
        fprintf(profile,"SAMPLE_UNIT \"reductions\"\n");
        fprintf(profile,"VALUE_UNIT \"cells\"\n");
    }
    else {
        ERRMSG(0) "Cannot open profile log file \"%s\"", s
        EEND;
    }
}
#endif

/* --------------------------------------------------------------------------
 * Code for saving last expression entered:
 *
 * This is a little tricky because some text values (e.g. strings or variable
 * names) may not be defined or have the same value when the expression is
 * recalled.  These text values are therefore saved in the top portion of
 * the text table.
 * ------------------------------------------------------------------------*/

static Cell lastExprSaved;              /* last expression to be saved     */

Void setLastExpr(e)                     /* save expression for later recall*/
Cell e; {
    lastExprSaved = NIL;                /* in case attempt to save fails   */
    savedText     = NUM_TEXT;
    lastExprSaved = lowLevelLastIn(e);
}

static Cell local lowLevelLastIn(c)     /* Duplicate expression tree (i.e. */
Cell c; {                               /* acyclic graph) for later recall */
    if (isPair(c)) {                    /* Duplicating any text strings    */
        if (isBoxTag(fst(c)))           /* in case these are lost at some  */
            switch (fst(c)) {           /* point before the expr is reused */
                case VARIDCELL :
                case VAROPCELL :
                case DICTVAR   :
                case CONIDCELL :
                case CONOPCELL :
                case STRCELL   : return pair(fst(c),saveText(textOf(c)));
                default        : return pair(fst(c),snd(c));
            }
        else
            return pair(lowLevelLastIn(fst(c)),lowLevelLastIn(snd(c)));
    }
#if TREX
    else if (isExt(c))
        return pair(EXTCOPY,saveText(extText(c)));
#endif
    else
        return c;
}

Cell getLastExpr() {                    /* recover previously saved expr   */
    return lowLevelLastOut(lastExprSaved);
}

static Cell local lowLevelLastOut(c)    /* As with lowLevelLastIn() above  */
Cell c; {                               /* except that Cells refering to   */
    if (isPair(c)) {                    /* Text values are restored to     */
        if (isBoxTag(fst(c)))           /* appropriate values              */
            switch (fst(c)) {
                case VARIDCELL :
                case VAROPCELL :
                case DICTVAR   :
                case CONIDCELL :
                case CONOPCELL :
                case STRCELL   : return pair(fst(c),
                                             findText(text+intValOf(c)));
#if TREX
                case EXTCOPY   : return mkExt(findText(text+intValOf(c)));
#endif
                default        : return pair(fst(c),snd(c));
            }
        else
            return pair(lowLevelLastOut(fst(c)),lowLevelLastOut(snd(c)));
    }
    else
        return c;
}

/* --------------------------------------------------------------------------
 * Miscellaneous operations on heap cells:
 * ------------------------------------------------------------------------*/

/* Profiling suggests that the number of calls to whatIs() is typically    */
/* rather high.  The recoded version below attempts to improve the average */
/* performance for whatIs() using a binary search for part of the analysis */

Cell whatIs(c)                         /* identify type of cell            */
register Cell c; {
    if (isPair(c)) {
        register Cell fstc = fst(c);
        return isTag(fstc) ? fstc : AP;
    }
    if (c<TUPMIN)    return c;
    if (c>=INTMIN)   return INTCELL;

    if (c>=NAMEMIN){if (c>=CLASSMIN)   {if (c>=CHARMIN) return CHARCELL;
                                        else            return CLASS;}
                    else                if (c>=INSTMIN) return INSTANCE;
                                        else            return NAME;}
    else            if (c>=MODMIN)     {if (c>=TYCMIN)  return TYCON;
                                        else            return MODULE;}
                    else                if (c>=OFFMIN)  return OFFSET;
#if TREX
                                        else            return (c>=EXTMIN) ?
                                                                EXT : TUPLE;
#else
                                        else            return TUPLE;
#endif

/*  if (isPair(c)) {
        register Cell fstc = fst(c);
        return isTag(fstc) ? fstc : AP;
    }
    if (c>=INTMIN)   return INTCELL;
    if (c>=CHARMIN)  return CHARCELL;
    if (c>=CLASSMIN) return CLASS;
    if (c>=INSTMIN)  return INSTANCE;
    if (c>=NAMEMIN)  return NAME;
    if (c>=TYCMIN)   return TYCON;
    if (c>=MODMIN)   return MODULE;
    if (c>=OFFMIN)   return OFFSET;
#if TREX
    if (c>=EXTMIN)   return EXT;
#endif
    if (c>=TUPMIN)   return TUPLE;
    return c;*/
}

#if DEBUG_PRINTER
/* A very, very simple printer.
 * Output is uglier than from printExp - but the printer is more
 * robust and can be used on any data structure irrespective of
 * its type.
 */
Void print Args((Cell, Int));
Void print(c, depth)
Cell c;
Int  depth; {
    if (0 == depth) {
        Printf("...");
#if 0 /* Not in this version of Hugs */
    } else if (isPair(c) && !isGenPair(c)) {
        extern Void printEvalCell Args((Cell, Int));
        printEvalCell(c,depth);
#endif
    } else {
        Int tag = whatIs(c);
        switch (tag) {
        case AP: 
                Putchar('(');
                print(fst(c), depth-1);
                Putchar(',');
                print(snd(c), depth-1);
                Putchar(')');
                break;
        case FREECELL:
                Printf("free(%d)", c);
                break;
        case INTCELL:
                Printf("int(%d)", intOf(c));
                break;
        case BIGCELL:
                Printf("bignum(%s)", bignumToString(c));
                break;
        case CHARCELL:
                Printf("char('%c')", charOf(c));
                break;
        case PTRCELL: 
                Printf("ptr(%p)",ptrOf(c));
                break;
        case CLASS:
                Printf("class(%d)", c-CLASSMIN);
                if (CLASSMIN <= c && c < classHw) {
                    Printf("=\"%s\"", textToStr(cclass(c).text));
                }
                break;
        case INSTANCE:
                Printf("instance(%d)", c - INSTMIN);
                break;
        case NAME:
                Printf("name(%d)", c-NAMEMIN);
                if (NAMEMIN <= c && c < nameHw) {
                    Printf("=\"%s\"", textToStr(name(c).text));
                }
                break;
        case TYCON:
                Printf("tycon(%d)", c-TYCMIN);
                if (TYCMIN <= c && c < tyconHw)
                    Printf("=\"%s\"", textToStr(tycon(c).text));
                break;
        case MODULE:
                Printf("module(%d)", c - MODMIN);
                break;
        case OFFSET:
                Printf("Offset %d", offsetOf(c));
                break;
        case TUPLE:
                Printf("Tuple %d", tupleOf(c));
                break;
        case POLYTYPE:
                Printf("Polytype");
                print(snd(c),depth-1);
                break;
        case RANK2:
                Printf("Rank2(");
                if (isPair(snd(c)) && isInt(fst(snd(c)))) {
                    Printf("%d ", intOf(fst(snd(c))));
                    print(snd(snd(c)),depth-1);
                } else {
                    print(snd(c),depth-1);
                }
                Printf(")");
                break;
        case NIL:
                Printf("NIL");
                break;
        case WILDCARD:
                Printf("_");
                break;
        case STAR:
                Printf("STAR");
                break;
        case DOTDOT:
                Printf("DOTDOT");
                break;
        case DICTVAR:
                Printf("{dict %d}",textOf(c));
                break;
        case VARIDCELL:
        case VAROPCELL:
        case CONIDCELL:
        case CONOPCELL:
                Printf("{id %s}",textToStr(textOf(c)));
                break;
        case QUALIDENT:
                Printf("{qid %s.%s}",textToStr(qmodOf(c)),textToStr(qtextOf(c)));
                break;
        case LETREC:
                Printf("LetRec(");
                print(fst(snd(c)),depth-1);
                Putchar(',');
                print(snd(snd(c)),depth-1);
                Putchar(')');
                break;
        case LAMBDA:
                Printf("Lambda(");
                print(snd(c),depth-1);
                Putchar(')');
                break;
        case FINLIST:
                Printf("FinList(");
                print(snd(c),depth-1);
                Putchar(')');
                break;
        case COMP:
                Printf("Comp(");
                print(fst(snd(c)),depth-1);
                Putchar(',');
                print(snd(snd(c)),depth-1);
                Putchar(')');
                break;
        case ASPAT:
                Printf("AsPat(");
                print(fst(snd(c)),depth-1);
                Putchar(',');
                print(snd(snd(c)),depth-1);
                Putchar(')');
                break;
        case FROMQUAL:
                Printf("FromQual(");
                print(fst(snd(c)),depth-1);
                Putchar(',');
                print(snd(snd(c)),depth-1);
                Putchar(')');
                break;
        case STGVAR:
                Printf("StgVar%d=",-c);
                print(snd(c), depth-1);
                break;
        case STGAPP:
                Printf("StgApp(");
                print(fst(snd(c)),depth-1);
                Putchar(',');
                print(snd(snd(c)),depth-1);
                Putchar(')');
                break;
        case STGPRIM:
                Printf("StgPrim(");
                print(fst(snd(c)),depth-1);
                Putchar(',');
                print(snd(snd(c)),depth-1);
                Putchar(')');
                break;
        case STGCON:
                Printf("StgCon(");
                print(fst(snd(c)),depth-1);
                Putchar(',');
                print(snd(snd(c)),depth-1);
                Putchar(')');
                break;
        case PRIMCASE:
                Printf("PrimCase(");
                print(fst(snd(c)),depth-1);
                Putchar(',');
                print(snd(snd(c)),depth-1);
                Putchar(')');
                break;
        default:
                if (isBoxTag(tag)) {
                    Printf("Tag(%d)=%d", c, tag);
                } else if (isConTag(tag)) {
                    Printf("%d@(%d,",c,tag);
                    print(snd(c), depth-1);
                    Putchar(')');
                    break;
                } else if (c == tag) {
                    Printf("Tag(%d)", c);
                } else {
                    Printf("Tag(%d)=%d", c, tag);
                }
                break;
        }
    }
    FlushStdout();
}
#endif

Bool isVar(c)                           /* is cell a VARIDCELL/VAROPCELL ? */
Cell c; {                               /* also recognises DICTVAR cells   */
    return isPair(c) &&
               (fst(c)==VARIDCELL || fst(c)==VAROPCELL || fst(c)==DICTVAR);
}

Bool isCon(c)                          /* is cell a CONIDCELL/CONOPCELL ?  */
Cell c; {
    return isPair(c) && (fst(c)==CONIDCELL || fst(c)==CONOPCELL);
}

Bool isQVar(c)                        /* is cell a [un]qualified varop/id? */
Cell c; {
    if (!isPair(c)) return FALSE;
    switch (fst(c)) {
        case VARIDCELL  :
        case VAROPCELL  : return TRUE;

        case QUALIDENT  : return isVar(snd(snd(c)));

        default         : return FALSE;
    }
}

Bool isQCon(c)                         /*is cell a [un]qualified conop/id? */
Cell c; {
    if (!isPair(c)) return FALSE;
    switch (fst(c)) {
        case CONIDCELL  :
        case CONOPCELL  : return TRUE;

        case QUALIDENT  : return isCon(snd(snd(c)));

        default         : return FALSE;
    }
}

Bool isQualIdent(c)                    /* is cell a qualified identifier?  */
Cell c; {
    return isPair(c) && (fst(c)==QUALIDENT);
}

Bool isIdent(c)                        /* is cell an identifier?           */
Cell c; {
    if (!isPair(c)) return FALSE;
    switch (fst(c)) {
        case VARIDCELL  :
        case VAROPCELL  :
        case CONIDCELL  :
        case CONOPCELL  : return TRUE;

        case QUALIDENT  : return TRUE;

        default         : return FALSE;
    }
}

Bool isInt(c)                          /* cell holds integer value?        */
Cell c; {
    return isSmall(c) || (isPair(c) && fst(c)==INTCELL);
}

Int intOf(c)                           /* find integer value of cell?      */
Cell c; {
    assert(isInt(c));
    return isPair(c) ? (Int)(snd(c)) : (Int)(c-INTZERO);
}

Cell mkInt(n)                          /* make cell representing integer   */
Int n; {
    return (MINSMALLINT <= n && n <= MAXSMALLINT)
           ? INTZERO+n
           : pair(INTCELL,n);
}

#if BIGNUMS
Bool isBignum(c)                       /* cell holds bignum value?         */
Cell c; {
    return c==ZERONUM || (isPair(c) && (fst(c)==POSNUM || fst(c)==NEGNUM));
}
#endif

#if SIZEOF_INTP == SIZEOF_INT
typedef union {Int i; Ptr p;} IntOrPtr;
Cell mkPtr(p)
Ptr p;
{
    IntOrPtr x;
    x.p = p;
    return pair(PTRCELL,x.i);
}

Ptr ptrOf(c)
Cell c;
{
    IntOrPtr x;
    assert(fst(c) == PTRCELL);
    x.i = snd(c);
    return x.p;
}
#elif SIZEOF_INTP == 2*SIZEOF_INT
typedef union {struct {Int i1; Int i2;} i; Ptr p;} IntOrPtr;
Cell mkPtr(p)
Ptr p;
{
    IntOrPtr x;
    x.p = p;
    return pair(PTRCELL,pair(mkInt(x.i.i1),mkInt(x.i.i2)));
}

Ptr ptrOf(c)
Cell c;
{
    IntOrPtr x;
    assert(fst(c) == PTRCELL);
    x.i.i1 = intOf(fst(snd(c)));
    x.i.i2 = intOf(snd(snd(c)));
    return x.p;
}
#else
#warning "type Addr not supported on this architecture - don't use it"
Cell mkPtr(p)
Ptr p;
{
    ERRMSG(0) "mkPtr: type Addr not supported on this architecture"
    EEND;
}

Ptr ptrOf(c)
Cell c;
{
    ERRMSG(0) "ptrOf: type Addr not supported on this architecture"
    EEND;
}
#endif

/* --------------------------------------------------------------------------
 * List operations:
 * ------------------------------------------------------------------------*/

Int length(xs)                         /* calculate length of list xs      */
List xs; {
    Int n = 0;
    for (; nonNull(xs); ++n)
        xs = tl(xs);
    return n;
}

List appendOnto(xs,ys)                 /* Destructively prepend xs onto    */
List xs, ys; {                         /* ys by modifying xs ...           */
    if (isNull(xs))
        return ys;
    else {
        List zs = xs;
        while (nonNull(tl(zs)))
            zs = tl(zs);
        tl(zs) = ys;
        return xs;
    }
}

List dupOnto(xs,ys)      /* non-destructively prepend xs backwards onto ys */
List xs; 
List ys; {
    for (; nonNull(xs); xs=tl(xs))
        ys = cons(hd(xs),ys);
    return ys;
}

List dupListOnto(xs,ys)              /* Duplicate spine of list xs onto ys */
List xs;
List ys; {
    return revOnto(dupOnto(xs,NIL),ys);
}

List dupList(xs)                       /* Duplicate spine of list xs       */
List xs; {
    List ys = NIL;
    for (; nonNull(xs); xs=tl(xs))
        ys = cons(hd(xs),ys);
    return rev(ys);
}

List revOnto(xs,ys)                    /* Destructively reverse elements of*/
List xs, ys; {                         /* list xs onto list ys...          */
    Cell zs;

    while (nonNull(xs)) {
        zs     = tl(xs);
        tl(xs) = ys;
        ys     = xs;
        xs     = zs;
    }
    return ys;
}

#if 0
List delete(xs,y)                      /* Delete first use of y from xs    */
List xs;
Cell y; {
    if (isNull(xs)) {
        return xs;
    } else if (hs(xs) == y) {
        return tl(xs);
    } else {
        tl(xs) = delete(tl(xs),y);
        return xs;
    }
}

List minus(xs,ys)                      /* Delete members of ys from xs     */
List xs, ys; {
    mapAccum(delete,xs,ys);
    return xs;
}
#endif

Cell varIsMember(t,xs)                 /* Test if variable is a member of  */
Text t;                                /* given list of variables          */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
        if (t==textOf(hd(xs)))
            return hd(xs);
    return NIL;
}

Name nameIsMember(t,ns)                 /* Test if name with text t is a   */
Text t;                                 /* member of list of names xs      */
List ns; {
    for (; nonNull(ns); ns=tl(ns))
        if (t==name(hd(ns)).text)
            return hd(ns);
    return NIL;
}

Cell intIsMember(n,xs)                 /* Test if integer n is member of   */
Int  n;                                /* given list of integers           */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
        if (n==intOf(hd(xs)))
            return hd(xs);
    return NIL;
}

Cell cellIsMember(x,xs)                /* Test for membership of specific  */
Cell x;                                /* cell x in list xs                */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
        if (x==hd(xs))
            return hd(xs);
    return NIL;
}

Cell cellAssoc(c,xs)                   /* Lookup cell in association list  */
Cell c;         
List xs; {
    for (; nonNull(xs); xs=tl(xs))
        if (c==fst(hd(xs)))
            return hd(xs);
    return NIL;
}

Cell cellRevAssoc(c,xs)                /* Lookup cell in range of          */
Cell c;                                /* association lists                */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
        if (c==snd(hd(xs)))
            return hd(xs);
    return NIL;
}

List replicate(n,x)                     /* create list of n copies of x    */
Int n;
Cell x; {
    List xs=NIL;
    while (0<n--)
        xs = cons(x,xs);
    return xs;
}

List diffList(from,take)               /* list difference: from\take       */
List from, take; {                     /* result contains all elements of  */
    List result = NIL;                 /* `from' not appearing in `take'   */

    while (nonNull(from)) {
        List next = tl(from);
        if (!cellIsMember(hd(from),take)) {
            tl(from) = result;
            result   = from;
        }
        from = next;
    }
    return rev(result);
}

List deleteCell(xs, y)                  /* copy xs deleting pointers to y  */
List xs;
Cell y; {
    List result = NIL; 
    for(;nonNull(xs);xs=tl(xs)) {
        Cell x = hd(xs);
        if (x != y) {
            result=cons(x,result);
        }
    }
    return rev(result);
}

List take(n,xs)                         /* destructively truncate list to  */
Int  n;                                 /* specified length                */
List xs; {
    List ys = xs;

    if (n==0)
        return NIL;
    while (1<n-- && nonNull(xs))
        xs = tl(xs);
    if (nonNull(xs))
        tl(xs) = NIL;
    return ys;
}

List splitAt(n,xs)                         /* drop n things from front of list*/
Int  n;       
List xs; {
    for(; n>0; --n) {
        xs = tl(xs);
    }
    return xs;
}

Cell nth(n,xs)                         /* extract n'th element of list    */
Int  n;
List xs; {
    for(; n>0 && nonNull(xs); --n, xs=tl(xs)) {
    }
    if (isNull(xs))
        internal("nth");
    return hd(xs);
}

List removeCell(x,xs)                   /* destructively remove cell from  */
Cell x;                                 /* list                            */
List xs; {
    if (nonNull(xs)) {
        if (hd(xs)==x)
            return tl(xs);              /* element at front of list        */
        else {
            List prev = xs;
            List curr = tl(xs);
            for (; nonNull(curr); prev=curr, curr=tl(prev))
                if (hd(curr)==x) {
                    tl(prev) = tl(curr);
                    return xs;          /* element in middle of list       */
                }
        }
    }
    return xs;                          /* here if element not found       */
}

/* --------------------------------------------------------------------------
 * Operations on applications:
 * ------------------------------------------------------------------------*/

Int argCount;                          /* number of args in application    */

Cell getHead(e)                        /* get head cell of application     */
Cell e; {                              /* set number of args in argCount   */
    for (argCount=0; isAp(e); e=fun(e))
        argCount++;
    return e;
}

List getArgs(e)                        /* get list of arguments in function*/
Cell e; {                              /* application:                     */
    List as;                           /* getArgs(f e1 .. en) = [e1,..,en] */

    for (as=NIL; isAp(e); e=fun(e))
        as = cons(arg(e),as);
    return as;
}

Cell nthArg(n,e)                       /* return nth arg in application    */
Int  n;                                /* of function to m args (m>=n)     */
Cell e; {                              /* nthArg n (f x0 x1 ... xm) = xn   */
    for (n=numArgs(e)-n-1; n>0; n--)
        e = fun(e);
    return arg(e);
}

Int numArgs(e)                         /* find number of arguments to expr */
Cell e; {
    Int n;
    for (n=0; isAp(e); e=fun(e))
        n++;
    return n;
}

Cell applyToArgs(f,args)               /* destructively apply list of args */
Cell f;                                /* to function f                    */
List args; {
    while (nonNull(args)) {
        Cell temp = tl(args);
        tl(args)  = hd(args);
        hd(args)  = f;
        f         = args;
        args      = temp;
    }
    return f;
}

/* --------------------------------------------------------------------------
 * Handle operations:
 * ------------------------------------------------------------------------*/

#if IO_HANDLES
struct strHandle DEFTABLE(handles,NUM_HANDLES);

Cell openHandle(s,hmode,binary)         /* open handle to file named s in  */
String s;                               /* the specified hmode             */
Int    hmode; 
Bool   binary; {
    Int i;

    for (i=0; i<NUM_HANDLES && nonNull(handles[i].hcell); ++i)
        ;                                       /* Search for unused handle*/
    if (i>=NUM_HANDLES) {                       /* If at first we don't    */
        garbageCollect();                       /* succeed, garbage collect*/
        for (i=0; i<NUM_HANDLES && nonNull(handles[i].hcell); ++i)
            ;                                   /* and try again ...       */
    }
    if (i>=NUM_HANDLES) {                       /* ... before we give up   */
        ERRMSG(0) "Too many handles open; cannot open \"%s\"", s
        EEND;
    }
    else {                                      /* prepare to open file    */
        String stmode;
        if (binary) {
            stmode = (hmode&HAPPEND) ? "ab+" :
                     (hmode&HWRITE)  ? "wb+" :
                     (hmode&HREAD)   ? "rb" : (String)0;
        } else {
            stmode = (hmode&HAPPEND) ? "a+"  :
                     (hmode&HWRITE)  ? "w+"  :
                     (hmode&HREAD)   ? "r"  : (String)0;
        }
        if (stmode && (handles[i].hfp=fopen(s,stmode))) {
            handles[i].hmode = hmode;
            return (handles[i].hcell = ap(HANDCELL,i));
        }
    }
    return NIL;
}

static Void local freeHandle(n)         /* release handle storage when no  */
Int n; {                                /* heap references to it remain    */
    if (0<=n && n<NUM_HANDLES && nonNull(handles[n].hcell)) {
        if (n>HSTDERR && handles[n].hmode!=HCLOSED && handles[n].hfp) {
            fclose(handles[n].hfp);
            handles[n].hfp = 0;
        }
        fst(handles[n].hcell) = snd(handles[n].hcell) = NIL;
        handles[n].hcell      = NIL;
    }
}
#endif

#if GC_MALLOCPTRS
/* --------------------------------------------------------------------------
 * Malloc Ptrs:
 * ------------------------------------------------------------------------*/

struct strMallocPtr mallocPtrs[NUM_MALLOCPTRS];

/* It might GC (because it uses a table not a list) which will trash any
 * unstable pointers.  
 * (It happens that we never use it with unstable pointers.)
 */
Cell mkMallocPtr(ptr,cleanup)            /* create a new malloc pointer    */
Ptr ptr;
Void (*cleanup) Args((Ptr)); {
    Int i;
    for (i=0; i<NUM_MALLOCPTRS && mallocPtrs[i].refCount!=0; ++i)
        ;                                       /* Search for unused entry */
    if (i>=NUM_MALLOCPTRS) {                    /* If at first we don't    */
        garbageCollect();                       /* succeed, garbage collect*/
        for (i=0; i<NUM_MALLOCPTRS && mallocPtrs[i].refCount!=0; ++i)
            ;                                   /* and try again ...       */
    }
    if (i>=NUM_MALLOCPTRS) {                    /* ... before we give up   */
        ERRMSG(0) "Too many ForeignObjs open"
        EEND;
    }
    mallocPtrs[i].ptr      = ptr;
    mallocPtrs[i].cleanup  = cleanup;
    mallocPtrs[i].refCount = 1;
    return (mallocPtrs[i].mpcell = ap(MPCELL,i));
}

Void incMallocPtrRefCnt(n,i)             /* change ref count of MallocPtr */
Int n;
Int i; {        
    if (!(0<=n && n<NUM_MALLOCPTRS && mallocPtrs[n].refCount > 0))
        internal("freeMallocPtr");
    mallocPtrs[n].refCount += i;
    if (mallocPtrs[n].refCount <= 0) {
        mallocPtrs[n].cleanup(mallocPtrs[n].ptr);

        mallocPtrs[n].ptr      = 0;
        mallocPtrs[n].cleanup  = 0;
        mallocPtrs[n].refCount = 0;
        mallocPtrs[n].mpcell   = NIL;
    }
}
#endif /* GC_MALLOCPTRS */

/* --------------------------------------------------------------------------
 * Stable pointers
 * This is a mechanism that allows the C world to manipulate pointers into the
 * Haskell heap without having to worry that the garbage collector is going
 * to delete it or move it around.
 * The implementation and interface is based on my implementation in
 * GHC - but, at least for now, is simplified by using a fixed size
 * table of stable pointers.
 * ------------------------------------------------------------------------*/

#if GC_STABLEPTRS

/* Each entry in the stable pointer table is either a heap pointer
 * or is not currently allocated.
 * Unallocated entries are threaded together into a freelist.
 * The last entry in the list contains the Cell 0; all other values
 * contain a Cell whose value is the next free stable ptr in the list.
 * It follows that stable pointers are strictly positive (>0).
 */
static Cell stablePtrTable[NUM_STABLEPTRS];
static Int  sptFreeList;
#define SPT(sp) stablePtrTable[(sp)-1]

static Void local resetStablePtrs() {
    Int i;
    /* It would be easier to build the free list in the other direction
     * but, when debugging, it's way easier to understand if the first
     * pointer allocated is "1".
     */
    for(i=1; i < NUM_STABLEPTRS; ++i)
        SPT(i) = i+1;
    SPT(NUM_STABLEPTRS) = 0;
    sptFreeList = 1;
}

Int mkStablePtr(c)                  /* Create a stable pointer            */
Cell c; {
    Int i = sptFreeList;
    if (i == 0)
        return 0;
    sptFreeList = SPT(i);
    SPT(i) = c;
    return i;
}

Cell derefStablePtr(p)              /* Dereference a stable pointer       */
Int p; {
    if (!(1 <= p && p <= NUM_STABLEPTRS)) {
        internal("derefStablePtr");
    }
    return SPT(p);
}

Void freeStablePtr(i)               /* Free a stable pointer             */
Int i; {
    SPT(i) = sptFreeList;
    sptFreeList = i;
}

#undef SPT
#endif /* GC_STABLEPTRS */

/* --------------------------------------------------------------------------
 * plugin support
 * ------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
 * GreenCard entry points
 *
 * GreenCard generated code accesses Hugs data structures and functions 
 * (only) via these functions (which are stored in the virtual function
 * table hugsAPI1.
 *-------------------------------------------------------------------------*/

#if GREENCARD

static Cell  makeTuple      Args((Int));
static Cell  makeInt        Args((Int));
static Cell  makeChar       Args((Char));
static Char  CharOf         Args((Cell));
static Cell  makeFloat      Args((FloatPro));
static Void* derefMallocPtr Args((Cell));
static Cell* Fst            Args((Cell));
static Cell* Snd            Args((Cell));

static Cell  makeTuple(n)      Int      n; { return mkTuple(n); }
static Cell  makeInt(n)        Int      n; { return mkInt(n); }
static Cell  makeChar(n)       Char     n; { return mkChar(n); }
static Char  CharOf(n)         Cell     n; { return charOf(n); }
static Cell  makeFloat(n)      FloatPro n; { return mkFloat(n); }
static Void* derefMallocPtr(n) Cell     n; { return derefMP(n); }
static Cell* Fst(n)            Cell     n; { return (Cell*)&fst(n); }
static Cell* Snd(n)            Cell     n; { return (Cell*)&snd(n); }

HugsAPI1* hugsAPI1() {
    static HugsAPI1 api;
    static Bool initialised = FALSE;
    if (!initialised) {
        api.nameTrue        = nameTrue;
        api.nameFalse       = nameFalse;
        api.nameNil         = nameNil;
        api.nameCons        = nameCons;
        api.nameJust        = nameJust;
        api.nameNothing     = nameNothing;
        api.nameLeft        = nameLeft;
        api.nameRight       = nameRight;
        api.nameUnit        = nameUnit;
        api.nameIORun       = nameIORun;
        api.makeInt         = makeInt;
        api.makeChar        = makeChar;
        api.CharOf          = CharOf;
        api.makeFloat       = makeFloat;
        api.makeTuple       = makeTuple;
        api.pair            = pair;
        api.mkMallocPtr     = mkMallocPtr;
        api.derefMallocPtr  = derefMallocPtr;
        api.mkStablePtr     = mkStablePtr;
        api.derefStablePtr  = derefStablePtr;
        api.freeStablePtr   = freeStablePtr;
        api.eval            = eval;
        api.evalWithNoError = evalWithNoError;
        api.evalFails       = evalFails;
        api.whnfArgs        = &whnfArgs;
        api.whnfHead        = &whnfHead;
        api.whnfInt         = &whnfInt;
        api.whnfFloat       = &whnfFloat;
        api.garbageCollect  = garbageCollect;
        api.stackOverflow   = hugsStackOverflow;
        api.internal        = internal;
        api.registerPrims   = registerPrims;
        api.addPrimCfun     = addPrimCfun;
        api.inventText      = inventText;
        api.Fst             = Fst;
        api.Snd             = Snd;
        api.cellStack       = cellStack;
        api.sp              = &sp;
    }
    return &api;
}

#endif /* GREENCARD */


/* --------------------------------------------------------------------------
 * storage control:
 * ------------------------------------------------------------------------*/

#if DYN_TABLES
static void far* safeFarCalloc Args((Int,Int));
static void far* safeFarCalloc(n,s)     /* allocate table storage and check*/
Int n, s; {                             /* for non-null return             */
    void far* tab = farCalloc(n,s);
    if (tab==0) {
        ERRMSG(0) "Cannot allocate run-time tables"
        EEND;
    }
    return tab;
}
#define TABALLOC(v,t,n)                 v=(t far*)safeFarCalloc(n,sizeof(t));
#else
#define TABALLOC(v,t,n)
#endif

Void storage(what)
Int what; {
    Int i;

    switch (what) {
        case RESET   : clearStack();

                       /* the next 2 statements are particularly important
                        * if you are using GLOBALfst or GLOBALsnd since the
                        * corresponding registers may be reset to their
                        * uninitialised initial values by a longjump.
                        */
                       heapTopFst = heapFst + heapSize;
                       heapTopSnd = heapSnd + heapSize;
#if PROFILING
                       heapTopThd = heapThd + heapSize;
                       if (profile) {
                           garbageCollect();
                           fclose(profile);
#if HAVE_HP2PS
                           system("hp2ps profile.hp");
#endif
                           profile = 0;
                       }
#endif
#if IO_HANDLES
                       handles[HSTDIN].hmode  = HREAD;
                       handles[HSTDOUT].hmode = HAPPEND;
                       handles[HSTDERR].hmode = HAPPEND;
#endif
#if GC_MALLOCPTRS
                       for (i=0; i<NUM_MALLOCPTRS; i++)
                           mallocPtrs[i].mpcell = NIL;
#endif
#if !HSCRIPT
#if GC_STABLEPTRS
                       resetStablePtrs();
#endif
#endif
                       consGC = TRUE;
                       lsave  = NIL;
                       rsave  = NIL;
                       if (isNull(lastExprSaved))
                           savedText = NUM_TEXT;
                       break;

        case MARK    : 
                       start();
                       for (i=NAMEMIN; i<nameHw; ++i) {
                           mark(name(i).parent);
                           mark(name(i).defn);
                           mark(name(i).stgVar);
                           mark(name(i).type);
                       }
                       end("Names", nameHw-NAMEMIN);

#if !IGNORE_MODULES
                       start();
                       for (i=MODMIN; i<moduleHw; ++i) {
                           mark(module(i).tycons);
                           mark(module(i).names);
                           mark(module(i).classes);
                           mark(module(i).exports);
                           mark(module(i).qualImports);
                       }
                       end("Modules", moduleHw-MODMIN);
#endif

                       start();
                       for (i=TYCMIN; i<tyconHw; ++i) {
                           mark(tycon(i).defn);
                           mark(tycon(i).kind);
                           mark(tycon(i).what);
                       }
                       end("Type constructors", tyconHw-TYCMIN);

                       start();
                       for (i=CLASSMIN; i<classHw; ++i) {
                           mark(cclass(i).head);
                           mark(cclass(i).kinds);
                           mark(cclass(i).dsels);
                           mark(cclass(i).supers);
                           mark(cclass(i).members);
                           mark(cclass(i).defaults);
                           mark(cclass(i).instances);
                       }
                       mark(classes);
                       end("Classes", classHw-CLASSMIN);

                       start();
                       for (i=INSTMIN; i<instHw; ++i) {
                           mark(inst(i).head);
                           mark(inst(i).kinds);
                           mark(inst(i).specifics);
                           mark(inst(i).implements);
                       }
                       end("Instances", instHw-INSTMIN);

                       start();
                       for (i=0; i<=sp; ++i)
                           mark(stack(i));
                       end("Stack", sp+1);

                       start();
                       mark(lastExprSaved);
                       mark(lsave);
                       mark(rsave);
                       end("Last expression", 3);
#if IO_HANDLES
                       start();
                       mark(handles[HSTDIN].hcell);
                       mark(handles[HSTDOUT].hcell);
                       mark(handles[HSTDERR].hcell);
                       end("Standard handles", 3);
#endif

#if GC_STABLEPTRS
                       start();
                       for (i=0; i<NUM_STABLEPTRS; ++i)
                           mark(stablePtrTable[i]);
                       end("Stable pointers", NUM_STABLEPTRS);
#endif

#if GC_WEAKPTRS
                       mark(finalizers);
#endif

                       if (consGC) {
                           start();
                           gcCStack();
                           end("C stack", stackRoots);
                       }

                       break;

        case INSTALL : heapFst = heapAlloc(heapSize);
                       heapSnd = heapAlloc(heapSize);

                       if (heapFst==(Heap)0 || heapSnd==(Heap)0) {
                           ERRMSG(0) "Cannot allocate heap storage (%d cells)",
                                     heapSize
                           EEND;
                       }

                       heapTopFst = heapFst + heapSize;
                       heapTopSnd = heapSnd + heapSize;
#if PROFILING
                       heapThd = heapAlloc(heapSize);
                       if (heapThd==(Heap)0) {
                           ERRMSG(0) "Cannot allocate profiler storage space"
                           EEND;
                       }
                       heapTopThd   = heapThd + heapSize;
                       profile      = 0;
                       if (0 == profInterval)
                           profInterval = heapSize / DEF_PROFINTDIV;
#endif
                       for (i=1; i<heapSize; ++i) {
                           fst(-i) = FREECELL;
                           snd(-i) = -(i+1);
                       }
                       snd(-heapSize) = NIL;
                       freeList  = -1;
                       numGcs    = 0;
                       consGC    = TRUE;
                       lsave     = NIL;
                       rsave     = NIL;

                       marksSize  = bitArraySize(heapSize);
                       if ((marks=(Int *)calloc(marksSize, sizeof(Int)))==0) {
                           ERRMSG(0) "Unable to allocate gc markspace"
                           EEND;
                       }

                       TABALLOC(text,      char,             NUM_TEXT)
                       TABALLOC(tyconHash, Tycon,            TYCONHSZ)
                       TABALLOC(tabTycon,  struct strTycon,  NUM_TYCON)
                       TABALLOC(nameHash,  Name,             NAMEHSZ)
                       TABALLOC(tabName,   struct strName,   NUM_NAME)
                       TABALLOC(tabClass,  struct strClass,  NUM_CLASSES)
                       TABALLOC(cellStack, Cell,             NUM_STACK)
                       TABALLOC(tabModule, struct Module,    NUM_SCRIPTS)
#if TREX
                       TABALLOC(tabExt,    Text,             NUM_EXT)
#endif
                       clearStack();

#if IO_HANDLES
                       TABALLOC(handles,   struct strHandle, NUM_HANDLES)
                       for (i=0; i<NUM_HANDLES; i++)
                           handles[i].hcell = NIL;
                       handles[HSTDIN].hcell  = ap(HANDCELL,HSTDIN);
                       handles[HSTDIN].hfp    = stdin;
                       handles[HSTDOUT].hcell = ap(HANDCELL,HSTDOUT);
                       handles[HSTDOUT].hfp   = stdout;
                       handles[HSTDERR].hcell = ap(HANDCELL,HSTDERR);
                       handles[HSTDERR].hfp   = stderr;
#endif

                       textHw        = 0;
                       nextNewText   = NUM_TEXT;
                       nextNewDText  = (-1);
                       lastExprSaved = NIL;
                       savedText     = NUM_TEXT;
                       for (i=0; i<TEXTHSZ; ++i)
                           textHash[i][0] = NOTEXT;


#if !IGNORE_MODULES
                       moduleHw = MODMIN;
#endif

                       tyconHw  = TYCMIN;
#if 0
                       for (i=0; i<TYCONHSZ; ++i)
                           tyconHash[i] = NIL;
#endif
#if GC_WEAKPTRS
                       finalizers   = NIL;
                       liveWeakPtrs = NIL;
#endif

#if GC_STABLEPTRS
                       resetStablePtrs();
#endif

#if TREX
                       extHw    = EXTMIN;
#endif

                       nameHw   = NAMEMIN;
                       for (i=0; i<NAMEHSZ; ++i)
                           nameHash[i] = NIL;

                       classHw  = CLASSMIN;

                       instHw   = INSTMIN;

#if USE_DICTHW
                       dictHw   = 0;
#endif

                       tabInst  = (struct strInst far *)
                                    farCalloc(NUM_INSTS,sizeof(struct strInst));

                       if (tabInst==0) {
                           ERRMSG(0) "Cannot allocate instance tables"
                           EEND;
                       }

                       scriptHw = 0;

                       break;
    }
}

/*-------------------------------------------------------------------------*/

/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Primitives for manipulating global data structures
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: storage.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:41 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "charset.h"
#include "errors.h"
#include "link.h"    /* for nameCons         */
#include <setjmp.h>

#include "machdep.h" /* gc-related functions */

/*#define DEBUG_SHOWUSE*/

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Int  local hash                  Args((String));
static Int  local saveText              Args((Text));
static Module local findQualifier       Args((Text));
static Void local hashTycon             Args((Tycon));
static List local insertTycon           Args((Tycon,List));
static Void local hashName              Args((Name));
static List local insertName            Args((Name,List));
static Void local patternError          Args((String));
static Bool local stringMatch           Args((String,String));
static Bool local typeInvolves          Args((Type,Type));
static Cell local markCell              Args((Cell));
static Void local markSnd               Args((Cell));
static Cell local lowLevelLastIn        Args((Cell));
static Cell local lowLevelLastOut       Args((Cell));
static Module local moduleOfScript      Args((Script));
static Script local scriptThisFile      Args((Text));


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
    static char newVar[33];

    assert(isPair(v));
    switch (fst(v)) {
        case VARIDCELL  :
        case VAROPCELL  : 
        case CONIDCELL  :
        case CONOPCELL  : return text+textOf(v);

        case QUALIDENT  : sprintf(newVar,"%s.%s",
                                  text+qmodOf(v),text+qtextOf(v));
                          return newVar;
    }
    internal("identToStr 2");
}

Syntax identSyntax(v)           /* find syntax of ident or qualified ident */
Cell v; {
    assert(isPair(v));
    switch (fst(v)) {
        case VARIDCELL  :
        case VAROPCELL  : 
        case CONIDCELL  :
        case CONOPCELL  : return syntaxOf(textOf(v));

        case QUALIDENT  : return syntaxOf(qtextOf(v));
    }
    internal("identSyntax 2");
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
 * Syntax storage:
 *
 * Operator declarations are stored in a table which associates Text values
 * with Syntax values.
 * ------------------------------------------------------------------------*/

static Int syntaxHw;                   /* next unused syntax table entry   */
static struct strSyntax {              /* table of Text <-> Syntax values  */
    Text   text;
    Syntax syntax;
} DEFTABLE(tabSyntax,NUM_SYNTAX);

Syntax defaultSyntax(t)                /* Find default syntax of var named */
Text t; {                              /* by t ...                         */
    String s = textToStr(t);
    return isIn(s[0],SYMBOL) ? DEF_OPSYNTAX : APPLIC;
}

Syntax syntaxOf(t)                     /* look up syntax of operator symbol*/
Text t; {
    int i;

    for (i=0; i<syntaxHw; ++i)
        if (tabSyntax[i].text==t)
            return tabSyntax[i].syntax;
    return defaultSyntax(t);
}

Void addSyntax(line,t,sy)              /* add (t,sy) to syntax table       */
Int    line;
Text   t;
Syntax sy; {
    int i;

    for (i=0; i<syntaxHw; ++i)
        if (tabSyntax[i].text==t) {
            /* There's no problem with multiple identical fixity declarations.
             * - but note that it's not allowed by the Haskell report.  ADR
             */
            if (tabSyntax[i].syntax == sy) return;
            ERRMSG(line) "Attempt to redefine syntax of operator \"%s\"",
                         textToStr(t)
            EEND;
        }

    if (syntaxHw>=NUM_SYNTAX) {
        ERRMSG(line) "Too many fixity declarations"
        EEND;
    }

    tabSyntax[syntaxHw].text   = t;
    tabSyntax[syntaxHw].syntax = sy;
    syntaxHw++;
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

#define TYCONHSZ 256                            /* Size of Tycon hash table*/
#define tHash(x) ((x)%TYCONHSZ)                 /* Tycon hash function     */
static  Tycon    tyconHw;                       /* next unused Tycon       */
static  Tycon    DEFTABLE(tyconHash,TYCONHSZ);  /* Hash table storage      */
struct  strTycon DEFTABLE(tabTycon,NUM_TYCON);  /* Tycon storage           */

Tycon newTycon(t)                       /* add new tycon to tycon table    */
Text t; {
    Int h = tHash(t);

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
    tycon(tyconHw).mod           = currentModule;
    module(currentModule).tycons = cons(tyconHw,module(currentModule).tycons);
    tycon(tyconHw).nextTyconHash = tyconHash[h];
    tyconHash[h]                 = tyconHw;

    return tyconHw++;
}

Tycon findTycon(t)                      /* locate Tycon in tycon table     */
Text t; {
    Tycon tc = tyconHash[tHash(t)];

    while (nonNull(tc) && tycon(tc).text!=t)
        tc = tycon(tc).nextTyconHash;
    return tc;
}

Tycon addTycon(tc)  /* Insert Tycon in tycon table - if no clash is caused */
Tycon tc; {
    Tycon oldtc = findTycon(tycon(tc).text);
    if (isNull(oldtc)) {
        hashTycon(tc);
        module(currentModule).tycons=cons(tc,module(currentModule).tycons);
        return tc;
    } else
        return oldtc;
}

static Void local hashTycon(tc)         /* Insert Tycon into hash table    */
Tycon tc; {
    Text  t = tycon(tc).text;
    Int   h = tHash(t);
    tycon(tc).nextTyconHash = tyconHash[h];
    tyconHash[h]            = tc;
}

Tycon findQualTycon(id) /*locate (possibly qualified) Tycon in tycon table */
Cell id; {
    assert(isPair(id));
    switch (fst(id)) {
        case CONIDCELL :
        case CONOPCELL :
            return findTycon(textOf(id));
        case QUALIDENT : {
            Text   t  = qtextOf(id);
            Module m  = findQualifier(qmodOf(id));
            List   es = NIL;
            if (isNull(m)) 
                return NIL;
            if (m==currentModule) {
                /* The Haskell report (rightly) forbids this.
                 * We added it to let the Prelude refer to itself
                 * without having to import itself.
                 */
                return findTycon(t);
            }
            for(es=module(m).exports; nonNull(es); es=tl(es)) {
                Cell e = hd(es);
                if (isPair(e) && isTycon(fst(e)) && tycon(fst(e)).text==t) 
                    return fst(e);
            }
            return NIL;
        }
        default : internal("findQualTycon2");
    }
}

Tycon addPrimTycon(t,kind,ar,what,defn) /* add new primitive type constr   */
Text   t;
Kind   kind;
Int    ar;
Cell   what;
Cell   defn; {
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
/*static*/Name   nameHw;                      /* next unused name        */
static  Name     DEFTABLE(nameHash,NAMEHSZ);    /* Hash table storage      */
struct  strName  DEFTABLE(tabName,NUM_NAME);    /* Name table storage      */

Name newName(t)                         /* add new name to name table      */
Text t; {
    if (nameHw-NAMEMIN >= NUM_NAME) {
        ERRMSG(0) "Name storage space exhausted"
        EEND;
    }
    name(nameHw).text         = t;      /* clear new name record           */
    name(nameHw).line         = 0;
    name(nameHw).arity        = 0;
    name(nameHw).number       = EXECNAME;
    name(nameHw).defn         = NIL;
    name(nameHw).stgVar       = NIL;
    name(nameHw).type         = NIL;
    name(nameHw).primop       = 0;
    name(nameHw).mod          = currentModule;
    hashName(nameHw);
    module(currentModule).names=cons(nameHw,module(currentModule).names);
    return nameHw++;
}

Name findName(t)                        /* locate name in name table       */
Text t; {
    Name n = nameHash[nHash(t)];

    while (nonNull(n) && name(n).text!=t) {
        n = name(n).nextNameHash;
    }
    assert(isNull(n) || (isName(n) && n < nameHw));
    return n;
}

Name addName(nm)      /* Insert Name in name table - if no clash is caused */
Name nm; {
    Name oldnm = findName(name(nm).text);
    if (isNull(oldnm)) {
        hashName(nm);
        module(currentModule).names=cons(nm,module(currentModule).names);
        return nm;
    } else {
        return oldnm;
    }
}

static Void local hashName(nm)          /* Insert Name into hash table       */
Name nm; {
    Text t = name(nm).text;
    Int  h = nHash(t);
    name(nm).nextNameHash = nameHash[h];
    nameHash[h]           = nm;
}

Name findQualName(line,id) /* locate (possibly qualified) name in name table */
Int  line;
Cell id; {
    assert(isPair(id));
    switch (fst(id)) {
        case VARIDCELL :
        case VAROPCELL :
        case CONIDCELL :
        case CONOPCELL :
            return findName(textOf(id));
        case QUALIDENT : {
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
                        && (tycon(c).what == DATATYPE 
                            || tycon(c).what == NEWTYPE))
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
        }
        default : internal("findQualName2");
    }
}

/* --------------------------------------------------------------------------
 * Primitive functions:
 * ------------------------------------------------------------------------*/

Name addPrimCfun(t,arity,no,rep)        /* add primitive constructor func  */
Text t;
Int  arity;
Int  no;
Int  rep; { /* Really AsmRep */
    Name n          = newName(t);
    name(n).arity   = arity;
    name(n).number  = cfunNo(no);
    name(n).type    = NIL;
    name(n).primop  = (void*)rep;
    return n;
}

Int sfunPos(s,c)                        /* Find position of field with     */
Name s;                                 /* selector s in constructor c.    */
Name c; {
    List cns;
    cns = name(s).defn;
    for (; nonNull(cns); cns=tl(cns)) {
        if (fst(hd(cns))==c)
            return intOf(snd(hd(cns)));
    }
    internal("sfunPos");
    return 0;/*NOTREACHED*/
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
    for (nm=NAMEMIN; nm<nameHw; ++nm)   /* or invented names are excluded) */
        if (!inventedText(name(nm).text) && nonNull(name(nm).type)) {
            String str = textToStr(name(nm).text);
            if (str[0]!='_' && (!pat || stringMatch(pat,str)))
                ns = insertName(nm,ns);
        }
    return ns;
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
#if USE_DICTHW
static Int   dictHw;                   /* next unused dictionary number    */
#endif

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
    cclass(classHw).mod       = currentModule;
    module(currentModule).classes=cons(classHw,module(currentModule).classes);
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

Class addClass(c)        /* Insert Class in class list - if no clash caused */
Class c; {
    Class oldc = findClass(cclass(c).text);
    if (isNull(oldc)) {
        classes=cons(c,classes);
        module(currentModule).classes=cons(c,module(currentModule).classes);
        return c;
    } else
        return oldc;
}

Class findQualClass(c) /* look for (possibly qualified) class in class list */
Cell c; {
    if (!isQualIdent(c)) {
        return findClass(textOf(c));
    } else {
        Text   t = qtextOf(c);
        Module m = findQualifier(qmodOf(c));
        List   es = NIL;
        if (isNull(m)) return NIL;
        for(es=module(m).exports; nonNull(es); es=tl(es)) {
            Cell e = hd(es);
            if (isPair(e) && isClass(fst(e)) && cclass(fst(e)).text==t) 
                return fst(e);
        }
    }
    return NIL;
}

Inst newInst() {                       /* add new instance to table        */
    if (instHw-INSTMIN >= NUM_INSTS) {
        ERRMSG(0) "Instance storage space exhausted"
        EEND;
    }
    inst(instHw).kinds      = NIL;
    inst(instHw).head       = NIL;
    inst(instHw).specifics  = NIL;
    inst(instHw).implements = NIL;
    inst(instHw).builder    = NIL;
    inst(instHw).mod        = currentModule;

    return instHw++;
}

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

Void hugsStackOverflow() {          /* Report stack overflow               */
    ERRMSG(0) "Control stack overflow"
    EEND;
}

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
        if (module(m).text==t) {
            return m;
        }
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
}

static local Module findQualifier(t)    /* locate Module in import list   */
Text t; {
    Module ms;
    if (t==module(modulePreludeHugs).text) {
        /* The Haskell report (rightly) forbids this.
         * We added it to let the Prelude refer to itself
         * without having to import itself.
         */
        return modulePreludeHugs;
    }
    for (ms=module(currentModule).qualImports; nonNull(ms); ms=tl(ms)) {
        if (textOf(fst(hd(ms)))==t) {
            return snd(hd(ms));
        }
    }
    return NIL;
}

Void setCurrModule(m)              /* set lookup tables for current module */
Module m; {
    Int i;
    if (m!=currentModule) {
        currentModule = m; /* This is the only assignment to currentModule */
        for (i=0; i<TYCONHSZ; ++i) {
            tyconHash[i] = NIL;
        }
        mapProc(hashTycon,module(m).tycons);
        for (i=0; i<NAMEHSZ; ++i) {
            nameHash[i] = NIL;
        }
        mapProc(hashName,module(m).names);
        classes = module(m).classes;
    }
}

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
    Int   syntaxHw;
    Module moduleHw;
    Tycon tyconHw;
    Name  nameHw;
    Class classHw;
    Inst  instHw;
#if USE_DICTHW
    Int   dictHw;
#endif
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
    showUse("Syntax", syntaxHw,         NUM_SYNTAX);
    showUse("Module", moduleHw-MODMIN,  NUM_MODULE);
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
    scripts[scriptHw].syntaxHw     = syntaxHw;
    scripts[scriptHw].moduleHw     = moduleHw;
    scripts[scriptHw].tyconHw      = tyconHw;
    scripts[scriptHw].nameHw       = nameHw;
    scripts[scriptHw].classHw      = classHw;
    scripts[scriptHw].instHw       = instHw;
#if USE_DICTHW
    scripts[scriptHw].dictHw       = dictHw;
#endif
#if TREX
    scripts[scriptHw].extHw        = extHw;
#endif
    return scriptHw++;
}

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

Module lastModule() {              /* Return module in current script file */
    return (moduleHw-1);
}

static Module local moduleOfScript(s)
Script s; {
    return scripts[s-1].moduleHw;
}

String fileOfModule(m)
Module m; {
    Script s;
    for(s=0; s<scriptHw; ++s) {
        if (scripts[s].moduleHw == m) {
            return textToStr(scripts[s].file);
        }
    }
    return 0;
}

static Script local scriptThisFile(f)
Text f; {
    Script s;
    for (s=0; s < scriptHw; ++s) {
        if (scripts[s].file == f) {
            return s+1;
        }
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
        syntaxHw     = scripts[sno].syntaxHw;
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
                printf("closing objectFile for module %d\n",i);
                dlclose(module(i).objectFile);
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

        currentModule=NIL;
        for (i=0; i<TYCONHSZ; ++i) {
            tyconHash[i] = NIL;
        }
        for (i=0; i<NAMEHSZ; ++i) {
            nameHash[i] = NIL;
        }

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
Heap    heapTopFst;
Heap    heapTopSnd;
Bool    consGC = TRUE;                  /* Set to FALSE to turn off gc from*/
                                        /* C stack; use with extreme care! */
Int     cellsRecovered;                 /* number of cells recovered       */

static  Cell freeList;                  /* free list of unused cells       */
static  Cell lsave, rsave;              /* save components of pair         */

#if GC_STATISTICS

static Int markCount, stackRoots;

#define initStackRoots() stackRoots = 0
#define recordStackRoot() stackRoots++

#define startGC()       \
    if (gcMessages) {   \
        printf("\n");   \
        fflush(stdout); \
    }
#define endGC()         \
    if (gcMessages) {   \
        printf("\n");   \
        fflush(stdout); \
    }

#define start()      markCount = 0
#define end(thing,rs) \
    if (gcMessages) { \
        printf("GC: %-18s: %4d cells, %4d roots.\n", thing, markCount, rs); \
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
    return c;
}

Void overwrite(dst,src)                 /* overwrite dst cell with src cell*/
Pair dst, src; {                        /* both *MUST* be pairs            */
    assert(isPair(dst) && isPair(src));
    fst(dst) = fst(src);
    snd(dst) = snd(src);
}

Void overwrite2(dst,src1,src2)          /* overwrite dst cell with src cell*/
Pair dst;
Cell src1, src2; {
    assert(isPair(dst));
    fst(dst) = src1;
    snd(dst) = src2;
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
    {   register place = placeInSet(c);
        register mask  = maskInSet(c);
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
    else if (isNull(fst(c)) || fst(c)>=BCSTAG)
        markSnd(c);

    return c;
}

static Void local markSnd(c)            /* Variant of markCell used to     */
Cell c; {                               /* update snd component of cell    */
    Cell t;                             /* using tail recursion            */

ma: t = c;                              /* Keep pointer to original pair   */
    c = snd(c);
mb: if (!isPair(c))
        return;

    {   register place = placeInSet(c);
        register mask  = maskInSet(c);
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
    setjmp(regs);

    gcStarted();
    for (i=0; i<marksSize; ++i)         /* initialise mark set to empty    */
        marks[i] = 0;

    everybody(MARK);                    /* Mark all components of system   */

    gcScanning();                       /* scan mark set                   */
    mask      = 1;
    place     = 0;
    recovered = 0;
    j         = 0;
    freeList = NIL;
    for (i=1; i<=heapSize; i++) {
        if ((marks[place] & mask) == 0) {
            snd(-i)  = freeList;
            fst(-i)  = FREECELL;
            freeList = -i;
            recovered++;
        }
        mask <<= 1;
        if (++j == bitsPerWord) {
            place++;
            mask = 1;
            j    = 0;
        }
    }

    gcRecovered(recovered);
    breakOn(breakStat);                 /* restore break trapping if nec.  */

    /* can only return if freeList is nonempty on return. */
    if (recovered<minRecovery || isNull(freeList)) {
        ERRMSG(0) "Garbage collection fails to reclaim sufficient space"
        EEND;
    }
    cellsRecovered = recovered;
}

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
    if (isPair(c))                      /* Duplicating any text strings    */
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
    if (isPair(c))                      /* Text values are restored to     */
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
    else
        return c;
}

/* --------------------------------------------------------------------------
 * Miscellaneous operations on heap cells:
 * ------------------------------------------------------------------------*/

/* profiling suggests that the number of calls to whatIs() is typically    */
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

    if (c>=NAMEMIN) if (c>=CLASSMIN)    if (c>=CHARMIN) return CHARCELL;
                                        else            return CLASS;
                    else                if (c>=INSTMIN) return INSTANCE;
                                        else            return NAME;
    else            if (c>=MODMIN)      if (c>=TYCMIN)  return TYCON;
                                        else            return MODULE;
                    else                if (c>=OFFMIN)  return OFFSET;
#if TREX
                                        else if (c>=EXTMIN) return EXT;
#endif
                                        else                return TUPLE;

/*  if (isPair(c)) {
        register Cell fstc = fst(c);
        return isTag(fstc) ? fstc : AP;
    }
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
    return isSmall(INTZERO+n) ? INTZERO+n : pair(INTCELL,n);
}

#if PTR_ON_HEAP
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
    assert(isPtr(c));
    x.i = snd(c);
    return x.p;
}
#else
/* For 8 byte addresses (used on the Alpha), we'll have to work harder */
#error "PTR_ON_HEAP not supported on this architecture"
#endif
#endif

String stringNegate( s )
String s;
{
    if (s[0] == '-') {
        return &s[1];
    } else {
        static char t[100];
        t[0] = '-';
        strcpy(&t[1],s);  /* ToDo: use strncpy instead */
        return t;
    }
}

/* --------------------------------------------------------------------------
 * List operations:
 * ------------------------------------------------------------------------*/

Int length(xs)                         /* calculate length of list xs      */
List xs; {
    Int n = 0;
    for (n=0; nonNull(xs); ++n)
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

List revDupOnto(xs,ys)   /* non-destructively prepend xs backwards onto ys */
List xs; 
List ys; {
    for( ; nonNull(xs); xs=tl(xs)) {
        ys = cons(hd(xs),ys);
    }
    return ys;
}

List dupListOnto(xs,ys)              /* Duplicate spine of list xs onto ys */
List xs;
List ys; {
    return revOnto(revDupOnto(xs,NIL),ys);
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

Bool eqList(as,bs)
List as;
List bs; {
    while (nonNull(as) && nonNull(bs) && hd(as)==hd(bs)) {
        as=tl(as);
        bs=tl(bs);
    }
    return (isNull(as) && isNull(bs));
}

Cell varIsMember(t,xs)                 /* Test if variable is a member of  */
Text t;                                /* given list of variables          */
List xs; {
    for (; nonNull(xs); xs=tl(xs))
        if (t==textOf(hd(xs)))
            return hd(xs);
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

List replicate(n,x)                    /* create list of n copies of x     */
Int n;
Cell x; {
    List xs=NIL;
    assert(n>=0);
    while (0<n--) {
        xs = cons(x,xs);
    }
    return xs;
}

List diffList(xs,ys)                   /* list difference: xs\ys           */
List xs, ys; {                         /* result contains all elements of  */
    List result = NIL;                 /* `xs' not appearing in `ys'       */
    while (nonNull(xs)) {
        List next = tl(xs);
        if (!cellIsMember(hd(xs),ys)) {
            tl(xs) = result;
            result = xs;
        }
        xs = next;
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

    assert(n>=0);
    if (n==0)
        return NIL;
    while (1<n-- && nonNull(xs))
        xs = tl(xs);
    if (nonNull(xs))
        tl(xs) = NIL;
    return ys;
}

List splitAt(n,xs)                    /* drop n things from front of list */
Int  n;       
List xs; {
    assert(n>=0);
    for(; n>0; --n) {
        xs = tl(xs);
    }
    return xs;
}

Cell nth(n,xs)                         /* extract n'th element of list    */
Int  n;
List xs; {
    assert(n>=0);
    for(; n>0 && nonNull(xs); --n, xs=tl(xs)) {
    }
    assert(nonNull(xs));
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
    assert(n>=0);
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

                       consGC = TRUE;
                       lsave  = NIL;
                       rsave  = NIL;
                       if (isNull(lastExprSaved))
                           savedText = NUM_TEXT;
                       break;

        case MARK    : 
                       start();
                       for (i=NAMEMIN; i<nameHw; ++i) {
                           mark(name(i).defn);
                           mark(name(i).stgVar);
                           mark(name(i).type);
                       }
                       end("Names", nameHw-NAMEMIN);

                       start();
                       for (i=MODMIN; i<moduleHw; ++i) {
                           mark(module(i).tycons);
                           mark(module(i).names);
                           mark(module(i).classes);
                           mark(module(i).exports);
                           mark(module(i).qualImports);
                       }
                       end("Modules", moduleHw-MODMIN);

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
                           mark(inst(i).kinds);
                           mark(inst(i).head);
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
                       for (i=1; i<heapSize; ++i) {
                           fst(-i) = FREECELL;
                           snd(-i) = -(i+1);
                       }
                       snd(-heapSize) = NIL;
                       freeList  = -1;
                       consGC    = TRUE;
                       lsave     = NIL;
                       rsave     = NIL;

                       marksSize  = bitArraySize(heapSize);
                       if ((marks=(Int *)calloc(marksSize, sizeof(Int)))==0) {
                           ERRMSG(0) "Unable to allocate gc markspace"
                           EEND;
                       }

                       TABALLOC(text,      char,             NUM_TEXT)
                       TABALLOC(tabSyntax, struct strSyntax, NUM_SYNTAX)
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

                       textHw        = 0;
                       nextNewText   = NUM_TEXT;
                       nextNewDText  = (-1);
                       lastExprSaved = NIL;
                       savedText     = NUM_TEXT;
                       for (i=0; i<TEXTHSZ; ++i)
                           textHash[i][0] = NOTEXT;

                       syntaxHw = 0;

                       moduleHw = MODMIN;

                       tyconHw  = TYCMIN;
                       for (i=0; i<TYCONHSZ; ++i)
                           tyconHash[i] = NIL;

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

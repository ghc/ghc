/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * GHC interface file processing for Hugs
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: interface.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:15 $
 * ------------------------------------------------------------------------*/

/* ToDo:
 * o use Z encoding
 * o use vectored CONSTR_entry when appropriate
 * o generate export list
 *
 * Needs GHC changes to generate member selectors,
 * superclass selectors, etc
 * o instance decls
 * o dictionary constructors ?
 *
 * o Get Hugs/GHC to agree on what interface files look like.
 * o figure out how to replace the Hugs Prelude with the GHC Prelude
 */

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "static.h"
#include "errors.h"
#include "link.h"
#include "modules.h"
#include "machdep.h"   /* for Time                 */
#include "input.h"     /* for parseInterface	   */
#include "type.h"      /* for offsetTyVarsIn	   */
#include "stg.h"       /* for wrapping GHC objects */
#include "Assembler.h" /* for wrapping GHC objects */
#include "interface.h"
#include "dynamic.h"

/* --------------------------------------------------------------------------
 * The "addGHC*" functions act as "impedence matchers" between GHC
 * interface files and Hugs.  Their main job is to convert abstract
 * syntax trees into Hugs' internal representations.
 *
 * The main trick here is how we deal with mutually recursive interface 
 * files:
 *
 * o As we read an import decl, we add it to a list of required imports
 *   (unless it's already loaded, of course).
 *
 * o Processing of declarations is split into two phases:
 *
 *   1) While reading the interface files, we construct all the Names,
 *      Tycons, etc declared in the interface file but we don't try to
 *      resolve references to any entities the declaration mentions.
 *
 *      This is done by the "addGHC*" functions.
 *
 *   2) After reading all the interface files, we finish processing the
 *      declarations by resolving any references in the declarations
 *      and doing any other processing that may be required.
 *
 *      This is done by the "finishGHC*" functions which use the 
 *      "fixup*" functions to assist them.
 *
 *   The interface between these two phases are the "ghc*Decls" which
 *   contain lists of decls that haven't been completed yet.
 *
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * local variables:
 * ------------------------------------------------------------------------*/

static List ghcVarDecls;     
static List ghcConDecls;     
static List ghcSynonymDecls; 
static List ghcClassDecls; 
static List ghcInstanceDecls;

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static List local addGHCConstrs Args((Int,List,List));
static Name local addGHCSel     Args((Int,Pair,List));
static Name local addGHCConstr  Args((Int,Int,Triple));


static Void  local finishGHCVar      Args((Name));     
static Void  local finishGHCCon      Args((Name));     
static Void  local finishGHCSynonym  Args((Tycon)); 
static Void  local finishGHCClass    Args((Class)); 
static Void  local finishGHCInstance Args((Inst));

static Name  local fixupSel              Args((Int,Pair,List));
static Name  local fixupConstr           Args((Int,Int,Triple));
static Name  local fixupMember           Args((Int,Int,Pair));
static List  local fixupMembers          Args((Int,List));
static Type  local fixupTypeVar          Args((Int,List,Text));
static Class local fixupClass            Args((Int,Text));
static Cell  local fixupPred             Args((Int,List,Pair));
static List  local fixupContext          Args((Int,List,List));
static Type  local fixupType             Args((Int,List,Type));
static Type  local fixupConType          Args((Int,Type));

static Void  local bindNameToClosure     Args((Name,AsmClosure));
static Kinds local tvsToKind             Args((List));
static Int   local arityFromType         Args((Type));
                                         
static AsmClosure local lookupGHCClosure Args((Module,Text));

/* --------------------------------------------------------------------------
 * code:
 * ------------------------------------------------------------------------*/

static List interfaces; /* Interface files that haven't been loaded yet */

Void loadInterface(String fname)
{
    ghcVarDecls      = NIL;
    ghcConDecls      = NIL;
    ghcSynonymDecls  = NIL;
    ghcClassDecls    = NIL;
    ghcInstanceDecls = NIL;

    /* Note: interfaces is added to by addGHCImport which is called by
     * parseInterface so each time round the loop we remove the 
     * current interface from the list before calling parseInterface again.
     */
    interfaces=singleton(mkCon(findText(fname)));
    while (nonNull(interfaces)) {
        String fname = textToStr(textOf(hd(interfaces)));
        Time timeStamp; /* not used */
        Long fileSize;
        getFileInfo(fname, &timeStamp, &fileSize);
        interfaces=tl(interfaces);
        parseInterface(fname,fileSize);
    }

    /* the order of these doesn't matter
     * (ToDo: unless synonyms have to be eliminated??)
     */
    mapProc(finishGHCVar,      ghcVarDecls);     
    mapProc(finishGHCCon,      ghcConDecls);     
    mapProc(finishGHCSynonym,  ghcSynonymDecls); 
    mapProc(finishGHCClass,    ghcClassDecls); 
    mapProc(finishGHCInstance, ghcInstanceDecls);
    ghcVarDecls      = NIL;
    ghcConDecls      = NIL;
    ghcSynonymDecls  = NIL;
    ghcClassDecls    = NIL;
    ghcInstanceDecls = NIL;
}

Void openGHCIface(t)
Text t; {
    Module m = findModule(t);
    if (isNull(m)) {
        m = newModule(t);
    } else if (m != modulePreludeHugs) {
        ERRMSG(0) "Module \"%s\" already loaded", textToStr(t)
        EEND;
    }
    setCurrModule(m);
}

Void addGHCImport(line,mn,fn)
Int  line;
Text mn;
String fn; {
#if 1 /* new */
    Text   t = findText(fn);
    Module m = findModule(mn);
    if (isNull(m)) {
        if (isNull(varIsMember(t,interfaces))) {
            interfaces = cons(mkCon(t),interfaces);
        }
    }
#else /* old - and probably wrong */
    Module m = findModule(t);
    if (isNull(m)) {
        ERRMSG(0) "Unknown module \"%s\"", textToStr(t)
        EEND;
    }
    /* ToDo: what to do if there's a name conflict? */
    {   /* copied from resolveImportList */
        List es      = module(m).exports;
        List imports = NIL;
        for(; nonNull(es); es=tl(es)) {
            Cell e = hd(es);
            if (isName(e)) {
                imports = cons(e,imports);
            } else {
                Cell c = fst(e);
                List subentities = NIL;
                imports = cons(c,imports);
                if (isTycon(c)
                    && (tycon(c).what == DATATYPE 
                        || tycon(c).what == NEWTYPE)) {
                    subentities = tycon(c).defn;
                } else if (isClass(c)) {
                    subentities = cclass(c).members;
                }
                if (DOTDOT == snd(e)) {
                    imports = revDupOnto(subentities,imports);
                }
            }
        }
        map1Proc(importEntity,m,imports);
    }
#endif
}

void addGHCVar(line,v,ty)
Int  line;
Text v;
Type ty;
{
    Name n = findName(v);
    if (nonNull(n)) {
        ERRMSG(0) "Attempt to redefine variable \"%s\"", textToStr(v)
        EEND;
    }
    n = newName(v);
    bindNameToClosure(n, lookupGHCClosure(name(n).mod,name(n).text));

    /* prepare for finishGHCVar */
    name(n).type = ty;
    ghcVarDecls = cons(n,ghcVarDecls);
}

static Void local finishGHCVar(Name n)
{
    Int  line = name(n).line;
    Type ty   = name(n).type;
    setCurrModule(name(n).mod);
    name(n).type = fixupType(line,NIL,ty);
}

Void addGHCSynonym(line,tycon,tvs,ty)
Int  line;
Cell tycon;  /* ConId          */
List tvs;    /* [(VarId,Kind)] */
Type ty; {
    /* ToDo: worry about being given a decl for (->) ?
     * and worry about qualidents for ()
     */
    Text t = textOf(tycon);
    if (nonNull(findTycon(t))) {
        ERRMSG(line) "Repeated definition of type constructor \"%s\"",
                     textToStr(t)
        EEND;
    } else {
        Tycon tc        = newTycon(t);
        tycon(tc).line  = line;
        tycon(tc).arity = length(tvs);
        tycon(tc).what  = SYNONYM;
        tycon(tc).kind  = tvsToKind(tvs);

        /* prepare for finishGHCSynonym */
        tycon(tc).defn  = pair(tvs,ty);
        ghcSynonymDecls = cons(tc,ghcSynonymDecls);
    }
}

static Void  local finishGHCSynonym(Tycon tc)
{
    Int  line = tycon(tc).line;
    List tvs  = fst(tycon(tc).defn);
    Type ty   = snd(tycon(tc).defn);

    setCurrModule(tycon(tc).mod);
    tycon(tc).defn = fixupType(line,singleton(tvs),ty);

    /* ToDo: can't really do this until I've done all synonyms
     * and then I have to do them in order
     * tycon(tc).defn = fullExpand(ty);
     */
}

Void addGHCDataDecl(line,tycon,tvs,constrs,sels)
Int  line;
Cell tycon;     /* ConId | QualConId      */
List tvs;       /* [(VarId,Kind)]         */
List constrs;   /* [(ConId,[VarId],Type)] */
List sels; {    /* [(VarId,Type)]         */
    /* ToDo: worry about being given a decl for (->) ?
     * and worry about qualidents for ()
     */
    Text t = textOf(tycon);
    if (nonNull(findTycon(t))) {
        ERRMSG(line) "Repeated definition of type constructor \"%s\"",
                     textToStr(t)
        EEND;
    } else {
        Tycon tc        = newTycon(t);
        tycon(tc).line  = line;
        tycon(tc).arity = length(tvs);
        tycon(tc).what  = DATATYPE;
        tycon(tc).kind  = tvsToKind(tvs);
        tycon(tc).defn  = addGHCConstrs(line,constrs,sels);
    }
}

static List local addGHCConstrs(line,cons,sels)
Int  line;
List cons;   /* [(ConId,[VarId],Type)] */
List sels; { /* [(VarId,Type)]         */
    List uses = NIL; /* [(ConName,[VarId])] */
    if (nonNull(cons) && isNull(tl(cons))) { /* Single constructor datatype? */
        List fs  = snd3(hd(cons));
        Name c   = addGHCConstr(line,0,hd(cons));
        uses     = cons(pair(c,fs),uses);
        hd(cons) = c;
    } else {
        Int  conNo = 0; /*  or maybe 1? */
        List cs    = cons;
        for(; nonNull(cs); cs=tl(cs), conNo++) {
            List fs = snd3(hd(cs));
            Name c  = addGHCConstr(line,conNo,hd(cs));
            uses    = cons(pair(c,fs),uses);
            hd(cs)  = c;
        }
    }
    {
        List ss    = sels;
        for(; nonNull(ss); ss=tl(ss)) {
            hd(ss) = addGHCSel(line,hd(ss),uses);
        }
    }
    return appendOnto(cons,sels);
}

static Name local addGHCSel(line,sel,uses)
Int  line;
Pair sel;    /* (VarId,Type)        */
List uses; { /* [(ConName,[VarId])] */
    Text t      = textOf(fst(sel));
    Type type   = snd(sel);
    List fields = NIL;
    
    Name n = findName(t);
    if (nonNull(n)) {
        ERRMSG(line) "Repeated definition for selector \"%s\"",
            textToStr(t)
        EEND;
    }

    n              = newName(t);
    name(n).line   = line;
    name(n).number = SELNAME;
    name(n).arity  = 1;

    for(; nonNull(uses); uses=tl(uses)) {
        Int  fNo = 1;
        Name c   = fst(hd(uses));
        List fs  = snd(hd(uses));
        for(; nonNull(fs); fs=tl(fs), fNo++) {
            if (textOf(hd(fs)) == t) {
                fields = cons(pair(c,mkInt(fNo)),fields);
            }
        }
    }
    name(n).defn   = fields;

    /* prepare for finishGHCVar */
    name(n).type = type;
    ghcVarDecls = cons(n,ghcVarDecls);

    return n;
}

static Name local addGHCConstr(line,conNo,constr)
Int    line;
Int    conNo;
Triple constr; { /* (ConId,[VarId],Type) */
    /* ToDo: add rank2 annotation and existential annotation
     * these affect how constr can be used.
     */
    Text con   = textOf(fst3(constr));
    Type type  = thd3(constr);
    Int  arity = arityFromType(type);
    Name n = findName(con);     /* Allocate constructor fun name   */
    if (isNull(n)) {
        n = newName(con);
    } else if (name(n).defn!=PREDEFINED) {
        ERRMSG(line) "Repeated definition for constructor \"%s\"",
            textToStr(con)
        EEND;
    }
    name(n).arity  = arity;     /* Save constructor fun details    */
    name(n).line   = line;
    name(n).number = cfunNo(conNo);
    bindNameToClosure(n, lookupGHCClosure(name(n).mod,name(n).text));

    /* prepare for finishGHCCon */
    name(n).type   = type;
    ghcConDecls = cons(n,ghcConDecls);

    return n;
}

static Void local finishGHCCon(Name n)
{
    Int  line = name(n).line;
    Type ty   = name(n).type;
    setCurrModule(name(n).mod);
    name(n).type = fixupConType(line,ty);
}

Void addGHCNewType(line,tycon,tvs,constr)
Int  line;
Cell tycon;     /* ConId | QualConId     */
List tvs;       /* [(VarId,Kind)]        */
Cell constr; {
    /* ToDo: worry about being given a decl for (->) ?
     * and worry about qualidents for ()
     */
    Text t = textOf(tycon);
    if (nonNull(findTycon(t))) {
        ERRMSG(line) "Repeated definition of type constructor \"%s\"",
                     textToStr(t)
        EEND;
    } else {
        Tycon tc        = newTycon(t);
        tycon(tc).line  = line;
        tycon(tc).arity = length(tvs);
        tycon(tc).what  = NEWTYPE;
        tycon(tc).kind  = tvsToKind(tvs);
        /* can't really do this until I've read in all synonyms */

        if (isNull(constr)) {
            tycon(tc).defn = NIL;
        } else {
            /* constr :: (ConId,Type) */
            Text con   = textOf(fst(constr));
            Type type  = snd(constr);
            Name n = findName(con);     /* Allocate constructor fun name   */
            if (isNull(n)) {
                n = newName(con);
            } else if (name(n).defn!=PREDEFINED) {
                ERRMSG(line) "Repeated definition for constructor \"%s\"",
                    textToStr(con)
                EEND;
            }
            name(n).arity  = 1;         /* Save constructor fun details    */
            name(n).line   = line;
            name(n).number = cfunNo(0);
            name(n).defn   = nameId;
            tycon(tc).defn = singleton(n);

            /* prepare for finishGHCCon */
            /* ToDo: we use finishGHCCon instead of finishGHCVar in case
             * there's any existential quantification in the newtype -
             * but I don't think that's allowed in newtype constrs.
             * Still, no harm done by doing it this way...
             */
            name(n).type   = type;
            ghcConDecls = cons(n,ghcConDecls);
        }
    }
}

Void addGHCClass(line,ctxt,tc_name,tvs,mems)
Int  line;
List ctxt;      /* [(ConId, [Type])]     */ 
Cell tc_name;   /* ConId | QualConId     */
List tvs;       /* [(VarId,Kind)]        */
List mems; {
    Text ct   = textOf(tc_name);
    if (nonNull(findClass(ct))) {
        ERRMSG(line) "Repeated definition of class \"%s\"",
                     textToStr(ct)
        EEND;
    } else if (nonNull(findTycon(ct))) {
        ERRMSG(line) "\"%s\" used as both class and type constructor",
                     textToStr(ct)
        EEND;
    } else {
        Class nw    = newClass(ct);
        Int   arity = length(tvs);
        Cell  head  = nw;
        Int   i;
        for(i=0; i < arity; ++i) {
            head = ap(head,mkOffset(i));
        }
        cclass(nw).line       = line;
        cclass(nw).arity      = arity;
        cclass(nw).head       = head;
        cclass(nw).kinds      = tvsToKind(tvs);  /* ToDo: I don't think this is right */
        cclass(nw).instances  = NIL;

        /* prepare for finishGHCClass */
        cclass(nw).supers  = pair(tvs,ctxt);    
        cclass(nw).members = mems;
        ghcClassDecls = cons(nw,ghcClassDecls);

        /* ToDo: 
         * cclass(nw).dsels    = ?;
         * cclass(nw).dbuild   = ?;
         * cclass(nm).dcon     = ?;
         * cclass(nm).defaults = ?;
         */
    }
}

static Void  local finishGHCClass(Class nw)
{
    Int  line = cclass(nw).line;
    List tvs  = fst(cclass(nw).supers);
    List ctxt = snd(cclass(nw).supers);
    List mems = cclass(nw).members;

    setCurrModule(cclass(nw).mod);

    cclass(nw).supers     = fixupContext(line,singleton(tvs),ctxt);
    cclass(nw).numSupers  = length(cclass(nw).supers);
    cclass(nw).members    = fixupMembers(line,mems);
    cclass(nw).numMembers = length(cclass(nw).members);
    cclass(nw).level      = 0;  /* ToDo: level = 1 + max (map level supers) */
}

Void addGHCInstance (line,quant,cls,var)
Int  line;
Cell quant;
Pair cls;   /* :: (ConId, [Type]) */
Text var; {
    Inst in = newInst();

    List ctxt   = nonNull(quant) ? snd(quant) : NIL; /* [(ConId, [Type])] */

    inst(in).line         = line;
    inst(in).implements   = NIL;

    {
        Name b         = newName(inventText());
        name(b).line   = line;
        name(b).arity  = length(ctxt); /* unused? */
        name(b).number = DFUNNAME;
        inst(in).builder = b;
        bindNameToClosure(b, lookupGHCClosure(inst(in).mod,var));
    }

    /* prepare for finishGHCInstance */
    inst(in).head      = cls;
    inst(in).specifics = quant;
    ghcInstanceDecls = cons(in,ghcInstanceDecls);
}

static Void  local finishGHCInstance(Inst in)
{
    Int  line   = inst(in).line;
    Cell cl     = fst(inst(in).head);
    List tys    = snd(inst(in).head);
    Cell quant  = inst(in).specifics;
    List tvs    = nonNull(quant) ? fst(quant) : NIL; /* [(VarId,Kind)]    */
    List ctxt   = nonNull(quant) ? snd(quant) : NIL; /* [(ConId, [Type])] */
    List tyvars = singleton(tvs);
    Class c;

    setCurrModule(inst(in).mod);
    c = findClass(textOf(cl));
    if (isNull(c)) {
        ERRMSG(line) "Unknown class \"%s\" in instance",
                     textToStr(textOf(cl))
        EEND;
    }
    map2Over(fixupType,line,tyvars,tys);
    inst(in).head         = applyToArgs(c,tys);
    inst(in).specifics    = fixupContext(line,tyvars,ctxt);
    inst(in).numSpecifics = length(inst(in).specifics);
    cclass(c).instances = cons(in,cclass(c).instances);
}

/* --------------------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------*/

static Name local fixupMember(line,memNo,mem)
Int  line;
Int  memNo;
Pair mem; { /* :: (Text,Type) */
    Text t    = textOf(fst(mem));
    Type type = snd(mem);
    Name m    = findName(t);

    if (isNull(m)) {
        m = newName(t);
    } else if (name(m).defn!=PREDEFINED) {
        ERRMSG(line) "Repeated definition for member function \"%s\"",
                     textToStr(t)
        EEND;
    }

    name(m).line   = line;
    name(m).arity  = 1;
    name(m).number = mfunNo(memNo);
    name(m).type   = fixupType(line,NIL,type);

    /* ToDo: name(m).stgVar = ?; */

    return m;
}


static List  local fixupMembers(line,ms)
Int line;
List ms; {
    Int  memNo = 1;
    List mems  = ms;
    for(; nonNull(mems); mems=tl(mems), memNo++) {
        hd(mems) = fixupMember(line,memNo,hd(mems));
    }
    return ms;
}

static Type local fixupTypeVar(line,tyvars,tv)
Int  line;
List tyvars; /* [[(VarId,Kind)]] */
Text tv; {
    Int  offset = 0;
    for (; nonNull(tyvars); tyvars=tl(tyvars)) {
        List tvs = hd(tyvars);
        for (; nonNull(tvs); offset++, tvs=tl(tvs)) {
            if (tv == textOf(fst(hd(tvs)))) {
                return mkOffset(offset);
            }
        }
    }
    ERRMSG(line) "Undefined type variable \"%s\"", textToStr(tv)
    EEND;
}

static Class local fixupClass(line,cls)
Int  line;
Text cls; {
    Class c = findClass(cls);
    if (isNull(c)) {
        ERRMSG(line)
            "Undefined class \"%s\"", textToStr(cls)
        EEND;
    }
    return c;
}

static Cell local fixupPred(line,tyvars,pred)
Int  line;
List tyvars; /* [[(VarId,Kind)]] */
Pair pred; { /* (ConId,[Type])   */
    Class c   = fixupClass(line,textOf(fst(pred)));
    List  tys = snd(pred);

    map2Over(fixupType,line,tyvars,tys);
    return applyToArgs(c,tys);
}

static List local fixupContext(line,tyvars,ctxt)
Int  line;
List tyvars; /* [[(VarId,Kind)]] */
List ctxt; { /* [(ConId,[Type])] */
    map2Over(fixupPred,line,tyvars,ctxt);
    return ctxt;
}

static Type local fixupType(line,tyvars,type)
Int  line;
List tyvars; /* [[(VarId,Kind)]] */
Type type; {
    switch (whatIs(type)) {
    case AP: 
        {
            fst(type) = fixupType(line,tyvars,fst(type));
            snd(type) = fixupType(line,tyvars,snd(type));
            break;
        }
    case DICTAP: 
        {
            /* Alternatively: raise an error.  These can only
             * occur in the types of instance variables which
             * we could easily separate from "real variables".
             */
            snd(type) = fixupPred(line,tyvars,snd(type));
            break;
        }
    case VARIDCELL: 
            return fixupTypeVar(line,tyvars,textOf(type));
    case CONIDCELL: 
        {   
            Tycon tc = findQualTycon(type);
            if (isNull(tc)) {
                ERRMSG(line)
                    "Undefined type constructor \"%s\"",
                    identToStr(type)
                EEND;
            }
            return tc;
        }
#if TREX
    case EXT:
#endif
    case TYCON:
    case TUPLE: 
            break;
    case POLYTYPE:
        {   
            List  tvs  = fst3(snd(type)); /* [(VarId, Kind)]   */
            List  ctxt = snd3(snd(type)); /* [(ConId, [Type])] */ 
            Type  ty   = thd3(snd(type)); 

            if (nonNull(tvs)) {
                tyvars = cons(tvs,tyvars);
            }
            type = fixupType(line,tyvars,ty);
            
            if (nonNull(ctxt)) {
                type = ap(QUAL,pair(fixupContext(line,tyvars,ctxt),type));
            }
            if (nonNull(tvs)) {
                type = mkPolyType(tvsToKind(tvs),type);
            }
        }
        break;
    default:
            internal("fixupType");
    }
    return type;
}

/*    forall as bs. C1 as, C2 as bs => Ts as bs -> T as
 * => forall as. C1 as => exists bs. C2 as bs => Ts as bs -> T as
 */
static Type local fixupConType(line,type)
Int  line;
Type type; {
    List sig  = NIL;
    List ctxt = NIL;
    type = fixupType(line,NIL,type);

    if (isPolyType(type)) {
        sig = polySigOf(type);
        type = monotypeOf(type);
    }
    if (whatIs(type) == QUAL) {
        ctxt = fst(snd(type));
        type = snd(snd(type));
    }
    { 
        Type r_ty = type;
        Int  nr2 = 0; /* maximum argnum which is a polytype */
        Int  argnum = 1;
        while (isAp(r_ty) && getHead(r_ty)==typeArrow) {
            if (isPolyType(arg(fun(r_ty)))) {
                nr2 = argnum;
            }
            argnum++;
            r_ty = arg(r_ty);
        }

        if (nr2>0) {
            type = ap(RANK2,pair(mkInt(nr2),type));
        }
        {   /* tyvars which don't appear in result are existentially quant'd */
            List result_tvs = offsetTyvarsIn(r_ty,NIL);
            List all_tvs    = offsetTyvarsIn(type,NIL);
            Int etvs = length(all_tvs);
            Int ntvs = length(result_tvs);
            if (etvs>ntvs) {
                /* ToDo: split the context into two parts */
                type = ap(EXIST,pair(mkInt(etvs-ntvs),type));
            }
        }
    }
    if (nonNull(ctxt)) {
        type = ap(QUAL,pair(ctxt,type));
    }
    if (nonNull(sig)) {
        type = mkPolyType(sig,type);
    }
    return type;
}

/* --------------------------------------------------------------------------
 * Utilities
 *
 * None of these do lookups or require that lookups have been resolved
 * so they can be performed while reading interfaces.
 * ------------------------------------------------------------------------*/

static Kinds local tvsToKind(tvs)
List tvs; { /* [(VarId,Kind)] */
    List  rs = NIL;
    Kinds r  = STAR; /* ToDo: hope this works */
    for(; nonNull(tvs); tvs=tl(tvs)) { /* make reversed list of kinds */
        rs = cons(snd(hd(tvs)),rs);
    }
    for(; nonNull(rs); rs=tl(rs)) { /* build full kind */
        r = ap(hd(rs),r);
    }
    return r;
}

static Int local arityFromType(type) /* arity of a constructor with this type */
Type type; {
    Int arity = 0;
    if (isPolyType(type)) {
        type = monotypeOf(type);
    }
    if (whatIs(type) == QUAL) {
        type = snd(snd(type));
    }
    if (whatIs(type) == EXIST) {
        type = snd(snd(type));
    }
    if (whatIs(type)==RANK2) {
        type = snd(snd(type));
    }
    while (isAp(type) && getHead(type)==typeArrow) {
        arity++;
        type = arg(type);
    }
    return arity;
}

/* --------------------------------------------------------------------------
 * Dynamic loading code (probably shouldn't be here)
 *
 * o .hi file explicitly says which .so file to load.
 *   This avoids the need for a 1-to-1 relationship between .hi and .so files.
 *
 *   ToDo: when doing a :reload, we ought to check the modification date 
 *         on the .so file.
 *
 * o module handles are unloaded (dlclosed) when we call dropScriptsFrom.
 *
 *   ToDo: do the same for foreign functions - but with complication that 
 *         there may be multiple .so files
 * ------------------------------------------------------------------------*/

/* ToDo: move some of this code (back) into dynamic.c and make it portable */
#include <stdio.h>

static AsmClosure local lookupGHCClosure( Module m, Text t )
{
    char symbol[100]; /* ToDo: arbitrary constants must die */
    void *c;
    sprintf(symbol,"%s_%s_closure",textToStr(module(m).text),textToStr(t));
    if (module(m).objectFile == NULL) {
        ERRMSG(0) "Interface file must \"require\" at least one file"
        EEND;
    }
    c = lookupSymbol(module(m).objectFile,symbol);
    if (NULL == c) {
        ERRMSG(0) "Error %s while importing symbol \"%s\"", dlerror(), symbol
        EEND;
    }
    return ((AsmClosure)c);
}

Void loadSharedLib( String fn )
{
    if (module(currentModule).objectFile != NULL) {
        ERRMSG(0) "Interface file \"require\"s two files"
        EEND;
    }
    module(currentModule).objectFile = loadLibrary(fn);
    if (NULL == module(currentModule).objectFile) {
        ERRMSG(0) "Error %s while importing DLL \"%s\"", dlerror(), fn
        EEND;
    }
}

static void bindNameToClosure(n,c)
Name n;
AsmClosure c; {
    StgVar v = mkStgVar(NIL,mkPtr(asmMkObject(c)));
    name(n).stgVar = v;
}

/* --------------------------------------------------------------------------
 * Control:
 * ------------------------------------------------------------------------*/

Void interface(what)
Int what; {
    switch (what) {
    case RESET: 
            interfaces       = NIL;
            ghcVarDecls      = NIL;     
            ghcConDecls      = NIL;     
            ghcSynonymDecls  = NIL;
            ghcClassDecls    = NIL;
            ghcInstanceDecls = NIL;
            break;
    case MARK: 
            mark(interfaces);
            mark(ghcVarDecls);     
            mark(ghcConDecls);     
            mark(ghcSynonymDecls); 
            mark(ghcClassDecls); 
            mark(ghcInstanceDecls);
            break;
    }
}

/*-------------------------------------------------------------------------*/



/* --------------------------------------------------------------------------
 * Static Analysis for Hugs
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: static.c,v $
 * $Revision: 1.33 $
 * $Date: 2000/03/31 04:13:27 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local kindError           ( Int,Constr,Constr,String,Kind,Int );
static Void   local checkQualImport     ( Pair );
static Void   local checkUnqualImport   ( Triple );

static Name   local lookupName          ( Text,List );
static List   local checkSubentities    ( List,List,List,String,Text );
static List   local checkExportTycon    ( List,Text,Cell,Tycon );
static List   local checkExportClass    ( List,Text,Cell,Class );
static List   local checkExport         ( List,Text,Cell );
static List   local checkImportEntity   ( List,Module,Bool,Cell );
static List   local resolveImportList   ( Module,Cell,Bool );
static Void   local checkImportList     ( Pair );

static Void   local importEntity        ( Module,Cell );
static Void   local importName          ( Module,Name );
static Void   local importTycon         ( Module,Tycon );
static Void   local importClass         ( Module,Class );
static List   local checkExports        ( List, Module );

static Void   local checkTyconDefn      ( Tycon );
static Void   local depConstrs          ( Tycon,List,Cell );
static List   local addSels             ( Int,Name,List,List );
static List   local selectCtxt          ( List,List );
static Void   local checkSynonyms       ( List );
static List   local visitSyn            ( List,Tycon,List );
static Type   local instantiateSyn      ( Type,Type );

static Void   local checkClassDefn      ( Class );
static Cell   local depPredExp		( Int,List,Cell );
static Void   local checkMems           ( Class,List,Cell );
static Void   local checkMems2          ( Class,Cell );
static Void   local addMembers          ( Class );
static Name   local newMember           ( Int,Int,Cell,Type,Class );
static Text   local generateText        ( String,Class );

static List   local classBindings       ( String,Class,List );
static Name   local memberName          ( Class,Text );
static List   local numInsert           ( Int,Cell,List );

static List   local maybeAppendVar      ( Cell,List );

static Type   local checkSigType        ( Int,String,Cell,Type );
static Void   local checkOptQuantVars	( Int,List,List );
static Type   local depTopType          ( Int,List,Type );
static Type   local depCompType         ( Int,List,Type );
static Type   local depTypeExp          ( Int,List,Type );
static Type   local depTypeVar          ( Int,List,Text );
static List   local checkQuantVars      ( Int,List,List,Cell );
static List   local otvars		( Cell,List );
static Bool   local osubset		( List,List );
static Void   local kindConstr          ( Int,Int,Int,Constr );
static Kind   local kindAtom            ( Int,Constr );
static Void   local kindPred            ( Int,Int,Int,Cell );
static Void   local kindType            ( Int,String,Type );
static Void   local fixKinds            ( Void );

static Void   local kindTCGroup         ( List );
static Void   local initTCKind          ( Cell );
static Void   local kindTC              ( Cell );
static Void   local genTC               ( Cell );

static Void   local checkInstDefn       ( Inst );
static Void   local insertInst          ( Inst );
static Bool   local instCompare         ( Inst,Inst );
static Name   local newInstImp          ( Inst );
static Void   local kindInst            ( Inst,Int );
static Void   local checkDerive         ( Tycon,List,List,Cell );
static Void   local addDerInst          ( Int,Class,List,List,Type,Int );
static Void   local deriveContexts      ( List );
static Void   local initDerInst         ( Inst );
static Void   local calcInstPreds       ( Inst );
static Void   local maybeAddPred        ( Cell,Int,Int,List );
static List   local calcFunDeps		( List );
static Cell   local copyAdj             ( Cell,Int,Int );
static Void   local tidyDerInst         ( Inst );
static List   local otvarsZonk		( Cell,List,Int );

static Void   local addDerivImp         ( Inst );

static Void   local checkDefaultDefns   ( Void );

static Void   local checkForeignImport  ( Name );
static Void   local checkForeignExport  ( Name );

static Cell   local tidyInfix           ( Int,Cell );
static Pair   local attachFixity        ( Int,Cell );
static Syntax local lookupSyntax        ( Text );

static Cell   local checkPat            ( Int,Cell );
static Cell   local checkMaybeCnkPat    ( Int,Cell );
static Cell   local checkApPat          ( Int,Int,Cell );
static Void   local addToPatVars        ( Int,Cell );
static Name   local conDefined          ( Int,Cell );
static Void   local checkIsCfun         ( Int,Name );
static Void   local checkCfunArgs       ( Int,Cell,Int );
static Cell   local checkPatType        ( Int,String,Cell,Type );
static Cell   local applyBtyvs          ( Cell );
static Cell   local bindPat             ( Int,Cell );
static Void   local bindPats            ( Int,List );

static List   local extractSigdecls     ( List );
static List   local extractFixdecls     ( List );
static List   local extractBindings     ( List );
static List   local getPatVars          ( Int,Cell,List );
static List   local addPatVar           ( Int,Cell,List );
static List   local eqnsToBindings      ( List,List,List,List );
static Void   local notDefined          ( Int,List,Cell );
static Cell   local findBinding         ( Text,List );
static Cell   local getAttr             ( List,Cell );
static Void   local addSigdecl          ( List,Cell );
static Void   local addFixdecl          ( List,List,List,List,Triple );
static Void   local dupFixity           ( Int,Text );
static Void   local missFixity          ( Int,Text );

static List   local dependencyAnal      ( List );
static List   local topDependAnal       ( List );
static Void   local addDepField         ( Cell );
static Void   local remDepField         ( List );
static Void   local remDepField1        ( Cell );
static Void   local clearScope          ( Void );
static Void   local withinScope         ( List );
static Void   local leaveScope          ( Void );
static Void   local saveSyntax          ( Cell,Cell );

static Void   local depBinding          ( Cell );
static Void   local depDefaults         ( Class );
static Void   local depInsts            ( Inst );
static Void   local depClassBindings    ( List );
static Void   local depAlt              ( Cell );
static Void   local depRhs              ( Cell );
static Void   local depGuard            ( Cell );
static Cell   local depExpr             ( Int,Cell );
static Void   local depPair             ( Int,Cell );
static Void   local depTriple           ( Int,Cell );
static Void   local depComp             ( Int,Cell,List );
static Void   local depCaseAlt          ( Int,Cell );
static Cell   local depVar              ( Int,Cell );
static Cell   local depQVar             ( Int,Cell );
static Void   local depConFlds          ( Int,Cell,Bool );
static Void   local depUpdFlds          ( Int,Cell );
static List   local depFields           ( Int,Cell,List,Bool );
#if IPARAM
static Void   local depWith		( Int,Cell );
static List   local depDwFlds		( Int,Cell,List );
#endif
#if TREX
static Cell   local depRecord           ( Int,Cell );
#endif

static List   local tcscc               ( List,List );
static List   local bscc                ( List );

static Void   local addRSsigdecls       ( Pair );
static Void   local allNoPrevDef        ( Cell );
static Void   local noPrevDef           ( Int,Cell );
static Bool   local odiff		( List,List );
 
static Void   local duplicateErrorAux   ( Int,Module,Text,String );
#define duplicateError(l,m,t,k) duplicateErrorAux(l,m,t,k)
static Void   local checkTypeIn         ( Pair );

/* --------------------------------------------------------------------------
 * The code in this file is arranged in roughly the following order:
 *  - Kind inference preliminaries
 *  - Module declarations
 *  - Type declarations (data, type, newtype, type in)
 *  - Class declarations
 *  - Type signatures
 *  - Instance declarations
 *  - Default declarations
 *  - Primitive definitions
 *  - Patterns
 *  - Infix expressions
 *  - Value definitions
 *  - Top-level static analysis and control
 *  - Haskell 98 compatibility tests
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Kind checking preliminaries:
 * ------------------------------------------------------------------------*/

Bool kindExpert = FALSE;                /* TRUE => display kind errors in  */
                                        /*         full detail             */

static Void local kindError(l,c,in,wh,k,o)
Int    l;                               /* line number near constuctor exp */
Constr c;                               /* constructor                     */
Constr in;                              /* context (if any)                */
String wh;                              /* place in which error occurs     */
Kind   k;                               /* expected kind (k,o)             */
Int    o; {                             /* inferred kind (typeIs,typeOff)  */
    clearMarks();

    if (!kindExpert) {                  /* for those with a fear of kinds  */
        ERRMSG(l) "Illegal type" ETHEN
        if (nonNull(in)) {
            ERRTEXT " \"" ETHEN ERRTYPE(in);
            ERRTEXT "\""  ETHEN
        }
        ERRTEXT " in %s\n", wh
        EEND;
    }

    ERRMSG(l) "Kind error in %s", wh ETHEN
    if (nonNull(in)) {
        ERRTEXT "\n*** expression     : " ETHEN ERRTYPE(in);
    }
    ERRTEXT "\n*** constructor    : " ETHEN ERRTYPE(c);
    ERRTEXT "\n*** kind           : " ETHEN ERRKIND(copyType(typeIs,typeOff));
    ERRTEXT "\n*** does not match : " ETHEN ERRKIND(copyType(k,o));
    if (unifyFails) {
        ERRTEXT "\n*** because        : %s", unifyFails ETHEN
    }
    ERRTEXT "\n"
    EEND;
}

#define shouldKind(l,c,in,wh,k,o)       if (!kunify(typeIs,typeOff,k,o)) \
                                            kindError(l,c,in,wh,k,o)
#define checkKind(l,a,m,c,in,wh,k,o)    kindConstr(l,a,m,c); \
                                        shouldKind(l,c,in,wh,k,o)
#define inferKind(k,o)                  typeIs=k; typeOff=o

static List unkindTypes;                /* types in need of kind annotation*/
#if TREX
Kind   extKind;                         /* Kind of extension, *->row->row  */
#endif

/* --------------------------------------------------------------------------
 * Static analysis of modules:
 * ------------------------------------------------------------------------*/

Void startModule ( Module m )                    /* switch to a new module */
{
    if (isNull(m)) internal("startModule");
    setCurrModule(m);
}

Void setExportList(exps)              /* Add export list to current module */
List exps; {
    module(currentModule).exports = exps;
}

Void addQualImport(orig,new)         /* Add to qualified import list       */
Cell orig;     /* Original name of module                                  */
Cell new;  {   /* Name module is called within this module (or NIL)        */
    module(currentModule).qualImports = 
      cons(pair(isNull(new)?orig:new,orig),module(currentModule).qualImports);
}

Void addUnqualImport(mod,entities)     /* Add to unqualified import list   */
Cell mod;         /* Name of module                                        */
List entities;  { /* List of entity names                                  */
    unqualImports = cons(pair(mod,entities),unqualImports);
}

static Void local checkQualImport(i)   /* Process qualified import         */
Pair i; {
    Module m = findModid(snd(i));
    if (isNull(m)) {
        ERRMSG(0) "Module \"%s\" not previously loaded", 
                  textToStr(textOf(snd(i)))
        EEND;
    }
    snd(i)=m;
}

static Void local checkUnqualImport(i) /* Process unqualified import       */
Pair i; {
    Module m = findModid(fst(i));
    if (isNull(m)) {
        ERRMSG(0) "Module \"%s\" not previously loaded", 
                  textToStr(textOf(fst(i)))
        EEND;
    }
    fst(i)=m;
}

static Name local lookupName(t,nms)    /* find text t in list of Names     */
Text t;
List nms; { /* :: [Name] */
    for(; nonNull(nms); nms=tl(nms)) {
        if (t == name(hd(nms)).text)
            return hd(nms);
    }
    return NIL;
}

static List local checkSubentities(imports,named,wanted,description,textParent)
List   imports;
List   named;       /* :: [ Q?(Var|Con)(Id|Op) ]                  */
List   wanted;      /* :: [Name]                                  */
String description; /* "<constructor>|<member> of <type>|<class>" */
Text   textParent; {
    for(; nonNull(named); named=tl(named)) {
        Pair x = hd(named);
        /* ToDo: ignores qualifier; doesn't check that entity is in scope */
        Text t = isPair(snd(x)) ? qtextOf(x) : textOf(x);
        Name n = lookupName(t,wanted);
        if (isNull(n)) {
            ERRMSG(0) "Entity \"%s\" is not a %s \"%s\"",
                      textToStr(t),
                      description,
                      textToStr(textParent)
            EEND;
        }
        imports = cons(n,imports);
    }
    return imports;
}

static List local checkImportEntity(imports,exporter,priv,entity)
List   imports; /* Accumulated list of things to import */
Module exporter;
Bool priv;
Cell entity; { /* Entry from import list */
    List oldImports = imports;
    Text t  = isIdent(entity) ? textOf(entity) : textOf(fst(entity));
    List es = NIL;
    if (priv) {
      es = module(exporter).names;
      es = dupOnto(module(exporter).tycons,es);
      es = dupOnto(module(exporter).classes,es);
    } else {
      es = module(exporter).exports; 
    }

    for(; nonNull(es); es=tl(es)) {
        Cell e = hd(es); /* :: Entity
			     | (Entity, NIL|DOTDOT)
			     | tycon 
			     | class
			  */
        if (isPair(e)) {
            Cell f = fst(e);
            if (isTycon(f)) {
                if (tycon(f).text == t) {
                    imports = cons(f,imports);
                    if (!isIdent(entity)) {
                        switch (tycon(f).what) {
                        case NEWTYPE:
                        case DATATYPE:
                            if (DOTDOT == snd(entity)) {
                                imports = dupOnto(tycon(f).defn,imports);
                            } else {
                                imports = checkSubentities(
                                             imports,snd(entity),tycon(f).defn,
                                             "constructor of type",t);
                            }
                            break;
                        default:;
                          /* deliberate fall thru */
                        }
                    }
                }
            } else if (isClass(f)) {
                if (cclass(f).text == t) {
                    imports = cons(f,imports);
                    if (!isIdent(entity)) {
                        if (DOTDOT == snd(entity)) {
                            return dupOnto(cclass(f).members,imports);
                        } else {
                            return checkSubentities(
                                      imports,snd(entity),cclass(f).members,
                                      "member of class",t);
                        }
                    }
                }
            } else {
                internal("checkImportEntity2");
            }
        } else if (isName(e)) {
            if (isIdent(entity) && name(e).text == t) {
                imports = cons(e,imports);
            }
        } else if (isTycon(e) && priv) {
	    if (tycon(e).text == t) {
	        imports = cons(e,imports);
		return dupOnto(tycon(e).defn,imports);
	    }
        } else if (isClass(e) && priv) {
	    if (cclass(e).text == t) {
	        imports = cons(e,imports);
		return dupOnto(cclass(e).members,imports);
	    }
        } else if (whatIs(e) == TUPLE && priv) {
	  // do nothing
        } else {
            internal("checkImportEntity3");
        }
    }
    if (imports == oldImports) {
        ERRMSG(0) "Unknown entity \"%s\" imported from module \"%s\"",
                  textToStr(t),
                  textToStr(module(exporter ).text)
        EEND;
    }
    return imports;
}

static List local resolveImportList(m,impList,priv)
Module m;  /* exporting module */
Cell impList; 
Bool priv; {
    List imports = NIL;
    if (DOTDOT == impList) {
        List es = module(m).exports;
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
                        || tycon(c).what == NEWTYPE))
                    subentities = tycon(c).defn;
                else if (isClass(c))
                    subentities = cclass(c).members;
                if (DOTDOT == snd(e)) {
                    imports = dupOnto(subentities,imports);
                }
            }
        }
    } else {
        map2Accum(checkImportEntity,imports,m,priv,impList);
    }
    return imports;
}

static Void local checkImportList(importSpec) /*Import a module unqualified*/
Pair importSpec; {
    Module m       = fst(importSpec);
    Cell   impList = snd(importSpec);

    List   imports = NIL; /* entities we want to import */
    List   hidden  = NIL; /* entities we want to hide   */

    if (isPair(impList) && HIDDEN == fst(impList)) {
        /* Somewhat inefficient - but obviously correct:
         * imports = importsOf("module Foo") `setDifference` hidden;
         */
        hidden  = resolveImportList(m, snd(impList),FALSE);
        imports = resolveImportList(m, DOTDOT,FALSE);
    } else if (isPair(impList) && STAR == fst(impList)) {
	// Previously, I was forcing an import Prelude,
	// but this precluded doing things like 
	// import Prelude hiding ( catch) 
	// so, for now, you need to put an explicit
	// import Prelude if you use import privileged.
      imports = resolveImportList(m, snd(impList),TRUE);
    } else {
        imports = resolveImportList(m, impList,FALSE);
    }

    for(; nonNull(imports); imports=tl(imports)) {
        Cell e = hd(imports);
        if (!cellIsMember(e,hidden))
            importEntity(m,e);
    }
    /* ToDo: hang onto the imports list for processing export list entries
     * of the form "module Foo"
     */
}

static Void local importEntity(source,e)
Module source;
Cell e; {
    switch (whatIs(e)) {
      case NAME  : importName(source,e); 
                   break;
      case TUPLE:
      case TYCON : importTycon(source,e); 
                   break;
      case CLASS : importClass(source,e);
                   break;
      default: internal("importEntity");
    }
}

static Void local importName(source,n)
Module source;
Name n; {
    Name clash = addName(n);
    if (nonNull(clash) && clash!=n) {
        ERRMSG(0) "Entity \"%s\" imported from module \"%s\""
                  " already defined in module \"%s\"",
                  textToStr(name(n).text), 
                  textToStr(module(source).text),
                  textToStr(module(name(clash).mod).text)
        EEND;
    }
}

static Void local importTycon(source,tc)
Module source;
Tycon tc; {
    Tycon clash=addTycon(tc);
    if (nonNull(clash) && clash!=tc) {
        ERRMSG(0) "Tycon \"%s\" imported from \"%s\" already defined in module \"%s\"",
                  textToStr(tycon(tc).text),
                  textToStr(module(source).text),
                  textToStr(module(tycon(clash).mod).text)      
        EEND;
    }
    if (nonNull(findClass(tycon(tc).text))) {
        ERRMSG(0) "Import of type constructor \"%s\" clashes with class in module \"%s\"",
                  textToStr(tycon(tc).text),
                  textToStr(module(tycon(tc).mod).text) 
        EEND;
    }
}

static Void local importClass(source,c)
Module source;
Class c; {
    Class clash=addClass(c);
    if (nonNull(clash) && clash!=c) {
        ERRMSG(0) "Class \"%s\" imported from \"%s\" already defined in module \"%s\"",
                  textToStr(cclass(c).text),
                  textToStr(module(source).text),
                  textToStr(module(cclass(clash).mod).text)     
        EEND;
    }
    if (nonNull(findTycon(cclass(c).text))) {
        ERRMSG(0) "Import of class \"%s\" clashes with type constructor in module \"%s\"",
                  textToStr(cclass(c).text),
                  textToStr(module(source).text)        
        EEND;
    }
}

static List local checkExportTycon(exports,mt,spec,tc)
List  exports;
Text  mt;
Cell  spec; 
Tycon tc; {
    if (DOTDOT == spec || SYNONYM == tycon(tc).what) {
        return cons(pair(tc,DOTDOT), exports);
    } else {
        return cons(pair(tc,NIL), exports);
    }
}

static List local checkExportClass(exports,mt,spec,cl)
List  exports;
Text  mt;
Class cl;
Cell  spec; {
    if (DOTDOT == spec) {
        return cons(pair(cl,DOTDOT), exports);
    } else {
        return cons(pair(cl,NIL), exports);
    }
}

static List local checkExport(exports,mt,e) /* Process entry in export list*/
List exports;
Text mt; 
Cell e; {
    if (isIdent(e)) {
        Cell export = NIL;
        List origExports = exports;
        if (nonNull(export=findQualName(e))) {
            exports=cons(export,exports);
        } 
        if (isQCon(e) && nonNull(export=findQualTycon(e))) {
            exports = checkExportTycon(exports,mt,NIL,export);
        } 
        if (isQCon(e) && nonNull(export=findQualClass(e))) {
            /* opaque class export */
            exports = checkExportClass(exports,mt,NIL,export);
        }
        if (exports == origExports) {
            ERRMSG(0) "Unknown entity \"%s\" exported from module \"%s\"",
                      identToStr(e),
                      textToStr(mt)
            EEND;
        }
        return exports;
    } else if (MODULEENT == fst(e)) {
        Module m = findModid(snd(e));
        /* ToDo: shouldn't allow export of module we didn't import */
        if (isNull(m)) {
            ERRMSG(0) "Unknown module \"%s\" exported from module \"%s\"",
                      textToStr(textOf(snd(e))),
                      textToStr(mt)
            EEND;
        }
        if (m == currentModule) {
            /* Exporting the current module exports local definitions */
            List xs;
            for(xs=module(m).classes; nonNull(xs); xs=tl(xs)) {
                if (cclass(hd(xs)).mod==m) 
                    exports = checkExportClass(exports,mt,DOTDOT,hd(xs));
            }
            for(xs=module(m).tycons; nonNull(xs); xs=tl(xs)) {
                if (tycon(hd(xs)).mod==m) 
                    exports = checkExportTycon(exports,mt,DOTDOT,hd(xs));
            }
            for(xs=module(m).names; nonNull(xs); xs=tl(xs)) {
                if (name(hd(xs)).mod==m) 
                    exports = cons(hd(xs),exports);
            }
        } else {
            /* Exporting other modules imports all things imported 
             * unqualified from it.  
             * ToDo: we reexport everything exported by a module -
             * whether we imported it or not.  This gives the wrong
             * result for "module M(module N) where import N(x)"
             */
            exports = dupOnto(module(m).exports,exports);
        }
        return exports;
    } else {
        Cell ident = fst(e); /* class name or type name */
        Cell parts = snd(e); /* members or constructors */
        Cell nm;
        if (isQCon(ident) && nonNull(nm=findQualTycon(ident))) {
            switch (tycon(nm).what) {
            case SYNONYM:
                if (DOTDOT!=parts) {
                    ERRMSG(0) "Explicit constructor list given for type synonym"
                              " \"%s\" in export list of module \"%s\"",
                              identToStr(ident),
                              textToStr(mt)
                    EEND;
                }
                return cons(pair(nm,DOTDOT),exports);
            case RESTRICTSYN:   
                ERRMSG(0) "Transparent export of restricted type synonym"
                          " \"%s\" in export list of module \"%s\"",
                          identToStr(ident),
                          textToStr(mt)
                EEND;
                return exports; /* Not reached */
            case NEWTYPE:
            case DATATYPE:
                if (DOTDOT==parts) {
                    return cons(pair(nm,DOTDOT),exports);
                } else {
                    exports = checkSubentities(exports,parts,tycon(nm).defn,
                                               "constructor of type",
                                               tycon(nm).text);
                    return cons(pair(nm,DOTDOT), exports);
                }
            default:
                internal("checkExport1");
            }
        } else if (isQCon(ident) && nonNull(nm=findQualClass(ident))) {
            if (DOTDOT == parts) {
                return cons(pair(nm,DOTDOT),exports);
            } else {
                exports = checkSubentities(exports,parts,cclass(nm).members,
                                           "member of class",cclass(nm).text);
                return cons(pair(nm,DOTDOT), exports);
            }
        } else {
            ERRMSG(0) "Explicit export list given for non-class/datatype \"%s\" in export list of module \"%s\"",
                      identToStr(ident),
                      textToStr(mt)
            EEND;
        }
    }
    return exports; /* NOTUSED */
}

static List local checkExports ( List exports, Module thisModule )
{
    Module m  = thisModule;
    Text   mt = module(m).text;
    List   es = NIL;

    map1Accum(checkExport,es,mt,exports);

#if DEBUG_MODULES
    for(xs=es; nonNull(xs); xs=tl(xs)) {
        Printf(" %s", textToStr(textOfEntity(hd(xs))));
    }
#endif
    return es;
}


/* --------------------------------------------------------------------------
 * Static analysis of type declarations:
 *
 * Type declarations come in two forms:
 * - data declarations - define new constructed data types
 * - type declarations - define new type synonyms
 *
 * A certain amount of work is carried out as the declarations are
 * read during parsing.  In particular, for each type constructor
 * definition encountered:
 * - check that there is no previous definition of constructor
 * - ensure type constructor not previously used as a class name
 * - make a new entry in the type constructor table
 * - record line number of declaration
 * - Build separate lists of newly defined constructors for later use.
 * ------------------------------------------------------------------------*/

Void tyconDefn(line,lhs,rhs,what)       /* process new type definition     */
Int  line;                              /* definition line number          */
Cell lhs;                               /* left hand side of definition    */
Cell rhs;                               /* right hand side of definition   */
Cell what; {                            /* SYNONYM/DATATYPE/etc...         */
    Text t = textOf(getHead(lhs));

    if (nonNull(findTycon(t))) {
        ERRMSG(line) "Repeated definition of type constructor \"%s\"",
                     textToStr(t)
        EEND;
    }
    else if (nonNull(findClass(t))) {
        ERRMSG(line) "\"%s\" used as both class and type constructor",
                     textToStr(t)
        EEND;
    }
    else {
        Tycon nw        = newTycon(t);
        tyconDefns      = cons(nw,tyconDefns);
        tycon(nw).line  = line;
        tycon(nw).arity = argCount;
        tycon(nw).what  = what;
        if (what==RESTRICTSYN) {
            h98DoesntSupport(line,"restricted type synonyms");
            typeInDefns = cons(pair(nw,snd(rhs)),typeInDefns);
            rhs         = fst(rhs);
        }
        tycon(nw).defn  = pair(lhs,rhs);
    }
}

Void setTypeIns(bs)                     /* set local synonyms for given    */
List bs; {                              /* binding group                   */
    List cvs = typeInDefns;
    for (; nonNull(cvs); cvs=tl(cvs)) {
        Tycon c  = fst(hd(cvs));
        List  vs = snd(hd(cvs));
        for (tycon(c).what = RESTRICTSYN; nonNull(vs); vs=tl(vs)) {
            if (nonNull(findBinding(textOf(hd(vs)),bs))) {
                tycon(c).what = SYNONYM;
                break;
            }
        }
    }
}

Void clearTypeIns() {                   /* clear list of local synonyms    */
    for (; nonNull(typeInDefns); typeInDefns=tl(typeInDefns))
        tycon(fst(hd(typeInDefns))).what = RESTRICTSYN;
}

/* --------------------------------------------------------------------------
 * Further analysis of Type declarations:
 *
 * In order to allow the definition of mutually recursive families of
 * data types, the static analysis of the right hand sides of type
 * declarations cannot be performed until all of the type declarations
 * have been read.
 *
 * Once parsing is complete, we carry out the following:
 *
 * - check format of lhs, extracting list of bound vars and ensuring that
 *   there are no repeated variables and no Skolem variables.
 * - run dependency analysis on rhs to check that only bound type vars
 *   appear in type and that all constructors are defined.
 *   Replace type variables by offsets, constructors by Tycons.
 * - use list of dependents to sort into strongly connected components.
 * - ensure that there is not more than one synonym in each group.
 * - kind-check each group of type definitions.
 *
 * - check that there are no previous definitions for constructor
 *   functions in data type definitions.
 * - install synonym expansions and constructor definitions.
 * ------------------------------------------------------------------------*/

static List tcDeps = NIL;               /* list of dependent tycons/classes*/

static Void local checkTyconDefn(d)     /* validate type constructor defn  */
Tycon d; {
    Cell lhs    = fst(tycon(d).defn);
    Cell rhs    = snd(tycon(d).defn);
    Int  line   = tycon(d).line;
    List tyvars = getArgs(lhs);
    List temp;
                                        /* check for repeated tyvars on lhs*/
    for (temp=tyvars; nonNull(temp); temp=tl(temp))
        if (nonNull(varIsMember(textOf(hd(temp)),tl(temp)))) {
            ERRMSG(line) "Repeated type variable \"%s\" on left hand side",
                         textToStr(textOf(hd(temp)))
            EEND;
        }

    tcDeps = NIL;                       /* find dependents                 */
    switch (whatIs(tycon(d).what)) {
        case RESTRICTSYN :
        case SYNONYM     : rhs = depTypeExp(line,tyvars,rhs);
                           if (cellIsMember(d,tcDeps)) {
                               ERRMSG(line) "Recursive type synonym \"%s\"",
                                            textToStr(tycon(d).text)
                               EEND;
                           }
                           break;

        case DATATYPE    :
        case NEWTYPE     : depConstrs(d,tyvars,rhs);
                           rhs = fst(rhs);
                           break;

        default          : internal("checkTyconDefn");
                           break;
    }

    tycon(d).defn = rhs;
    tycon(d).kind = tcDeps;
    tcDeps        = NIL;
}

static Void local depConstrs(t,tyvars,cd)
Tycon t;                                /* Define constructor functions and*/
List  tyvars;                           /* do dependency analysis for data */
Cell  cd; {                             /* definitions (w or w/o deriving) */
    Int  line      = tycon(t).line;
    List ctxt      = NIL;
    Int  conNo     = 1;
    Type lhs       = t;
    List cs        = fst(cd);
    List derivs    = snd(cd);
    List compTypes = NIL;
    List sels      = NIL;
    Int  i;

    for (i=0; i<tycon(t).arity; ++i)    /* build representation for tycon  */
        lhs = ap(lhs,mkOffset(i));      /* applied to full comp. of args   */

    if (isQualType(cs)) {		/* allow for possible context	   */
        ctxt = fst(snd(cs));
        cs   = snd(snd(cs));
	map2Over(depPredExp,line,tyvars,ctxt);
        h98CheckCtxt(line,"context",TRUE,ctxt,NIL);
    }

    if (nonNull(cs) && isNull(tl(cs)))  /* Single constructor datatype?    */
        conNo = 0;

    for (; nonNull(cs); cs=tl(cs)) {    /* For each constructor function:  */
        Cell con   = hd(cs);
        List sig   = dupList(tyvars);
        List evs   = NIL;               /* locally quantified vars         */
        List lps   = NIL;               /* locally bound predicates        */
        List ctxt1 = ctxt;              /* constructor function context    */
        List scs   = NIL;               /* strict components               */
        List fs    = NONE;              /* selector names                  */
        Type type  = lhs;               /* constructor function type       */
        Int  arity = 0;                 /* arity of constructor function   */
        Int  nr2   = 0;                 /* Number of rank 2 args           */
        Name n;                         /* name for constructor function   */

        if (whatIs(con)==POLYTYPE) {    /* Locally quantified vars         */
            evs = fst(snd(con));
            con = snd(snd(con));
            sig = checkQuantVars(line,evs,sig,con);
        }

	if (isQualType(con)) {		/* Local predicates		   */
            List us;
            lps     = fst(snd(con));
	    for (us = typeVarsIn(lps,NIL,NIL,NIL); nonNull(us); us=tl(us))
                if (!varIsMember(textOf(hd(us)),evs)) {
                    ERRMSG(line)
                        "Variable \"%s\" in constraint is not locally bound",
                        textToStr(textOf(hd(us)))
                    EEND;
                }
	    map2Over(depPredExp,line,sig,lps);
            con     = snd(snd(con));
            arity   = length(lps);
        }

        if (whatIs(con)==LABC) {        /* Skeletize constr components     */
            Cell fls = snd(snd(con));   /* get field specifications        */
            con      = fst(snd(con));
            fs       = NIL;
            for (; nonNull(fls); fls=tl(fls)) { /* for each field spec:    */
                List vs     = fst(hd(fls));
                Type t      = snd(hd(fls));     /* - scrutinize type       */
                Bool banged = whatIs(t)==BANG;
                t           = depCompType(line,sig,(banged ? arg(t) : t));
                while (nonNull(vs)) {           /* - add named components  */
                    Cell us = tl(vs);
                    tl(vs)  = fs;
                    fs      = vs;
                    vs      = us;
                    con     = ap(con,t);
                    arity++;
                    if (banged)
                        scs = cons(mkInt(arity),scs);
                }
            }
            fs  = rev(fs);
            scs = rev(scs);             /* put strict comps in ascend ord  */
        }
        else {                          /* Non-labelled constructor        */
            Cell c = con;
            Int  compNo;
            for (; isAp(c); c=fun(c))
                arity++;
            for (compNo=arity, c=con; isAp(c); c=fun(c)) {
                Type t = arg(c);
                if (whatIs(t)==BANG) {
                    scs = cons(mkInt(compNo),scs);
                    t   = arg(t);
                }
                compNo--;
                arg(c) = depCompType(line,sig,t);
            }
        }

        if (nonNull(ctxt1))             /* Extract relevant part of context*/
            ctxt1 = selectCtxt(ctxt1,offsetTyvarsIn(con,NIL));

        for (i=arity; isAp(con); i--) { /* Calculate type of constructor   */
            Type ty  = fun(con);
            Type cmp = arg(con);
            fun(con) = typeArrow;
	    if (isPolyOrQualType(cmp)) {
                if (nonNull(derivs)) {
                    ERRMSG(line) "Cannot derive instances for types" ETHEN
		    ERRTEXT      " with polymorphic or qualified components"
                    EEND;
                }
                if (nr2==0)
                    nr2 = i;
            }
            if (nonNull(derivs))        /* and build list of components    */
                compTypes = cons(cmp,compTypes);
            type     = ap(con,type);
            con      = ty;
        }

	if (nr2>0) {			/* Add rank 2 annotation	   */
	    type = ap(RANK2,pair(mkInt(nr2-length(lps)),type));
	}

        if (nonNull(evs)) {             /* Add existential annotation      */
            if (nonNull(derivs)) {
                ERRMSG(line) "Cannot derive instances for types" ETHEN
                ERRTEXT      " with existentially typed components"
                EEND;
            }
            if (fs!=NONE) {
                ERRMSG(line)
                   "Cannot use selectors with existentially typed components"
                EEND;
            }
            type = ap(EXIST,pair(mkInt(length(evs)),type));
        }

        if (nonNull(lps)) {             /* Add local preds part to type    */
            type = ap(CDICTS,pair(lps,type));
        }

        if (nonNull(ctxt1)) {           /* Add context part to type        */
            type = ap(QUAL,pair(ctxt1,type));
        }

        if (nonNull(sig)) {             /* Add quantifiers to type         */
            List ts1 = sig;
            for (; nonNull(ts1); ts1=tl(ts1)) {
                hd(ts1) = NIL;
            }
            type = mkPolyType(sig,type);
        }

        n = findName(textOf(con));      /* Allocate constructor fun name   */
        if (isNull(n)) {
            n = newName(textOf(con),NIL);
        } else if (name(n).defn!=PREDEFINED) {
            duplicateError(line,name(n).mod,name(n).text,
                           "constructor function");
        }
        name(n).arity  = arity;         /* Save constructor fun details    */
        name(n).line   = line;
        name(n).parent = t;
        name(n).number = cfunNo(conNo++);
        name(n).type   = type;
        if (tycon(t).what==NEWTYPE) {
            if (nonNull(lps)) {
                ERRMSG(line)
                   "A newtype constructor cannot have class constraints"
                EEND;
            }
            if (arity!=1) {
                ERRMSG(line)
                   "A newtype constructor must have exactly one argument"
                EEND;
            }
            if (nonNull(scs)) {
                ERRMSG(line)
                   "Illegal strictess annotation for newtype constructor"
                EEND;
            }
            name(n).defn = nameId;
        } else {
            implementCfun(n,scs);
        }

        hd(cs) = n;
        if (fs!=NONE) {
            sels = addSels(line,n,fs,sels);
        }
    }

    if (nonNull(sels)) {
        sels     = rev(sels);
        fst(cd)  = appendOnto(fst(cd),sels);
        selDefns = cons(sels,selDefns);
    }

    if (nonNull(derivs)) {              /* Generate derived instances      */
        map3Proc(checkDerive,t,ctxt,compTypes,derivs);
    }
}

Int userArity(c)                        /* Find arity for cfun, ignoring   */
Name c; {                               /* CDICTS parameters               */
    Int  a = name(c).arity;
    Type t = name(c).type;
    Int  w;
    if (isPolyType(t)) {
        t = monotypeOf(t);
    }
    if ((w=whatIs(t))==QUAL) {
        w = whatIs(t=snd(snd(t)));
    }
    if (w==CDICTS) {
        a -= length(fst(snd(t)));
    }
    return a;
}


static List local addSels(line,c,fs,ss) /* Add fields to selector list     */
Int  line;                              /* line number of constructor      */
Name c;                                 /* corresponding constr function   */
List fs;                                /* list of fields (varids)         */
List ss; {                              /* list of existing selectors      */
    Int sn    = 1;
    cfunSfuns = cons(pair(c,fs),cfunSfuns);
    for (; nonNull(fs); fs=tl(fs), ++sn) {
        List ns = ss;
        Text t  = textOf(hd(fs));

        if (nonNull(varIsMember(t,tl(fs)))) {
            ERRMSG(line) "Repeated field name \"%s\" for constructor \"%s\"",
                         textToStr(t), textToStr(name(c).text)
            EEND;
        }

        while (nonNull(ns) && t!=name(hd(ns)).text) {
            ns = tl(ns);
        }

        if (nonNull(ns)) {
            name(hd(ns)).defn = cons(pair(c,mkInt(sn)),name(hd(ns)).defn);
        } else {
            Name n = findName(t);
            if (nonNull(n)) {
                ERRMSG(line) "Repeated definition for selector \"%s\"",
                             textToStr(t)
                EEND;
            }
            n              = newName(t,c);
            name(n).line   = line;
            name(n).number = SELNAME;
            name(n).defn   = singleton(pair(c,mkInt(sn)));
            ss             = cons(n,ss);
        }
    }
    return ss;
}

static List local selectCtxt(ctxt,vs)   /* calculate subset of context     */
List ctxt;
List vs; {
    if (isNull(vs)) {
        return NIL;
    } else {
        List ps = NIL;
        for (; nonNull(ctxt); ctxt=tl(ctxt)) {
            List us = offsetTyvarsIn(hd(ctxt),NIL);
            for (; nonNull(us) && cellIsMember(hd(us),vs); us=tl(us)) {
            }
            if (isNull(us)) {
                ps = cons(hd(ctxt),ps);
            }
        }
        return rev(ps);
    }
}

static Void local checkSynonyms(ts)     /* Check for mutually recursive    */
List ts; {                              /* synonyms                        */
    List syns = NIL;
    for (; nonNull(ts); ts=tl(ts)) {    /* build list of all synonyms      */
        Tycon t = hd(ts);
        switch (whatIs(tycon(t).what)) {
            case SYNONYM     :
            case RESTRICTSYN : syns = cons(t,syns);
                               break;
        }
    }
    while (nonNull(syns)) {             /* then visit each synonym         */
        syns = visitSyn(NIL,hd(syns),syns);
    }
}

static List local visitSyn(path,t,syns) /* visit synonym definition to look*/
List  path;                             /* for cycles                      */
Tycon t;
List  syns; {
    if (cellIsMember(t,path)) {         /* every elt in path depends on t  */
        ERRMSG(tycon(t).line)
            "Type synonyms \"%s\" and \"%s\" are mutually recursive",
            textToStr(tycon(t).text), textToStr(tycon(hd(path)).text)
        EEND;
    } else {
        List ds    = tycon(t).kind;
        List path1 = NIL;
        for (; nonNull(ds); ds=tl(ds)) {
            if (cellIsMember(hd(ds),syns)) {
                if (isNull(path1)) {
                    path1 = cons(t,path);
                }
                syns = visitSyn(path1,hd(ds),syns);
            }
        }
    }
    tycon(t).defn = fullExpand(tycon(t).defn);
    return removeCell(t,syns);
}

/* --------------------------------------------------------------------------
 * Expanding out all type synonyms in a type expression:
 * ------------------------------------------------------------------------*/

Type fullExpand(t)                      /* find full expansion of type exp */
Type t; {                               /* assuming that all relevant      */
    Cell h = t;                         /* synonym defns of lower rank have*/
    Int  n = 0;                         /* already been fully expanded     */
    List args;
    for (args=NIL; isAp(h); h=fun(h), n++) {
        args = cons(fullExpand(arg(h)),args);
    }
    t = applyToArgs(h,args);
    if (isSynonym(h) && n>=tycon(h).arity) {
        if (n==tycon(h).arity) {
            t = instantiateSyn(tycon(h).defn,t);
        } else {
            Type p = t;
            while (--n > tycon(h).arity) {
                p = fun(p);
            }
            fun(p) = instantiateSyn(tycon(h).defn,fun(p));
        }
    }
    return t;
}

static Type local instantiateSyn(t,env) /* instantiate type according using*/
Type t;                                 /* env to determine appropriate    */
Type env; {                             /* values for OFFSET type vars     */
    switch (whatIs(t)) {
        case AP      : return ap(instantiateSyn(fun(t),env),
                                 instantiateSyn(arg(t),env));

        case OFFSET  : return nthArg(offsetOf(t),env);

        default      : return t;
    }
}

/* --------------------------------------------------------------------------
 * Static analysis of class declarations:
 *
 * Performed in a similar manner to that used for type declarations.
 *
 * The first part of the static analysis is performed as the declarations
 * are read during parsing.  The parser ensures that:
 * - the class header and all superclass predicates are of the form
 *   ``Class var''
 *
 * The classDefn() function:
 * - ensures that there is no previous definition for class
 * - checks that class name has not previously been used as a type constr.
 * - make new entry in class table
 * - record line number of declaration
 * - build list of classes defined in current script for use in later
 *   stages of static analysis.
 * ------------------------------------------------------------------------*/

Void classDefn(line,head,ms,fds)       /* process new class definition	   */
Int  line;			       /* definition line number	   */
Cell head;			       /* class header :: ([Supers],Class) */
List ms;			       /* class definition body		   */
List fds; {			       /* functional dependencies	   */
    Text ct    = textOf(getHead(snd(head)));
    Int  arity = argCount;

    if (nonNull(findClass(ct))) {
	ERRMSG(line) "Repeated definition of class \"%s\"",
		     textToStr(ct)
	EEND;
    } else if (nonNull(findTycon(ct))) {
	ERRMSG(line) "\"%s\" used as both class and type constructor",
		     textToStr(ct)
	EEND;
    } else {
	Class nw	   = newClass(ct);
	cclass(nw).line    = line;
	cclass(nw).arity   = arity;
	cclass(nw).head    = snd(head);
	cclass(nw).supers  = fst(head);
	cclass(nw).members = ms;
	cclass(nw).level   = 0;
	cclass(nw).fds	   = fds;
	cclass(nw).xfds	   = NIL;
	classDefns	   = cons(nw,classDefns);
	if (arity!=1)
	    h98DoesntSupport(line,"multiple parameter classes");
    }
}

/* --------------------------------------------------------------------------
 * Further analysis of class declarations:
 *
 * Full static analysis of class definitions must be postponed until the
 * complete script has been read and all static analysis on type definitions
 * has been completed.
 *
 * Once this has been achieved, we carry out the following checks on each
 * class definition:
 * - check that variables in header are distinct
 * - replace head by skeleton
 * - check superclass declarations, replace by skeletons
 * - split body of class into members and declarations
 * - make new name entry for each member function
 * - record member function number (eventually an offset into dictionary!)
 * - no member function has a previous definition ...
 * - no member function is mentioned more than once in the list of members
 * - each member function type is valid, replace vars by offsets
 * - qualify each member function type by class header
 * - only bindings for members appear in defaults
 * - only function bindings appear in defaults
 * - check that extended class hierarchy does not contain any cycles
 * ------------------------------------------------------------------------*/

static Void local checkClassDefn(c)    /* validate class definition        */
Class c; {
    List tyvars = NIL;
    Int  args   = cclass(c).arity - 1;
    Cell temp   = cclass(c).head;
    List fs     = NIL;
    List ss     = NIL;

    for (; isAp(temp); temp=fun(temp)) {
        if (!isVar(arg(temp))) {
            ERRMSG(cclass(c).line) "Type variable required in class head"
            EEND;
        }
        if (nonNull(varIsMember(textOf(arg(temp)),tyvars))) {
            ERRMSG(cclass(c).line)
                "Repeated type variable \"%s\" in class head",
                textToStr(textOf(arg(temp)))
            EEND;
        }
        tyvars = cons(arg(temp),tyvars);
    }

    for (fs=cclass(c).fds; nonNull(fs); fs=tl(fs)) {
	Pair fd = hd(fs);
	List vs = snd(fd);

	/* Check for trivial dependency
	 */
	if (isNull(vs)) {
	    ERRMSG(cclass(c).line) "Functional dependency is trivial"
	    EEND;
	}

	/* Check for duplicated vars on right hand side, and for vars on
	 * right that also appear on the left:
	 */
	for (vs=snd(fd); nonNull(vs); vs=tl(vs)) {
	    if (varIsMember(textOf(hd(vs)),fst(fd))) {
		ERRMSG(cclass(c).line)
		    "Trivial dependency for variable \"%s\"",
		    textToStr(textOf(hd(vs)))
		EEND;
	    }
	    if (varIsMember(textOf(hd(vs)),tl(vs))) {
		ERRMSG(cclass(c).line)
		    "Repeated variable \"%s\" in functional dependency",
		    textToStr(textOf(hd(vs)))
		EEND;
	    }
	    hd(vs) = depTypeVar(cclass(c).line,tyvars,textOf(hd(vs)));
	}

	/* Check for duplicated vars on left hand side:
	 */
	for (vs=fst(fd); nonNull(vs); vs=tl(vs)) {
	    if (varIsMember(textOf(hd(vs)),tl(vs))) {
		ERRMSG(cclass(c).line)
		    "Repeated variable \"%s\" in functional dependency",
		    textToStr(textOf(hd(vs)))
		EEND;
	    }
	    hd(vs) = depTypeVar(cclass(c).line,tyvars,textOf(hd(vs)));
	}
    }

    if (cclass(c).arity==0) {
	cclass(c).head = c;
    } else {
	Int args = cclass(c).arity - 1;
	for (temp=cclass(c).head; args>0; temp=fun(temp), args--) {
	    arg(temp) = mkOffset(args);
	}
	arg(temp) = mkOffset(0);
	fun(temp) = c;
    }

    tcDeps	        = NIL;		/* find dependents		   */
    map2Over(depPredExp,cclass(c).line,tyvars,cclass(c).supers);
    h98CheckCtxt(cclass(c).line,"class definition",FALSE,cclass(c).supers,NIL);
    cclass(c).numSupers = length(cclass(c).supers);
    cclass(c).defaults  = extractBindings(cclass(c).members);   /* defaults*/
    ss                  = extractSigdecls(cclass(c).members);
    fs                  = extractFixdecls(cclass(c).members);
    cclass(c).members   = pair(ss,fs);
    map2Proc(checkMems,c,tyvars,ss);

    cclass(c).kinds     = tcDeps;
    tcDeps              = NIL;
}


/* --------------------------------------------------------------------------
 * Functional dependencies are inherited from superclasses.
 * For example, if I've got the following classes:
 *
 * class C a b | a -> b
 * class C [b] a => D a b
 *
 * then C will have the dependency ([a], [b]) as expected, and D will inherit
 * the dependency ([b], [a]) from C.
 * When doing pairwise improvement, we have to consider not just improving
 * when we see a pair of Cs or a pair of Ds in the context, but when we've
 * got a C and a D as well.  In this case, we only improve when the
 * predicate in question matches the type skeleton in the relevant superclass
 * constraint.  E.g., we improve the pair (C [Int] a, D b Int) (unifying
 * a and b), but we don't improve the pair (C Int a, D b Int).
 * To implement functional dependency inheritance, we calculate
 * the closure of all functional dependencies, and store the result
 * in an additional field `xfds' (extended functional dependencies).
 * The `xfds' field is a list of functional dependency lists, annotated
 * with a list of predicate skeletons constraining when improvement can
 * happen against this dependency list.  For example, the xfds field
 * for C above would be:
 *     [([C a b], [([a], [b])])]
 * and the xfds field for D would be:
 *     [([C [b] a, D a b], [([b], [a])])]
 * Self-improvement (of a C with a C, or a D with a D) is treated as a
 * special case of an inherited dependency.
 * ------------------------------------------------------------------------*/
static List local inheritFundeps ( Class c, Cell pi, Int o )
{
    Int alpha = newKindedVars(cclass(c).kinds);
    List scs = cclass(c).supers;
    List xfds = NIL;
    Cell this = NIL;
    /* better not fail ;-) */
    if (!matchPred(pi,o,cclass(c).head,alpha))
	internal("inheritFundeps - predicate failed to match it's own head!");
    this = copyPred(pi,o);
    for (; nonNull(scs); scs=tl(scs)) {
	Class s = getHead(hd(scs));
	if (isClass(s)) {
	    List sfds = inheritFundeps(s,hd(scs),alpha);
	    for (; nonNull(sfds); sfds=tl(sfds)) {
		Cell h = hd(sfds);
		xfds = cons(pair(cons(this,fst(h)),snd(h)),xfds);
	    }
	}
    }
    if (nonNull(cclass(c).fds)) {
	List fds = NIL, fs = cclass(c).fds;
	for (; nonNull(fs); fs=tl(fs)) {
	    fds = cons(pair(otvars(this,fst(hd(fs))),
			    otvars(this,snd(hd(fs)))),fds);
	}
	xfds = cons(pair(cons(this,NIL),fds),xfds);
    }
    return xfds;
}

static Void local extendFundeps ( Class c )
{ 
    Int alpha;
    emptySubstitution();
    alpha = newKindedVars(cclass(c).kinds);
    cclass(c).xfds = inheritFundeps(c,cclass(c).head,alpha);

    /* we can now check for ambiguity */
    map1Proc(checkMems2,c,fst(cclass(c).members));
}


static Cell local depPredExp(line,tyvars,pred)
Int  line;
List tyvars;
Cell pred; {
    Int  args = 0;
    Cell prev = NIL;
    Cell h    = pred;
    for (; isAp(h); args++) {
	arg(h) = depTypeExp(line,tyvars,arg(h));
	prev   = h;
	h      = fun(h);
    }

    if (args==0) {
	h98DoesntSupport(line,"tag classes");
    } else if (args!=1) {
	h98DoesntSupport(line,"multiple parameter classes");
    }

    if (isQCon(h)) {                    /* standard class constraint       */
        Class c = findQualClass(h);
        if (isNull(c)) {
            ERRMSG(line) "Undefined class \"%s\"", identToStr(h)
            EEND;
        }
	if (isNull(prev)) {
	    pred = c;
	} else {
	    fun(prev) = c;
	}
        if (args!=cclass(c).arity) {
            ERRMSG(line) "Wrong number of arguments for class \"%s\"",
                        textToStr(cclass(c).text)
            EEND;
        }
        if (cellIsMember(c,classDefns) && !cellIsMember(c,tcDeps)) {
            tcDeps = cons(c,tcDeps);
        }
    }
#if TREX
    else if (isExt(h)) {                /* Lacks predicate                 */
        if (args!=1) {                  /* parser shouldn't let this happen*/
            ERRMSG(line) "Wrong number of arguments for lacks predicate"
            EEND;
        }
    }
#endif
    else 
#if IPARAM
         if (whatIs(h) != IPCELL)
#endif
    {
	internal("depPredExp");
    }
    return pred;
}

static Void local checkMems(c,tyvars,m) /* check member function details   */
Class c;
List  tyvars;
Cell  m; {
    Int  line = intOf(fst3(m));
    List vs   = snd3(m);
    Type t    = thd3(m);
    List sig  = NIL;
    List tvs  = NIL;
    List xtvs = NIL;

    if (isPolyType(t)) {
	xtvs = fst(snd(t));
	t    = monotypeOf(t);
    }
  

    tyvars    = typeVarsIn(t,NIL,xtvs,tyvars);
					/* Look for extra type vars.	   */
    checkOptQuantVars(line,xtvs,tyvars);

    if (isQualType(t)) {		/* Overloaded member signatures?   */
	map2Over(depPredExp,line,tyvars,fst(snd(t)));
    } else {
        t = ap(QUAL,pair(NIL,t));
    }

    fst(snd(t)) = cons(cclass(c).head,fst(snd(t)));/* Add main predicate   */
    snd(snd(t)) = depTopType(line,tyvars,snd(snd(t)));

    for (tvs=tyvars; nonNull(tvs); tvs=tl(tvs)){/* Quantify                */
        sig = ap(NIL,sig);
    }
    if (nonNull(sig)) {
  	t = mkPolyType(sig,t);
    }
    thd3(m) = t;                                /* Save type               */
    take(cclass(c).arity,tyvars);               /* Delete extra type vars  */

    if (isAmbiguous(t)) {
        ambigError(line,"class declaration",hd(vs),t);
    }
    h98CheckType(line,"member type",hd(vs),t);
}

static Void local checkMems2(c,m) /* check member function details   */
Class c;
Cell  m; {
    Int  line = intOf(fst3(m));
    List vs   = snd3(m);
    Type t    = thd3(m);
}

static Void local addMembers(c)         /* Add definitions of member funs  */
Class c; {                              /* and other parts of class struct.*/
    List ms  = fst(cclass(c).members);
    List fs  = snd(cclass(c).members);
    List ns  = NIL;                     /* List of names                   */
    Int  mno;                           /* Member function number          */

    for (mno=0; mno<cclass(c).numSupers; mno++) {
        ns = cons(newDSel(c,mno),ns);
    }
    cclass(c).dsels = rev(ns);          /* Save dictionary selectors       */

    for (mno=1, ns=NIL; nonNull(ms); ms=tl(ms)) {
        Int  line = intOf(fst3(hd(ms)));
        List vs   = rev(snd3(hd(ms)));
        Type t    = thd3(hd(ms));
        for (; nonNull(vs); vs=tl(vs)) {
            ns = cons(newMember(line,mno++,hd(vs),t,c),ns);
        }
    }
    cclass(c).members    = rev(ns);     /* Save list of members            */
    cclass(c).numMembers = length(cclass(c).members);

    for (; nonNull(fs); fs=tl(fs)) {    /* fixity declarations             */
        Int    line = intOf(fst3(hd(fs)));
        List   ops  = snd3(hd(fs));
        Syntax s    = intOf(thd3(hd(fs)));
        for (; nonNull(ops); ops=tl(ops)) {
            Name n = nameIsMember(textOf(hd(ops)),cclass(c).members);
            if (isNull(n)) {
                missFixity(line,textOf(hd(ops)));
            } else if (name(n).syntax!=NO_SYNTAX) {
                dupFixity(line,textOf(hd(ops)));
            }
            name(n).syntax = s;
        }
    }

/*  Not actually needed just yet; for the time being, dictionary code will
    not be passed through the type checker.

    cclass(c).dtycon    = addPrimTycon(generateText("Dict.%s",c),
                                       NIL,
                                       cclass(c).arity,
                                       DATATYPE,
                                       NIL);
*/

    mno                  = cclass(c).numSupers + cclass(c).numMembers;
    /* cclass(c).dcon       = addPrimCfun(generateText("Make.%s",c),mno,0,NIL); */
    cclass(c).dcon       = addPrimCfun(generateText(":D%s",c),mno,0,NIL);
    /* implementCfun(cclass(c).dcon,NIL);
       Don't manufacture a wrapper fn for dictionary constructors.
       Applications of dictionary constructors are always saturated,
       and translate.c:stgExpr() special-cases saturated constructor apps.
    */

    if (mno==1) {                       /* Single entry dicts use newtype  */
        name(cclass(c).dcon).defn = nameId;
	if (nonNull(cclass(c).members)) {
	    name(hd(cclass(c).members)).number = mfunNo(0);
	}
    }
    cclass(c).defaults   = classBindings("class",c,cclass(c).defaults);
}

static Name local newMember(l,no,v,t,parent)
Int   l;                                /* Make definition for member fn   */
Int   no;
Cell  v;
Type  t; 
Class parent; {
    Name m = findName(textOf(v));

    if (isNull(m)) {
        m = newName(textOf(v),parent);
    } else if (name(m).defn!=PREDEFINED) {
        ERRMSG(l) "Repeated definition for member function \"%s\"",
                  textToStr(name(m).text)
        EEND;
    }

    name(m).line     = l;
    name(m).arity    = 1;
    name(m).number   = mfunNo(no);
    name(m).type     = t;
    return m;
}

Name newDSel(c,no)                      /* Make definition for dict selectr*/
Class c;
Int   no; {
    Name s;
    char buf[16];

    /* sprintf(buf,"sc%d.%s",no,"%s"); */
    sprintf(buf,"$p%d%s",no+1,"%s");
    s                = newName(generateText(buf,c),c);
    name(s).line     = cclass(c).line;
    name(s).arity    = 1;
    name(s).number   = DFUNNAME;
    return s;
}

#define MAX_GEN  128

static Text local generateText(sk,c)    /* We need to generate names for   */
String sk;                              /* certain objects corresponding   */
Class  c; {                             /* to each class.                  */
    String cname = textToStr(cclass(c).text);
    char buffer[MAX_GEN+1];

    if ((strlen(sk)+strlen(cname))>=MAX_GEN) {
        ERRMSG(0) "Please use a shorter name for class \"%s\"", cname
        EEND;
    }
    sprintf(buffer,sk,cname);
    return findText(buffer);
}

       Int visitClass(c)                /* visit class defn to check that  */
Class c; {                              /* class hierarchy is acyclic      */
#if TREX
    if (isExt(c)) {                     /* special case for lacks preds    */
        return 0;
    }
#endif
    if (cclass(c).level < 0) {          /* already visiting this class?    */
        ERRMSG(cclass(c).line) "Class hierarchy for \"%s\" is not acyclic",
                               textToStr(cclass(c).text)
        EEND;
    } else if (cclass(c).level == 0) {  /* visiting class for first time   */
        List scs = cclass(c).supers;
        Int  lev = 0;
        cclass(c).level = (-1);
        for (; nonNull(scs); scs=tl(scs)) {
            Int l = visitClass(getHead(hd(scs)));
            if (l>lev) lev=l;
        }
        cclass(c).level = 1+lev;        /* level = 1 + max level of supers */
    }
    return cclass(c).level;
}

/* --------------------------------------------------------------------------
 * Process class and instance declaration binding groups:
 * ------------------------------------------------------------------------*/

static List local classBindings(where,c,bs)
String where;                           /* Check validity of bindings bs   */
Class  c;                               /* for class c (or an inst of c)   */
List   bs; {                            /* sort into approp. member order  */
    List nbs = NIL;

    for (; nonNull(bs); bs=tl(bs)) {
        Cell b    = hd(bs);
        Cell body = snd(snd(b));
        Name mnm;

        if (!isVar(fst(b))) {           /* Only allow function bindings    */
            ERRMSG(rhsLine(snd(body)))
                "Pattern binding illegal in %s declaration", where
            EEND;
        }

        if (isNull(mnm=memberName(c,textOf(fst(b))))) {
            ERRMSG(rhsLine(snd(hd(body))))
                "No member \"%s\" in class \"%s\"",
                textToStr(textOf(fst(b))), textToStr(cclass(c).text)
            EEND;
        }
        snd(b) = body;
        nbs    = numInsert(mfunOf(mnm)-1,b,nbs);
    }
    return nbs;
}

static Name local memberName(c,t)       /* return name of member function  */
Class c;                                /* with name t in class c          */
Text  t; {                              /* return NIL if not a member      */
    List ms = cclass(c).members;
    for (; nonNull(ms); ms=tl(ms)) {
        if (t==name(hd(ms)).text) {
            return hd(ms);
        }
    }
    return NIL;
}

static List local numInsert(n,x,xs)    /* insert x at nth position in xs,  */
Int  n;                                /* filling gaps with NIL            */
Cell x;
List xs; {
    List start = isNull(xs) ? cons(NIL,NIL) : xs;

    for (xs=start; 0<n--; xs=tl(xs)) {
        if (isNull(tl(xs))) {
            tl(xs) = cons(NIL,NIL);
        }
    }
    hd(xs) = x;
    return start;
}

/* --------------------------------------------------------------------------
 * Calculate set of variables appearing in a given type expression (possibly
 * qualified) as a list of distinct values.  The order in which variables
 * appear in the list is the same as the order in which those variables
 * occur in the type expression when read from left to right.
 * ------------------------------------------------------------------------*/

List local typeVarsIn(ty,us,ws,vs)      /*Calculate list of type variables*/
Cell ty;				/* used in type expression, reading*/
List us;				/* from left to right ignoring any */
List ws;				/* listed in us.		   */
List vs; {				/* ws = explicitly quantified vars */
    if (isNull(ty)) return vs;
    switch (whatIs(ty)) {
        case DICTAP    : return typeVarsIn(snd(snd(ty)),us,ws,vs);
        case UNBOXEDTUP: return typeVarsIn(snd(ty),us,ws,vs);

	case AP        : return typeVarsIn(snd(ty),us,ws,
					   typeVarsIn(fst(ty),us,ws,vs));

	case VARIDCELL :
	case VAROPCELL : if ((nonNull(findBtyvs(textOf(ty)))
			      && !varIsMember(textOf(ty),ws))
			     || varIsMember(textOf(ty),us)) {
			     return vs;
			 } else {
			     return maybeAppendVar(ty,vs);
			 }

	case POLYTYPE  : return typeVarsIn(monotypeOf(ty),polySigOf(ty),ws,vs);

	case QUAL      : {   vs = typeVarsIn(fst(snd(ty)),us,ws,vs);
			     return typeVarsIn(snd(snd(ty)),us,ws,vs);
			 }

	case BANG      : return typeVarsIn(snd(ty),us,ws,vs);

	case LABC      : {   List fs = snd(snd(ty));
			     for (; nonNull(fs); fs=tl(fs)) {
				vs = typeVarsIn(snd(hd(fs)),us,ws,vs);
			     }
 			     return vs;
  			 }
        case TUPLE:
        case TYCON:
        case CONIDCELL:
        case QUALIDENT: return vs;

        default: fprintf(stderr, " bad tag = %d\n", whatIs(ty));internal("typeVarsIn");
    }
    assert(0);
}

static List local maybeAppendVar(v,vs) /* append variable to list if not   */
Cell v;                                /* already included                 */
List vs; {
    Text t = textOf(v);
    List p = NIL;
    List c = vs;

    while (nonNull(c)) {
        if (textOf(hd(c))==t) {
            return vs;
        }
        p = c;
        c = tl(c);
    }

    if (nonNull(p)) {
        tl(p) = cons(v,NIL);
    } else {
        vs    = cons(v,NIL);
    }

    return vs;
}

/* --------------------------------------------------------------------------
 * Static analysis for type expressions is required to:
 *   - ensure that each type constructor or class used has been defined.
 *   - replace type variables by offsets, constructor names by Tycons.
 *   - ensure that the type is well-kinded.
 * ------------------------------------------------------------------------*/

static Type local checkSigType(line,where,e,type)
Int    line;                            /* Check validity of type expr in  */
String where;                           /* explicit type signature         */
Cell   e;
Type   type; {
    List tvs  = NIL;
    List sunk = NIL;
    List xtvs = NIL;

    if (isPolyType(type)) {
	xtvs = fst(snd(type));
	type = monotypeOf(type);
    }
    tvs  = typeVarsIn(type,NIL,xtvs,NIL);
    sunk = unkindTypes;
    checkOptQuantVars(line,xtvs,tvs);

    if (isQualType(type)) {
	map2Over(depPredExp,line,tvs,fst(snd(type)));
	snd(snd(type)) = depTopType(line,tvs,snd(snd(type)));

        if (isAmbiguous(type)) {
            ambigError(line,where,e,type);
        }
    } else {
        type = depTopType(line,tvs,type);
    }

    if (nonNull(tvs)) {
	if (length(tvs) >= (OFF_MAX-OFF_MIN+1)) {
            ERRMSG(line) "Too many type variables in %s\n", where
            EEND;
        } else {
            List ts = tvs;
            for (; nonNull(ts); ts=tl(ts)) {
                hd(ts) = NIL;
            }
            type    = mkPolyType(tvs,type);
        }
    }

    unkindTypes = NIL;
    kindType(line,"type expression",type);
    fixKinds();
    unkindTypes = sunk;

    h98CheckType(line,where,e,type);
    return type;
}

static Void local checkOptQuantVars(line,xtvs,tvs)
Int  line;
List xtvs;				/* Explicitly quantified vars	   */
List tvs; {				/* Implicitly quantified vars	   */
    if (nonNull(xtvs)) {
	List vs = tvs;
	for (; nonNull(vs); vs=tl(vs)) {
	    if (!varIsMember(textOf(hd(vs)),xtvs)) {
		ERRMSG(line) "Quantifier does not mention type variable \"%s\"",
			     textToStr(textOf(hd(vs)))
		EEND;
	    }
	}
	for (vs=xtvs; nonNull(vs); vs=tl(vs)) {
	    if (!varIsMember(textOf(hd(vs)),tvs)) {
		ERRMSG(line) "Quantified type variable \"%s\" is not used",
			     textToStr(textOf(hd(vs)))
		EEND;
	    }
	    if (varIsMember(textOf(hd(vs)),tl(vs))) {
		ERRMSG(line) "Quantified type variable \"%s\" is repeated",
			     textToStr(textOf(hd(vs)))
		EEND;
	    }
	}
    }
}

static Type local depTopType(l,tvs,t)   /* Check top-level of type sig     */
Int  l;
List tvs;
Type t; {
    Type prev = NIL;
    Type t1   = t;
    Int  nr2  = 0;
    Int  i    = 1;
    for (; getHead(t1)==typeArrow && argCount==2; ++i) {
        arg(fun(t1)) = depCompType(l,tvs,arg(fun(t1)));
	if (isPolyOrQualType(arg(fun(t1)))) {
            nr2 = i;
        }
        prev = t1;
        t1   = arg(t1);
    }
    if (nonNull(prev)) {
        arg(prev) = depTypeExp(l,tvs,t1);
    } else {
        t = depTypeExp(l,tvs,t1);
    }
    if (nr2>0) {
        t = ap(RANK2,pair(mkInt(nr2),t));
    }
    return t;
}

static Type local depCompType(l,tvs,t)  /* Check component type for constr */
Int  l;
List tvs;
Type t; {
  Int  ntvs = length(tvs);
  List nfr  = NIL;
  if (isPolyType(t)) {
    List vs  = fst(snd(t));
    t        = monotypeOf(t);
    tvs      = checkQuantVars(l,vs,tvs,t);
    nfr      = replicate(length(vs),NIL);
  }
  if (isQualType(t)) {
    map2Over(depPredExp,l,tvs,fst(snd(t)));
    snd(snd(t)) = depTypeExp(l,tvs,snd(snd(t)));
    if (isAmbiguous(t)) {
      ambigError(l,"type component",NIL,t);
    }
  } else {
    t = depTypeExp(l,tvs,t);
  }
  if (isNull(nfr)) {
    return t;
  }
  take(ntvs,tvs);
  return mkPolyType(nfr,t);
}

static Type local depTypeExp(line,tyvars,type)
Int  line;
List tyvars;
Type type; {
    switch (whatIs(type)) {
        case AP         : fst(type) = depTypeExp(line,tyvars,fst(type));
                          snd(type) = depTypeExp(line,tyvars,snd(type));
                          break;

        case VARIDCELL  : return depTypeVar(line,tyvars,textOf(type));

        case QUALIDENT  : if (isQVar(type)) {
                              ERRMSG(line) "Qualified type variables not allowed"
                              EEND;
                          }
                          /* deliberate fall through */
        case CONIDCELL  : {   Tycon tc = findQualTycon(type);
                              if (isNull(tc)) {
                                  ERRMSG(line)
                                      "Undefined type constructor \"%s\"",
                                      identToStr(type)
                                  EEND;
                              }
                              if (cellIsMember(tc,tyconDefns) &&
                                  !cellIsMember(tc,tcDeps)) {
                                  tcDeps = cons(tc,tcDeps);
                              }
                              return tc;
                          }

#if TREX
        case EXT        : h98DoesntSupport(line,"extensible records");
#endif
        case TYCON      :
        case TUPLE      : break;

        default         : internal("depTypeExp");
    }
    return type;
}

static Type local depTypeVar(line,tyvars,tv)
Int  line;
List tyvars;
Text tv; {
    Int offset = 0;
    Int found  = (-1);

    for (; nonNull(tyvars); offset++) {
	if (tv==textOf(hd(tyvars))) {
	    found = offset;
	}
	tyvars = tl(tyvars);
    }
    if (found<0) {
	Cell vt = findBtyvs(tv);
	if (nonNull(vt)) {
	    return fst(vt);
	}
	ERRMSG(line) "Undefined type variable \"%s\"", textToStr(tv)
	EEND;
    }
    return mkOffset(found);
}

static List local checkQuantVars(line,vs,tvs,body)
Int  line;
List vs;                                /* variables to quantify over      */
List tvs;                               /* variables already in scope      */
Cell body; {                            /* type/constr for scope of vars   */
    if (nonNull(vs)) {
	List bvs = typeVarsIn(body,NIL,NIL,NIL);
        List us  = vs;
        for (; nonNull(us); us=tl(us)) {
            Text u = textOf(hd(us));
            if (varIsMember(u,tl(us))) {
                ERRMSG(line) "Duplicated quantified variable %s",
                             textToStr(u)
                EEND;
            }
#if 0
            if (varIsMember(u,tvs)) {
                ERRMSG(line) "Local quantifier for %s hides an outer use",
                             textToStr(u)
                EEND;
            }
#endif
            if (!varIsMember(u,bvs)) {
                ERRMSG(line) "Locally quantified variable %s is not used",
                             textToStr(u)
                EEND;
            }
        }
        tvs = appendOnto(tvs,vs);
    }
    return tvs;
}

/* --------------------------------------------------------------------------
 * Check for ambiguous types:
 * A type  Preds => type  is ambiguous if not (TV(P) `subset` TV(type))
 * ------------------------------------------------------------------------*/

List offsetTyvarsIn(t,vs)               /* add list of offset tyvars in t  */
Type t;                                 /* to list vs                      */
List vs; {
    switch (whatIs(t)) {
        case AP       : return offsetTyvarsIn(fun(t),
                                offsetTyvarsIn(arg(t),vs));

        case OFFSET   : if (cellIsMember(t,vs))
                            return vs;
                        else
                            return cons(t,vs);

        case QUAL     : return offsetTyvarsIn(snd(t),vs);

        case POLYTYPE : return offsetTyvarsIn(monotypeOf(t),vs);
                        /* slightly inaccurate, but won't matter here      */

        case EXIST    :
        case RANK2    : return offsetTyvarsIn(snd(snd(t)),vs);

        default       : return vs;
    }
}

List zonkTyvarsIn(t,vs)
Type t;
List vs; {
    switch (whatIs(t)) {
	case AP	      : return zonkTyvarsIn(fun(t),
			         zonkTyvarsIn(arg(t),vs));

	case INTCELL  : if (cellIsMember(t,vs))
			    return vs;
			else
			    return cons(t,vs);

	/* this case will lead to a type error --
	   much better than reporting an internal error ;-) */
	/* case OFFSET   : internal("zonkTyvarsIn"); */

	default	      : return vs;
    }
}

static List local otvars(pi,os)		/* os is a list of offsets that	   */
Cell pi;				/* refer to the arguments of pi;   */
List os; {				/* find list of offsets in those   */
    List us = NIL;			/* positions			   */
    for (; nonNull(os); os=tl(os)) {
	us = offsetTyvarsIn(nthArg(offsetOf(hd(os)),pi),us);
    }
    return us;
}

static List local otvarsZonk(pi,os,o)	/* same as above, but zonks	   */
Cell pi;
List os; {
    List us = NIL;
    for (; nonNull(os); os=tl(os)) {
        Type t = zonkType(nthArg(offsetOf(hd(os)),pi),o);
	us = zonkTyvarsIn(t,us);
    }
    return us;
}

static Bool local odiff(us,vs)
List us, vs; {
    while (nonNull(us) && cellIsMember(hd(us),vs)) {
	us = tl(us);
    }
    return us;
}

static Bool local osubset(us,vs)	/* Determine whether us is subset  */
List us, vs; {				/* of vs			   */
    while (nonNull(us) && cellIsMember(hd(us),vs)) {
	us = tl(us);
    }
    return isNull(us);
}

List oclose(fds,vs)	/* Compute closure of vs wrt to fds*/
List fds;
List vs; {
    Bool changed = TRUE;
    while (changed) {
	List fds1 = NIL;
	changed = FALSE;
        while (nonNull(fds)) {
	    Cell fd   = hd(fds);
	    List next = tl(fds);
	    if (osubset(fst(fd),vs)) {	/* Test if fd applies		   */
		List os = snd(fd);
		for (; nonNull(os); os=tl(os)) {
		    if (!cellIsMember(hd(os),vs)) {
			vs      = cons(hd(os),vs);
			changed = TRUE;
		    }
		}
	    } else {			/* Didn't apply this time, so keep */
		tl(fds) = fds1;
		fds1    = fds;
	    }
	    fds = next;
	}
	fds = fds1;
    }
    return vs;
}

Bool isAmbiguous(type)			/* Determine whether type is	   */
Type type; {				/* ambiguous 			   */
    if (isPolyType(type)) {
	type = monotypeOf(type);
    }
    if (isQualType(type)) {		/* only qualified types can be	   */
	List ps   = fst(snd(type));	/* ambiguous			   */
	List tvps = offsetTyvarsIn(ps,NIL);
	List tvts = offsetTyvarsIn(snd(snd(type)),NIL);
    	List fds  = calcFunDeps(ps);

	tvts = oclose(fds,tvts);	/* Close tvts under fds		   */
	return !osubset(tvps,tvts);
    }
    return FALSE;
}

List calcFunDeps(ps)
List ps; {
    List fds  = NIL;
    for (; nonNull(ps); ps=tl(ps)) {/* Calc functional dependencies	   */
	Cell pi = hd(ps);
	Cell c  = getHead(pi);
	if (isClass(c)) {
	    List xfs = cclass(c).xfds;
	    for (; nonNull(xfs); xfs=tl(xfs)) {
		List fs = snd(hd(xfs));
		for (; nonNull(fs); fs=tl(fs)) {
		    fds = cons(pair(otvars(pi,fst(hd(fs))),
				    otvars(pi,snd(hd(fs)))),fds);
		}
  	    }
	}
#if IPARAM
	else if (isIP(c)) {
	    fds = cons(pair(NIL,offsetTyvarsIn(arg(pi),NIL)),fds);
	}
#endif
    }
    return fds;
}

List calcFunDepsPreds(ps)
List ps; {
    List fds  = NIL;
    for (; nonNull(ps); ps=tl(ps)) {/* Calc functional dependencies	   */
	Cell pi3 = hd(ps);
	Cell pi = fst3(pi3);
	Cell c  = getHead(pi);
	Int o = intOf(snd3(pi3));
	if (isClass(c)) {
	    List xfs = cclass(c).xfds;
	    for (; nonNull(xfs); xfs=tl(xfs)) {
		List fs = snd(hd(xfs));
		for (; nonNull(fs); fs=tl(fs)) {
		    fds = cons(pair(otvarsZonk(pi,fst(hd(fs)),o),
				    otvarsZonk(pi,snd(hd(fs)),o)),fds);
		}
	    }
	}
#if IPARAM
	else if (isIP(c)) {
	    fds = cons(pair(NIL,zonkTyvarsIn(arg(pi),NIL)),fds);
	}
#endif
    }
    return fds;
}

Void ambigError(line,where,e,type)      /* produce error message for       */
Int    line;                            /* ambiguity                       */
String where;
Cell   e;
Type   type; {
    ERRMSG(line) "Ambiguous type signature in %s", where ETHEN
    ERRTEXT      "\n*** ambiguous type : " ETHEN ERRTYPE(type);
    if (nonNull(e)) {
        ERRTEXT  "\n*** assigned to    : " ETHEN ERREXPR(e);
    }
    ERRTEXT      "\n"
    EEND;
}

/* --------------------------------------------------------------------------
 * Kind inference for simple types:
 * ------------------------------------------------------------------------*/

static Void local kindConstr(line,alpha,m,c)
Int  line;                              /* Determine kind of constructor   */
Int  alpha;
Int  m;
Cell c; {
    Cell h = getHead(c);
    Int  n = argCount;

#ifdef DEBUG_KINDS
    Printf("kindConstr: alpha=%d, m=%d, c=",alpha,m);
    printType(stdout,c);
    Printf("\n");
#endif

    switch (whatIs(h)) {
        case POLYTYPE : if (n!=0) {
                            internal("kindConstr1");
                        } else {
                            static String pt = "polymorphic type";
                            Type  t  = dropRank1(c,alpha,m);
                            Kinds ks = polySigOf(t);
                            Int   m1 = 0;
                            Int   beta;
                            for (; isAp(ks); ks=tl(ks)) {
                                m1++;
                            }
                            beta        = newKindvars(m1);
                            unkindTypes = cons(pair(mkInt(beta),t),unkindTypes);
                            checkKind(line,beta,m1,monotypeOf(t),NIL,pt,STAR,0);
                        }
                        return;

        case CDICTS   :
        case QUAL     : if (n!=0) {
                            internal("kindConstr2");
                        }
                        map3Proc(kindPred,line,alpha,m,fst(snd(c)));
                        kindConstr(line,alpha,m,snd(snd(c)));
                        return;

        case EXIST    :
        case RANK2    : kindConstr(line,alpha,m,snd(snd(c)));
                        return;

#if TREX
        case EXT      : if (n!=2) {
                            ERRMSG(line)
                                "Illegal use of row in " ETHEN ERRTYPE(c);
                            ERRTEXT "\n"
                            EEND;
                        }
                        break;
#endif

        case TYCON    : if (isSynonym(h) && n<tycon(h).arity) {
                            ERRMSG(line)
                              "Not enough arguments for type synonym \"%s\"",
                              textToStr(tycon(h).text)
                            EEND;
                        }
                        break;
    }

    if (n==0) {                         /* trivial case, no arguments      */
        typeIs = kindAtom(alpha,c);
    } else {                            /* non-trivial application         */
        static String app = "constructor application";
        Cell   a = c;
        Int    i;
        Kind   k;
        Int    beta;

        varKind(n);
        beta   = typeOff;
        k      = typeIs;

        typeIs = kindAtom(alpha,h);     /* h  :: v1 -> ... -> vn -> w      */
        shouldKind(line,h,c,app,k,beta);

        for (i=n; i>0; --i) {           /* ci :: vi for each 1 <- 1..n     */
            checkKind(line,alpha,m,arg(a),c,app,aVar,beta+i-1);
            a = fun(a);
        }
        tyvarType(beta+n);              /* inferred kind is w              */
    }
}

static Kind local kindAtom(alpha,c)     /* Find kind of atomic constructor */
Int  alpha;
Cell c; {
    switch (whatIs(c)) {
        case TUPLE     : return simpleKind(tupleOf(c)); /*(,)::* -> * -> * */
        case OFFSET    : return mkInt(alpha+offsetOf(c));
        case TYCON     : return tycon(c).kind;
        case INTCELL   : return c;
        case VARIDCELL :
        case VAROPCELL : {   Cell vt = findBtyvs(textOf(c));
                             if (nonNull(vt)) {
                                 return snd(vt);
                             }
                         }
#if TREX
        case EXT       : return extKind;
#endif
    }
#if DEBUG_KINDS
    Printf("kindAtom(%d,whatIs(%d)) on ",alpha,whatIs(c));
    printType(stdout,c);
    Printf("\n");
#endif
    internal("kindAtom");
    return STAR;/* not reached */
}

static Void local kindPred(l,alpha,m,pi)/* Check kinds of arguments in pred*/
Int  l;
Int  alpha;
Int  m;
Cell pi; {
#if TREX
    if (isAp(pi) && isExt(fun(pi))) {
        static String lackspred = "lacks predicate";
        checkKind(l,alpha,m,arg(pi),NIL,lackspred,ROW,0);
        return;
    }
#endif
#if IPARAM
    if (isAp(pi) && whatIs(fun(pi)) == IPCELL) {
	static String ippred = "iparam predicate";
	checkKind(l,alpha,m,arg(pi),NIL,ippred,STAR,0);
	return;
    }
#endif
    {   static String predicate = "class constraint";
        Class c  = getHead(pi);
        List  as = getArgs(pi);
        Kinds ks = cclass(c).kinds;

        while (nonNull(ks)) {
            checkKind(l,alpha,m,hd(as),NIL,predicate,hd(ks),0);
            ks = tl(ks);
            as = tl(as);
        }
    }
}

static Void local kindType(line,wh,type)/* check that (poss qualified) type*/
Int    line;                            /* is well-kinded                  */
String wh;
Type   type; {
    checkKind(line,0,0,type,NIL,wh,STAR,0);
}

static Void local fixKinds() {          /* add kind annotations to types   */
    for (; nonNull(unkindTypes); unkindTypes=tl(unkindTypes)) {
        Pair pr   = hd(unkindTypes);
        Int  beta = intOf(fst(pr));
        Cell qts  = polySigOf(snd(pr));
        for (;;) {
            if (isNull(hd(qts))) {
                hd(qts) = copyKindvar(beta++);
            } else {
                internal("fixKinds");
            }
            if (nonNull(tl(qts))) {
                qts = tl(qts);
            } else {
                tl(qts) = STAR;
                break;
            }
        }
#ifdef DEBUG_KINDS
        Printf("Type expression: ");
        printType(stdout,snd(pr));
        Printf(" :: ");
        printKind(stdout,polySigOf(snd(pr)));
        Printf("\n");
#endif
    }
}

/* --------------------------------------------------------------------------
 * Kind checking of groups of type constructors and classes:
 * ------------------------------------------------------------------------*/

static Void local kindTCGroup(tcs)      /* find kinds for mutually rec. gp */
List tcs; {                             /* of tycons and classes           */
    emptySubstitution();
    unkindTypes = NIL;
    mapProc(initTCKind,tcs);
    mapProc(kindTC,tcs);
    mapProc(genTC,tcs);
    fixKinds();
    emptySubstitution();
}
    
static Void local initTCKind(c)         /* build initial kind/arity for c  */
Cell c; {
    if (isTycon(c)) {                   /* Initial kind of tycon is:       */
        Int beta = newKindvars(1);      /*    v1 -> ... -> vn -> vn+1      */
        varKind(tycon(c).arity);        /* where n is the arity of c.      */
        bindTv(beta,typeIs,typeOff);    /* For data definitions, vn+1 == * */
        switch (whatIs(tycon(c).what)) {
            case NEWTYPE  :
            case DATATYPE : bindTv(typeOff+tycon(c).arity,STAR,0);
        }
        tycon(c).kind = mkInt(beta);
    } else {
        Int n    = cclass(c).arity;
        Int beta = newKindvars(n);
        cclass(c).kinds = NIL;
	while (n>0) {
            n--;
            cclass(c).kinds = pair(mkInt(beta+n),cclass(c).kinds);
        }
    }
}

static Void local kindTC(c)             /* check each part of a tycon/class*/
Cell c; {                               /* is well-kinded                  */
    if (isTycon(c)) {
        static String cfun = "constructor function";
        static String tsyn = "synonym definition";
        Int line = tycon(c).line;
        Int beta = tyvar(intOf(tycon(c).kind))->offs;
        Int m    = tycon(c).arity;
        switch (whatIs(tycon(c).what)) {
            case NEWTYPE     :
            case DATATYPE    : {   List cs = tycon(c).defn;
				   if (isQualType(cs)) {
                                       map3Proc(kindPred,line,beta,m,
                                                                fst(snd(cs)));
                                       tycon(c).defn = cs = snd(snd(cs));
                                   }
                                   for (; hasCfun(cs); cs=tl(cs)) {
                                       kindType(line,cfun,name(hd(cs)).type);
                                   }
                                   break;
                               }

            default          : checkKind(line,beta,m,tycon(c).defn,NIL,
                                                        tsyn,aVar,beta+m);
        }
    }
    else {                              /* scan type exprs in class defn to*/
        List ms   = fst(cclass(c).members);
        Int  m    = cclass(c).arity;    /* determine the class signature   */
        Int  beta = newKindvars(m);
        kindPred(cclass(c).line,beta,m,cclass(c).head);
        map3Proc(kindPred,cclass(c).line,beta,m,cclass(c).supers);
        for (; nonNull(ms); ms=tl(ms)) {
            Int  line = intOf(fst3(hd(ms)));
            Type type = thd3(hd(ms));
            kindType(line,"member function type signature",type);
        }
    }
}

static Void local genTC(c)              /* generalise kind inferred for    */
Cell c; {                               /* given tycon/class               */
    if (isTycon(c)) {
        tycon(c).kind = copyKindvar(intOf(tycon(c).kind));
#ifdef DEBUG_KINDS
        Printf("%s :: ",textToStr(tycon(c).text));
        printKind(stdout,tycon(c).kind);
        Putchar('\n');
#endif
    } else {
        Kinds ks = cclass(c).kinds;
        for (; nonNull(ks); ks=tl(ks)) {
            hd(ks) = copyKindvar(intOf(hd(ks)));
        }
#ifdef DEBUG_KINDS
        Printf("%s :: ",textToStr(cclass(c).text));
        printKinds(stdout,cclass(c).kinds);
        Putchar('\n');
#endif
    }
}

/* --------------------------------------------------------------------------
 * Static analysis of instance declarations:
 *
 * The first part of the static analysis is performed as the declarations
 * are read during parsing:
 * - make new entry in instance table
 * - record line number of declaration
 * - build list of instances defined in current script for use in later
 *   stages of static analysis.
 * ------------------------------------------------------------------------*/

Void instDefn(line,head,ms)            /* process new instance definition  */
Int  line;                             /* definition line number           */
Cell head;                             /* inst header :: (context,Class)   */
List ms; {                             /* instance members                 */
    Inst nw             = newInst();
    inst(nw).line       = line;
    inst(nw).specifics  = fst(head);
    inst(nw).head       = snd(head);
    inst(nw).implements = ms;
    instDefns           = cons(nw,instDefns);
}

/* --------------------------------------------------------------------------
 * Further static analysis of instance declarations:
 *
 * Makes the following checks:
 * - Class part of header has form C (T a1 ... an) where C is a known
 *   class, and T is a known datatype constructor (or restricted synonym),
 *   and there is no previous C-T instance, and (T a1 ... an) has a kind
 *   appropriate for the class C.
 * - Each element of context is a valid class expression, with type vars
 *   drawn from a1, ..., an.
 * - All bindings are function bindings
 * - All bindings define member functions for class C
 * - Arrange bindings into appropriate order for member list
 * - No top level type signature declarations
 * ------------------------------------------------------------------------*/

Bool allowOverlap = FALSE;              /* TRUE => allow overlapping insts */
Name nameListMonad = NIL;               /* builder function for List Monad */

static Void local checkInstDefn(in)     /* Validate instance declaration   */
Inst in; {
    Int  line   = inst(in).line;
    List tyvars = typeVarsIn(inst(in).head,NIL,NIL,NIL);
    List tvps = NIL, tvts = NIL;
    List fds = NIL;

    if (haskell98) {                    /* Check for `simple' type         */
        List tvs = NIL;
        Cell t   = arg(inst(in).head);
        for (; isAp(t); t=fun(t)) {
            if (!isVar(arg(t))) {
                ERRMSG(line)
                   "syntax error in instance head (variable expected)"
                EEND;
            }
            if (varIsMember(textOf(arg(t)),tvs)) {
                ERRMSG(line) "repeated type variable \"%s\" in instance head",
                             textToStr(textOf(arg(t)))
                EEND;
            }
            tvs = cons(arg(t),tvs);
        }
        if (isVar(t)) {
            ERRMSG(line)
                "syntax error in instance head (constructor expected)"
            EEND;
        }
    }

    /* add in the tyvars from the `specifics' so that we don't
       prematurely complain about undefined tyvars */
    tyvars = typeVarsIn(inst(in).specifics,NIL,NIL,tyvars);
    inst(in).head = depPredExp(line,tyvars,inst(in).head);

    if (haskell98) {
        Type h = getHead(arg(inst(in).head));
        if (isSynonym(h)) {
            ERRMSG(line) "Cannot use type synonym in instance head"
            EEND;
        }
    }

    map2Over(depPredExp,line,tyvars,inst(in).specifics);

    /* OK, now we start over, and test for ambiguity */
    tvts = offsetTyvarsIn(inst(in).head,NIL);
    tvps = offsetTyvarsIn(inst(in).specifics,NIL);
    fds  = calcFunDeps(inst(in).specifics);
    tvts = oclose(fds,tvts);
    tvts = odiff(tvps,tvts);
    if (!isNull(tvts)) {
	ERRMSG(line) "Undefined type variable \"%s\"",
	  textToStr(textOf(nth(offsetOf(hd(tvts)),tyvars)))
	EEND;
    }

    h98CheckCtxt(line,"instance definition",FALSE,inst(in).specifics,NIL);
    inst(in).numSpecifics = length(inst(in).specifics);
    inst(in).c            = getHead(inst(in).head);
    if (!isClass(inst(in).c)) {
        ERRMSG(line) "Illegal predicate in instance declaration"
        EEND;
    }

    if (nonNull(cclass(inst(in).c).fds)) {
        List fds = cclass(inst(in).c).fds;
        for (; nonNull(fds); fds=tl(fds)) {
            List as = otvars(inst(in).head, fst(hd(fds)));
            List bs = otvars(inst(in).head, snd(hd(fds)));
	    List fs = calcFunDeps(inst(in).specifics);
	    as = oclose(fs,as);
            if (!osubset(bs,as)) {
		ERRMSG(inst(in).line)
		   "Instance is more general than a dependency allows"
		ETHEN
		ERRTEXT "\n*** Instance         : "
		ETHEN ERRPRED(inst(in).head);
		ERRTEXT "\n*** For class        : "
		ETHEN ERRPRED(cclass(inst(in).c).head);
		ERRTEXT "\n*** Under dependency : "
		ETHEN ERRFD(hd(fds));
		ERRTEXT "\n"
		EEND;
            }
        }
    }

    kindInst(in,length(tyvars));
    insertInst(in);

    if (nonNull(extractSigdecls(inst(in).implements))) {
        ERRMSG(line)
          "Type signature declarations not permitted in instance declaration"
        EEND;
    }
    if (nonNull(extractFixdecls(inst(in).implements))) {
        ERRMSG(line)
          "Fixity declarations not permitted in instance declaration"
        EEND;
    }
    inst(in).implements = classBindings("instance",
                                        inst(in).c,
                                        extractBindings(inst(in).implements));
    inst(in).builder    = newInstImp(in);
    if (!preludeLoaded && isNull(nameListMonad) && isAp(inst(in).head)
        && fun(inst(in).head)==classMonad && arg(inst(in).head)==typeList) {
        nameListMonad = inst(in).builder;
    }
}

static Void local insertInst(in)        /* Insert instance into class      */
Inst in; {
    Class c    = inst(in).c;
    List  ins  = cclass(c).instances;
    List  prev = NIL;

    if (nonNull(cclass(c).fds)) {	/* Check for conflicts with fds	   */
	List ins1 = cclass(c).instances;
	for (; nonNull(ins1); ins1=tl(ins1)) {
	    List fds = cclass(c).fds;
	    substitution(RESET);
	    for (; nonNull(fds); fds=tl(fds)) {
		Int  alpha = newKindedVars(inst(in).kinds);
		Int  beta  = newKindedVars(inst(hd(ins1)).kinds);
		List as    = fst(hd(fds));
		Bool same  = TRUE;
		for (; same && nonNull(as); as=tl(as)) {
		    Int n = offsetOf(hd(as));
		    same &= unify(nthArg(n,inst(in).head),alpha,
				  nthArg(n,inst(hd(ins1)).head),beta);
		}
		if (isNull(as) && same) {
		    for (as=snd(hd(fds)); same && nonNull(as); as=tl(as)) {
			Int n = offsetOf(hd(as));
			same &= sameType(nthArg(n,inst(in).head),alpha,
					 nthArg(n,inst(hd(ins1)).head),beta);
		    }
		    if (!same) {
			ERRMSG(inst(in).line)
			   "Instances are not consistent with dependencies"
			ETHEN
			ERRTEXT "\n*** This instance    : "
			ETHEN ERRPRED(inst(in).head);
			ERRTEXT "\n*** Conflicts with   : "
			ETHEN ERRPRED(inst(hd(ins)).head);
			ERRTEXT "\n*** For class        : "
			ETHEN ERRPRED(cclass(c).head);
			ERRTEXT "\n*** Under dependency : "
			ETHEN ERRFD(hd(fds));
			ERRTEXT "\n"
			EEND;
		    }
		}
	    }
	}
    }


    substitution(RESET);
    while (nonNull(ins)) {              /* Look for overlap w/ other insts */
        Int alpha = newKindedVars(inst(in).kinds);
        Int beta  = newKindedVars(inst(hd(ins)).kinds);
        if (unifyPred(inst(in).head,alpha,inst(hd(ins)).head,beta)) {
            Cell pi  = copyPred(inst(in).head,alpha);
            if (allowOverlap && !haskell98) {
                Bool bef = instCompare(in,hd(ins));
                Bool aft = instCompare(hd(ins),in);
                if (bef && !aft) {      /* in comes strictly before hd(ins)*/
                    break;
                }
                if (aft && !bef) {      /* in comes strictly after hd(ins) */
                    prev = ins;
                    ins  = tl(ins);
                    continue;
                }
            }
#if MULTI_INST
	    if (multiInstRes && nonNull(inst(in).specifics)) {
		break;
	    } else {
#endif
            ERRMSG(inst(in).line) "Overlapping instances for class \"%s\"",
                                  textToStr(cclass(c).text)
            ETHEN
            ERRTEXT "\n*** This instance   : " ETHEN ERRPRED(inst(in).head);
            ERRTEXT "\n*** Overlaps with   : " ETHEN
                                               ERRPRED(inst(hd(ins)).head);
            ERRTEXT "\n*** Common instance : " ETHEN
                                               ERRPRED(pi);
            ERRTEXT "\n"
            EEND;
        }
#if MULTI_INST
	    }
#endif
        prev = ins;                     /* No overlap detected, so move on */
        ins  = tl(ins);                 /* to next instance                */
    }
    substitution(RESET);

    if (nonNull(prev)) {                /* Insert instance at this point   */
        tl(prev) = cons(in,ins);
    } else {
        cclass(c).instances = cons(in,ins);
    }
}

static Bool local instCompare(ia,ib)    /* See if ia is an instance of ib  */
Inst ia, ib;{
    Int alpha = newKindedVars(inst(ia).kinds);
    Int beta  = newKindedVars(inst(ib).kinds);
    return matchPred(inst(ia).head,alpha,inst(ib).head,beta);
}

static Name local newInstImp(in)        /* Make definition for inst builder*/
Inst in; {
    Name b         = newName(inventText(),in);
    name(b).line   = inst(in).line;
    name(b).arity  = inst(in).numSpecifics;
    name(b).number = DFUNNAME;
    return b;
}

/* --------------------------------------------------------------------------
 * Kind checking of instance declaration headers:
 * ------------------------------------------------------------------------*/

static Void local kindInst(in,freedom)  /* check predicates in instance    */
Inst in;
Int  freedom; {
    Int beta;

    emptySubstitution();
    beta = newKindvars(freedom);
    kindPred(inst(in).line,beta,freedom,inst(in).head);
    if (whatIs(inst(in).specifics)!=DERIVE) {
        map3Proc(kindPred,inst(in).line,beta,freedom,inst(in).specifics);
    }
    for (inst(in).kinds = NIL; 0<freedom--; ) {
        inst(in).kinds = cons(copyKindvar(beta+freedom),inst(in).kinds);
    }
#ifdef DEBUG_KINDS
    Printf("instance ");
    printPred(stdout,inst(in).head);
    Printf(" :: ");
    printKinds(stdout,inst(in).kinds);
    Putchar('\n');
#endif
    emptySubstitution();
}

/* --------------------------------------------------------------------------
 * Process derived instance requests:
 * ------------------------------------------------------------------------*/

static List derivedInsts;               /* list of derived instances       */

static Void local checkDerive(t,p,ts,ct)/* verify derived instance request */
Tycon t;                                /* for tycon t, with explicit      */
List  p;                                /* context p, component types ts   */
List  ts;                               /* and named class ct              */
Cell  ct; {
    Int   line = tycon(t).line;
    Class c    = findQualClass(ct);
    if (isNull(c)) {
        ERRMSG(line) "Unknown class \"%s\" in derived instance",
 		     identToStr(ct)
        EEND;
    }
    addDerInst(line,c,p,dupList(ts),t,tycon(t).arity);
}

static Void local addDerInst(line,c,p,cts,t,a)  /* Add a derived instance  */
Int   line;
Class c;
List  p, cts;
Type  t;
Int   a; {
    Inst in;
    Cell head = t;                              /* Build instance head     */
    Int  i    = 0;

    for (; i<a; i++) {
        head = ap(head,mkOffset(i));
    }
    head = ap(c,head);

    in                  = newInst();
    inst(in).c          = c;
    inst(in).line       = line;
    inst(in).head       = head;
    inst(in).specifics  = ap(DERIVE,pair(dupList(p),cts));
    inst(in).implements = NIL;
    inst(in).kinds      = mkInt(a);
    derivedInsts        = cons(in,derivedInsts);
}

Void addTupInst(c,n)                    /* Request derived instance of c   */
Class c;                                /* for mkTuple(n) constructor      */
Int   n; {
    Int  m   = n;
    List cts = NIL;
    while (0<m--) {
        cts = cons(mkOffset(m),cts);
    }
    cts = rev(cts);
    addDerInst(0,c,NIL,cts,mkTuple(n),n);
}

#if TREX
Inst addRecShowInst(c,e)                /* Generate instance for ShowRecRow*/
Class c;                                /* c *must* be ShowRecRow          */
Ext   e; {
    Inst in               = newInst();
    inst(in).c            = c;
    inst(in).head         = ap(c,ap2(e,aVar,bVar));
    inst(in).kinds        = extKind;
    inst(in).specifics    = cons(ap(classShow,aVar),
                                 cons(ap(e,bVar),
                                      cons(ap(c,bVar),NIL)));
    inst(in).numSpecifics = 3;
    inst(in).builder      = implementRecShw(extText(e),in);
    cclass(c).instances   = appendOnto(cclass(c).instances,singleton(in));
    return in;
}

Inst addRecEqInst(c,e)                  /* Generate instance for EqRecRow  */
Class c;                                /* c *must* be EqRecRow            */
Ext   e; {
    Inst in               = newInst();
    inst(in).c            = c;
    inst(in).head         = ap(c,ap2(e,aVar,bVar));
    inst(in).kinds        = extKind;
    inst(in).specifics    = cons(ap(classEq,aVar),
                                 cons(ap(e,bVar),
                                      cons(ap(c,bVar),NIL)));
    inst(in).numSpecifics = 3;
    inst(in).builder      = implementRecEq(extText(e),in);
    cclass(c).instances   = appendOnto(cclass(c).instances,singleton(in));
    return in;
}
#endif

/* --------------------------------------------------------------------------
 * Calculation of contexts for derived instances:
 *
 * Allowing arbitrary types to appear in contexts makes it rather harder
 * to decide what the context for a derived instance should be.  For
 * example, given:
 *
 *    data T a = MkT [a] deriving Show,
 *
 * we could have either of the following:
 *
 *    instance (Show [a]) => Show (T a) where ...
 *    instance (Show a) => Show (T a) where ...
 *
 * (assuming, of course, that instance (Show a) => Show [a]).  For now, we
 * choose to reduce contexts in the hope of detecting errors at an earlier
 * stage---in contrast with value definitions, there is no way for a user
 * to provide something analogous to a `type signature' by which they might
 * be able to control this behaviour themselves.  We eliminate tautological
 * predicates, but only allow predicates to appear in the final result if
 * they have at least one argument with a variable at its head.
 *
 * In general, we have to deal with mutually recursive instance declarations.
 * We find a solution in the obvious way by iterating to find a fixed point.
 * Of course, without restrictions on the form of instance declarations, we
 * cannot be sure that this will always terminate!
 *
 * For each instance we maintain a pair of the form DERIVE (ctxt,ps).
 * Ctxt is a list giving the parts of the context that have been produced
 * so far in the form of predicate skeletons.  During the calculation of
 * derived instances, we attach a dummy NIL value to the end of the list
 * which acts as a kind of `variable': other parts of the system maintain
 * pointers to this variable, and use it to detect when the context has
 * been extended with new elements.  Meanwhile, ps is a list containing
 * predicates (pi,o) together with (delayed) substitutions of the form
 * (o,xs) where o is an offset and xs is one of the context variables
 * described above, which may have been partially instantiated.
 * ------------------------------------------------------------------------*/

static Bool instsChanged;

static Void local deriveContexts(is)    /* Calc contexts for derived insts */
List is; {
    emptySubstitution();
    mapProc(initDerInst,is);            /* Prepare derived instances       */

    do {                                /* Main calculation of contexts    */
        instsChanged = FALSE;
        mapProc(calcInstPreds,is);
    } while (instsChanged);

    mapProc(tidyDerInst,is);            /* Tidy up results                 */
}

static Void local initDerInst(in)       /* Prepare instance for calculation*/
Inst in; {                              /* of derived instance context     */
    Cell spcs = inst(in).specifics;
    Int  beta = newKindedVars(inst(in).kinds);
    if (whatIs(spcs)!=DERIVE) {
        internal("initDerInst");
    }
    fst(snd(spcs)) = appendOnto(fst(snd(spcs)),singleton(NIL));
    for (spcs=snd(snd(spcs)); nonNull(spcs); spcs=tl(spcs)) {
        hd(spcs) = ap2(inst(in).c,hd(spcs),mkInt(beta));
    }
    inst(in).numSpecifics = beta;

#ifdef DEBUG_DERIVING
    Printf("initDerInst: ");
    printPred(stdout,inst(in).head);
    Printf("\n");
    printContext(stdout,snd(snd(inst(in).specifics)));
    Printf("\n");
#endif
}

static Void local calcInstPreds(in)     /* Calculate next approximation    */
Inst in; {                              /* of the context for a derived    */
    List retain = NIL;                  /* instance                        */
    List ps     = snd(snd(inst(in).specifics));
    List spcs   = fst(snd(inst(in).specifics));
    Int  beta   = inst(in).numSpecifics;
    Int  its    = 1;
    Int  factor = 1+length(ps);

#ifdef DEBUG_DERIVING
    Printf("calcInstPreds: ");
    printPred(stdout,inst(in).head);
    Printf("\n");
#endif

    while (nonNull(ps)) {
        Cell p = hd(ps);
        ps     = tl(ps);
	if (its++ >= factor*cutoff) {
	    Cell bpi = inst(in).head;
	    ERRMSG(inst(in).line) "\n*** Cannot derive " ETHEN ERRPRED(bpi);
	    ERRTEXT " after %d iterations.", its-1   ETHEN
	    ERRTEXT
		"\n*** This may indicate that the problem is undecidable.  However,\n"
	    ETHEN ERRTEXT
		"*** you may still try to increase the cutoff limit using the -c\n"
	    ETHEN ERRTEXT
		"*** option and then try again.  (The current setting is -c%d)\n",
		cutoff
	    EEND;
	}
        if (isInt(fst(p))) {                    /* Delayed substitution?   */
            List qs = snd(p);
            for (; nonNull(hd(qs)); qs=tl(qs)) {
                ps = cons(pair(hd(qs),fst(p)),ps);
            }
            retain = cons(pair(fst(p),qs),retain);
        }
#if TREX
        else if (isExt(fun(fst(p)))) {          /* Lacks predicate         */
            Text   l = extText(fun(fst(p)));
            Type   t = arg(fst(p));
            Int    o = intOf(snd(p));
            Type   h;
            Tyvar *tyv;

            deRef(tyv,t,o);
            h = getDerefHead(t,o);
            while (isExt(h) && argCount==2 && l!=extText(h)) {
                t = arg(t);
                deRef(tyv,t,o);
                h = getDerefHead(t,o);
            }
            if (argCount==0 && isOffset(h)) {
                maybeAddPred(ap(fun(fun(p)),h),o,beta,spcs);
            } else if (argCount!=0 || h!=typeNoRow) {
                Cell bpi = inst(in).head;
                Cell pi  = copyPred(fun(p),intOf(snd(p)));
                ERRMSG(inst(in).line) "Cannot derive " ETHEN ERRPRED(bpi);
                ERRTEXT " because predicate " ETHEN ERRPRED(pi);
                ERRTEXT " does not hold\n"
                EEND;
            }
        }
#endif
        else {                                  /* Class predicate         */
            Cell pi  = fst(p);
            Int  o   = intOf(snd(p));
            Inst in1 = findInstFor(pi,o);
            if (nonNull(in1)) {
                List qs  = inst(in1).specifics;
                Int  off = mkInt(typeOff);
                if (whatIs(qs)==DERIVE) {       /* Still being derived     */
                    for (qs=fst(snd(qs)); nonNull(hd(qs)); qs=tl(qs)) {
                        ps = cons(pair(hd(qs),off),ps);
                    }
                    retain = cons(pair(off,qs),retain);
                } else {                        /* Previously def'd inst   */
                    for (; nonNull(qs); qs=tl(qs)) {
                        ps = cons(pair(hd(qs),off),ps);
                    }
                }
            } else {                            /* No matching instance    */
                Cell qi = pi;
                while (isAp(qi) && isOffset(getDerefHead(arg(qi),o))) {
                    qi = fun(qi);
                }
                if (isAp(qi)) {
                    Cell bpi = inst(in).head;
                    pi       = copyPred(pi,o);
                    ERRMSG(inst(in).line) "An instance of " ETHEN ERRPRED(pi);
                    ERRTEXT " is required to derive "       ETHEN ERRPRED(bpi);
                    ERRTEXT "\n"
                    EEND;
                } else {
                    maybeAddPred(pi,o,beta,spcs);
                }
            }
        }
    }
    snd(snd(inst(in).specifics)) = retain;
}

static Void local maybeAddPred(pi,o,beta,ps)
Cell pi;                                /* Add predicate pi to the list ps,*/
Int  o;                                 /* setting the instsChanged flag if*/
Int  beta;                              /* pi is not already a member and  */
List ps; {                              /* using beta to adjust vars       */
    Cell c = getHead(pi);
    for (; nonNull(ps); ps=tl(ps)) {
        if (isNull(hd(ps))) {           /* reached the `dummy' end of list?*/
            hd(ps)       = copyAdj(pi,o,beta);
            tl(ps)       = pair(NIL,NIL);
            instsChanged = TRUE;
            return;
        } else if (c==getHead(hd(ps)) && samePred(pi,o,hd(ps),beta)) {
            return;
        }
    }
}

static Cell local copyAdj(c,o,beta)     /* Copy (c,o), replacing vars with */
Cell c;                                 /* offsets relative to beta.       */
Int  o;
Int  beta; {
    switch (whatIs(c)) {
        case AP     : {   Cell l = copyAdj(fst(c),o,beta);
                          Cell r = copyAdj(snd(c),o,beta);
                          return ap(l,r);
                      }

        case OFFSET : {   Int   vn   = o+offsetOf(c);
                          Tyvar *tyv = tyvar(vn);
                          if (isBound(tyv)) {
                              return copyAdj(tyv->bound,tyv->offs,beta);
                          }
                          vn -= beta;
                          if (vn<0 || vn>=(OFF_MAX-OFF_MIN+1)) {
                              internal("copyAdj");
                          }
                          return mkOffset(vn);
                      }
    }
    return c;
}

static Void local tidyDerInst(in)       /* Tidy up results of derived inst */
Inst in; {                              /* calculations                    */
    Int  o  = inst(in).numSpecifics;
    List ps = tl(rev(fst(snd(inst(in).specifics))));
    clearMarks();
    copyPred(inst(in).head,o);
    inst(in).specifics    = simpleContext(ps,o);
    h98CheckCtxt(inst(in).line,"derived instance",FALSE,inst(in).specifics,in);
    inst(in).numSpecifics = length(inst(in).specifics);

#ifdef DEBUG_DERIVING
    Printf("Derived instance: ");
    printContext(stdout,inst(in).specifics);
    Printf(" ||- ");
    printPred(stdout,inst(in).head);
    Printf("\n");
#endif
}

/* --------------------------------------------------------------------------
 * Generate code for derived instances:
 * ------------------------------------------------------------------------*/

static Void local addDerivImp(in)
Inst in; {
    List  imp = NIL;
    Type  t   = getHead(arg(inst(in).head));
    Class c   = inst(in).c;
    if (c==classEq) {
        imp = deriveEq(t);
    } else if (c==classOrd) {
        imp = deriveOrd(t);
    } else if (c==classEnum) {
        imp = deriveEnum(t);
    } else if (c==classIx) {
        imp = deriveIx(t);
    } else if (c==classShow) {
        imp = deriveShow(t);
    } else if (c==classRead) {
        imp = deriveRead(t);
    } else if (c==classBounded) {
        imp = deriveBounded(t);
    } else {
        ERRMSG(inst(in).line) "Cannot derive instances of class \"%s\"",
                              textToStr(cclass(inst(in).c).text)
        EEND;
    }

    kindInst(in,intOf(inst(in).kinds));
    insertInst(in);
    inst(in).builder    = newInstImp(in);
    inst(in).implements = classBindings("derived instance",
                                        inst(in).c,
                                        imp);
}


/* --------------------------------------------------------------------------
 * Default definitions; only one default definition is permitted in a
 * given script file.  If no default is supplied, then a standard system
 * default will be used where necessary.
 * ------------------------------------------------------------------------*/

Void defaultDefn(line,defs)             /* Handle default types definition */
Int  line;
List defs; {
    if (defaultLine!=0) {
        ERRMSG(line) "Multiple default declarations are not permitted in" ETHEN
        ERRTEXT     "a single script file.\n"
        EEND;
    }
    defaultDefns = defs;
    defaultLine  = line;
}

static Void local checkDefaultDefns() { /* check that default types are    */
    List ds = NIL;                      /* well-kinded instances of Num    */

    if (defaultLine!=0) {
        map2Over(depTypeExp,defaultLine,NIL,defaultDefns);
        emptySubstitution();
        unkindTypes = NIL;
        map2Proc(kindType,defaultLine,"default type",defaultDefns);
        fixKinds();
        emptySubstitution();
        mapOver(fullExpand,defaultDefns);
    } else {
        defaultDefns = stdDefaults;
    }

    if (isNull(classNum)) {
        classNum = findClass(findText("Num"));
    }

    for (ds=defaultDefns; nonNull(ds); ds=tl(ds)) {
        if (isNull(provePred(NIL,NIL,ap(classNum,hd(ds))))) {
            ERRMSG(defaultLine)
                "Default types must be instances of the Num class"
            EEND;
        }
    }
}


/* --------------------------------------------------------------------------
 * Foreign import declarations are Hugs' equivalent of GHC's ccall mechanism.
 * They are used to "import" C functions into a module.
 * They are usually not written by hand but, rather, generated automatically
 * by GreenCard, IDL compilers or whatever.  We support foreign import 
 * (static) and foreign import dynamic.  In the latter case, extName==NIL.
 *
 * Foreign export declarations generate C wrappers for Hugs functions.
 * Hugs only provides "foreign export dynamic" because it's not obvious
 * what "foreign export static" would mean in an interactive setting.
 * ------------------------------------------------------------------------*/

Void foreignImport(line,callconv,extName,intName,type) 
                                              /* Handle foreign imports    */
Cell line;
Text callconv;
Pair extName;
Cell intName;
Cell type; {
    Text t = textOf(intName);
    Name n = findName(t);
    Int  l = intOf(line);

    if (isNull(n)) {
        n = newName(t,NIL);
    } else if (name(n).defn!=PREDEFINED) {
        ERRMSG(l) "Redeclaration of foreign \"%s\"", textToStr(t)
        EEND;
    }
    name(n).line     = l;
    name(n).defn     = extName;
    name(n).type     = type;
    name(n).callconv = callconv;
    foreignImports   = cons(n,foreignImports);
}

static Void local checkForeignImport(p)   /* Check foreign import          */
Name p; {
    emptySubstitution();
    name(p).type = checkSigType(name(p).line,
                                "foreign import declaration",
                                p,
                                name(p).type);
    /* We don't expand synonyms here because we don't want the IO
     * part to be expanded.
     * name(p).type = fullExpand(name(p).type);
     */
    implementForeignImport(p);
}

Void foreignExport(line,callconv,extName,intName,type)
                                              /* Handle foreign exports    */
Cell line;
Text callconv;
Cell extName;
Cell intName;
Cell type; {
    Text t = textOf(intName);
    Name n = findName(t);
    Int  l = intOf(line);

    if (isNull(n)) {
        n = newName(t,NIL);
    } else if (name(n).defn!=PREDEFINED) {
        ERRMSG(l) "Redeclaration of foreign \"%s\"", textToStr(t)
        EEND;
    }
    name(n).line     = l;
    name(n).defn     = NIL;  /* nothing to say */
    name(n).type     = type;
    name(n).callconv = callconv;
    foreignExports   = cons(n,foreignExports);
}

static Void local checkForeignExport(p)       /* Check foreign export      */
Name p; {
    emptySubstitution();
    name(p).type = checkSigType(name(p).line,
                                "foreign export declaration",
                                p,
                                name(p).type);
    implementForeignExport(p);
}



/* --------------------------------------------------------------------------
 * Static analysis of patterns:
 *
 * Patterns are parsed as ordinary (atomic) expressions.  Static analysis
 * makes the following checks:
 *  - Patterns are well formed (according to pattern syntax), including the
 *    special case of (n+k) patterns.
 *  - All constructor functions have been defined and are used with the
 *    correct number of arguments.
 *  - No variable name is used more than once in a pattern.
 *
 * The list of pattern variables occuring in each pattern is accumulated in
 * a global list `patVars', which must be initialised to NIL at appropriate
 * points before using these routines to check for valid patterns.  This
 * mechanism enables the pattern checking routine to be mapped over a list
 * of patterns, ensuring that no variable occurs more than once in the
 * complete pattern list (as is required on the lhs of a function defn).
 * ------------------------------------------------------------------------*/

static List patVars;                   /* List of vars bound in pattern    */

static Cell local checkPat(line,p)     /* Check valid pattern syntax       */
Int  line;
Cell p; {
    switch (whatIs(p)) {
        case VARIDCELL :
        case VAROPCELL : addToPatVars(line,p);
                         break;

        case INFIX     : return checkPat(line,tidyInfix(line,snd(p)));

        case AP        : return checkMaybeCnkPat(line,p);

        case NAME      :
        case QUALIDENT : 
        case CONIDCELL : 
        case CONOPCELL : return checkApPat(line,0,p);

        case WILDCARD  :
        case STRCELL   :
        case CHARCELL  :
        case FLOATCELL : break;
        case INTCELL   : break;

        case ASPAT     : addToPatVars(line,fst(snd(p)));
                         snd(snd(p)) = checkPat(line,snd(snd(p)));
                         break;

        case LAZYPAT   : snd(p) = checkPat(line,snd(p));
                         break;

        case FINLIST   : map1Over(checkPat,line,snd(p));
                         break;

        case CONFLDS   : depConFlds(line,p,TRUE);
                         break;

        case ESIGN     : snd(snd(p)) = checkPatType(line,
                                                    "pattern",
                                                    fst(snd(p)),
                                                    snd(snd(p)));
                         fst(snd(p)) = checkPat(line,fst(snd(p)));
                         break;

        default        : ERRMSG(line) "Illegal pattern syntax"
                         EEND;
    }
    return p;
}

static Cell local checkMaybeCnkPat(l,p)/* Check applicative pattern with   */
Int  l;                                /* the possibility of n+k pattern   */
Cell p; {
    Cell h = getHead(p);

    if (argCount==2 && isVar(h) && textOf(h)==textPlus) {       /* n+k     */
        Cell v = arg(fun(p));
        if (!isInt(arg(p))) {
            ERRMSG(l) "Second argument in (n+k) pattern must be an integer"
            EEND;
        }
        if (intOf(arg(p))<=0) {
            ERRMSG(l) "Integer k in (n+k) pattern must be > 0"
            EEND;
        }
        fst(fun(p))      = ADDPAT;
        intValOf(fun(p)) = intOf(arg(p));
        arg(p)           = checkPat(l,v);
        return p;
    }
    return checkApPat(l,0,p);
}

static Cell local checkApPat(line,args,p)
Int  line;                             /* check validity of application    */
Int  args;                             /* of constructor to arguments      */
Cell p; {
    switch (whatIs(p)) {
        case AP        : fun(p) = checkApPat(line,args+1,fun(p));
                         arg(p) = checkPat(line,arg(p));
                         break;

        case TUPLE     : if (tupleOf(p)!=args) {
                             ERRMSG(line) "Illegal tuple pattern"
                             EEND;
                         }
                         break;

#if TREX
        case EXT       : h98DoesntSupport(line,"extensible records");
                         if (args!=2) {
                             ERRMSG(line) "Illegal record pattern"
                             EEND;
                         }
                         break;
#endif

        case QUALIDENT : if (!isQCon(p)) {
                            ERRMSG(line)
                                "Illegal use of qualified variable in pattern"
                            EEND;
                         }
                         /* deliberate fall through */
        case CONIDCELL :
        case CONOPCELL : p = conDefined(line,p);
                         checkCfunArgs(line,p,args);
                         break;

        case NAME      : checkIsCfun(line,p);
                         checkCfunArgs(line,p,args);
                         break;

        default        : ERRMSG(line) "Illegal pattern syntax"
                         EEND;
    }
    return p;
}

static Void local addToPatVars(line,v)  /* Add variable v to list of vars  */
Int  line;                              /* in current pattern, checking    */
Cell v; {                               /* for repeated variables.         */
    Text t = textOf(v);
    List p = NIL;
    List n = patVars;

    for (; nonNull(n); p=n, n=tl(n)) {
        if (textOf(hd(n))==t) {
            ERRMSG(line) "Repeated variable \"%s\" in pattern",
                         textToStr(t)
            EEND;
        }
    }

    if (isNull(p)) {
         patVars = cons(v,NIL);
    } else {
         tl(p)   = cons(v,NIL);
    }
}

static Name local conDefined(line,nm)  /* check that nm is the name of a   */
Int  line;                             /* previously defined constructor   */
Cell nm; {                             /* function.                        */
    Name n = findQualName(nm);
    if (isNull(n)) {
        ERRMSG(line) "Undefined constructor function \"%s\"", identToStr(nm)
        EEND;
    }
    checkIsCfun(line,n);
    return n;
}

static Void local checkIsCfun(line,c)  /* Check that c is a constructor fn */
Int  line;
Name c; {
    if (!isCfun(c)) {
        ERRMSG(line) "\"%s\" is not a constructor function",
                     textToStr(name(c).text)
        EEND;
    }
}

static Void local checkCfunArgs(line,c,args)
Int  line;                             /* Check constructor applied with   */
Cell c;                                /* correct number of arguments      */
Int  args; {
    Int a = userArity(c);
    if (a!=args) {
        ERRMSG(line)
          "Constructor \"%s\" must have exactly %d argument%s in pattern",
          textToStr(name(c).text), a, ((a==1)?"":"s")
        EEND;
    }
}

static Cell local checkPatType(l,wh,e,t)/* Check type appearing in pattern */
Int    l;
String wh;
Cell   e;
Type   t; {
    List tvs = typeVarsIn(t,NIL,NIL,NIL);
    h98DoesntSupport(l,"pattern type annotations");
    for (; nonNull(tvs); tvs=tl(tvs)) {
        Int beta    = newKindvars(1);
        hd(btyvars) = cons(pair(hd(tvs),mkInt(beta)), hd(btyvars));
    }
    t = checkSigType(l,"pattern type",e,t);
    if (isPolyOrQualType(t) || whatIs(t)==RANK2) {
        ERRMSG(l) "Illegal syntax in %s type annotation", wh
        EEND;
    }
    return t;
}

static Cell local applyBtyvs(pat)       /* Record bound type vars in pat   */
Cell pat; {
    List bts = hd(btyvars);
    leaveBtyvs();
    if (nonNull(bts)) {
        pat = ap(BIGLAM,pair(bts,pat));
        for (; nonNull(bts); bts=tl(bts)) {
            snd(hd(bts)) = copyKindvar(intOf(snd(hd(bts))));
        }
    }
    return pat;
}

/* --------------------------------------------------------------------------
 * Maintaining lists of bound variables and local definitions, for
 * dependency and scope analysis.
 * ------------------------------------------------------------------------*/

static List bounds;                    /* list of lists of bound vars      */
static List bindings;                  /* list of lists of binds in scope  */
static List depends;                   /* list of lists of dependents      */

/* bounds   :: [[Var]]        -- var equality used on Vars     */
/* bindings :: [[([Var],?)]]  -- var equality used on Vars     */
/* depends  :: [[Var]]        -- pointer equality used on Vars */

#define saveBvars()      hd(bounds)    /* list of bvars in current scope   */
#define restoreBvars(bs) hd(bounds)=bs /* restore list of bound variables  */

static Cell local bindPat(line,p)      /* add new bound vars for pattern   */
Int  line;
Cell p; {
    patVars    = NIL;
    p          = checkPat(line,p);
    hd(bounds) = revOnto(patVars,hd(bounds));
    return p;
}

static Void local bindPats(line,ps)    /* add new bound vars for patterns  */
Int  line;
List ps; {
    patVars    = NIL;
    map1Over(checkPat,line,ps);
    hd(bounds) = revOnto(patVars,hd(bounds));
}

/* --------------------------------------------------------------------------
 * Before processing value and type signature declarations, all data and
 * type definitions have been processed so that:
 * - all valid type constructors (with their arities) are known.
 * - all valid constructor functions (with their arities and types) are
 *   known.
 *
 * The result of parsing a list of value declarations is a list of Eqns:
 *       Eqn ::= (SIGDECL,(Line,[Var],type))
 *            |  (FIXDECL,(Line,[Op],SyntaxInt))
 *            |  (Expr,Rhs)
 * The ordering of the equations in this list is the reverse of the original
 * ordering in the script parsed.  This is a consequence of the structure of
 * the parser ... but also turns out to be most convenient for the static
 * analysis.
 *
 * As the first stage of the static analysis of value declarations, each
 * list of Eqns is converted to a list of Bindings.  As part of this
 * process:
 * - The ordering of the list of Bindings produced is the same as in the
 *   original script.
 * - When a variable (function) is defined over a number of lines, all
 *   of the definitions should appear together and each should give the
 *   same arity to the variable being defined.
 * - No variable can have more than one definition.
 * - For pattern bindings:
 *   - Each lhs is a valid pattern/function lhs, all constructor functions
 *     have been defined and are used with the correct number of arguments.
 *   - Each lhs contains no repeated pattern variables.
 *   - Each equation defines at least one variable (e.g. True = False is
 *     not allowed).
 * - Types appearing in type signatures are well formed:
 *    - Type constructors used are defined and used with correct number
 *      of arguments.
 *    - type variables are replaced by offsets, type constructor names
 *      by Tycons.
 * - Every variable named in a type signature declaration is defined by
 *   one or more equations elsewhere in the script.
 * - No variable has more than one type declaration.
 * - Similar properties for fixity declarations.
 *
 * ------------------------------------------------------------------------*/

#define bindingAttr(b) fst(snd(b))     /* type(s)/fixity(ies) for binding  */
#define fbindAlts(b)   snd(snd(b))     /* alternatives for function binding*/

static List local extractSigdecls(es)  /* Extract the SIGDECLS from list   */
List es; {                             /* of equations                     */
    List sigdecls = NIL;               /* :: [(Line,[Var],Type)]           */

    for(; nonNull(es); es=tl(es)) {
        if (fst(hd(es))==SIGDECL) {                  /* type-declaration?  */
            Pair sig  = snd(hd(es));
            Int  line = intOf(fst3(sig));
            List vs   = snd3(sig);
            for(; nonNull(vs); vs=tl(vs)) {
                if (isQualIdent(hd(vs))) {
                    ERRMSG(line) "Type signature for qualified variable \"%s\" is not allowed",
                                 identToStr(hd(vs))
                    EEND;
                }
            }
            sigdecls = cons(sig,sigdecls);           /* discard SIGDECL tag*/
        }
    }
    return sigdecls;
}

static List local extractFixdecls(es)   /* Extract the FIXDECLS from list  */
List es; {                              /* of equations                    */
    List fixdecls = NIL;                /* :: [(Line,SyntaxInt,[Op])]      */

    for(; nonNull(es); es=tl(es)) {
        if (fst(hd(es))==FIXDECL) {                  /* fixity declaration?*/
            fixdecls = cons(snd(hd(es)),fixdecls);   /* discard FIXDECL tag*/
        }
    }
    return fixdecls;
}

static List local extractBindings(ds)   /* extract untyped bindings from   */
List ds; {                              /* given list of equations         */
    Cell lastVar   = NIL;               /* = var def'd in last eqn (if any)*/
    Int  lastArity = 0;                 /* = number of args in last defn   */
    List bs        = NIL;               /* :: [Binding]                    */

    for(; nonNull(ds); ds=tl(ds)) {
        Cell d = hd(ds);
        if (fst(d)==FUNBIND) {          /* Function bindings               */
            Cell rhs    = snd(snd(d));
            Int  line   = rhsLine(rhs);
            Cell lhs    = fst(snd(d));
            Cell v      = getHead(lhs);
            Cell newAlt = pair(getArgs(lhs),rhs);
            if (!isVar(v)) {
                internal("FUNBIND");
            }
            if (nonNull(lastVar) && textOf(v)==textOf(lastVar)) {
                if (argCount!=lastArity) {
                    ERRMSG(line) "Equations give different arities for \"%s\"",
                                 textToStr(textOf(v))
                    EEND;
                }
                fbindAlts(hd(bs)) = cons(newAlt,fbindAlts(hd(bs)));
            }
            else {
                lastVar   = v;
                lastArity = argCount;
                notDefined(line,bs,v);
                bs        = cons(pair(v,pair(NIL,singleton(newAlt))),bs);
            }

        } else if (fst(d)==PATBIND) {   /* Pattern bindings                */
            Cell rhs  = snd(snd(d));
            Int  line = rhsLine(rhs);
            Cell pat  = fst(snd(d));
            while (whatIs(pat)==ESIGN) {/* Move type annotations to rhs   */
                Cell p        = fst(snd(pat));
                fst(snd(pat)) = rhs;
                snd(snd(d))   = rhs = pat;
                fst(snd(d))   = pat = p;
                fst(rhs)      = RSIGN;
            }
            if (isVar(pat)) {           /* Convert simple pattern bind to */
                notDefined(line,bs,pat);/* a function binding             */
                bs = cons(pair(pat,pair(NIL,singleton(pair(NIL,rhs)))),bs);
            } else {
                List vs = getPatVars(line,pat,NIL);
                if (isNull(vs)) {
                    ERRMSG(line) "No variables defined in lhs pattern"
                    EEND;
                }
                map2Proc(notDefined,line,bs,vs);
                bs          = cons(pair(vs,pair(NIL,snd(d))),bs);
            }
            lastVar = NIL;
        }
    }
    return bs;
}

static List local getPatVars(line,p,vs) /* Find list of variables bound in */
Int  line;                              /* pattern p                       */
Cell p;
List vs; {
    switch (whatIs(p)) {
        case AP         : do {
                              vs = getPatVars(line,arg(p),vs);
                              p  = fun(p);
                          } while (isAp(p));
                          return vs;    /* Ignore head of application      */

        case CONFLDS    : {   List pfs = snd(snd(p));
                              for (; nonNull(pfs); pfs=tl(pfs)) {
                                  if (isVar(hd(pfs))) {
                                      vs = addPatVar(line,hd(pfs),vs);
                                  } else {
                                      vs = getPatVars(line,snd(hd(pfs)),vs);
                                  }
                              }
                          }
                          return vs;

        case FINLIST    : {   List ps = snd(p);
                              for (; nonNull(ps); ps=tl(ps)) {
                                  vs = getPatVars(line,hd(ps),vs);
                              }
                          }
                          return vs;

        case ESIGN      : return getPatVars(line,fst(snd(p)),vs);

        case LAZYPAT    :
        case NEG        :
        case ONLY       :
        case INFIX      : return getPatVars(line,snd(p),vs);

        case ASPAT      : return addPatVar(line,fst(snd(p)),
                                             getPatVars(line,snd(snd(p)),vs));

        case VARIDCELL  :
        case VAROPCELL  : return addPatVar(line,p,vs);

        case CONIDCELL  :
        case CONOPCELL  :
        case QUALIDENT  :
        case INTCELL    :
        case FLOATCELL  :
        case CHARCELL   :
        case STRCELL    :
        case NAME       :
        case WILDCARD   : return vs;

        default         : internal("getPatVars");
    }
    return vs;
}

static List local addPatVar(line,v,vs)  /* Add var to list of previously   */
Int  line;                              /* encountered variables           */
Cell v;
List vs; {
    if (varIsMember(textOf(v),vs)) {
        ERRMSG(line) "Repeated use of variable \"%s\" in pattern binding",
                     textToStr(textOf(v))
        EEND;
    }
    return cons(v,vs);
}

static List local eqnsToBindings(es,ts,cs,ps)
List es;                                /* Convert list of equations to    */
List ts;                                /* list of typed bindings          */
List cs;
List ps; {
    List bs = extractBindings(es);
    map1Proc(addSigdecl,bs,extractSigdecls(es));
    map4Proc(addFixdecl,bs,ts,cs,ps,extractFixdecls(es));
    return bs;
}

static Void local notDefined(line,bs,v)/* check if name already defined in */
Int  line;                             /* list of bindings                 */
List bs;
Cell v; {
    if (nonNull(findBinding(textOf(v),bs))) {
        ERRMSG(line) "\"%s\" multiply defined", textToStr(textOf(v))
        EEND;
    }
}

static Cell local findBinding(t,bs)    /* look for binding for variable t  */
Text t;                                /* in list of bindings bs           */
List bs; {
    for (; nonNull(bs); bs=tl(bs)) {
        if (isVar(fst(hd(bs)))) {                     /* function-binding? */
            if (textOf(fst(hd(bs)))==t) {
                return hd(bs);
            }
        } else if (nonNull(varIsMember(t,fst(hd(bs))))){/* pattern-binding?*/
            return hd(bs);
        }
    }
    return NIL;
}

static Cell local getAttr(bs,v)         /* Locate type/fixity attribute    */
List bs;                                /* for variable v in bindings bs   */
Cell v; {
    Text t = textOf(v);
    Cell b = findBinding(t,bs);

    if (isNull(b)) {                                    /* No binding      */
        return NIL;
    } else if (isVar(fst(b))) {                         /* func binding?   */
        if (isNull(bindingAttr(b))) {
            bindingAttr(b) = pair(NIL,NIL);
        }
        return bindingAttr(b);
    } else {                                            /* pat binding?    */
        List vs = fst(b);
        List as = bindingAttr(b);

        if (isNull(as)) {
            bindingAttr(b) = as = replicate(length(vs),NIL);
        }

        while (nonNull(vs) && t!=textOf(hd(vs))) {
            vs = tl(vs);
            as = tl(as);
        }

        if (isNull(vs)) {
            internal("getAttr");
        } else if (isNull(hd(as))) {
            hd(as) = pair(NIL,NIL);
        }
        return hd(as);
    }
}

static Void local addSigdecl(bs,sigdecl)/* add type information to bindings*/
List bs;                               /* :: [Binding]                     */
Cell sigdecl; {                        /* :: (Line,[Var],Type)             */
    Int  l    = intOf(fst3(sigdecl));
    List vs   = snd3(sigdecl);
    Type type = checkSigType(l,"type declaration",hd(vs),thd3(sigdecl));

    for (; nonNull(vs); vs=tl(vs)) {
        Cell v    = hd(vs);
        Pair attr = getAttr(bs,v);
        if (isNull(attr)) {
            ERRMSG(l) "Missing binding for variable \"%s\" in type signature",
                      textToStr(textOf(v))
            EEND;
        } else if (nonNull(fst(attr))) {
            ERRMSG(l) "Repeated type signature for \"%s\"",
                      textToStr(textOf(v))
            EEND;
        }
        fst(attr) = type;
    }
}

static Void local addFixdecl(bs,ts,cs,ps,fixdecl)
List   bs;
List   ts;
List   cs;
List   ps;
Triple fixdecl; {
    Int  line = intOf(fst3(fixdecl));
    List ops  = snd3(fixdecl);
    Cell sy   = thd3(fixdecl);

    for (; nonNull(ops); ops=tl(ops)) {
        Cell op   = hd(ops);
        Text t    = textOf(op);
        Cell attr = getAttr(bs,op);
        if (nonNull(attr)) {            /* Found name in binding?          */
            if (nonNull(snd(attr))) {
                dupFixity(line,t);
            }
            snd(attr) = sy;
        } else {                        /* Look in tycons, classes, prims  */
            Name n   = NIL;
            List ts1 = ts;
            List cs1 = cs;
            List ps1 = ps;
            for (; isNull(n) && nonNull(ts1); ts1=tl(ts1)) {    /* tycons  */
                Tycon tc = hd(ts1);
                if (tycon(tc).what==DATATYPE || tycon(tc).what==NEWTYPE) {
                    n = nameIsMember(t,tycon(tc).defn);
                }
            }
            for (; isNull(n) && nonNull(cs1); cs1=tl(cs1)) {    /* classes */
                n = nameIsMember(t,cclass(hd(cs1)).members);
            }
            for (; isNull(n) && nonNull(ps1); ps1=tl(ps1)) {    /* prims   */
                n = nameIsMember(t,hd(ps1));
            }

            if (isNull(n)) {
                missFixity(line,t);
            } else if (name(n).syntax!=NO_SYNTAX) {
                dupFixity(line,t);
            }
            name(n).syntax = intOf(sy);
        }
    }
}

static Void local dupFixity(line,t)     /* Report repeated fixity decl     */
Int  line;
Text t; {
    ERRMSG(line)
        "Repeated fixity declaration for operator \"%s\"", textToStr(t)
    EEND;
}

static Void local missFixity(line,t)    /* Report missing op for fixity    */
Int  line;
Text t; {
    ERRMSG(line)
        "Cannot find binding for operator \"%s\" in fixity declaration",
        textToStr(t)
    EEND;
}

/* --------------------------------------------------------------------------
 * Dealing with infix operators:
 *
 * Expressions involving infix operators or unary minus are parsed as
 * elements of the following type:
 *
 *     data InfixExp = Only Exp | Neg InfixExp | Infix InfixExp Op Exp
 *
 * (The algorithms here do not assume that negation can be applied only once,
 * i.e., that - - x is a syntax error, as required by the Haskell report.
 * Instead, that restriction is captured by the grammar itself, given above.)
 *
 * There are rules of precedence and grouping, expressed by two functions:
 *
 *     prec :: Op -> Int;   assoc :: Op -> Assoc    (Assoc = {L, N, R})
 *
 * InfixExp values are rearranged accordingly when a complete expression
 * has been read using a simple shift-reduce parser whose result may be taken
 * to be a value of the following type:
 *
 *     data Exp = Atom Int | Negate Exp | Apply Op Exp Exp | Error String
 *
 * The machine on which this parser is based can be defined as follows:
 *
 *     tidy                         :: InfixExp -> [(Op,Exp)] -> Exp
 *     tidy (Only a)      []         = a
 *     tidy (Only a)      ((o,b):ss) = tidy (Only (Apply o a b)) ss
 *     tidy (Infix a o b) []         = tidy a [(o,b)]
 *     tidy (Infix a o b) ((p,c):ss)
 *                      | shift  o p = tidy a ((o,b):(p,c):ss)
 *                      | red    o p = tidy (Infix a o (Apply p b c)) ss
 *                      | ambig  o p = Error "ambiguous use of operators"
 *     tidy (Neg e)       []         = tidy (tidyNeg e) []
 *     tidy (Neg e)       ((o,b):ss)
 *                      | nshift o   = tidy (Neg (underNeg o b e)) ss
 *                      | nred   o   = tidy (tidyNeg e) ((o,b):ss)
 *                      | nambig o   = Error "illegal use of negation"
 *
 * At each stage, the parser can either shift, reduce, accept, or error.
 * The transitions when dealing with juxtaposed operators o and p are
 * determined by the following rules:
 *
 *     shift o p  = (prec o > prec p)
 *               || (prec o == prec p && assoc o == L && assoc p == L)
 *
 *     red o p    = (prec o < prec p)
 *               || (prec o == prec p && assoc o == R && assoc p == R)
 *
 *     ambig o p  = (prec o == prec p)
 *               && (assoc o == N || assoc p == N || assoc o /= assoc p)
 *
 * The transitions when dealing with juxtaposed unary minus and infix
 * operators are as follows.  The precedence of unary minus (infixl 6) is
 * hardwired in to these definitions, as it is to the definitions of the
 * Haskell grammar in the official report.
 *
 *     nshift o   = (prec o > 6)
 *     nred   o   = (prec o < 6) || (prec o == 6 && assoc o == L)
 *     nambig o   = prec o == 6 && (assoc o == R || assoc o == N)
 *
 * An InfixExp of the form (Neg e) means negate the last thing in
 * the InfixExp e; we can force this negation using:
 *
 *     tidyNeg              :: OpExp -> OpExp
 *     tidyNeg (Only e)      = Only (Negate e)
 *     tidyNeg (Infix a o b) = Infix a o (Negate b)
 *     tidyNeg (Neg e)       = tidyNeg (tidyNeg e)
 * 
 * On the other hand, if we want to sneak application of an infix operator
 * under a negation, then we use:
 *
 *     underNeg                  :: Op -> Exp -> OpExp -> OpExp
 *     underNeg o b (Only e)      = Only (Apply o e b)
 *     underNeg o b (Neg e)       = Neg (underNeg o b e)
 *     underNeg o b (Infix e p f) = Infix e p (Apply o f b)
 *
 * As a concession to efficiency, we lower the number of calls to syntaxOf
 * by keeping track of the values of sye, sys throughout the process.  The
 * value APPLIC is used to indicate that the syntax value is unknown.
 * ------------------------------------------------------------------------*/

static Cell local tidyInfix(line,e)     /* Convert infixExp to Exp         */
Int  line;
Cell e; {                               /* :: OpExp                        */
    Cell   s   = NIL;                   /* :: [(Op,Exp)]                   */
    Syntax sye = APPLIC;                /* Syntax of op in e (init unknown)*/
    Syntax sys = APPLIC;                /* Syntax of op in s (init unknown)*/
    Cell   d   = e;

    while (fst(d)!=ONLY) {              /* Attach fixities to operators    */
        if (fst(d)==NEG) {
            d = snd(d);
        } else {
            fun(fun(d)) = attachFixity(line,fun(fun(d)));
            d           = arg(fun(d));
        }
    }

    for (;;)
        switch (whatIs(e)) {
            case ONLY : e = snd(e);
                        while (nonNull(s)) {
                            Cell next   = arg(fun(s));
                            arg(fun(s)) = e;
                            fun(fun(s)) = snd(fun(fun(s)));
                            e           = s;
                            s           = next;
                        }
                        return e;

            case NEG  : if (nonNull(s)) {
                            if (sys==APPLIC) {  /* calculate sys           */
                                sys = intOf(fst(fun(fun(s))));
                            }

                            if (precOf(sys)==UMINUS_PREC &&     /* nambig  */
                                assocOf(sys)!=UMINUS_ASSOC) {
                                ERRMSG(line)
                                 "Ambiguous use of unary minus with \""
                                ETHEN ERREXPR(snd(fun(fun(s))));
                                ERRTEXT "\""
                                EEND;
                            }

                            if (precOf(sys)>UMINUS_PREC) {      /* nshift  */
                                Cell e1    = snd(e);
                                Cell t     = s;
                                s          = arg(fun(s));
                                while (whatIs(e1)==NEG)
                                    e1 = snd(e1);
                                arg(fun(t)) = arg(e1);
                                fun(fun(t)) = snd(fun(fun(t)));
                                arg(e1)     = t;
                                sys         = APPLIC;
                                continue;
                            }
                        }

                        /* Intentional fall-thru for nreduce and isNull(s) */

                        {   Cell prev = e;              /* e := tidyNeg e  */
                            Cell temp = arg(prev);
                            Int  nneg = 1;
                            for (; whatIs(temp)==NEG; nneg++) {
                                fun(prev) = nameNegate;
                                prev      = temp;
                                temp      = arg(prev);
                            }
                            if (isInt(arg(temp))) {     /* special cases   */
                                if (nneg&1)             /* for literals    */
                                    arg(temp) = mkInt(-intOf(arg(temp)));
                            }
                            else if (isFloat(arg(temp))) {
                                if (nneg&1)
                                    arg(temp) = floatNegate(arg(temp));
                                                //mkFloat(-floatOf(arg(temp)));
                            }
                            else {
                                fun(prev) = nameNegate;
                                arg(prev) = arg(temp);
                                arg(temp) = e;
                            }
                            e = temp;
                        }
                        continue;

            default   : if (isNull(s)) {/* Move operation onto empty stack */
                            Cell next   = arg(fun(e));
                            s           = e;
                            arg(fun(s)) = NIL;
                            e           = next;
                            sys         = sye;
                            sye         = APPLIC;
                        }
                        else {          /* deal with pair of operators     */

                            if (sye==APPLIC) {  /* calculate sys and sye   */
                                sye = intOf(fst(fun(fun(e))));
                            }
                            if (sys==APPLIC) {
                                sys = intOf(fst(fun(fun(s))));
                            }

                            if (precOf(sye)==precOf(sys) &&     /* ambig   */
                                (assocOf(sye)!=assocOf(sys) ||
                                 assocOf(sye)==NON_ASS)) {
                                ERRMSG(line) "Ambiguous use of operator \""
                                ETHEN ERREXPR(snd(fun(fun(e))));
                                ERRTEXT "\" with \""
                                ETHEN ERREXPR(snd(fun(fun(s))));
                                ERRTEXT "\""
                                EEND;
                            }

                            if (precOf(sye)>precOf(sys) ||      /* shift   */
                                (precOf(sye)==precOf(sys) &&
                                 assocOf(sye)==LEFT_ASS &&
                                 assocOf(sys)==LEFT_ASS)) {
                                Cell next   = arg(fun(e));
                                arg(fun(e)) = s;
                                s           = e;
                                e           = next;
                                sys         = sye;
                                sye         = APPLIC;
                            }
                            else {                              /* reduce  */
                                Cell next   = arg(fun(s));
                                arg(fun(s)) = arg(e);
                                fun(fun(s)) = snd(fun(fun(s)));
                                arg(e)      = s;
                                s           = next;
                                sys         = APPLIC;
                                /* sye unchanged */
                            }
                        }
                        continue;
        }
}

static Pair local attachFixity(line,op) /* Attach fixity to operator in an */
Int  line;                              /* infix expression                */
Cell op; {
    Syntax sy = DEF_OPSYNTAX;

    switch (whatIs(op)) {
        case VAROPCELL :
        case VARIDCELL : if ((sy=lookupSyntax(textOf(op)))==NO_SYNTAX) {
                             Name n = findName(textOf(op));
                             if (isNull(n)) {
                                ERRMSG(line) "Undefined variable \"%s\"",
                                             textToStr(textOf(op))
                                EEND;
                             }
                             sy = syntaxOf(n);
                             op = n;
                         }
                         break;

        case CONOPCELL :
        case CONIDCELL : sy = syntaxOf(op = conDefined(line,op));
                         break;

        case QUALIDENT : {   Name n = findQualName(op);
                             if (nonNull(n)) {
                                 op = n;
                                 sy = syntaxOf(n);
                             } else {
                                 ERRMSG(line)
                                   "Undefined qualified variable \"%s\"",
                                   identToStr(op)
                                 EEND;
                             }
                         }
                         break;
    }
    if (sy==APPLIC) {
        sy = DEF_OPSYNTAX;
    }
    return pair(mkInt(sy),op);          /* Pair fixity with (possibly)     */
                                        /* translated operator             */
}

static Syntax local lookupSyntax(t)     /* Try to find fixity for var in   */
Text t; {                               /* enclosing bindings              */
    List bounds1   = bounds;
    List bindings1 = bindings;

    while (nonNull(bindings1)) {
        if (nonNull(varIsMember(t,hd(bounds1)))) {
            return DEF_OPSYNTAX;
        } else {
            Cell b = findBinding(t,hd(bindings1));
            if (nonNull(b)) {
                Cell a = fst(snd(b));
                if (isVar(fst(b))) {    /* Function binding                */
                    if (nonNull(a) && nonNull(snd(a))) {
                        return intOf(snd(a));
                    }
                } else {                /* Pattern binding                 */
                    List vs = fst(b);
                    while (nonNull(vs) && nonNull(a)) {
                        if (t==textOf(hd(vs))) {
                            if (nonNull(hd(a)) && isInt(snd(hd(a)))) {
                                return intOf(snd(hd(a)));
                            }
                            break;
                        }
                        vs = tl(vs);
                        a  = tl(a);
                    }
                }
                return DEF_OPSYNTAX;
            }
        }
        bounds1   = tl(bounds1);
        bindings1 = tl(bindings1);
    }
    return NO_SYNTAX;
}

/* --------------------------------------------------------------------------
 * To facilitate dependency analysis, lists of bindings are temporarily
 * augmented with an additional field, which is used in two ways:
 * - to build the `adjacency lists' for the dependency graph. Represented by
 *   a list of pointers to other bindings in the same list of bindings.
 * - to hold strictly positive integer values (depth first search numbers) of
 *   elements `on the stack' during the strongly connected components search
 *   algorithm, or a special value mkInt(0), once the binding has been added
 *   to a particular strongly connected component.
 *
 * Using this extra field, the type of each list of declarations during
 * dependency analysis is [Binding'] where:
 *
 *    Binding' ::= (Var, (Attr, (Dep, [Alt])))         -- function binding
 *              |  ([Var], ([Attr], (Dep, (Pat,Rhs)))) -- pattern binding
 *
 * ------------------------------------------------------------------------*/

#define depVal(d) (fst(snd(snd(d))))    /* Access to dependency information*/

static List local dependencyAnal(bs)    /* Separate lists of bindings into */
List bs; {                              /* mutually recursive groups in    */
					/* order of dependency		   */
    mapProc(addDepField,bs);            /* add extra field for dependents  */
    mapProc(depBinding,bs);             /* find dependents of each binding */
    bs = bscc(bs);                      /* sort to strongly connected comps*/
    mapProc(remDepField,bs);            /* remove dependency info field    */
    return bs;
}

static List local topDependAnal(bs)     /* Like dependencyAnal(), but at   */
List bs; {                              /* top level, reporting on progress*/
    List xs;
    Int  i = 0;

    setGoal("Dependency analysis",(Target)(length(bs)));

    mapProc(addDepField,bs);           /* add extra field for dependents   */
    for (xs=bs; nonNull(xs); xs=tl(xs)) {
        emptySubstitution();
        depBinding(hd(xs));
        soFar((Target)(i++));
    }
    bs = bscc(bs);                     /* sort to strongly connected comps */
    mapProc(remDepField,bs);           /* remove dependency info field     */
    done();
    return bs;
}

static Void local addDepField(b)       /* add extra field to binding to    */
Cell b; {                              /* hold list of dependents          */
    snd(snd(b)) = pair(NIL,snd(snd(b)));
}

static Void local remDepField(bs)      /* remove dependency field from     */
List bs; {                             /* list of bindings                 */
    mapProc(remDepField1,bs);
}

static Void local remDepField1(b)      /* remove dependency field from     */
Cell b; {                              /* single binding                   */
    snd(snd(b)) = snd(snd(snd(b)));
}

static Void local clearScope() {       /* initialise dependency scoping    */
    bounds   = NIL;
    bindings = NIL;
    depends  = NIL;
}

static Void local withinScope(bs)       /* Enter scope of bindings bs      */
List bs; {
    bounds   = cons(NIL,bounds);
    bindings = cons(bs,bindings);
    depends  = cons(NIL,depends);
}

static Void local leaveScope() {        /* Leave scope of last withinScope */
    List bs       = hd(bindings);       /* Remove fixity info from binds   */
    Bool toplevel = isNull(tl(bindings));
    for (; nonNull(bs); bs=tl(bs)) {
        Cell b = hd(bs);
        if (isVar(fst(b))) {            /* Variable binding                */
            Cell a = fst(snd(b));
            if (isPair(a)) {
                if (toplevel) {
                    saveSyntax(fst(b),snd(a));
                }
                fst(snd(b)) = fst(a);
            }
        } else {                        /* Pattern binding                 */
            List vs = fst(b);
            List as = fst(snd(b));
            while (nonNull(vs) && nonNull(as)) {
                if (isPair(hd(as))) {
                    if (toplevel) {
                        saveSyntax(hd(vs),snd(hd(as)));
                    }
                    hd(as) = fst(hd(as));
                }
                vs = tl(vs);
                as = tl(as);
            }
        }
    }
    bounds   = tl(bounds);
    bindings = tl(bindings);
    depends  = tl(depends);
}

static Void local saveSyntax(v,sy)      /* Save syntax of top-level var    */
Cell v;                                 /* in corresponding Name           */
Cell sy; {
    Name n = findName(textOf(v));
    if (isNull(n) || name(n).syntax!=NO_SYNTAX) {
        internal("saveSyntax");
    }
    if (nonNull(sy)) {
        name(n).syntax = intOf(sy);
    }
}

/* --------------------------------------------------------------------------
 * As a side effect of the dependency analysis we also make the following
 * checks:
 * - Each lhs is a valid pattern/function lhs, all constructor functions
 *   have been defined and are used with the correct number of arguments.
 * - No lhs contains repeated pattern variables.
 * - Expressions used on the rhs of an eqn should be well formed.  This
 *   includes:
 *   - Checking for valid patterns (including repeated vars) in lambda,
 *     case, and list comprehension expressions.
 *   - Recursively checking local lists of equations.
 * - No free (i.e. unbound) variables are used in the declaration list.
 * ------------------------------------------------------------------------*/

static Void local depBinding(b)        /* find dependents of binding       */
Cell b; {
    Cell defpart = snd(snd(snd(b)));   /* definition part of binding       */

    hd(depends) = NIL;

    if (isVar(fst(b))) {               /* function-binding?                */
        mapProc(depAlt,defpart);
        if (isNull(fst(snd(b)))) {      /* Save dep info if no type sig    */
            fst(snd(b)) = pair(ap(IMPDEPS,hd(depends)),NIL);
        } else if (isNull(fst(fst(snd(b))))) {
            fst(fst(snd(b))) = ap(IMPDEPS,hd(depends));
        }
    } else {                           /* pattern-binding?                 */
        Int line = rhsLine(snd(defpart));
        enterBtyvs();
        patVars = NIL;
        fst(defpart) = checkPat(line,fst(defpart));
        depRhs(snd(defpart));
#if 0
        if (nonNull(hd(btyvars))) {
            ERRMSG(line)
              "Sorry, no type variables are allowed in pattern binding type annotations"
            EEND;
        }
#endif
        fst(defpart) = applyBtyvs(fst(defpart));
    }
    depVal(b) = hd(depends);
}

static Void local depDefaults(c)       /* dependency analysis on defaults  */
Class c; {                             /* from class definition            */
    depClassBindings(cclass(c).defaults);
}

static Void local depInsts(in)         /* dependency analysis on instance  */
Inst in; {                             /* bindings                         */
    depClassBindings(inst(in).implements);
}

static Void local depClassBindings(bs) /* dependency analysis on list of   */
List bs; {                             /* bindings, possibly containing    */
    for (; nonNull(bs); bs=tl(bs)) {   /* NIL bindings ...                 */
        if (nonNull(hd(bs))) {         /* No need to add extra field for   */
           mapProc(depAlt,snd(hd(bs)));/* dependency information...        */
        }
    }
}

static Void local depAlt(a)             /* Find dependents of alternative  */
Cell a; {
    List obvs = saveBvars();            /* Save list of bound variables    */
    enterBtyvs();
    bindPats(rhsLine(snd(a)),fst(a));   /* add new bound vars for patterns */
    depRhs(snd(a));                     /* find dependents of rhs          */
    fst(a)    = applyBtyvs(fst(a));
    restoreBvars(obvs);                 /* restore original list of bvars  */
}

static Void local depRhs(r)             /* Find dependents of rhs          */
Cell r; {
    switch (whatIs(r)) {
        case GUARDED : mapProc(depGuard,snd(r));
                       break;

        case LETREC  : fst(snd(r)) = eqnsToBindings(fst(snd(r)),NIL,NIL,NIL);
                       withinScope(fst(snd(r)));
                       fst(snd(r)) = dependencyAnal(fst(snd(r)));
                       hd(depends) = fst(snd(r));
                       depRhs(snd(snd(r)));
                       leaveScope();
                       break;

        case RSIGN   : snd(snd(r)) = checkPatType(rhsLine(fst(snd(r))),
                                                  "result",
                                                  rhsExpr(fst(snd(r))),
                                                  snd(snd(r)));
                       depRhs(fst(snd(r)));
                       break;

        default      : snd(r) = depExpr(intOf(fst(r)),snd(r));
                       break;
    }
}

static Void local depGuard(g)          /* find dependents of single guarded*/
Cell g; {                              /* expression                       */
    depPair(intOf(fst(g)),snd(g));
}

static Cell local depExpr(line,e)      /* find dependents of expression    */
Int  line;
Cell e; {
  //Printf( "\n\n"); print(e,100); Printf("\n");
  //printExp(stdout,e);
    switch (whatIs(e)) {

        case VARIDCELL  :
        case VAROPCELL  : return depVar(line,e);

        case CONIDCELL  :
        case CONOPCELL  : return conDefined(line,e);

        case QUALIDENT  : if (isQVar(e)) {
                              return depQVar(line,e);
                          } else { /* QConOrConOp */
                              return conDefined(line,e);
                          }

        case INFIX     : return depExpr(line,tidyInfix(line,snd(e)));

#if TREX
        case RECSEL     : break;

        case AP         : if (isAp(e) && isAp(fun(e)) && isExt(fun(fun(e)))) {
                              return depRecord(line,e);
                          } else {
                              Cell nx = e;
                              Cell a;
                              do {
                                  a      = nx;
                                  arg(a) = depExpr(line,arg(a));
                                  nx     = fun(a);
                              } while (isAp(nx));
                              fun(a) = depExpr(line,fun(a));
                          }
                          break;
#else
        case AP         : depPair(line,e);
                          break;
#endif

#if IPARAM
	case IPVAR	:
#endif

        case NAME       :
        case TUPLE      :
        case STRCELL    :
        case CHARCELL   :
        case FLOATCELL  :
        case BIGCELL    :
        case INTCELL    : break;

        case COND       : depTriple(line,snd(e));
                          break;

        case FINLIST    : map1Over(depExpr,line,snd(e));
                          break;

        case LETREC     : fst(snd(e)) = eqnsToBindings(fst(snd(e)),NIL,NIL,NIL);
                          withinScope(fst(snd(e)));
                          fst(snd(e)) = dependencyAnal(fst(snd(e)));
                          hd(depends) = fst(snd(e));
                          snd(snd(e)) = depExpr(line,snd(snd(e)));
                          leaveScope();
                          break;

        case LAMBDA     : depAlt(snd(e));
                          break;

        case DOCOMP     : /* fall-thru */
        case COMP       : depComp(line,snd(e),snd(snd(e)));
                          break;

        case ESIGN      : fst(snd(e)) = depExpr(line,fst(snd(e)));
                          snd(snd(e)) = checkSigType(line,
                                                     "expression",
                                                     fst(snd(e)),
                                                     snd(snd(e)));
                          break;

        case CASE       : fst(snd(e)) = depExpr(line,fst(snd(e)));
                          map1Proc(depCaseAlt,line,snd(snd(e)));
                          break;

        case CONFLDS    : depConFlds(line,e,FALSE);
                          break;

        case UPDFLDS    : depUpdFlds(line,e);
                          break;

#if IPARAM
	case WITHEXP	: depWith(line,e);
			  break;
#endif

        case ASPAT      : ERRMSG(line) "Illegal `@' in expression"
                          EEND;

        case LAZYPAT    : ERRMSG(line) "Illegal `~' in expression"
                          EEND;

        case WILDCARD   : ERRMSG(line) "Illegal `_' in expression"
                          EEND;

#if TREX
        case EXT        : ERRMSG(line) "Illegal application of record"
                          EEND;
#endif

        default         : internal("depExpr");
   }
   return e;
}

static Void local depPair(line,e)       /* find dependents of pair of exprs*/
Int  line;
Cell e; {
    fst(e) = depExpr(line,fst(e));
    snd(e) = depExpr(line,snd(e));
}

static Void local depTriple(line,e)     /* find dependents of triple exprs */
Int  line;
Cell e; {
    fst3(e) = depExpr(line,fst3(e));
    snd3(e) = depExpr(line,snd3(e));
    thd3(e) = depExpr(line,thd3(e));
}

static Void local depComp(l,e,qs)       /* find dependents of comprehension*/
Int  l;
Cell e;
List qs; {
    if (isNull(qs)) {
        fst(e) = depExpr(l,fst(e));
    } else {
        Cell q   = hd(qs);
        List qs1 = tl(qs);
        switch (whatIs(q)) {
            case FROMQUAL : {   List obvs   = saveBvars();
                                snd(snd(q)) = depExpr(l,snd(snd(q)));
                                enterBtyvs();
                                fst(snd(q)) = bindPat(l,fst(snd(q)));
                                depComp(l,e,qs1);
                                fst(snd(q)) = applyBtyvs(fst(snd(q)));
                                restoreBvars(obvs);
                            }
                            break;

            case QWHERE   : snd(q)      = eqnsToBindings(snd(q),NIL,NIL,NIL);
                            withinScope(snd(q));
                            snd(q)      = dependencyAnal(snd(q));
                            hd(depends) = snd(q);
                            depComp(l,e,qs1);
                            leaveScope();
                            break;

            case DOQUAL   : /* fall-thru */
            case BOOLQUAL : snd(q) = depExpr(l,snd(q));
                            depComp(l,e,qs1);
                            break;
        }
    }
}

static Void local depCaseAlt(line,a)    /* Find dependents of case altern. */
Int  line;
Cell a; {
    List obvs = saveBvars();            /* Save list of bound variables    */
    enterBtyvs();
    fst(a)    = bindPat(line,fst(a));   /* Add new bound vars for pats     */
    depRhs(snd(a));                     /* Find dependents of rhs          */
    fst(a)    = applyBtyvs(fst(a));
    restoreBvars(obvs);                 /* Restore original list of bvars  */
}

static Cell local depVar(line,e)        /* Register occurrence of variable */
Int line;
Cell e; {
    List bounds1   = bounds;
    List bindings1 = bindings;
    List depends1  = depends;
    Text t         = textOf(e);
    Cell n;

    while (nonNull(bindings1)) {
        n = varIsMember(t,hd(bounds1));   /* look for t in bound variables */
        if (nonNull(n)) {
            return n;
        }
        n = findBinding(t,hd(bindings1)); /* look for t in var bindings    */
        if (nonNull(n)) {
            if (!cellIsMember(n,hd(depends1))) {
                hd(depends1) = cons(n,hd(depends1));
            }
           return (isVar(fst(n)) ? fst(n) : e);
        }

        bounds1   = tl(bounds1);
        bindings1 = tl(bindings1);
        depends1  = tl(depends1);
    }

    if (isNull(n=findName(t))) {               /* check global definitions */
        ERRMSG(line) "Undefined variable \"%s\"", textToStr(t)
        EEND;
    }

#if 0
    what is this for??
    if (!moduleThisScript(name(n).mod)) {
        return n;
    }
#endif
    /* Later phases of the system cannot cope if we resolve references
     * to unprocessed objects too early.  This is the main reason that
     * we cannot cope with recursive modules at the moment.
     */
    return e;
}

static Cell local depQVar(line,e)/* register occurrence of qualified variable */
Int line;
Cell e; {
    Name n = findQualName(e);
    if (isNull(n)) {                            /* check global definitions */
        ERRMSG(line) "Undefined qualified variable \"%s\"", identToStr(e)
        EEND;
    }
    if (name(n).mod != currentModule) {
        return n;
    }
    if (fst(e) == VARIDCELL) {
        e = mkVar(qtextOf(e));
    } else {
        e = mkVarop(qtextOf(e));
    }
    return depVar(line,e);
}

static Void local depConFlds(line,e,isP)/* check construction using fields */
Int  line;
Cell e;
Bool isP; {
    Name c = conDefined(line,fst(snd(e)));
    if (isNull(snd(snd(e))) ||
        nonNull(cellIsMember(c,depFields(line,e,snd(snd(e)),isP)))) {
        fst(snd(e)) = c;
    } else {
        ERRMSG(line) "Constructor \"%s\" does not have selected fields in ",
                     textToStr(name(c).text)
        ETHEN ERREXPR(e);
        ERRTEXT "\n"
        EEND;
    }
    if (!isP && isPair(name(c).defn)) { /* Check that banged fields defined*/
        List scs = fst(name(c).defn);   /* List of strict components       */
        Type t   = name(c).type;
        Int  a   = userArity(c);
        List fs  = snd(snd(e));
        List ss;
        if (isPolyType(t)) {            /* Find tycon that c belongs to    */
            t = monotypeOf(t);
        }
 	if (isQualType(t)) {
            t = snd(snd(t));
        }
        if (whatIs(t)==CDICTS) {
            t = snd(snd(t));
        }
        while (0<a--) {
            t = arg(t);
        }
        while (isAp(t)) {
            t = fun(t);
        }
        for (ss=tycon(t).defn; hasCfun(ss); ss=tl(ss)) {
        }
        /* Now we know the tycon t that c belongs to, and the corresponding
         * list of selectors for that type, ss.  Now we have to check that
         * each of the fields identified by scs appears in fs, using ss to
         * cross reference, and convert integers to selector names.
         */
        for (; nonNull(scs); scs=tl(scs)) {
            Int  i   = intOf(hd(scs));
            List ss1 = ss;
            for (; nonNull(ss1); ss1=tl(ss1)) {
                List cns = name(hd(ss1)).defn;
                for (; nonNull(cns); cns=tl(cns)) {
                    if (fst(hd(cns))==c) {
                        break;
                    }
                }
                if (nonNull(cns) && intOf(snd(hd(cns)))==i) {
                    break;
                }
            }
            if (isNull(ss1)) {
                internal("depConFlds");
            } else {
                Name s   = hd(ss1);
                List fs1 = fs;
                for (; nonNull(fs1) && s!=fst(hd(fs1)); fs1=tl(fs1)) {
                }
                if (isNull(fs1)) {
                    ERRMSG(line) "Construction does not define strict field"
                    ETHEN
                    ERRTEXT      "\nExpression : " ETHEN ERREXPR(e);
                    ERRTEXT      "\nField      : " ETHEN ERREXPR(s);
                    ERRTEXT      "\n"
                    EEND;
                }
            }
        }
    }
}

static Void local depUpdFlds(line,e)    /* check update using fields       */
Int  line;
Cell e; {
    if (isNull(thd3(snd(e)))) {
        ERRMSG(line) "Empty field list in update"
        EEND;
    }
    fst3(snd(e)) = depExpr(line,fst3(snd(e)));
    snd3(snd(e)) = depFields(line,e,thd3(snd(e)),FALSE);
}

static List local depFields(l,e,fs,isP) /* check field binding list        */
Int  l;
Cell e;
List fs;
Bool isP; {
    List cs = NIL;
    List ss = NIL;

    for (; nonNull(fs); fs=tl(fs)) {    /* for each field binding          */
        Cell fb = hd(fs);
        Name s;

        if (isVar(fb)) {                /* expand  var  to  var = var      */
            h98DoesntSupport(l,"missing field bindings");
            fb = hd(fs) = pair(fb,fb);
        }

        s = findQualName(fst(fb));      /* check for selector              */
        if (nonNull(s) && isSfun(s)) {
            fst(fb) = s;
        } else {
            ERRMSG(l) "\"%s\" is not a selector function/field name",
                      textToStr(textOf(fst(fb)))
            EEND;
        }

        if (isNull(ss)) {               /* for first named selector        */
            List scs = name(s).defn;    /* calculate list of constructors  */
            for (; nonNull(scs); scs=tl(scs)) {
                cs = cons(fst(hd(scs)),cs);
            }
            ss = singleton(s);          /* initialize selector list        */
        } else {                        /* for subsequent selectors        */
            List ds = cs;               /* intersect constructor lists     */
            for (cs=NIL; nonNull(ds); ) {
                List scs = name(s).defn;
                while (nonNull(scs) && fst(hd(scs))!=hd(ds)) {
                    scs = tl(scs);
                }
                if (isNull(scs)) {
                    ds = tl(ds);
                } else {
                    List next = tl(ds);
                    tl(ds)    = cs;
                    cs        = ds;
                    ds        = next;
                }
            }

            if (cellIsMember(s,ss)) {   /* check for repeated uses         */
                ERRMSG(l) "Repeated field name \"%s\" in field list",
                          textToStr(name(s).text)
                EEND;
            }
            ss = cons(s,ss);
        }

        if (isNull(cs)) {               /* Are there any matching constrs? */
            ERRMSG(l) "No constructor has all of the fields specified in "
            ETHEN ERREXPR(e);
            ERRTEXT "\n"
            EEND;
        }

        snd(fb) = (isP ? checkPat(l,snd(fb)) : depExpr(l,snd(fb)));
    }
    return cs;
}

#if IPARAM
static Void local depWith(line,e)	/* check with using fields	   */
Int  line;
Cell e; {
    fst(snd(e)) = depExpr(line,fst(snd(e)));
    snd(snd(e)) = depDwFlds(line,e,snd(snd(e)));
}

static List local depDwFlds(l,e,fs)/* check field binding list	   */
Int  l;
Cell e;
List fs;
{
    Cell c = fs;
    for (; nonNull(c); c=tl(c)) {	/* for each field binding	   */
	snd(hd(c)) = depExpr(l,snd(hd(c)));
    }
    return fs;
}
#endif

#if TREX
static Cell local depRecord(line,e)     /* find dependents of record and   */
Int  line;                              /* sort fields into approp. order  */
Cell e; {                               /* to make construction and update */
    List exts = NIL;                    /* more efficient.                 */
    Cell r    = e;

    h98DoesntSupport(line,"extensible records");
    do {                                /* build up list of extensions     */
        Text   t    = extText(fun(fun(r)));
        String s    = textToStr(t);
        List   prev = NIL;
        List   nx   = exts;
        while (nonNull(nx) && strcmp(textToStr(extText(fun(fun(nx)))),s)>0) {
            prev = nx;
            nx   = extRow(nx);
        }
        if (nonNull(nx) && t==extText(fun(fun(nx)))) {
            ERRMSG(line) "Repeated label \"%s\" in record ", s
            ETHEN ERREXPR(e);
            ERRTEXT "\n"
            EEND;
        }
        if (isNull(prev)) {
            exts = cons(fun(r),exts);
        } else {
            tl(prev) = cons(fun(r),nx);
        }
        extField(r) = depExpr(line,extField(r));
        r           = extRow(r);
    } while (isAp(r) && isAp(fun(r)) && isExt(fun(fun(r))));
    r = depExpr(line,r);
    return revOnto(exts,r);
}
#endif


/* --------------------------------------------------------------------------
 * Several parts of this program require an algorithm for sorting a list
 * of values (with some added dependency information) into a list of strongly
 * connected components in which each value appears before its dependents.
 *
 * Each of these algorithms is obtained by parameterising a standard
 * algorithm in "scc.c" as shown below.
 * ------------------------------------------------------------------------*/

#define  SCC2            tcscc          /* make scc algorithm for Tycons   */
#define  LOWLINK         tclowlink
#define  DEPENDS(c)      (isTycon(c) ? tycon(c).kind : cclass(c).kinds)
#define  SETDEPENDS(c,v) if(isTycon(c)) tycon(c).kind=v; else cclass(c).kinds=v
#include "scc.c"
#undef   SETDEPENDS
#undef   DEPENDS
#undef   LOWLINK
#undef   SCC2

#define  SCC             bscc           /* make scc algorithm for Bindings */
#define  LOWLINK         blowlink
#define  DEPENDS(t)      depVal(t)
#define  SETDEPENDS(c,v) depVal(c)=v
#include "scc.c"
#undef   SETDEPENDS
#undef   DEPENDS
#undef   LOWLINK
#undef   SCC

/* --------------------------------------------------------------------------
 * Main static analysis:
 * ------------------------------------------------------------------------*/

Void checkExp() {                       /* Top level static check on Expr  */
    staticAnalysis(RESET);
    clearScope();                       /* Analyse expression in the scope */
    withinScope(NIL);                   /* of no local bindings            */
    inputExpr = depExpr(0,inputExpr);
    leaveScope();
    staticAnalysis(RESET);
}

#if EXPLAIN_INSTANCE_RESOLUTION
Void checkContext(void) {		/* Top level static check on Expr  */
    List vs, qs;

    staticAnalysis(RESET);
    clearScope();			/* Analyse expression in the scope */
    withinScope(NIL);			/* of no local bindings		   */
    qs = inputContext;
    for (vs = NIL; nonNull(qs); qs=tl(qs)) {
	vs = typeVarsIn(hd(qs),NIL,NIL,vs);
    }
    map2Proc(depPredExp,0,vs,inputContext);
    leaveScope();
    staticAnalysis(RESET);
}
#endif

Void checkDefns ( Module thisModule ) { /* Top level static analysis       */

    staticAnalysis(RESET);

    setCurrModule(thisModule);

    /* Resolve module references */
    mapProc(checkQualImport,  module(thisModule).qualImports);
    mapProc(checkUnqualImport,unqualImports);
    /* Add "import Prelude" if there`s no explicit import */
    if (thisModule!=modulePrelude
        && isNull(cellAssoc(modulePrelude,unqualImports))
        && isNull(cellRevAssoc(modulePrelude,module(thisModule).qualImports))) {
        unqualImports = cons(pair(modulePrelude,DOTDOT),unqualImports);
    } else {
        /* Every module (including the Prelude) implicitly contains 
         * "import qualified Prelude" 
         */
        module(thisModule).qualImports
           =cons(pair(mkCon(textPrelude),modulePrelude),
                 module(thisModule).qualImports);
    }
    mapProc(checkImportList, unqualImports);

    /* Note: there's a lot of side-effecting going on here, so
       don't monkey about with the order of operations here unless
       you know what you are doing */
    if (!combined) linkPreludeTC();     /* Get prelude tycons and classes  */

    mapProc(checkTyconDefn,tyconDefns); /* validate tycon definitions      */
    checkSynonyms(tyconDefns);          /* check synonym definitions       */
    mapProc(checkClassDefn,classDefns); /* process class definitions       */
    mapProc(kindTCGroup,tcscc(tyconDefns,classDefns)); /* attach kinds     */
    mapProc(visitClass,classDefns);	/* check class hierarchy	   */
    mapProc(extendFundeps,classDefns);  /* finish class definitions	   */
					/* (convenient if we do this after */
					/* calling `visitClass' so that we */
					/* know the class hierarchy is     */
					/* acyclic)                        */

    mapProc(addMembers,classDefns);     /* add definitions for member funs */

    if (!combined) linkPreludeCM();     /* Get prelude cfuns and mfuns     */
    
    instDefns = rev(instDefns);         /* process instance definitions    */
    mapProc(checkInstDefn,instDefns);

    setCurrModule(thisModule);
    mapProc(addRSsigdecls,typeInDefns);	/* add sigdecls for RESTRICTSYN	   */
    valDefns   = eqnsToBindings(valDefns,tyconDefns,classDefns,/*primDefns*/NIL);
    mapProc(allNoPrevDef,valDefns);	/* check against previous defns	   */
    mapProc(addDerivImp,derivedInsts);  /* Add impls for derived instances */
    deriveContexts(derivedInsts);       /* Calculate derived inst contexts */
    instDefns  = appendOnto(instDefns,derivedInsts);
    checkDefaultDefns();                /* validate default definitions    */

    mapProc(allNoPrevDef,valDefns);     /* check against previous defns    */

    if (!combined) linkPrimNames();     /* link primitive names           */

    mapProc(checkForeignImport,foreignImports); /* check foreign imports   */
    mapProc(checkForeignExport,foreignExports); /* check foreign exports   */
    foreignImports = NIL;
    foreignExports = NIL;

    /* Every top-level name has now been created - so we can build the     */
    /* export list.  Note that this has to happen before dependency        */
    /* analysis so that references to Prelude.foo will be resolved         */
    /* when compiling the prelude.                                         */
    module(thisModule).exports 
       = checkExports ( module(thisModule).exports, thisModule );

    mapProc(checkTypeIn,typeInDefns);   /* check restricted synonym defns  */

    clearScope();
    withinScope(valDefns);
    valDefns = topDependAnal(valDefns); /* top level dependency ordering   */
    mapProc(depDefaults,classDefns);    /* dep. analysis on class defaults */
    mapProc(depInsts,instDefns);        /* dep. analysis on inst defns     */
    leaveScope();

    /* ToDo: evalDefaults should match current evaluation module */
    evalDefaults = defaultDefns;        /* Set defaults for evaluator      */

    staticAnalysis(RESET);
}




static Void local addRSsigdecls(pr)     /* add sigdecls from TYPE ... IN ..*/
Pair pr; {
    List vs = snd(pr);                  /* get list of variables           */
    for (; nonNull(vs); vs=tl(vs)) {
        if (fst(hd(vs))==SIGDECL) {     /* find a sigdecl                  */
            valDefns = cons(hd(vs),valDefns);   /* add to valDefns         */
            hd(vs)   = hd(snd3(snd(hd(vs))));   /* and replace with var    */
        }
    }
}

static Void local allNoPrevDef(b)        /* ensure no previous bindings for*/
Cell b; {                                /* variables in new binding       */
    if (isVar(fst(b))) {
        noPrevDef(rhsLine(snd(hd(snd(snd(b))))),fst(b));
    } else {
        Int line = rhsLine(snd(snd(snd(b))));
        map1Proc(noPrevDef,line,fst(b));
    }
}

static Void local noPrevDef(line,v)      /* ensure no previous binding for */
Int  line;                               /* new variable                   */
Cell v; {
    Name n = findName(textOf(v));

    if (isNull(n)) {
        n            = newName(textOf(v),NIL);
        name(n).defn = PREDEFINED;
    } else if (name(n).defn!=PREDEFINED) {
        duplicateError(line,name(n).mod,name(n).text,"variable");
    }
    name(n).line = line;
}

static Void local duplicateErrorAux(line,mod,t,kind)/* report duplicate defn */
Int    line;
Module mod;
Text   t;
String kind; {
    if (mod == currentModule) {
        ERRMSG(line) "Repeated definition for %s \"%s\"", kind, 
                     textToStr(t)
        EEND;
    } else {
        ERRMSG(line) "Definition of %s \"%s\" clashes with import", kind,
                     textToStr(t)
        EEND;
    }
}

static Void local checkTypeIn(cvs)      /* Check that vars in restricted   */
Pair cvs; {                             /* synonym are defined             */
    Tycon c  = fst(cvs);
    List  vs = snd(cvs);

    for (; nonNull(vs); vs=tl(vs)) {
        if (isNull(findName(textOf(hd(vs))))) {
            ERRMSG(tycon(c).line)
                "No top level binding of \"%s\" for restricted synonym \"%s\"",
                textToStr(textOf(hd(vs))), textToStr(tycon(c).text)
            EEND;
        }
    }
}

/* --------------------------------------------------------------------------
 * Haskell 98 compatibility tests:
 * ------------------------------------------------------------------------*/

Bool h98Pred(allowArgs,pi)              /* Check syntax of Hask98 predicate*/
Bool allowArgs;
Cell pi; {
    return isClass(getHead(pi)) && argCount==1 &&
           isOffset(getHead(arg(pi))) && (argCount==0 || allowArgs);
}

Cell h98Context(allowArgs,ps)           /* Check syntax of Hask98 context  */
Bool allowArgs;
List ps; {
    for (; nonNull(ps); ps=tl(ps)) {
        if (!h98Pred(allowArgs,hd(ps))) {
            return hd(ps);
        }
    }
    return NIL;
}

Void h98CheckCtxt(line,wh,allowArgs,ps,in)
Int    line;                            /* Report illegal context/predicate*/
String wh;
Bool   allowArgs;
List   ps;
Inst   in; {
    if (haskell98) {
        Cell pi = h98Context(allowArgs,ps);
        if (nonNull(pi)) {
            ERRMSG(line) "Illegal Haskell 98 class constraint in %s",wh ETHEN
            if (nonNull(in)) {
                ERRTEXT  "\n*** Instance   : " ETHEN ERRPRED(inst(in).head);
            }
            ERRTEXT      "\n*** Constraint : " ETHEN ERRPRED(pi);
            if (nonNull(ps) && nonNull(tl(ps))) {
                ERRTEXT  "\n*** Context    : " ETHEN ERRCONTEXT(ps);
            }
            ERRTEXT      "\n"
            EEND;
        }
    }
}

Void h98CheckType(line,wh,e,t)          /* Check for Haskell 98 type       */
Int    line;
String wh;
Cell   e;
Type   t; {
    if (haskell98) {
        Type ty = t;
        if (isPolyType(t))
            t = monotypeOf(t);
	if (isQualType(t)) {
            Cell pi = h98Context(TRUE,fst(snd(t)));
            if (nonNull(pi)) {
                ERRMSG(line) "Illegal Haskell 98 class constraint in %s",wh
                ETHEN
                ERRTEXT  "\n*** Expression : " ETHEN ERREXPR(e);
                ERRTEXT  "\n*** Type       : " ETHEN ERRTYPE(ty);
                ERRTEXT  "\n"
                EEND;
            }
        }
    }
}

Void h98DoesntSupport(line,wh)          /* Report feature missing in H98   */
Int    line;
String wh; {
    if (haskell98) {
        ERRMSG(line) "Haskell 98 does not support %s", wh
        EEND;
    }
}

/* --------------------------------------------------------------------------
 * Static Analysis control:
 * ------------------------------------------------------------------------*/

Void staticAnalysis(what)
Int what; {
    switch (what) {
        case RESET   : cfunSfuns    = NIL;
                       daSccs       = NIL;
                       patVars      = NIL;
                       bounds       = NIL;
                       bindings     = NIL;
                       depends      = NIL;
                       tcDeps       = NIL;
                       derivedInsts = NIL;
                       diVars       = NIL;
                       diNum        = 0;
                       unkindTypes  = NIL;
                       break;

        case MARK    : mark(daSccs);
                       mark(patVars);
                       mark(bounds);
                       mark(bindings);
                       mark(depends);
                       mark(tcDeps);
                       mark(derivedInsts);
                       mark(diVars);
                       mark(cfunSfuns);
                       mark(unkindTypes);
#if TREX
                       mark(extKind);
#endif
                       break;

        case POSTPREL: break;

        case PREPREL : staticAnalysis(RESET);
#if TREX
                       extKind = pair(STAR,pair(ROW,ROW));
#endif
    }
}

/*-------------------------------------------------------------------------*/

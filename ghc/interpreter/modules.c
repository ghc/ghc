/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Import-Export processing for Hugs
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: modules.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:21 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "static.h"
#include "errors.h"
#include "link.h"
#include "modules.h"

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Name  local lookupName           Args((Text,List));
static List  local checkSubentities     Args((List,List,List,String,Text));
static List  local checkExportTycon     Args((List,Text,Cell,Tycon));
static List  local checkExportClass     Args((List,Text,Cell,Class));
static List  local checkExport          Args((List,Text,Cell));
static List  local checkImportEntity    Args((List,Module,Cell));
static List  local resolveImportList    Args((Module,Cell));

static Void  local importName           Args((Module,Name));
static Void  local importTycon          Args((Module,Tycon));
static Void  local importClass          Args((Module,Class));

/* --------------------------------------------------------------------------
 * Static analysis of modules:
 * ------------------------------------------------------------------------*/

Void startModule(nm)                             /* switch to a new module */
Cell nm; {
    Module m;
    if (!isCon(nm)) internal("startModule");
    if (isNull(m = findModule(textOf(nm)))) {
        m = newModule(textOf(nm));
    } else if (m != modulePreludeHugs) {
        ERRMSG(0) "Module \"%s\" already loaded", textToStr(textOf(nm))
        EEND;
    }
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

Void checkQualImport(i)                /* Process qualified import         */
Pair i; {
    Module m = findModid(snd(i));
    if (isNull(m)) {
        ERRMSG(0) "Module \"%s\" not previously loaded", 
            textToStr(textOf(snd(i)))
        EEND;
    }
    snd(i)=m;
}

Void checkUnqualImport(i)              /* Process unqualified import       */
Pair i; {
    Module m = findModid(fst(i));
    if (isNull(m)) {
        ERRMSG(0) "Module \"%s\" not previously loaded", 
            textToStr(textOf(fst(i)))
        EEND;
    }
    fst(i)=m;
}

static Name local lookupName(t,nms)     /* find text t in list of Names     */
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
List   named;                 /* :: [ Q?(Var|Con)(Id|Op) ]                  */
List   wanted;                /* :: [Name]                                  */
String description;           /* "<constructor>|<member> of <type>|<class>" */
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

static List local checkImportEntity(imports,exporter,entity)
List   imports; /* Accumulated list of things to import */
Module exporter;
Cell   entity; { /* Entry from import list */
    List oldImports = imports;
    Text t  = isIdent(entity) ? textOf(entity) : textOf(fst(entity));
    List es = module(exporter).exports; 
    for(; nonNull(es); es=tl(es)) {
        Cell e = hd(es); /* :: Entity | (Entity, NIL|DOTDOT) */
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
                                    imports=revDupOnto(tycon(f).defn,imports);
                                } else {
                                    imports=checkSubentities(imports,snd(entity),tycon(f).defn,"constructor of type",t);
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
                            return revDupOnto(cclass(f).members,imports);
                        } else {
                            return checkSubentities(imports,snd(entity),cclass(f).members,"member of class",t);
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

static List local resolveImportList(m,impList)
Module m;  /* exporting module */
Cell   impList; {
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
                    imports = revDupOnto(subentities,imports);
                }
            }
        }
    } else {
        map1Accum(checkImportEntity,imports,m,impList);
    }
    return imports;
}

Void checkImportList(thisModule,importSpec)  /* Import a module unqualified */
Module thisModule;
Pair   importSpec; {
    Module m       = fst(importSpec);
    Cell   impList = snd(importSpec);

    List   imports = NIL; /* entities we want to import */
    List   hidden  = NIL; /* entities we want to hide   */

    if (m == thisModule) {
        ERRMSG(0) "Module \"%s\" recursively imports itself",
            textToStr(module(m).text)
        EEND;
    }
    if (isPair(impList) && HIDDEN == fst(impList)) {
        /* Somewhat inefficient - but obviously correct:
         * imports = importsOf("module Foo") `setDifference` hidden;
         */
        hidden  = resolveImportList(m, snd(impList));
        imports = resolveImportList(m, DOTDOT);
    } else {
        imports = resolveImportList(m, impList);
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

Void importEntity(source,e)
Module source;
Cell e; {
    switch (whatIs(e)) {
    case NAME  : importName(source,e); 
            break;
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
        ERRMSG(0) "Entity \"%s\" imported from module \"%s\" already defined in module \"%s\"",
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
        if (nonNull(export=findQualName(0,e))) {
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
            exports = revDupOnto(module(m).exports,exports);
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
                        ERRMSG(0) "Explicit constructor list given for type synonym \"%s\" in export list of module \"%s\"",
                            identToStr(ident),
                            textToStr(mt)
                        EEND;
                    }
                    return cons(pair(nm,DOTDOT),exports);
            case RESTRICTSYN:   
                    ERRMSG(0) "Transparent export of restricted type synonym \"%s\" in export list of module \"%s\"",
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
}

List checkExports(thisModule,exports)
Module thisModule;
List   exports; {
    Text   mt = module(thisModule).text;
    List   es = NIL;

    map1Accum(checkExport,es,mt,exports);

#if DEBUG_MODULES
    for(xs=es; nonNull(xs); xs=tl(xs)) {
        printf(" %s", textToStr(textOfEntity(hd(xs))));
    }
#endif
    return es;
}

/*-------------------------------------------------------------------------*/



/* --------------------------------------------------------------------------
 * GHC interface file processing for Hugs
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: interface.c,v $
 * $Revision: 1.5 $
 * $Date: 1999/07/06 15:24:38 $
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
#include "backend.h"
#include "connect.h"
#include "errors.h"
#include "link.h"
#include "Assembler.h" /* for wrapping GHC objects */
#include "dynamic.h"

#define DEBUG_IFACE

extern void print ( Cell, Int );

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
static List ghcConstrDecls;     
static List ghcSynonymDecls; 
static List ghcClassDecls; 
static List ghcInstanceDecls;

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static List local addGHCConstrs Args((Int,List,List));
static Name local addGHCSel     Args((Int,Pair));
static Name local addGHCConstr  Args((Int,Int,Triple));


static Void  local finishGHCVar      Args((Name));     
static Void  local finishGHCConstr   Args((Name));     
static Void  local finishGHCSynonym  Args((Tycon)); 
static Void  local finishGHCClass    Args((Class)); 
static Void  local finishGHCInstance Args((Inst));
static Void  local finishGHCImports  Args((Triple));
static Void  local finishGHCExports  Args((Pair));
static Void  local finishGHCModule   Args((Module));

static Kinds local tvsToKind             Args((List));
static Int   local arityFromType         Args((Type));
static Int   local arityInclDictParams   Args((Type));

                                         
static List       local ifTyvarsIn       Args((Type));

static Type       local tvsToOffsets       Args((Int,Type,List));
static Type       local conidcellsToTycons Args((Int,Type));

static Void       local resolveReferencesInObjectModule Args((Module,Bool));
static Bool       local validateOImage Args((void*, Int, Bool));
static Void       local readSyms Args((Module));

static void*      local lookupObjName ( char* );


/* --------------------------------------------------------------------------
 * code:
 * ------------------------------------------------------------------------*/

List ifImports;   /* [ConId] -- modules imported by current interface */

List ghcImports;  /* [(Module, Text, [ConId|VarId])]
                     each (m1, m2, names) in this list
                     represents 'module m1 where ... import m2 ( names ) ...'
                     The list acts as a list of names to fix up in
                        finishInterfaces().
		  */

List ghcExports;  /* [(ConId,   -- module name
                        [ ConId | VarId | pair(ConId,[ConId|VarId])] )]
                                -- list of entities
                  */

List ghcModules;  /* [Module] -- modules of the .his loaded in this group */

Void addGHCExports(mod,stuff)
Cell mod;
List stuff; {
   ghcExports = cons( pair(mod,stuff), ghcExports );
}

static Void local finishGHCExports(paire)
Pair paire; {
   Text modTxt   = textOf(fst(paire));
   List entities = snd(paire);
   Module mod    = findModule(modTxt);
   if (isNull(mod)) {
      ERRMSG(0) "Can't find module \"%s\" mentioned in export list",
                textToStr(modTxt)
      EEND;
   }
fprintf(stderr, "----------------------------------finishexports\n");
   /* Assume that each .hi file only contains one export decl */
   if (nonNull(module(mod).exports))
      internal("finishGHCExports: non-empty export list");

   /* Run along what the parser gave us and make export list entries */
   for (; nonNull(entities); entities=tl(entities)) {
      Cell ent = hd(entities);
      List subents;
      Cell c;
      switch (whatIs(ent)) {
         case VARIDCELL: /* variable */
            c = findName ( snd(ent) );
            assert(nonNull(c));
fprintf(stderr, "var %s\n", textToStr(name(c).text));
            module(mod).exports = cons(c, module(mod).exports);
            break;
         case CONIDCELL: /* non data tycon */
            c = findTycon ( snd(ent) );
            assert(nonNull(c));
fprintf(stderr, "non data tycon %s\n", textToStr(tycon(c).text));
            module(mod).exports = cons(c, module(mod).exports);
            break;
         default: /* data T = C1 ... Cn  or class C where f1 ... fn */
            if (!isPair(ent)) internal("finishExports(2)");
            subents = snd(ent);
            ent = fst(ent);
            c = findTycon ( snd(ent) );
            if (nonNull(c)) {
	      /* data */
fprintf(stderr, "data %s = ", textToStr(tycon(c).text));
               module(mod).exports = cons(pair(c,DOTDOT), module(mod).exports);
               for (; nonNull(subents); subents = tl(subents)) {
                  Cell ent2 = hd(subents);
                  assert(isCon(ent2));
                  c = findName ( snd(ent2) );
fprintf(stderr, "%s ", textToStr(name(c).text));
                  assert(nonNull(c));
                  module(mod).exports = cons(c, module(mod).exports);
               }
fprintf(stderr, "\n" );
            } else {
               /* class */
               c = findClass ( snd(ent) );
               assert(nonNull(c));            
fprintf(stderr, "class %s where ", textToStr(cclass(c).text));
               module(mod).exports = cons(pair(c,DOTDOT), module(mod).exports);

               for (; nonNull(subents); subents = tl(subents)) {
                  Cell ent2 = hd(subents);
                  assert(isVar(ent2));
                  c = findName ( snd(ent2) );
fprintf(stderr, "%s ", textToStr(name(c).text));
                  assert(nonNull(c));
                  module(mod).exports = cons(c, module(mod).exports);
               }
fprintf(stderr, "\n" );

            }
            break;
      }
   }
}

static Void local finishGHCImports(triple)
Triple triple;
{
   Module dstMod = fst3(triple);  // the importing module
   Text   srcTxt = snd3(triple);
   List   names  = thd3(triple);
   Module srcMod = findModule ( srcTxt );
   Module tmpCurrentModule = currentModule;
   List   exps;
   Bool   found;
   Text   tnm;
   Cell   nm;
   Cell   x;
   //fprintf(stderr, "finishGHCImports: dst=%s   src=%s\n", 
   //                textToStr(module(dstMod).text),
   //                textToStr(srcTxt) );
   //print(names, 100);
   //printf("\n");
   /* for each nm in names
      nm should be in module(src).exports -- if not, error
      if nm notElem module(dst).names cons it on
   */

   if (isNull(srcMod)) {
      /* I don't think this can actually ever happen, but still ... */
      ERRMSG(0) "Interface for module \"%s\" imports unknown module \"%s\"",
                textToStr(module(dstMod).text),
                textToStr(srcTxt)
      EEND;
   }
   //printf ( "exports of %s are\n", textToStr(module(srcMod).text) );
   //print( module(srcMod).exports, 100 );
   //printf( "\n" );

   setCurrModule ( srcMod ); // so that later lookups succeed

   for (; nonNull(names); names=tl(names)) {
      nm = hd(names);
      /* Check the exporting module really exports it. */
      found = FALSE;
      for (exps=module(srcMod).exports; nonNull(exps); exps=tl(exps)) {
         Cell c = hd(exps);
         //if (isPair(c)) c=fst(c);
         assert(whatIs(c)==CONIDCELL || whatIs(c)==VARIDCELL);
         assert(whatIs(nm)==CONIDCELL || whatIs(nm)==VARIDCELL);
	 //printf( "   compare `%s' `%s'\n", textToStr(textOf(c)), textToStr(textOf(nm)));
         if (textOf(c)==textOf(nm)) { found=TRUE; break; }
      }
      if (!found) {
         ERRMSG(0) "Interface for module \"%s\" imports \"%s\" from\n"
                   "module \"%s\", but the latter doesn't export it",
                   textToStr(module(dstMod).text), textToStr(textOf(nm)),
                   textToStr(module(srcMod).text)
         EEND;
      }
      /* Ok, it's exported.  Now figure out what it is we're really
         importing. 
      */
      tnm = textOf(nm);

      x = findName(tnm);
      if (nonNull(x)) {
         if (!cellIsMember(x,module(dstMod).names))
            module(dstMod).names = cons(x, module(dstMod).names);
         continue;
      }

      x = findTycon(tnm);
      if (nonNull(x)) {
         if (!cellIsMember(x,module(dstMod).tycons))
            module(dstMod).tycons = cons(x, module(dstMod).tycons);
         continue;
      }

      x = findClass(tnm);
      if (nonNull(x)) {
         if (!cellIsMember(x,module(dstMod).classes))
            module(dstMod).classes = cons(x, module(dstMod).classes);
         continue;
      }

      fprintf(stderr, "\npanic: Can't figure out what this is in finishGHCImports\n"
                      "\t%s\n", textToStr(tnm) );
      internal("finishGHCImports");
   }

   setCurrModule(tmpCurrentModule);
}


Void loadInterface(String fname, Long fileSize)
{
    ifImports = NIL;
    parseInterface(fname,fileSize);
    if (nonNull(ifImports))
       chase(ifImports);
}


Void finishInterfaces ( void )
{
    /* the order of these doesn't matter
     * (ToDo: unless synonyms have to be eliminated??)
     */
    mapProc(finishGHCVar,      ghcVarDecls);     
    mapProc(finishGHCConstr,   ghcConstrDecls);     
    mapProc(finishGHCSynonym,  ghcSynonymDecls); 
    mapProc(finishGHCClass,    ghcClassDecls); 
    mapProc(finishGHCInstance, ghcInstanceDecls);
    mapProc(finishGHCExports,  ghcExports);
    mapProc(finishGHCImports,  ghcImports);
    mapProc(finishGHCModule,   ghcModules);
    ghcVarDecls      = NIL;
    ghcConstrDecls   = NIL;
    ghcSynonymDecls  = NIL;
    ghcClassDecls    = NIL;
    ghcInstanceDecls = NIL;
    ghcImports       = NIL;
    ghcExports       = NIL;
    ghcModules       = NIL;
}


static Void local finishGHCModule(mod)
Module mod; {
   // do the implicit 'import Prelude' thing
   List pxs = module(modulePrelude).exports;
   for (; nonNull(pxs); pxs=tl(pxs)) {
      Cell px = hd(pxs);
      again:
      switch (whatIs(px)) {
         case AP: 
            px = fst(px); 
            goto again;
         case NAME: 
            module(mod).names = cons ( px, module(mod).names );
            break;
         case TYCON: 
            module(mod).tycons = cons ( px, module(mod).tycons );
            break;
         case CLASS: 
            module(mod).classes = cons ( px, module(mod).classes );
            break;
         default:
            fprintf(stderr, "finishGHCModule: unknown tag %d\n", whatIs(px));
            break;
      }
   }

   // Last, but by no means least ...
   resolveReferencesInObjectModule ( mod, FALSE );
}

Void openGHCIface(t)
Text t; {
    FILE* f;
    void* img;

    Module m = findModule(t);
    if (isNull(m)) {
        m = newModule(t);
printf ( "new module %s\n", textToStr(t) );
    } else if (m != modulePrelude) {
        ERRMSG(0) "Module \"%s\" already loaded", textToStr(t)
        EEND;
    }

    // sizeObj and nameObj will magically be set to the right
    // thing when we arrive here.
    // All this crud should be replaced with mmap when we do this
    // for real(tm)
    img = malloc ( sizeObj );
    if (!img) {
       ERRMSG(0) "Can't allocate memory to load object file for module \"%s\"",
                 textToStr(t)
       EEND;
    }
    f = fopen( nameObj, "rb" );
    if (!f) {
       // Really, this shouldn't happen, since makeStackEntry ensures the
       // object is available.  Nevertheless ...
       ERRMSG(0) "Object file \"%s\" can't be opened to read -- oops!",
	         &(nameObj[0])
       EEND;
    }
    if (sizeObj != fread ( img, 1, sizeObj, f)) {
       ERRMSG(0) "Read of object file \"%s\" failed", nameObj
       EEND;
    }
    if (!validateOImage(img,sizeObj,FALSE)) {
       ERRMSG(0) "Validation of object file \"%s\" failed", nameObj 
       EEND;
    }
    
    assert(!module(m).oImage);
    module(m).oImage = img;

    readSyms(m);

    if (!cellIsMember(m, ghcModules))
       ghcModules = cons(m, ghcModules);

    setCurrModule(m);
}


Void addGHCImports(line,mn,syms)
Int    line;
Text   mn;       /* the module to import from */
List   syms; {   /* [ConId | VarId] -- the names to import */
    List t;
    Bool found;
#   ifdef DEBUG_IFACE
    printf("\naddGHCImport %s\n", textToStr(mn) );
#   endif
  
    // Hack to avoid chasing Prel* junk right now
    if (strncmp(textToStr(mn), "Prel",4)==0) return;

    found = FALSE;
    for (t=ifImports; nonNull(t); t=tl(t)) {
       if (textOf(hd(t)) == mn) {
          found = TRUE;
          break;
       }
    }
    if (!found) {
       ifImports = cons(mkCon(mn),ifImports);
       ghcImports = cons( triple(currentModule,mn,syms), ghcImports );
    }
}

void addGHCVar(line,v,ty)
Int  line;
Text v;
Type ty;
{
    Name   n;
    String s;
    List   tmp, tvs;
    /* if this var is the name of a ghc-compiled dictionary,
       ie, starts zdfC   where C is a capital,
       ignore it.
    */
    s = textToStr(v);
#   ifdef DEBUG_IFACE
    printf("\nbegin addGHCVar %s\n", s);
#   endif
    if (s[0]=='z' && s[1]=='d' && s[2]=='f' && isupper((int)s[3])) {
#      ifdef DEBUG_IFACE
       printf("       ignoring %s\n", s);
#      endif
       return;
    }
    n = findName(v);
    if (nonNull(n)) {
        ERRMSG(0) "Attempt to redefine variable \"%s\"", textToStr(v)
        EEND;
    }
    n = newName(v,NIL);

    tvs = nubList(ifTyvarsIn(ty));
    for (tmp=tvs; nonNull(tmp); tmp=tl(tmp))
       hd(tmp) = pair(hd(tmp),STAR);
    if (nonNull(tvs))
       ty = mkPolyType(tvsToKind(tvs),ty);

    ty = tvsToOffsets(line,ty,tvs);
    
    /* prepare for finishGHCVar */
    name(n).type = ty;
    name(n).arity = arityInclDictParams(ty);
    name(n).line = line;
    ghcVarDecls = cons(n,ghcVarDecls);
#   ifdef DEBUG_IFACE
    printf("end   addGHCVar %s\n", s);
#   endif
}

static Void local finishGHCVar(Name n)
{
    Int  line = name(n).line;
    Type ty   = name(n).type;
#   ifdef DEBUG_IFACE
    fprintf(stderr, "\nbegin finishGHCVar %s\n", textToStr(name(n).text) );
#   endif
    setCurrModule(name(n).mod);
    name(n).type = conidcellsToTycons(line,ty);
#   ifdef DEBUG_IFACE
    fprintf(stderr, "end   finishGHCVar %s\n", textToStr(name(n).text) );
#   endif
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
        tycon(tc).defn  = tvsToOffsets(line,ty,tvs);
        ghcSynonymDecls = cons(tc,ghcSynonymDecls);
    }
}

static Void  local finishGHCSynonym(Tycon tc)
{
    Int  line = tycon(tc).line;

    setCurrModule(tycon(tc).mod);
    tycon(tc).defn = conidcellsToTycons(line,tycon(tc).defn);

    /* ToDo: can't really do this until I've done all synonyms
     * and then I have to do them in order
     * tycon(tc).defn = fullExpand(ty);
     */
}

Void addGHCDataDecl(line,ctx0,tycon,ktyvars,constrs0)
Int  line;
List ctx0;      /* [(QConId,VarId)]              */
Cell tycon;     /* ConId                         */
List ktyvars;   /* [(VarId,Kind)] */
List constrs0;  /* [(ConId,[(Type,Text)],NIL)]  
                   The NIL will become the constr's type
                   The Text is an optional field name */
    /* ToDo: worry about being given a decl for (->) ?
     * and worry about qualidents for ()
     */
{
    Type    ty, resTy, selTy, conArgTy;
    List    tmp, conArgs, sels, constrs, fields, tyvarsMentioned;
    List    ctx, ctx2;
    Triple  constr;
    Cell    conid;
    Pair    conArg, ctxElem;
    Text    conArgNm;

    Text t = textOf(tycon);
#   ifdef DEBUG_IFACE
    fprintf(stderr, "\nbegin addGHCDataDecl %s\n",textToStr(t));
#   endif
    if (nonNull(findTycon(t))) {
        ERRMSG(line) "Repeated definition of type constructor \"%s\"",
                     textToStr(t)
        EEND;
    } else {
        Tycon tc        = newTycon(t);
        tycon(tc).text  = t;
        tycon(tc).line  = line;
        tycon(tc).arity = length(ktyvars);
        tycon(tc).kind  = tvsToKind(ktyvars);
        tycon(tc).what  = DATATYPE;

        /* a list to accumulate selectors in :: [(VarId,Type)] */
        sels = NIL;

        /* make resTy the result type of the constr, T v1 ... vn */
        resTy = tycon;
        for (tmp=ktyvars; nonNull(tmp); tmp=tl(tmp))
           resTy = ap(resTy,fst(hd(tmp)));

        /* for each constructor ... */
        for (constrs=constrs0; nonNull(constrs); constrs=tl(constrs)) {
           constr = hd(constrs);
           conid  = fst3(constr);
           fields = snd3(constr);
           assert(isNull(thd3(constr)));

           /* Build type of constr and handle any selectors found.
              Also collect up tyvars occurring in the constr's arg
              types, so we can throw away irrelevant parts of the
              context later.
           */
           ty = resTy;
           tyvarsMentioned = NIL;  /* [VarId] */
           conArgs = reverse(fields);
           for (; nonNull(conArgs); conArgs=tl(conArgs)) {
              conArg   = hd(conArgs); /* (Type,Text) */
              conArgTy = fst(conArg);
              conArgNm = snd(conArg);
              tyvarsMentioned = dupListOnto(ifTyvarsIn(conArgTy),
                                            tyvarsMentioned);
              ty = fn(conArgTy,ty);
              if (nonNull(conArgNm)) {
		 /* a field name is mentioned too */
                 selTy = fn(resTy,conArgTy);
                 if (whatIs(tycon(tc).kind) != STAR)
                    selTy = pair(POLYTYPE,pair(tycon(tc).kind, selTy));
                 selTy = tvsToOffsets(line,selTy, ktyvars);

                 sels = cons( pair(conArgNm,selTy), sels);
              }
           }

           /* Now ty is the constructor's type, not including context.
              Throw away any parts of the context not mentioned in 
              tyvarsMentioned, and use it to qualify ty.
	   */
           ctx2 = NIL;
           for (ctx=ctx0; nonNull(ctx); ctx=tl(ctx)) {
              ctxElem = hd(ctx);     /* (QConId,VarId) */
              if (nonNull(cellIsMember(textOf(snd(ctxElem)),tyvarsMentioned)))
                 ctx2 = cons(ctxElem, ctx2);
           }
           if (nonNull(ctx2))
              ty = ap(QUAL,pair(ctx2,ty));

           /* stick the tycon's kind on, if not simply STAR */
           if (whatIs(tycon(tc).kind) != STAR)
              ty = pair(POLYTYPE,pair(tycon(tc).kind, ty));

           ty = tvsToOffsets(line,ty, ktyvars);

           /* Finally, stick the constructor's type onto it. */
           thd3(hd(constrs)) = ty;
        }

        /* Final result is that 
           constrs :: [(ConId,[(Type,Text)],Type)]   
                      lists the constructors and their types
           sels :: [(VarId,Type)]
                   lists the selectors and their types
	*/
        tycon(tc).defn  = addGHCConstrs(line,constrs0,sels);
    }
#   ifdef DEBUG_IFACE
    fprintf(stderr, "end   addGHCDataDecl %s\n",textToStr(t));
#   endif
}


static List local addGHCConstrs(line,cons,sels)
Int  line;
List cons;   /* [(ConId,[(Type,Text)],Type)] */
List sels; { /* [(VarId,Type)]         */
    List cs, ss;
    Int  conNo = 0; /*  or maybe 1? */
    for(cs=cons; nonNull(cs); cs=tl(cs), conNo++) {
        Name c  = addGHCConstr(line,conNo,hd(cs));
        hd(cs)  = c;
    }
    for(ss=sels; nonNull(ss); ss=tl(ss)) {
        hd(ss) = addGHCSel(line,hd(ss));
    }
    return appendOnto(cons,sels);
}

static Name local addGHCSel(line,sel)
Int  line;
Pair sel;    /* (VarId,Type)        */
{
    Text t      = textOf(fst(sel));
    Type type   = snd(sel);
    
    Name n = findName(t);
    if (nonNull(n)) {
        ERRMSG(line) "Repeated definition for selector \"%s\"",
            textToStr(t)
        EEND;
    }

    n              = newName(t,NIL);
    name(n).line   = line;
    name(n).number = SELNAME;
    name(n).arity  = 1;
    name(n).defn   = NIL;

    /* prepare for finishGHCVar */
    name(n).type = type;
    ghcVarDecls = cons(n,ghcVarDecls);

    return n;
}

static Name local addGHCConstr(line,conNo,constr)
Int    line;
Int    conNo;
Triple constr; { /* (ConId,[(Type,Text)],Type) */
    /* ToDo: add rank2 annotation and existential annotation
     * these affect how constr can be used.
     */
    Text con   = textOf(fst3(constr));
    Type type  = thd3(constr);
    Int  arity = arityFromType(type);
    Name n = findName(con);     /* Allocate constructor fun name   */
    if (isNull(n)) {
        n = newName(con,NIL);
    } else if (name(n).defn!=PREDEFINED) {
        ERRMSG(line) "Repeated definition for constructor \"%s\"",
            textToStr(con)
        EEND;
    }
    name(n).arity  = arity;     /* Save constructor fun details    */
    name(n).line   = line;
    name(n).number = cfunNo(conNo);

    /* prepare for finishGHCCon */
    name(n).type   = type;
    ghcConstrDecls = cons(n,ghcConstrDecls);

    return n;
}

static Void local finishGHCConstr(Name n)
{
    Int  line = name(n).line;
    Type ty   = name(n).type;
    setCurrModule(name(n).mod);
#   ifdef DEBUG_IFACE
    printf ( "\nbegin finishGHCConstr %s\n", textToStr(name(n).text));
#   endif
    name(n).type = conidcellsToTycons(line,ty);
#   ifdef DEBUG_IFACE
    printf ( "end   finishGHCConstr %s\n", textToStr(name(n).text));
#   endif
}


Void addGHCNewType(line,ctx0,tycon,tvs,constr)
Int  line;
List ctx0;      /* [(QConId,VarId)]      */
Cell tycon;     /* ConId | QualConId     */
List tvs;       /* [(VarId,Kind)]        */
Cell constr; {  /* (ConId,Type)          */
    /* ToDo: worry about being given a decl for (->) ?
     * and worry about qualidents for ()
     */
    List tmp;
    Type resTy;
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

        assert(nonNull(constr));
        if (isNull(constr)) {
            tycon(tc).defn = NIL;
        } else {
            /* constr :: (ConId,Type) */
            Text con   = textOf(fst(constr));
            Type type  = snd(constr);
            Name n = findName(con);     /* Allocate constructor fun name   */
            if (isNull(n)) {
                n = newName(con,NIL);
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

             /* make resTy the result type of the constr, T v1 ... vn */
            resTy = tycon;
            for (tmp=tvs; nonNull(tmp); tmp=tl(tmp))
               resTy = ap(resTy,fst(hd(tmp)));
            type = fn(type,resTy);
            if (nonNull(ctx0))
               type = ap(QUAL,pair(ctx0,type));

            type = tvsToOffsets(line,type,tvs);

            name(n).type   = type;
            ghcConstrDecls = cons(n,ghcConstrDecls);
        }
    }
}

Void addGHCClass(line,ctxt,tc_name,tv,mems0)
Int  line;
List ctxt;       /* [(QConId, VarId)]     */ 
Cell tc_name;    /* ConId                 */
Text tv;         /* VarId                 */
List mems0; {    /* [(VarId, Type)]       */
    List mems;   /* [(VarId, Type)]       */
    List tvsInT; /* [VarId] and then [(VarId,Kind)] */
    List tvs;    /* [(VarId,Kind)]        */
    Text ct     = textOf(tc_name);
    Pair newCtx = pair(tc_name, tv);
#   ifdef DEBUG_IFACE
    printf ( "\nbegin addGHCclass %s\n", textToStr(ct) );
#   endif
    if (nonNull(findClass(ct))) {
        ERRMSG(line) "Repeated definition of class \"%s\"",
                     textToStr(ct)
        EEND;
    } else if (nonNull(findTycon(ct))) {
        ERRMSG(line) "\"%s\" used as both class and type constructor",
                     textToStr(ct)
        EEND;
    } else {
        Class nw              = newClass(ct);
        cclass(nw).text       = ct;
        cclass(nw).line       = line;
        cclass(nw).arity      = 1;
        cclass(nw).head       = ap(nw,mkOffset(0));
        cclass(nw).kinds      = singleton(STAR); /* absolutely no idea at all */
        cclass(nw).instances  = NIL;             /* what the kind should be   */
        cclass(nw).numSupers  = length(ctxt);

        /* Kludge to map the single tyvar in the context to Offset 0.
           Need to do something better for multiparam type classes.
        */
        cclass(nw).supers     = tvsToOffsets(line,ctxt,
                                             singleton(pair(tv,STAR)));

        for (mems=mems0; nonNull(mems); mems=tl(mems)) {
           Pair mem  = hd(mems);
           Type memT = snd(mem);
           Text mnt  = textOf(fst(mem));
           Name mn;

           /* Stick the new context on the member type */
           if (whatIs(memT)==POLYTYPE) internal("addGHCClass");
           if (whatIs(memT)==QUAL) {
              memT = pair(QUAL,
                          pair(cons(newCtx,fst(snd(memT))),snd(snd(memT))));
           } else {
              memT = pair(QUAL,
                          pair(singleton(newCtx),memT));
           }

           /* Cook up a kind for the type. */
           tvsInT = nubList(ifTyvarsIn(memT));

           /* ToDo: maximally bogus */
           for (tvs=tvsInT; nonNull(tvs); tvs=tl(tvs))
              hd(tvs) = pair(hd(tvs),STAR);

           memT = mkPolyType(tvsToKind(tvsInT),memT);
           memT = tvsToOffsets(line,memT,tvsInT);

           /* Park the type back on the member */
           snd(mem) = memT;

           /* Bind code to the member */
           mn = findName(mnt);
           if (nonNull(mn)) {
              ERRMSG(line) 
                 "Repeated definition for class method \"%s\"",
                 textToStr(mnt)
              EEND;
           }
           mn = newName(mnt,NIL);
        }

        cclass(nw).members    = mems0;
        cclass(nw).numMembers = length(mems0);
        ghcClassDecls = cons(nw,ghcClassDecls);

        /* ToDo: 
         * cclass(nw).dsels    = ?;
         * cclass(nw).dbuild   = ?;
         * cclass(nm).dcon     = ?;
         * cclass(nm).defaults = ?;
         */
    }
#   ifdef DEBUG_IFACE
    printf ( "end   addGHCclass %s\n", textToStr(ct) );
#   endif
}

static Void  local finishGHCClass(Class nw)
{
    List mems;
    Int line = cclass(nw).line;
    Int ctr  = - length(cclass(nw).members);

#   ifdef DEBUG_IFACE
    printf ( "\nbegin finishGHCclass %s\n", textToStr(cclass(nw).text) );
#   endif

    setCurrModule(cclass(nw).mod);

    cclass(nw).level      = 0;  /* ToDo: 1 + max (map level supers) */
    cclass(nw).head       = conidcellsToTycons(line,cclass(nw).head);
    cclass(nw).supers     = conidcellsToTycons(line,cclass(nw).supers);
    cclass(nw).members    = conidcellsToTycons(line,cclass(nw).members);

    for (mems=cclass(nw).members; nonNull(mems); mems=tl(mems)) {
       Pair mem = hd(mems); /* (VarId, Type) */
       Text txt = textOf(fst(mem));
       Type ty  = snd(mem);
       Name n   = findName(txt);
       assert(nonNull(n));
       name(n).line   = cclass(nw).line;
       name(n).type   = ty;
       name(n).number = ctr++;
       hd(mems) = n;
    }
#   ifdef DEBUG_IFACE
    printf ( "end   finishGHCclass %s\n", textToStr(cclass(nw).text) );
#   endif
}

Void addGHCInstance (line,ctxt0,cls,var)
Int  line;
List ctxt0;  /* [(QConId, Type)] */
Pair cls;    /* (ConId, [Type])  */
Text var; {  /* Text */
    List tmp, tvs, ks;
    Inst in = newInst();
#   ifdef DEBUG_IFACE
    printf ( "\nbegin addGHCInstance\n" );
#   endif

    /* Make tvs into a list of tyvars with bogus kinds. */
    tvs = nubList(ifTyvarsIn(snd(cls)));
    ks = NIL;
    for (tmp = tvs; nonNull(tmp); tmp=tl(tmp)) {
       hd(tmp) = pair(hd(tmp),STAR);
       ks = cons(STAR,ks);
    }

    inst(in).line         = line;
    inst(in).implements   = NIL;
    inst(in).kinds        = ks;
    inst(in).specifics    = tvsToOffsets(line,ctxt0,tvs);
    inst(in).numSpecifics = length(ctxt0);
    inst(in).head         = tvsToOffsets(line,cls,tvs);
#if 0
Is this still needed?
    {
        Name b         = newName(inventText(),NIL);
        name(b).line   = line;
        name(b).arity  = length(ctxt); /* unused? */
        name(b).number = DFUNNAME;
        inst(in).builder = b;
        bindNameToClosure(b, lookupGHCClosure(inst(in).mod,var));
    }
#endif
    ghcInstanceDecls = cons(in, ghcInstanceDecls);
#   ifdef DEBUG_IFACE
    printf ( "end   addGHCInstance\n" );
#   endif
}

static Void  local finishGHCInstance(Inst in)
{
    Int  line   = inst(in).line;
    Cell cl     = fst(inst(in).head);
    Class c;
#   ifdef DEBUG_IFACE
    printf ( "\nbegin finishGHCInstance\n" );
#   endif

    setCurrModule(inst(in).mod);
    c = findClass(textOf(cl));
    if (isNull(c)) {
        ERRMSG(line) "Unknown class \"%s\" in instance",
                     textToStr(textOf(cl))
        EEND;
    }
    inst(in).head         = conidcellsToTycons(line,inst(in).head);
    inst(in).specifics    = conidcellsToTycons(line,inst(in).specifics);
    cclass(c).instances   = cons(in,cclass(c).instances);
#   ifdef DEBUG_IFACE
    printf ( "end   finishGHCInstance\n" );
#   endif
}

/* --------------------------------------------------------------------------
 * Helper fns
 * ------------------------------------------------------------------------*/

/* This is called from the addGHC* functions.  It traverses a structure
   and converts varidcells, ie, type variables parsed by the interface
   parser, into Offsets, which is how Hugs wants to see them internally.
   The Offset for a type variable is determined by its place in the list
   passed as the second arg; the associated kinds are irrelevant.
*/
static Type local tvsToOffsets(line,type,ktyvars)
Int  line;
Type type;
List ktyvars; { /* [(VarId|Text,Kind)] */
   switch (whatIs(type)) {
      case NIL:
      case TUPLE:
      case QUALIDENT:
      case CONIDCELL:
      case TYCON:
         return type;
      case AP: 
         return ap( tvsToOffsets(line,fun(type),ktyvars),
                    tvsToOffsets(line,arg(type),ktyvars) );
      case POLYTYPE: 
         return mkPolyType ( 
                   polySigOf(type),
                   tvsToOffsets(line,monotypeOf(type),ktyvars)
                );
         break;
      case QUAL:
         return pair(QUAL,pair(tvsToOffsets(line,fst(snd(type)),ktyvars),
                               tvsToOffsets(line,snd(snd(type)),ktyvars)));
      case VARIDCELL: /* Ha! some real work to do! */
       { Int i = 0;
         Text tv = textOf(type);
         for (; nonNull(ktyvars); i++,ktyvars=tl(ktyvars)) {
            Cell varid = fst(hd(ktyvars));
            Text tt = isVar(varid) ? textOf(varid) : varid;
            if (tv == tt) return mkOffset(i);            
         }
         ERRMSG(line) "Undefined type variable \"%s\"", textToStr(tv)
         EEND;
         break;
       }
      default: 
         fprintf(stderr, "tvsToOffsets: unknown stuff %d\n", whatIs(type));
         print(type,20);
         fprintf(stderr,"\n");
         assert(0);
   }
   assert(0); /* NOTREACHED */
}


/* This is called from the finishGHC* functions.  It traverses a structure
   and converts conidcells, ie, type constructors parsed by the interface
   parser, into Tycons (or Classes), which is how Hugs wants to see them
   internally.  Calls to this fn have to be deferred to the second phase
   of interface loading (finishGHC* rather than addGHC*) so that all relevant
   Tycons or Classes have been loaded into the symbol tables and can be
   looked up.
*/
static Type local conidcellsToTycons(line,type)
Int  line;
Type type; {
   switch (whatIs(type)) {
      case NIL:
      case OFFSET:
      case TYCON:
      case CLASS:
      case VARIDCELL:
         return type;
      case QUALIDENT:
       { List t;
         Text m     = qmodOf(type);
         Text v     = qtextOf(type);
         Module mod = findModule(m);
printf ( "lookup qualident " ); print(type,100); printf("\n");
         if (isNull(mod)) {
            ERRMSG(line)
               "Undefined module in qualified name \"%s\"",
               identToStr(type)
            EEND;
            return NIL;
         }
         for (t=module(mod).tycons; nonNull(t); t=tl(t))
            if (v == tycon(hd(t)).text) return hd(t);
         for (t=module(mod).classes; nonNull(t); t=tl(t))
            if (v == cclass(hd(t)).text) return hd(t);
         ERRMSG(line)
              "Undefined qualified class or type \"%s\"",
              identToStr(type)
         EEND;
         return NIL;
       }
      case CONIDCELL:
       { Tycon tc;
         Class cl;
         tc = findQualTycon(type);
         if (nonNull(tc)) return tc;
         cl = findQualClass(type);
         if (nonNull(cl)) return cl;
         ERRMSG(line)
             "Undefined class or type constructor \"%s\"",
             identToStr(type)
         EEND;
         return NIL;
       }
      case AP: 
         return ap( conidcellsToTycons(line,fun(type)),
                    conidcellsToTycons(line,arg(type)) );
      case POLYTYPE: 
         return mkPolyType ( 
                   polySigOf(type),
                   conidcellsToTycons(line,monotypeOf(type))
                );
         break;
      case QUAL:
         return pair(QUAL,pair(conidcellsToTycons(line,fst(snd(type))),
                               conidcellsToTycons(line,snd(snd(type)))));
      default: 
         fprintf(stderr, "conidcellsToTycons: unknown stuff %d\n", 
                 whatIs(type));
         print(type,20);
         fprintf(stderr,"\n");
         assert(0);
   }
   assert(0); /* NOTREACHED */
}


/* --------------------------------------------------------------------------
 * Utilities
 *
 * None of these do lookups or require that lookups have been resolved
 * so they can be performed while reading interfaces.
 * ------------------------------------------------------------------------*/

static Kinds local tvsToKind(tvs)
List tvs; { /* [(VarId,Kind)] */
    List  rs;
    Kinds r  = STAR;
    for (rs=reverse(tvs); nonNull(rs); rs=tl(rs)) {
        r = ap(snd(hd(rs)),r);
    }
    return r;
}


static Int local arityInclDictParams ( Type type )
{
   Int arity = 0;
   if (isPolyType(type)) type = monotypeOf(type);
   
   if (whatIs(type) == QUAL)
   {
      arity += length ( fst(snd(type)) );
      type = snd(snd(type));
   }
   while (isAp(type) && getHead(type)==typeArrow) {
      arity++;
      type = arg(type);
   }
   return arity;
}

/* arity of a constructor with this type */
static Int local arityFromType(type) 
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


static List local ifTyvarsIn(type)
Type type; {
    List vs = typeVarsIn(type,NIL,NIL);
    List vs2 = vs;
    for (; nonNull(vs2); vs2=tl(vs2)) {
       Cell v = hd(vs2);
       if (whatIs(v)==VARIDCELL || whatIs(v)==VAROPCELL) {
          hd(vs2) = textOf(hd(vs2)); 
       } else {
          internal("ifTyvarsIn");
       }
    }
    return vs;
}


/* --------------------------------------------------------------------------
 * ELF specifics
 * ------------------------------------------------------------------------*/

#include <elf.h>

static char* local findElfSection ( void* objImage, Elf32_Word sh_type )
{
   Int i;
   char* ehdrC = (char*)objImage;
   Elf32_Ehdr* ehdr = ( Elf32_Ehdr*)ehdrC;
   Elf32_Shdr* shdr = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);
   char* ptr = NULL;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == sh_type &&
          i !=  ehdr->e_shstrndx) {
         ptr = ehdrC + shdr[i].sh_offset;
         break;
      }
   }
   return ptr;
}


static Void local resolveReferencesInObjectModule_elf ( Module m, 
                                                        Bool   verb )
{
   char symbol[1000]; // ToDo
   int i, j;
   Elf32_Sym*  stab;
   char* strtab;
   char* ehdrC = (char*)(module(m).oImage);
   Elf32_Ehdr* ehdr = (Elf32_Ehdr*) ehdrC;
   Elf32_Shdr* shdr = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);
   Elf32_Word* targ;
   // first find "the" symbol table
   //stab = findElfSection ( objImage, SHT_SYMTAB );

   // also go find the string table
   strtab = findElfSection ( ehdrC, SHT_STRTAB );

   if (!stab || !strtab) 
      internal("resolveReferencesInObjectModule_elf");

   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_REL ) {
         Elf32_Rel*  rtab = (Elf32_Rel*) (ehdrC + shdr[i].sh_offset);
         Int         nent = shdr[i].sh_size / sizeof(Elf32_Rel);
         Int target_shndx = shdr[i].sh_info;
         Int symtab_shndx = shdr[i].sh_link;
         stab  = (Elf32_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
         targ  = (Elf32_Word*)(ehdrC + shdr[ target_shndx ].sh_offset);
         if (verb)
         fprintf ( stderr,
                  "relocations for section %d using symtab %d\n",
                  target_shndx, symtab_shndx );
         for (j = 0; j < nent; j++) {
            Elf32_Addr offset = rtab[j].r_offset;
            Elf32_Word info   = rtab[j].r_info;

            Elf32_Addr  P = ((Elf32_Addr)targ) + offset;
            Elf32_Word* pP = (Elf32_Word*)P;
            Elf32_Addr  A = *pP;
            Elf32_Addr  S;

            if (verb) fprintf ( stderr, "Rel entry %3d is raw(%6p %6p)   ", 
                                j, (void*)offset, (void*)info );
            if (!info) {
               if (verb) fprintf ( stderr, " ZERO\n" );
               S = 0;
            } else {
               if (stab[ ELF32_R_SYM(info)].st_name == 0) {
                  if (verb) fprintf ( stderr, "(noname)  ");
                  /* nameless (local) symbol */
                  S = (Elf32_Addr)(ehdrC
                                   + shdr[stab[ELF32_R_SYM(info)].st_shndx ].sh_offset
                                   + stab[ELF32_R_SYM(info)].st_value
                                  );
                  strcpy ( symbol, "(noname)");
               } else {
                  strcpy ( symbol, strtab+stab[ ELF32_R_SYM(info)].st_name );
                  if (verb) fprintf ( stderr, "`%s'  ", symbol );
                  S = (Elf32_Addr)lookupObjName ( symbol );
               }
               if (verb) fprintf ( stderr, "resolves to %p\n", (void*)S );
               if (!S) {
                  fprintf ( stderr, "link failure for `%s'\n",
                                    strtab+stab[ ELF32_R_SYM(info)].st_name );
                  assert(0);
               }
	    }
            //fprintf ( stderr, "Reloc: P = %p   S = %p   A = %p\n\n",
            //      (void*)P, (void*)S, (void*)A );
            switch (ELF32_R_TYPE(info)) {
               case R_386_32:   *pP = S + A;     break;
               case R_386_PC32: *pP = S + A - P; break;
               default: fprintf(stderr, 
                                "unhandled ELF relocation type %d\n",
                                ELF32_R_TYPE(info));
                        assert(0);
	    }

         }
      }
      else
      if (shdr[i].sh_type == SHT_RELA) {
         fprintf ( stderr, "RelA style reloc table -- not yet done" );
         assert(0);
      }
   }
}


static Bool local validateOImage_elf ( void*  imgV, 
                                       Int    size, 
                                       Bool   verb )
{
   Elf32_Shdr* shdr;
   Elf32_Sym*  stab;
   int i, j, nent, nstrtab, nsymtabs;
   char* sh_strtab;
   char* strtab;

   char* ehdrC = (char*)imgV;
   Elf32_Ehdr* ehdr = ( Elf32_Ehdr*)ehdrC;

   if (ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
       ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
       ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
       ehdr->e_ident[EI_MAG3] != ELFMAG3) {
      if (verb) fprintf ( stderr, "Not an ELF header\n" ); 
      return FALSE;
   }
   if (verb) fprintf ( stderr, "Is an ELF header\n" );

   if (ehdr->e_ident[EI_CLASS] != ELFCLASS32) {
      if (verb) fprintf ( stderr, "Not 32 bit ELF\n" );
      return FALSE;
   }
   if (verb) fprintf ( stderr, "Is 32 bit ELF\n" );

   if (ehdr->e_ident[EI_DATA] == ELFDATA2LSB) {
      if (verb) fprintf ( stderr, "Is little-endian\n" );
   } else
   if (ehdr->e_ident[EI_DATA] == ELFDATA2MSB) {
      if (verb) fprintf ( stderr, "Is big-endian\n" );
   } else {
      if (verb) fprintf ( stderr, "Unknown endiannness\n" );
      return FALSE;
   }

   if (ehdr->e_type != ET_REL) {
      if (verb) fprintf ( stderr, "Not a relocatable object (.o) file\n" );
      return FALSE;
   }
   if (verb) fprintf ( stderr, "Is a relocatable object (.o) file\n" );

   if (verb) fprintf ( stderr, "Architecture is " );
   switch (ehdr->e_machine) {
      case EM_386:   if (verb) fprintf ( stderr, "x86\n" ); break;
      case EM_SPARC: if (verb) fprintf ( stderr, "sparc\n" ); break;
      default:       if (verb) fprintf ( stderr, "unknown\n" ); return FALSE;
   }

   if (verb) 
   fprintf ( stderr,
             "\nSection header table: start %d, n_entries %d, ent_size %d\n", 
             ehdr->e_shoff, ehdr->e_shnum, ehdr->e_shentsize  );

   assert (ehdr->e_shentsize == sizeof(Elf32_Shdr));

   shdr = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);

   if (ehdr->e_shstrndx == SHN_UNDEF) {
      if (verb) fprintf ( stderr, "No section header string table\n" );
      sh_strtab = NULL;
      return FALSE;
   } else {
      if (verb) fprintf (  stderr,"Section header string table is section %d\n", 
                          ehdr->e_shstrndx);
      sh_strtab = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;
   }

   for (i = 0; i < ehdr->e_shnum; i++) {
      if (verb) fprintf ( stderr, "%2d:  ", i );
      if (verb) fprintf ( stderr, "type=%2d  ", shdr[i].sh_type );
      if (verb) fprintf ( stderr, "size=%4d  ", shdr[i].sh_size );
      if (verb) fprintf ( stderr, "offs=%4d  ", shdr[i].sh_offset );
      if (verb) fprintf ( stderr, "  (%p .. %p)  ",
               ehdrC + shdr[i].sh_offset, 
               ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1);

      if (shdr[i].sh_type == SHT_REL  && verb) fprintf ( stderr, "Rel  " ); else
      if (shdr[i].sh_type == SHT_RELA && verb) fprintf ( stderr, "RelA " ); else
      if (verb)                                fprintf ( stderr, "     " );
      if (sh_strtab && verb) fprintf ( stderr, "sname=%s", sh_strtab + shdr[i].sh_name );
      if (verb) fprintf ( stderr, "\n" );
   }

   if (verb) fprintf ( stderr, "\n\nString tables\n" );
   strtab = NULL;
   nstrtab = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB &&
          i !=  ehdr->e_shstrndx) {
         if (verb) fprintf ( stderr, "   section %d is a normal string table\n", i );
         strtab = ehdrC + shdr[i].sh_offset;
         nstrtab++;
      }
   }  
   if (nstrtab != 1) {
      if (verb) fprintf ( stderr, "WARNING: no string tables, or too many\n" );
      return FALSE;
   }

   nsymtabs = 0;
   if (verb) fprintf ( stderr, "\n\nSymbol tables\n" ); 
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type != SHT_SYMTAB) continue;
      if (verb) fprintf ( stderr, "section %d is a symbol table\n", i );
      nsymtabs++;
      stab = (Elf32_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf32_Sym);
      if (verb) fprintf ( stderr, "   number of entries is apparently %d (%d rem)\n",
               nent,
               shdr[i].sh_size % sizeof(Elf32_Sym)
             );
      if (0 != shdr[i].sh_size % sizeof(Elf32_Sym)) {
         if (verb) fprintf ( stderr, "non-integral number of symbol table entries\n");
         return FALSE;
      }
      for (j = 0; j < nent; j++) {
         if (verb) fprintf ( stderr, "   %2d  ", j );
         if (verb) fprintf ( stderr, "  sec=%-5d  size=%-3d  val=%-5p  ", 
                             (int)stab[j].st_shndx,
                             (int)stab[j].st_size,
                             (char*)stab[j].st_value );

         if (verb) fprintf ( stderr, "type=" );
         switch (ELF32_ST_TYPE(stab[j].st_info)) {
            case STT_NOTYPE:  if (verb) fprintf ( stderr, "notype " ); break;
            case STT_OBJECT:  if (verb) fprintf ( stderr, "object " ); break;
            case STT_FUNC  :  if (verb) fprintf ( stderr, "func   " ); break;
            case STT_SECTION: if (verb) fprintf ( stderr, "section" ); break;
            case STT_FILE:    if (verb) fprintf ( stderr, "file   " ); break;
            default:          if (verb) fprintf ( stderr, "?      " ); break;
         }
         if (verb) fprintf ( stderr, "  " );

         if (verb) fprintf ( stderr, "bind=" );
         switch (ELF32_ST_BIND(stab[j].st_info)) {
            case STB_LOCAL :  if (verb) fprintf ( stderr, "local " ); break;
            case STB_GLOBAL:  if (verb) fprintf ( stderr, "global" ); break;
            case STB_WEAK  :  if (verb) fprintf ( stderr, "weak  " ); break;
            default:          if (verb) fprintf ( stderr, "?     " ); break;
         }
         if (verb) fprintf ( stderr, "  " );

         if (verb) fprintf ( stderr, "name=%s\n", strtab + stab[j].st_name );
      }
   }

   if (nsymtabs == 0) {
      if (verb) fprintf ( stderr, "Didn't find any symbol tables\n" );
      return FALSE;
   }

   return TRUE;
}


static void readSyms_elf ( Module m )
{
   int i, j, k, nent;
   Elf32_Sym* stab;

   char*       ehdrC      = (char*)(module(m).oImage);
   Elf32_Ehdr* ehdr       = (Elf32_Ehdr*)ehdrC;
   char*       strtab     = findElfSection ( ehdrC, SHT_STRTAB );
   Elf32_Shdr* shdr       = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);
   char*       sh_strtab  = ehdrC + shdr[ehdr->e_shstrndx].sh_offset;

   if (!strtab) internal("readSyms_elf");

   k = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {

      /* make a HugsDLSection entry for relevant sections */
      DLSect kind = HUGS_DL_SECTION_OTHER;
      if (0==strcmp(".data",sh_strtab+shdr[i].sh_name) ||
          0==strcmp(".data1",sh_strtab+shdr[i].sh_name))
         kind = HUGS_DL_SECTION_RWDATA;
      if (0==strcmp(".text",sh_strtab+shdr[i].sh_name) ||
          0==strcmp(".rodata",sh_strtab+shdr[i].sh_name) ||
          0==strcmp(".rodata1",sh_strtab+shdr[i].sh_name))
         kind = HUGS_DL_SECTION_CODE_OR_RODATA;
      if (kind != HUGS_DL_SECTION_OTHER)
         addDLSect (
            m,
            ehdrC + shdr[i].sh_offset, 
            ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1,
            kind
         );

      if (shdr[i].sh_type != SHT_SYMTAB) continue;

      /* copy stuff into this module's object symbol table */
      stab = (Elf32_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf32_Sym);
      for (j = 0; j < nent; j++) {
         if ( ( ELF32_ST_BIND(stab[j].st_info)==STB_GLOBAL ||
                ELF32_ST_BIND(stab[j].st_info)==STB_LOCAL
              )
              &&
              ( ELF32_ST_TYPE(stab[j].st_info)==STT_FUNC ||
                ELF32_ST_TYPE(stab[j].st_info)==STT_OBJECT )
	      ) {
            char* nm = strtab + stab[j].st_name;
            char* ad = ehdrC 
                       + shdr[ stab[j].st_shndx ].sh_offset
                       + stab[j].st_value;
            assert(nm);
            assert(ad);
            /* fprintf(stderr, "addOTabName: %s %s %p\n",
               textToStr(module(m).text), nm, ad );
            */
            addOTabName ( m, nm, ad );
         }
      }

   }
}


/* --------------------------------------------------------------------------
 * Arch-independent interface to the runtime linker
 * ------------------------------------------------------------------------*/

static Bool local validateOImage ( void* img, Int size, Bool verb )
{
   return
      validateOImage_elf ( img, size, verb );
}


static Void local resolveReferencesInObjectModule ( Module m, Bool verb )
{
   resolveReferencesInObjectModule_elf ( m, verb );
}


static Void local readSyms ( Module m )
{
   readSyms_elf ( m );
}


/* --------------------------------------------------------------------------
 * General object symbol query stuff
 * ------------------------------------------------------------------------*/

/* entirely bogus claims about types of these symbols */
extern int stg_gc_enter_1;
extern int stg_chk_0;
extern int stg_chk_1;
extern int stg_update_PAP;
extern int __ap_2_upd_info;
extern int MainRegTable;
extern int Upd_frame_info;

OSym rtsTab[] 
   = { 
       { "stg_gc_enter_1",    &stg_gc_enter_1  },
       { "stg_chk_0",         &stg_chk_0       },
       { "stg_chk_1",         &stg_chk_1       },
       { "stg_update_PAP",    &stg_update_PAP  },
       { "__ap_2_upd_info",   &__ap_2_upd_info },
       { "MainRegTable",      &MainRegTable    },
       { "Upd_frame_info",    &Upd_frame_info  },
       {0,0} 
     };


void* lookupObjName ( char* nm )
{
   int    k;
   char*  pp;
   void*  a;
   Text   t;
   Module m;
   char   nm2[200];

   nm2[199] = 0;
   strncpy(nm2,nm,200);

   // first see if it's an RTS name
   for (k = 0; rtsTab[k].nm; k++)
      if (0==strcmp(nm2,rtsTab[k].nm))
         return rtsTab[k].ad;

   // if not an RTS name, look in the 
   // relevant module's object symbol table
   pp = strchr(nm2, '_');
   if (!pp) goto not_found;
   *pp = 0;
   t = findText(nm2);
   m = findModule(t);
   if (isNull(m)) goto not_found;
   a = lookupOTabName ( m, nm );
   if (a) return a;

  not_found:
   fprintf ( stderr, 
             "lookupObjName: can't resolve name `%s'\n", 
             nm );
   return NULL;
}


int is_dynamically_loaded_code_or_rodata_ptr ( char* p )
{
   return 
      lookupDLSect(p) == HUGS_DL_SECTION_CODE_OR_RODATA;
}


int is_dynamically_loaded_rwdata_ptr ( char* p )
{
   return
      lookupDLSect(p) == HUGS_DL_SECTION_RWDATA;
}


int is_not_dynamically_loaded_ptr ( char* p )
{
   return
      lookupDLSect(p) == HUGS_DL_SECTION_OTHER;
}


/* --------------------------------------------------------------------------
 * Control:
 * ------------------------------------------------------------------------*/

Void interface(what)
Int what; {
    switch (what) {
    case INSTALL:
    case RESET: 
            ifImports           = NIL;
            ghcVarDecls         = NIL;     
            ghcConstrDecls      = NIL;     
            ghcSynonymDecls     = NIL;
            ghcClassDecls       = NIL;
            ghcInstanceDecls    = NIL;
            ghcExports          = NIL;
            ghcImports          = NIL;
            ghcModules          = NIL;
            break;
    case MARK: 
            mark(ifImports);
            mark(ghcVarDecls);     
            mark(ghcConstrDecls);     
            mark(ghcSynonymDecls); 
            mark(ghcClassDecls); 
            mark(ghcInstanceDecls);
            mark(ghcImports);
            mark(ghcExports);
            mark(ghcModules);
            break;
    }
}

/*-------------------------------------------------------------------------*/


/* --------------------------------------------------------------------------
 * GHC interface file processing for Hugs
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: interface.c,v $
 * $Revision: 1.12 $
 * $Date: 1999/12/16 16:42:56 $
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
#include "Assembler.h"  /* for wrapping GHC objects */
#include "dynamic.h"

#define DEBUG_IFACE
#define VERBOSE FALSE

extern void print ( Cell, Int );

/* --------------------------------------------------------------------------
 * (This comment is now out of date.  JRS, 991216).
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


/*
New comment, 991216, explaining roughly how it all works.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Interfaces can contain references to unboxed types, and these need to
be handled carefully.  The following is a summary of how the interface
loader now works.  It is applied to groups of interfaces simultaneously,
viz, the entire Prelude at once:

0.  Parse interfaces, chasing imports until a complete
    strongly-connected-component of ifaces has been parsed.
    All interfaces in this scc are processed together, in
    steps 1 .. 8 below.

1.  Throw away any entity not mentioned in the export lists.

2.  Delete type (not data or newtype) definitions which refer to 
    unknown types in their right hand sides.  Because Hugs doesn't
    know of any unboxed types, this has the side effect of removing
    all type defns referring to unboxed types.  Repeat step 2 until
    a fixed point is reached.

3.  Make abstract all data/newtype defns which refer to an unknown
    type.  eg, data Word = MkW Word# becomes data Word, because 
    Word# is unknown.  Hugs is happy to know about abstract boxed
    Words, but not about Word#s.

4.  Step 2 could delete types referred to by values, instances and
    classes.  So filter all entities, and delete those referring to
    unknown types _or_ classes.  This could cause other entities
    to become invalid, so iterate step 4 to a fixed point.

    After step 4, the interfaces no longer contain anything
    unpalatable to Hugs.

5.  Steps 1-4 operate purely on the iface syntax trees.  We now start
    creating symbol table entries.  First, create a module table
    entry for each interface, and locate and read in the corresponding
    object file.  This is done by the startGHCModule function.

6.  Traverse all interfaces.  For each entity, create an entry in
    the name, tycon, class or instance table, and fill in relevant
    fields, but do not attempt to link tycon/class/instance/name uses
    to their symbol table entries.  This is done by the startGHC*
    functions.

7.  Revisit all symbol table entries created in step 6.  We should
    now be able to replace all references to tycons/classes/instances/
    names with the relevant symbol table entries.  This is done by
    the finishGHC* functions.

8.  Traverse all interfaces.  For each iface, examine the export lists
    and use it to build export lists in the module table.  Do the
    implicit 'import Prelude' thing if necessary.  Finally, resolve
    references in the object code for this module.  This is done
    by the finishGHCModule function.
*/

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Void startGHCValue       Args((Int,VarId,Type));
static Void finishGHCValue      Args((VarId));

static Void startGHCSynonym     Args((Int,Cell,List,Type));
static Void finishGHCSynonym    Args((Tycon)); 

static Void startGHCClass       Args((Int,List,Cell,List,List));
static Void finishGHCClass      Args((Class)); 

static Inst startGHCInstance    Args((Int,List,Pair,VarId));
static Void finishGHCInstance   Args((Inst));

static Void startGHCImports     Args((ConId,List));
static Void finishGHCImports    Args((ConId,List));

static Void startGHCExports     Args((ConId,List));
static Void finishGHCExports    Args((ConId,List));

static Void finishGHCModule     Args((Cell));
static Void startGHCModule      Args((Text, Int, Text));

static Void startGHCDataDecl    Args((Int,List,Cell,List,List));
static Void finishGHCDataDecl   ( ConId tyc );

static Void startGHCNewType     Args((Int,List,Cell,List,Cell));
static Void finishGHCNewType    ( ConId tyc );


/* Supporting stuff for {start|finish}GHCDataDecl */
static List startGHCConstrs Args((Int,List,List));
static Name startGHCSel     Args((Int,Pair));
static Name startGHCConstr  Args((Int,Int,Triple));



static Kinds tvsToKind             Args((List));
static Int   arityFromType         Args((Type));
static Int   arityInclDictParams   Args((Type));
static Bool  allTypesKnown ( Type type, List aktys /* [QualId] */, ConId thisMod );
                                         
static List       ifTyvarsIn       Args((Type));

static Type       tvsToOffsets       Args((Int,Type,List));
static Type       conidcellsToTycons Args((Int,Type));

static Void       resolveReferencesInObjectModule Args((Module,Bool));
static Bool       validateOImage Args((void*, Int, Bool));
static Void       readSyms Args((Module,Bool));

static void*      lookupObjName ( char* );





/* --------------------------------------------------------------------------
 * Top-level interface processing
 * ------------------------------------------------------------------------*/

/* getIEntityName :: I_IMPORT..I_VALUE -> ConVarId | NIL */
ConVarId getIEntityName ( Cell c )
{
   switch (whatIs(c)) {
      case I_IMPORT:     return NIL;
      case I_INSTIMPORT: return NIL;
      case I_EXPORT:     return NIL;
      case I_FIXDECL:    return zthd3(unap(I_FIXDECL,c));
      case I_INSTANCE:   return NIL;
      case I_TYPE:       return zsel24(unap(I_TYPE,c));
      case I_DATA:       return zsel35(unap(I_DATA,c));
      case I_NEWTYPE:    return zsel35(unap(I_NEWTYPE,c));
      case I_CLASS:      return zsel35(unap(I_CLASS,c));
      case I_VALUE:      return zsnd3(unap(I_VALUE,c));
      default:           internal("getIEntityName");
   }
}


/* Filter the contents of an interface, using the supplied predicate.
   For flexibility, the predicate is passed as a second arg the value
   extraArgs.  This is a hack to get round the lack of partial applications
   in C.  Pred should not have any side effects.  The dumpaction param
   gives us the chance to print a message or some such for dumped items.
   When a named entity is deleted, filterInterface also deletes the name
   in the export lists.
*/
Cell filterInterface ( Cell root, 
                       Bool (*pred)(Cell,Cell), 
                       Cell extraArgs,
                       Void (*dumpAction)(Cell) )
{
   List tops;
   Cell iface       = unap(I_INTERFACE,root);
   List tops2       = NIL;
   List deleted_ids = NIL; /* :: [ConVarId] */

   for (tops = zsnd(iface); nonNull(tops); tops=tl(tops)) {
      if (pred(hd(tops),extraArgs)) {
         tops2 = cons( hd(tops), tops2 );
      } else {
         ConVarId deleted_id = getIEntityName ( hd(tops) );
         if (nonNull(deleted_id))
            deleted_ids = cons ( deleted_id, deleted_ids );
         if (dumpAction)
            dumpAction ( hd(tops) );
      }
   }
   tops2 = reverse(tops2);

   /* Clean up the export list now. */
   for (tops=tops2; nonNull(tops); tops=tl(tops)) {
      if (whatIs(hd(tops))==I_EXPORT) {
         Cell exdecl  = unap(I_EXPORT,hd(tops));
         List exlist  = zsnd(exdecl);
         List exlist2 = NIL;
         for (; nonNull(exlist); exlist=tl(exlist)) {
            Cell ex       = hd(exlist);
            ConVarId exid = isZPair(ex) ? zfst(ex) : ex;
            assert (isCon(exid) || isVar(exid));
            if (!varIsMember(textOf(exid),deleted_ids))
               exlist2 = cons(ex, exlist2);
	 }
         hd(tops) = ap(I_EXPORT,zpair(zfst(exdecl),exlist2));
      }
   }

   return ap(I_INTERFACE, zpair(zfst(iface),tops2));
}


ZPair readInterface(String fname, Long fileSize)
{
    List  tops;
    List  imports = NIL;
    ZPair iface   = parseInterface(fname,fileSize);
    assert (whatIs(iface)==I_INTERFACE);

    for (tops = zsnd(unap(I_INTERFACE,iface)); nonNull(tops); tops=tl(tops))
       if (whatIs(hd(tops)) == I_IMPORT) {
          ZPair imp_decl = unap(I_IMPORT,hd(tops));
          ConId m_to_imp = zfst(imp_decl);
          if (textOf(m_to_imp) != findText("PrelGHC")) {
             imports = cons(m_to_imp,imports);
             /* fprintf(stderr, "add iface %s\n", textToStr(textOf(m_to_imp))); */
          }
       }
    return zpair(iface,imports);
}


/* getExportDeclsInIFace :: I_INTERFACE -> [I_EXPORT] */
static List getExportDeclsInIFace ( Cell root )
{
   Cell  iface   = unap(I_INTERFACE,root);
   List  decls   = zsnd(iface);
   List  exports = NIL;
   List  ds;
   for (ds=decls; nonNull(ds); ds=tl(ds))
      if (whatIs(hd(ds))==I_EXPORT)
         exports = cons(hd(ds), exports);
   return exports;
}



static Bool isExportedIFaceEntity ( Cell ife, List exlist_list )
{
   /* ife         :: I_IMPORT..I_VALUE                      */
   /* exlist_list :: [[ ConVarId | ((ConId, [ConVarId])) ]] */
   Text  tnm;
   List  exlist;
   List  t;

   ConVarId ife_id = getIEntityName ( ife );

   if (isNull(ife_id)) return TRUE;

   tnm = textOf(ife_id);

   /* for each export list ... */
   for (; nonNull(exlist_list); exlist_list=tl(exlist_list)) {
      exlist = hd(exlist_list);

      /* for each entity in an export list ... */
      for (t=exlist; nonNull(t); t=tl(t)) {
         if (isZPair(hd(t))) {
            /* A pair, which means an export entry 
               of the form ClassName(foo,bar). */
            List subents = cons(zfst(hd(t)),zsnd(hd(t)));
            for (; nonNull(subents); subents=tl(subents))
               if (textOf(hd(subents)) == tnm) goto retain;
         } else {
            /* Single name in the list. */
            if (textOf(hd(t)) == tnm) goto retain;
         }
      }

   }
   fprintf ( stderr, "     dump %s\n", textToStr(tnm) );
   return FALSE;

 retain:
   fprintf ( stderr, "   retain %s\n", textToStr(tnm) );
   return TRUE;
}


static Bool isExportedAbstractly ( ConId ife_id, List exlist_list )
{
   /* ife_id      :: ConId                                  */
   /* exlist_list :: [[ ConVarId | ((ConId, [ConVarId])) ]] */
   Text  tnm;
   List  exlist;
   List  t;

   assert (isCon(ife_id));
   tnm = textOf(ife_id);

   /* for each export list ... */
   for (; nonNull(exlist_list); exlist_list=tl(exlist_list)) {
      exlist = hd(exlist_list);

      /* for each entity in an export list ... */
      for (t=exlist; nonNull(t); t=tl(t)) {
         if (isZPair(hd(t))) {
            /* A pair, which means an export entry 
               of the form ClassName(foo,bar). */
            if (textOf(zfst(hd(t))) == tnm) return FALSE;
         } else {
            if (textOf(hd(t)) == tnm) return TRUE;
         }
      }
   }
   internal("isExportedAbstractly");
   return FALSE; /*notreached*/
}


/* Remove entities not mentioned in any of the export lists. */
static Cell deleteUnexportedIFaceEntities ( Cell root )
{
   Cell  iface       = unap(I_INTERFACE,root);
   ConId iname       = zfst(iface);
   List  decls       = zsnd(iface);
   List  decls2      = NIL;
   List  exlist_list = NIL;
   List  t;

   fprintf(stderr, "\ncleanIFace: %s\n", textToStr(textOf(iname)));

   exlist_list = getExportDeclsInIFace ( root );
   /* exlist_list :: [I_EXPORT] */
   
   for (t=exlist_list; nonNull(t); t=tl(t))
      hd(t) = zsnd(unap(I_EXPORT,hd(t)));
   /* exlist_list :: [[ ConVarId | ((ConId, [ConVarId])) ]] */

   if (isNull(exlist_list)) {
      ERRMSG(0) "Can't find any export lists in interface file"
      EEND;
   }

   return filterInterface ( root, isExportedIFaceEntity, 
                            exlist_list, NULL );
}


/* addTyconsAndClassesFromIFace :: I_INTERFACE -> [QualId] -> [QualId] */
List addTyconsAndClassesFromIFace ( Cell root, List aktys )
{
   Cell iface = unap(I_INTERFACE,root);
   Text mname = textOf(zfst(iface));
   List defns = zsnd(iface);
   for (; nonNull(defns); defns = tl(defns)) {
      Cell defn = hd(defns);
      Cell what = whatIs(defn);
      if (what==I_TYPE || what==I_DATA 
          || what==I_NEWTYPE || what==I_CLASS) {
         QualId q = mkQCon ( mname, textOf(getIEntityName(defn)) );
         if (!qualidIsMember ( q, aktys ))
            aktys = cons ( q, aktys );
      }
   }
   return aktys;
}


Void ifentityAllTypesKnown_dumpmsg ( Cell entity )
{
   ConVarId id = getIEntityName ( entity );
   fprintf ( stderr, 
             "dumping %s because of unknown type(s)\n",
             isNull(id) ? "(nameless entity?!)" : textToStr(textOf(id)) );
}

/* ifentityAllTypesKnown :: I_IMPORT..I_VALUE -> (([QualId], ConId)) -> Bool */
/* mod is the current module being processed -- so we can qualify unqual'd
   names.  Strange calling convention for aktys and mod is so we can call this
   from filterInterface.
*/
Bool ifentityAllTypesKnown ( Cell entity, ZPair aktys_mod )
{
   List  t, u;
   List  aktys = zfst ( aktys_mod );
   ConId mod   = zsnd ( aktys_mod );
   switch (whatIs(entity)) {
      case I_IMPORT:
      case I_INSTIMPORT:
      case I_EXPORT:
      case I_FIXDECL: 
         return TRUE;
      case I_INSTANCE: {
         Cell inst = unap(I_INSTANCE,entity);
         List ctx  = zsel25 ( inst ); /* :: [((QConId,VarId))] */
         Type cls  = zsel35 ( inst ); /* :: Type */
         for (t = ctx; nonNull(t); t=tl(t))
            if (!allTypesKnown(zfst(hd(t)),aktys,mod)) return FALSE;
         if (!allTypesKnown(cls, aktys,mod)) return FALSE;
         return TRUE;
      }
      case I_TYPE:
         return allTypesKnown( zsel44(unap(I_TYPE,entity)), aktys,mod );
      case I_DATA: {
         Cell data    = unap(I_DATA,entity);
         List ctx     = zsel25 ( data ); /* :: [((QConId,VarId))] */
         List constrs = zsel55 ( data ); /* :: [ ((ConId, [((Type,VarId,Int))] )) ] */
         for (t = ctx; nonNull(t); t=tl(t))
            if (!allTypesKnown(zfst(hd(t)),aktys,mod)) return FALSE;
         for (t = constrs; nonNull(t); t=tl(t))
            for (u = zsnd(hd(t)); nonNull(u); u=tl(u))
               if (!allTypesKnown(zfst3(hd(u)),aktys,mod)) return FALSE;
         return TRUE;
      }
      case I_NEWTYPE: {
         Cell  newty  = unap(I_NEWTYPE,entity);
         List  ctx    = zsel25(newty);    /* :: [((QConId,VarId))] */
         ZPair constr = zsel55 ( newty ); /* :: ((ConId,Type)) */
         for (t = ctx; nonNull(t); t=tl(t))
            if (!allTypesKnown(zfst(hd(t)),aktys,mod)) return FALSE;
         if (nonNull(constr)
             && !allTypesKnown(zsnd(constr),aktys,mod)) return FALSE;
         return TRUE;
      }
      case I_CLASS: {
         Cell klass = unap(I_CLASS,entity);
         List ctx   = zsel25(klass);  /* :: [((QConId,VarId))] */
         List sigs  = zsel55(klass);  /* :: [((VarId,Type))] */
         for (t = ctx; nonNull(t); t=tl(t))
            if (!allTypesKnown(zfst(hd(t)),aktys,mod)) return FALSE;
         for (t = sigs; nonNull(t); t=tl(t)) 
            if (!allTypesKnown(zsnd(hd(t)),aktys,mod)) return FALSE;
         return TRUE;
      }
      case I_VALUE: 
         return allTypesKnown( zthd3(unap(I_VALUE,entity)), aktys,mod );
      default: 
         internal("ifentityAllTypesKnown");
   }
}


#if 0
I hope this can be nuked.
/* Kludge.  Stuff imported from PrelGHC isn't referred to in a 
   qualified way, so arrange it so it is.
*/
QualId magicRequalify ( ConId id )
{
   Text tid;
   Text tmid;
   assert(isCon(id));
   tid = textOf(id);

   fprintf ( stderr, "$--$--$--$--$--$ magicRequalify: %s",
             textToStr(tid) );

   if (tid == findText("[]")) {
      tmid = findText("PrelList");
   } else 
   if (tid == findText("Ratio")) {
      tmid = findText("PrelNum");
   } else
   if (tid == findText("Char")) {
      tmid = findText("PrelGHC");
   } else {
      fprintf(stderr, "??? \n");
      return id;
   }

   fprintf ( stderr, " -> %s.%s\n",
             textToStr(tmid), textToStr(tid) );
   return mkQualId ( mkCon(tmid), id );
}
#endif


/* ifTypeDoesntRefUnknownTycon :: I_IMPORT..I_VALUE -> (([QualId], ConId)) -> Bool */
/* mod is the current module being processed -- so we can qualify unqual'd
   names.  Strange calling convention for aktys and mod is so we can call this
   from filterInterface.
*/
Bool ifTypeDoesntRefUnknownTycon ( Cell entity, ZPair aktys_mod )
{
   List  t, u;
   List  aktys = zfst ( aktys_mod );
   ConId mod   = zsnd ( aktys_mod );
   if (whatIs(entity) != I_TYPE) {
      return TRUE;
   } else {
      return allTypesKnown( zsel44(unap(I_TYPE,entity)), aktys,mod );
   }
}

Void ifTypeDoesntRefUnknownTycon_dumpmsg ( Cell entity )
{
   ConVarId id = getIEntityName ( entity );
   assert (whatIs(entity)==I_TYPE);
   assert (isCon(id));
   fprintf ( stderr, 
             "dumping type %s because of unknown tycon(s)\n",
             textToStr(textOf(id)) );
}


/* abstractifyExport :: I_EXPORT -> ConId -> I_EXPORT
*/
List abstractifyExDecl ( Cell root, ConId toabs )
{
   ZPair exdecl = unap(I_EXPORT,root);
   List  exlist = zsnd(exdecl);
   List  res    = NIL;
   for (; nonNull(exlist); exlist = tl(exlist)) {
      if (isZPair(hd(exlist)) 
          && textOf(toabs) == textOf(zfst(hd(exlist)))) {
         /* it's toabs, exported non-abstractly */
         res = cons ( zfst(hd(exlist)), res );
      } else {
         res = cons ( hd(exlist), res );
      }
   }
   return ap(I_EXPORT,zpair(zfst(exdecl),reverse(res)));
}


Void ppModule ( Text modt )
{
   fflush(stderr); fflush(stdout);
   fprintf(stderr, "---------------- MODULE %s ----------------\n", 
                   textToStr(modt) );
}


/* ifaces_outstanding holds a list of parsed interfaces
   for which we need to load objects and create symbol
   table entries.
*/
Void processInterfaces ( void )
{
    List    tmp;
    List    xs;
    ZTriple tr;
    Cell    iface;
    Int     sizeObj;
    Text    nameObj;
    Text    mname;
    List    decls;
    Module  mod;
    List    all_known_types;
    Int     num_known_types;

    List ifaces       = NIL;  /* :: List I_INTERFACE */
    List iface_sizes  = NIL;  /* :: List Int         */
    List iface_onames = NIL;  /* :: List Text        */

    fprintf ( stderr, 
              "processInterfaces: %d interfaces to process\n", 
              length(ifaces_outstanding) );


    /* unzip3 ifaces_outstanding into ifaces, iface_sizes, iface_onames */
    for (xs = ifaces_outstanding; nonNull(xs); xs=tl(xs)) {
       ifaces       = cons ( zfst3(hd(xs)), ifaces       );
       iface_onames = cons ( zsnd3(hd(xs)), iface_onames );
       iface_sizes  = cons ( zthd3(hd(xs)), iface_sizes  );
    }

    ifaces       = reverse(ifaces);
    iface_onames = reverse(iface_onames);
    iface_sizes  = reverse(iface_sizes);

    /* Clean up interfaces -- dump non-exported value, class, type decls */
    for (xs = ifaces; nonNull(xs); xs = tl(xs))
       hd(xs) = deleteUnexportedIFaceEntities(hd(xs));


    /* Iteratively delete any type declarations which refer to unknown
       tycons. 
    */
    num_known_types = 999999999;
    while (TRUE) {
       Int i;

       /* Construct a list of all known tycons.  This is a list of QualIds. 
          Unfortunately it also has to contain all known class names, since
          allTypesKnown cannot distinguish between tycons and classes -- a
          deficiency of the iface abs syntax.
       */
       all_known_types = getAllKnownTyconsAndClasses();
       for (xs = ifaces; nonNull(xs); xs=tl(xs))
          all_known_types = addTyconsAndClassesFromIFace ( hd(xs), all_known_types );

       /* Have we reached a fixed point? */
       i = length(all_known_types);
       printf ( "\n============= %d known types =============\n", i );
       if (num_known_types == i) break;
       num_known_types = i;

       /* Delete all entities which refer to unknown tycons. */
       for (xs = ifaces; nonNull(xs); xs = tl(xs)) {
          ConId mod = zfst(unap(I_INTERFACE,hd(xs)));
          assert(nonNull(mod));
          hd(xs) = filterInterface ( hd(xs), 
                                     ifTypeDoesntRefUnknownTycon,
                                     zpair(all_known_types,mod),
                                     ifTypeDoesntRefUnknownTycon_dumpmsg );
       }
    }

    /* Now abstractify any datas and newtypes which refer to unknown tycons
       -- including, of course, the type decls just deleted.
    */
    for (xs = ifaces; nonNull(xs); xs = tl(xs)) {
       List  absify = NIL;                      /* :: [ConId] */
       ZPair iface  = unap(I_INTERFACE,hd(xs)); /* ((ConId, [I_IMPORT..I_VALUE])) */
       ConId mod    = zfst(iface);
       List  aktys  = all_known_types;          /* just a renaming */
       List  es,t,u;
       List  exlist_list;

       /* Compute into absify the list of all ConIds (tycons) we need to
          abstractify. 
       */
       for (es = zsnd(iface); nonNull(es); es=tl(es)) {
          Cell ent      = hd(es);
          Bool allKnown = TRUE;

          if (whatIs(ent)==I_DATA) {
             Cell data    = unap(I_DATA,ent);
             List ctx     = zsel25 ( data ); /* :: [((QConId,VarId))] */
             List constrs = zsel55 ( data ); /* :: [ ((ConId, [((Type,VarId,Int))] )) ] */
             for (t = ctx; nonNull(t); t=tl(t))
                if (!allTypesKnown(zfst(hd(t)),aktys,mod)) allKnown = FALSE;
             for (t = constrs; nonNull(t); t=tl(t))
                for (u = zsnd(hd(t)); nonNull(u); u=tl(u))
                    if (!allTypesKnown(zfst3(hd(u)),aktys,mod)) allKnown = FALSE;          
          }
          else if (whatIs(ent)==I_NEWTYPE) {
             Cell  newty  = unap(I_NEWTYPE,ent);
             List  ctx    = zsel25(newty);    /* :: [((QConId,VarId))] */
             ZPair constr = zsel55 ( newty ); /* :: ((ConId,Type)) */
             for (t = ctx; nonNull(t); t=tl(t))
                if (!allTypesKnown(zfst(hd(t)),aktys,mod)) allKnown = FALSE;
             if (!allTypesKnown(zsnd(constr),aktys,mod)) allKnown = FALSE;
          }

          if (!allKnown) {
             absify = cons ( getIEntityName(ent), absify );
             fprintf ( stderr, 
                       "abstractifying %s because it uses an unknown type\n",
                       textToStr(textOf(getIEntityName(ent))) );
          }
       }

       /* mark in exports as abstract all names in absify (modifies iface) */
       for (; nonNull(absify); absify=tl(absify)) {
          ConId toAbs = hd(absify);
          for (es = zsnd(iface); nonNull(es); es=tl(es)) {
             if (whatIs(hd(es)) != I_EXPORT) continue;
             hd(es) = abstractifyExDecl ( hd(es), toAbs );
          }
       }

       /* For each data/newtype in the export list marked as abstract,
          remove the constructor lists.  This catches all abstractification
          caused by the code above, and it also catches tycons which really
          were exported abstractly.
       */

       exlist_list = getExportDeclsInIFace ( ap(I_INTERFACE,iface) );
       /* exlist_list :: [I_EXPORT] */
       for (t=exlist_list; nonNull(t); t=tl(t))
          hd(t) = zsnd(unap(I_EXPORT,hd(t)));
       /* exlist_list :: [[ ConVarId | ((ConId, [ConVarId])) ]] */

       for (es = zsnd(iface); nonNull(es); es=tl(es)) {
          Cell ent = hd(es);
          if (whatIs(ent)==I_DATA
              && isExportedAbstractly ( getIEntityName(ent),
                                        exlist_list )) {
             Cell data = unap(I_DATA,ent);
             data = z5ble ( zsel15(data), zsel25(data), zsel35(data),
                            zsel45(data), NIL /* the constr list */ );
             hd(es) = ap(I_DATA,data);
fprintf(stderr, "abstractify data %s\n", textToStr(textOf(getIEntityName(ent))) );
	  }
          else if (whatIs(ent)==I_NEWTYPE
              && isExportedAbstractly ( getIEntityName(ent), 
                                        exlist_list )) {
             Cell data = unap(I_NEWTYPE,ent);
             data = z5ble ( zsel15(data), zsel25(data), zsel35(data),
                            zsel45(data), NIL /* the constr-type pair */ );
             hd(es) = ap(I_NEWTYPE,data);
fprintf(stderr, "abstractify newtype %s\n", textToStr(textOf(getIEntityName(ent))) );
          }
       }

       /* We've finally finished mashing this iface.  Update the iface list. */
       hd(xs) = ap(I_INTERFACE,iface);
    }


    /* At this point, the interfaces are cleaned up so that no type, data or
       newtype defn refers to a non-existant type.  However, there still may
       be value defns, classes and instances which refer to unknown types.
       Delete iteratively until a fixed point is reached.
    */
printf("\n");

    num_known_types = 999999999;
    while (TRUE) {
       Int i;

       /* Construct a list of all known tycons.  This is a list of QualIds. 
          Unfortunately it also has to contain all known class names, since
          allTypesKnown cannot distinguish between tycons and classes -- a
          deficiency of the iface abs syntax.
       */
       all_known_types = getAllKnownTyconsAndClasses();
       for (xs = ifaces; nonNull(xs); xs=tl(xs))
          all_known_types = addTyconsAndClassesFromIFace ( hd(xs), all_known_types );

       /* Have we reached a fixed point? */
       i = length(all_known_types);
       printf ( "\n------------- %d known types -------------\n", i );
       if (num_known_types == i) break;
       num_known_types = i;

       /* Delete all entities which refer to unknown tycons. */
       for (xs = ifaces; nonNull(xs); xs = tl(xs)) {
          ConId mod = zfst(unap(I_INTERFACE,hd(xs)));
          assert(nonNull(mod));

          hd(xs) = filterInterface ( hd(xs),
                                     ifentityAllTypesKnown,
                                     zpair(all_known_types,mod), 
                                     ifentityAllTypesKnown_dumpmsg );
       }
    }


    /* Allocate module table entries and read in object code. */
    for (xs=ifaces; 
         nonNull(xs);
         xs=tl(xs), iface_sizes=tl(iface_sizes), iface_onames=tl(iface_onames)) {
       startGHCModule ( textOf(zfst(unap(I_INTERFACE,hd(xs)))),
                        intOf(hd(iface_sizes)),
                        hd(iface_onames) );
    }
    assert (isNull(iface_sizes));
    assert (isNull(iface_onames));


    /* Now work through the decl lists of the modules, and call the
       startGHC* functions on the entities.  This creates names in
       various tables but doesn't bind them to anything.
    */

    for (xs = ifaces; nonNull(xs); xs = tl(xs)) {
       iface   = unap(I_INTERFACE,hd(xs));
       mname   = textOf(zfst(iface));
       mod     = findModule(mname);
       if (isNull(mod)) internal("processInterfaces(4)");
       setCurrModule(mod);
       ppModule ( module(mod).text );

       for (decls = zsnd(iface); nonNull(decls); decls = tl(decls)) {
          Cell decl = hd(decls);
          switch(whatIs(decl)) {
             case I_EXPORT: {
                Cell exdecl = unap(I_EXPORT,decl);
                startGHCExports ( zfst(exdecl), zsnd(exdecl) );
                break;
             }
             case I_IMPORT: {
                Cell imdecl = unap(I_IMPORT,decl);
                startGHCImports ( zfst(imdecl), zsnd(imdecl) );
                break;
             }
             case I_FIXDECL: {
                break;
             }
             case I_INSTANCE: {
                /* Trying to find the instance table location allocated by
                   startGHCInstance in subsequent processing is a nightmare, so
                   cache it on the tree. 
                */
                Cell instance = unap(I_INSTANCE,decl);
                Inst in = startGHCInstance ( zsel15(instance), zsel25(instance),
                                             zsel35(instance), zsel45(instance) );
                hd(decls) = ap(I_INSTANCE,
                               z5ble( zsel15(instance), zsel25(instance),
                                      zsel35(instance), zsel45(instance), in ));
                break;
             }
             case I_TYPE: {
                Cell tydecl = unap(I_TYPE,decl);
                startGHCSynonym ( zsel14(tydecl), zsel24(tydecl),
                                  zsel34(tydecl), zsel44(tydecl) );
                break;
             }
             case I_DATA: {
                Cell ddecl = unap(I_DATA,decl);
                startGHCDataDecl ( zsel15(ddecl), zsel25(ddecl), 
                                   zsel35(ddecl), zsel45(ddecl), zsel55(ddecl) );
                break;
             }
             case I_NEWTYPE: {
                Cell ntdecl = unap(I_NEWTYPE,decl);
                startGHCNewType ( zsel15(ntdecl), zsel25(ntdecl), 
                                  zsel35(ntdecl), zsel45(ntdecl), 
                                  zsel55(ntdecl) );
                break;
             }
             case I_CLASS: {
                Cell klass = unap(I_CLASS,decl);
                startGHCClass ( zsel15(klass), zsel25(klass), 
                                zsel35(klass), zsel45(klass), 
                                zsel55(klass) );
                break;
             }
             case I_VALUE: {
                Cell value = unap(I_VALUE,decl);
                startGHCValue ( zfst3(value), zsnd3(value), zthd3(value) );
                break;
             }
             default:
                internal("processInterfaces(1)");
          }
       }       
    }

    fprintf(stderr, "\n=========================================================\n");
    fprintf(stderr, "=========================================================\n");

    /* Traverse again the decl lists of the modules, this time 
       calling the finishGHC* functions.  But don't process
       the export lists; those must wait for later.
    */
    for (xs = ifaces; nonNull(xs); xs = tl(xs)) {
       iface   = unap(I_INTERFACE,hd(xs));
       mname   = textOf(zfst(iface));
       mod     = findModule(mname);
       if (isNull(mod)) internal("processInterfaces(3)");
       setCurrModule(mod);
       ppModule ( module(mod).text );

       for (decls = zsnd(iface); nonNull(decls); decls = tl(decls)) {
          Cell decl = hd(decls);
          switch(whatIs(decl)) {
             case I_EXPORT: {
                break;
             }
             case I_IMPORT: {
                break;
             }
             case I_FIXDECL: {
                break;
             }
             case I_INSTANCE: {
                Cell instance = unap(I_INSTANCE,decl);
                finishGHCInstance ( zsel55(instance) );
                break;
             }
             case I_TYPE: {
                Cell tydecl = unap(I_TYPE,decl);
                finishGHCSynonym ( zsel24(tydecl) );
                break;
             }
             case I_DATA: {
                Cell ddecl = unap(I_DATA,decl);
                finishGHCDataDecl ( zsel35(ddecl) );
                break;
             }
             case I_NEWTYPE: {
                Cell ntdecl = unap(I_NEWTYPE,decl);
                finishGHCNewType ( zsel35(ntdecl) );
                break;
             }
             case I_CLASS: {
                Cell klass = unap(I_CLASS,decl);
                finishGHCClass ( zsel35(klass) );
                break;
             }
             case I_VALUE: {
                Cell value = unap(I_VALUE,decl);
                finishGHCValue ( zsnd3(value) );
                break;
             }
             default:
                internal("processInterfaces(2)");
          }
       }       
    }

    fprintf(stderr, "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
    fprintf(stderr, "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
    /* Build the module(m).export lists for each module, by running
       through the export lists in the iface.  Also, do the implicit
       'import Prelude' thing.  And finally, do the object code 
       linking.
    */
    for (xs = ifaces; nonNull(xs); xs = tl(xs))
       finishGHCModule(hd(xs));

    /* Finished! */
    ifaces_outstanding = NIL;
}


/* --------------------------------------------------------------------------
 * Modules
 * ------------------------------------------------------------------------*/

Void startGHCModule ( Text mname, Int sizeObj, Text nameObj )
{
    FILE* f;
    void* img;

    Module m = findModule(mname);
    if (isNull(m)) {
       m = newModule(mname);
       fprintf ( stderr, "startGHCIface: name %16s   objsize %d\n", 
                          textToStr(mname), sizeObj );
    } else {
       if (module(m).fake) {
          module(m).fake = FALSE;
       } else {
          ERRMSG(0) "Module \"%s\" already loaded", textToStr(mname)
          EEND;
       }
    }

    img = malloc ( sizeObj );
    if (!img) {
       ERRMSG(0) "Can't allocate memory to load object file for module \"%s\"",
                 textToStr(mname)
       EEND;
    }
    f = fopen( textToStr(nameObj), "rb" );
    if (!f) {
       /* Really, this shouldn't happen, since makeStackEntry ensures the
          object is available.  Nevertheless ...
       */
       ERRMSG(0) "Object file \"%s\" can't be opened to read -- oops!",
	         &(textToStr(nameObj)[0])
       EEND;
    }
    if (sizeObj != fread ( img, 1, sizeObj, f)) {
       ERRMSG(0) "Read of object file \"%s\" failed", textToStr(nameObj)
       EEND;
    }
    if (!validateOImage(img,sizeObj,VERBOSE)) {
       ERRMSG(0) "Validation of object file \"%s\" failed", 
                 textToStr(nameObj)
       EEND;
    }
    
    assert(!module(m).oImage);
    module(m).oImage = img;

    readSyms(m,VERBOSE);

    /* setCurrModule(m); */
}


/* For the module mod, augment both the export environment (.exports) 
   and the eval environment (.names, .tycons, .classes)
   with the symbols mentioned in exlist.  We don't actually need
   to modify the names, tycons, classes or instances in the eval 
   environment, since previous processing of the
   top-level decls in the iface should have done this already.

   mn is the module mentioned in the export list; it is the "original"
   module for the symbols in the export list.  We should also record
   this info with the symbols, since references to object code need to
   refer to the original module in which a symbol was defined, rather
   than to some module it has been imported into and then re-exported.

   We take the policy that if something mentioned in an export list
   can't be found in the symbol tables, it is simply ignored.  After all,
   previous processing of the iface syntax trees has already removed 
   everything which Hugs can't handle, so if there is mention of these
   things still lurking in export lists somewhere, about the only thing
   to do is to ignore it.

   Also do an implicit 'import Prelude' thingy for the module,
   if appropriate.
*/


Void finishGHCModule ( Cell root ) 
{
   /* root :: I_INTERFACE */
   Cell   iface       = unap(I_INTERFACE,root);
   ConId  iname       = zfst(iface);
   Module mod         = findModule(textOf(iname));
   List   exlist_list = NIL;
   List   t;

   fprintf(stderr, "begin finishGHCModule %s\n", textToStr(textOf(iname)));

   if (isNull(mod)) internal("finishExports(1)");
   setCurrModule(mod);

   exlist_list = getExportDeclsInIFace ( root );
   /* exlist_list :: [I_EXPORT] */
   
   for (; nonNull(exlist_list); exlist_list=tl(exlist_list)) {
      ZPair exdecl = unap(I_EXPORT,hd(exlist_list));
      ConId exmod  = zfst(exdecl);
      List  exlist = zsnd(exdecl);
      /* exlist :: [ ConVarId | ((ConId, [ConVarId])) ] */

      for (; nonNull(exlist); exlist=tl(exlist)) {
         Bool   abstract;
         List   subents;
         Cell   c;
         QualId q;
         Cell   ex = hd(exlist);

         switch (whatIs(ex)) {

            case VARIDCELL: /* variable */
               q = mkQualId(exmod,ex);
               c = findQualNameWithoutConsultingExportList ( q );
               if (isNull(c)) goto notfound;
               fprintf(stderr, "   var %s\n", textToStr(textOf(ex)) );
               module(mod).exports = cons(c, module(mod).exports);
               break;

            case CONIDCELL: /* non data tycon */
               q = mkQualId(exmod,ex);
               c = findQualTyconWithoutConsultingExportList ( q );
               if (isNull(c)) goto notfound;
               fprintf(stderr, "   type %s\n", textToStr(textOf(ex)) );
               module(mod).exports = cons(c, module(mod).exports);
               break;

            case ZTUP2: /* data T = C1 ... Cn  or class C where f1 ... fn */
               subents = zsnd(ex);  /* :: [ConVarId] */
               ex      = zfst(ex);  /* :: ConId */
               q       = mkQualId(exmod,ex);
               c       = findQualTyconWithoutConsultingExportList ( q );

               if (nonNull(c)) { /* data */
                  fprintf(stderr, "   data/newtype %s = { ", textToStr(textOf(ex)) );
                  assert(tycon(c).what == DATATYPE || tycon(c).what==NEWTYPE);
                  abstract = isNull(tycon(c).defn);
                  /* This data/newtype could be abstract even tho the export list
                     says to export it non-abstractly.  That happens if it was 
                     imported from some other module and is now being re-exported,
                     and previous cleanup phases have abstractified it in the 
                     original (defining) module.
		  */
                  if (abstract) {
                     module(mod).exports = cons ( ex, module(mod).exports );
                     fprintf ( stderr, "(abstract) ");
		  } else {
                     module(mod).exports = cons(pair(c,DOTDOT), module(mod).exports);
                     for (; nonNull(subents); subents = tl(subents)) {
                        Cell ent2 = hd(subents);
                        assert(isCon(ent2) || isVar(ent2)); 
                                              /* isVar since could be a field name */
                        q = mkQualId(exmod,ent2);
                        c = findQualNameWithoutConsultingExportList ( q );
                        fprintf(stderr, "%s ", textToStr(name(c).text));
                        assert(nonNull(c));
                        module(mod).exports = cons(c, module(mod).exports);
                     }
                  }
                  fprintf(stderr, "}\n" );
               } else { /* class */
                  q = mkQualId(exmod,ex);
                  c = findQualClassWithoutConsultingExportList ( q );
                  if (isNull(c)) goto notfound;
                  fprintf(stderr, "   class %s { ", textToStr(textOf(ex)) );
                  module(mod).exports = cons(pair(c,DOTDOT), module(mod).exports);
                  for (; nonNull(subents); subents = tl(subents)) {
                     Cell ent2 = hd(subents);
                     assert(isVar(ent2));
                     q = mkQualId(exmod,ent2);
                     c = findQualNameWithoutConsultingExportList ( q );
                     fprintf(stderr, "%s ", textToStr(name(c).text));
                     if (isNull(c)) goto notfound;
                     module(mod).exports = cons(c, module(mod).exports);
                  }
                  fprintf(stderr, "}\n" );
               }
               break;

            default:
               internal("finishExports(2)");

         } /* switch */
         continue;  /* so notfound: can be placed after this */
  
        notfound:
         /* q holds what ain't found */
         assert(whatIs(q)==QUALIDENT);
         fprintf( stderr, "   ------ IGNORED: %s.%s\n",
                  textToStr(qmodOf(q)), textToStr(qtextOf(q)) );
         continue;
      }
   }

   if (preludeLoaded) {
      /* do the implicit 'import Prelude' thing */
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
               internal("finishGHCModule -- implicit import Prelude");
               break;
         }
      }
   }

   /* Last, but by no means least ... */
   resolveReferencesInObjectModule ( mod, VERBOSE );
}


/* --------------------------------------------------------------------------
 * Exports
 * ------------------------------------------------------------------------*/

Void startGHCExports ( ConId mn, List exlist )
{
#   ifdef DEBUG_IFACE
    printf("startGHCExports %s\n", textToStr(textOf(mn)) );
#   endif
   /* Nothing to do. */
}

Void finishGHCExports ( ConId mn, List exlist )
{
#   ifdef DEBUG_IFACE
    printf("finishGHCExports %s\n", textToStr(textOf(mn)) );
#   endif
   /* Nothing to do. */
}


/* --------------------------------------------------------------------------
 * Imports
 * ------------------------------------------------------------------------*/

Void startGHCImports ( ConId mn, List syms )
/* nm     the module to import from */
/* syms   [ConId | VarId] -- the names to import */
{
#  ifdef DEBUG_IFACE
   printf("startGHCImports %s\n", textToStr(textOf(mn)) );
#  endif
   /* Nothing to do. */
}


Void finishGHCImports ( ConId nm, List syms )
/* nm     the module to import from */
/* syms   [ConId | VarId] -- the names to import */
{
#  ifdef DEBUG_IFACE
   printf("finishGHCImports %s\n", textToStr(textOf(nm)) );
#  endif
  /* Nothing to do. */
}


/* --------------------------------------------------------------------------
 * Vars (values)
 * ------------------------------------------------------------------------*/

void startGHCValue ( Int line, VarId vid, Type ty )
{
    Name   n;
    List   tmp, tvs;
    Text   v = textOf(vid);

#   ifdef DEBUG_IFACE
    printf("begin startGHCValue %s\n", textToStr(v));
#   endif

    n = findName(v);
    if (nonNull(n)) {
        ERRMSG(0) "Attempt to redefine variable \"%s\"", textToStr(v)
        EEND;
    }
    n = newName(v,NIL);

    tvs = ifTyvarsIn(ty);
    for (tmp=tvs; nonNull(tmp); tmp=tl(tmp))
       hd(tmp) = zpair(hd(tmp),STAR);
    if (nonNull(tvs))
       ty = mkPolyType(tvsToKind(tvs),ty);

    ty = tvsToOffsets(line,ty,tvs);
    name(n).type  = ty;
    name(n).arity = arityInclDictParams(ty);
    name(n).line  = line;
}


void finishGHCValue ( VarId vid )
{
    Name n    = findName ( textOf(vid) );
    Int  line = name(n).line;
#   ifdef DEBUG_IFACE
    fprintf(stderr, "begin finishGHCValue %s\n", textToStr(name(n).text) );
#   endif
    assert(currentModule == name(n).mod);
    name(n).type = conidcellsToTycons(line,name(n).type);
}


/* --------------------------------------------------------------------------
 * Type synonyms
 * ------------------------------------------------------------------------*/

Void startGHCSynonym ( Int line, ConId tycon, List tvs, Type ty )
{
    /* tycon :: ConId             */
    /* tvs   ::  [((VarId,Kind))] */
    /* ty    :: Type              */ 
    Text t = textOf(tycon);
#   ifdef DEBUG_IFACE
    fprintf(stderr, "begin startGHCSynonym %s\n", textToStr(t) );
#   endif
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
    }
}


static Void  finishGHCSynonym ( ConId tyc )
{
    Tycon tc   = findTycon(textOf(tyc)); 
    Int   line = tycon(tc).line;
#   ifdef DEBUG_IFACE
    fprintf(stderr, "begin finishGHCSynonym %s\n", textToStr(textOf(tyc)) );
#   endif

    assert (currentModule == tycon(tc).mod);
    //    setCurrModule(tycon(tc).mod);
    tycon(tc).defn = conidcellsToTycons(line,tycon(tc).defn);

    /* (ADR) ToDo: can't really do this until I've done all synonyms
     * and then I have to do them in order
     * tycon(tc).defn = fullExpand(ty);
     * (JRS) What?!?!  i don't understand
     */
}


/* --------------------------------------------------------------------------
 * Data declarations
 * ------------------------------------------------------------------------*/

Void startGHCDataDecl(line,ctx0,tycon,ktyvars,constrs0)
Int   line;
List  ctx0;      /* [((QConId,VarId))]                */
Cell  tycon;     /* ConId                             */
List  ktyvars;   /* [((VarId,Kind))]                  */
List  constrs0;  /* [((ConId,[((Type,VarId,Int))]))]  */
                 /* The Text is an optional field name
                    The Int indicates strictness */
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
    Int     conArgStrictness;

    Text t = textOf(tycon);
#   ifdef DEBUG_IFACE
    fprintf(stderr, "begin startGHCDataDecl %s\n",textToStr(t));
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

        /* a list to accumulate selectors in :: [((VarId,Type))] */
        sels = NIL;

        /* make resTy the result type of the constr, T v1 ... vn */
        resTy = tycon;
        for (tmp=ktyvars; nonNull(tmp); tmp=tl(tmp))
           resTy = ap(resTy,fst(hd(tmp)));

        /* for each constructor ... */
        for (constrs=constrs0; nonNull(constrs); constrs=tl(constrs)) {
           constr = hd(constrs);
           conid  = zfst(constr);
           fields = zsnd(constr);

           /* Build type of constr and handle any selectors found.
              Also collect up tyvars occurring in the constr's arg
              types, so we can throw away irrelevant parts of the
              context later.
           */
           ty = resTy;
           tyvarsMentioned = NIL;  
           /* tyvarsMentioned :: [VarId] */

           conArgs = reverse(fields);
           for (; nonNull(conArgs); conArgs=tl(conArgs)) {
              conArg           = hd(conArgs); /* (Type,Text) */
              conArgTy         = zfst3(conArg);
              conArgNm         = zsnd3(conArg);
              conArgStrictness = intOf(zthd3(conArg));
              tyvarsMentioned = dupListOnto(ifTyvarsIn(conArgTy),
                                            tyvarsMentioned);
              if (conArgStrictness > 0) conArgTy = bang(conArgTy);
              ty = fn(conArgTy,ty);
              if (nonNull(conArgNm)) {
                 /* a field name is mentioned too */
                 selTy = fn(resTy,conArgTy);
                 if (whatIs(tycon(tc).kind) != STAR)
                    selTy = pair(POLYTYPE,pair(tycon(tc).kind, selTy));
                 selTy = tvsToOffsets(line,selTy, ktyvars);
                 sels = cons( zpair(conArgNm,selTy), sels);
              }
           }

           /* Now ty is the constructor's type, not including context.
              Throw away any parts of the context not mentioned in 
              tyvarsMentioned, and use it to qualify ty.
	   */
           ctx2 = NIL;
           for (ctx=ctx0; nonNull(ctx); ctx=tl(ctx)) {
              ctxElem = hd(ctx);     
              /* ctxElem :: ((QConId,VarId)) */
              if (nonNull(cellIsMember(textOf(zsnd(ctxElem)),tyvarsMentioned)))
                 ctx2 = cons(ctxElem, ctx2);
           }
           if (nonNull(ctx2))
              ty = ap(QUAL,pair(ctx2,ty));

           /* stick the tycon's kind on, if not simply STAR */
           if (whatIs(tycon(tc).kind) != STAR)
              ty = pair(POLYTYPE,pair(tycon(tc).kind, ty));

           ty = tvsToOffsets(line,ty, ktyvars);

           /* Finally, stick the constructor's type onto it. */
           hd(constrs) = ztriple(conid,fields,ty);
        }

        /* Final result is that 
           constrs :: [((ConId,[((Type,Text))],Type))]   
                      lists the constructors and their types
           sels :: [((VarId,Type))]
                   lists the selectors and their types
	*/
        tycon(tc).defn = startGHCConstrs(line,constrs0,sels);
    }
}


static List startGHCConstrs ( Int line, List cons, List sels )
{
    /* cons :: [((ConId,[((Type,Text,Int))],Type))] */
    /* sels :: [((VarId,Type))]                     */
    /* returns [Name]                               */
    List cs, ss;
    Int  conNo = length(cons)>1 ? 1 : 0;
    for(cs=cons; nonNull(cs); cs=tl(cs), conNo++) {
        Name c  = startGHCConstr(line,conNo,hd(cs));
        hd(cs)  = c;
    }
    /* cons :: [Name] */

    for(ss=sels; nonNull(ss); ss=tl(ss)) {
        hd(ss) = startGHCSel(line,hd(ss));
    }
    /* sels :: [Name] */
    return appendOnto(cons,sels);
}


static Name startGHCSel ( Int line, ZPair sel )
{
    /* sel :: ((VarId, Type))  */
    Text t      = textOf(zfst(sel));
    Type type   = zsnd(sel);
    
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
    name(n).type = type;
    return n;
}


static Name startGHCConstr ( Int line, Int conNo, ZTriple constr )
{
    /* constr :: ((ConId,[((Type,Text,Int))],Type)) */
    /* (ADR) ToDo: add rank2 annotation and existential annotation
     * these affect how constr can be used.
     */
    Text con   = textOf(zfst3(constr));
    Type type  = zthd3(constr);
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
    name(n).type   = type;
    return n;
}


static Void finishGHCDataDecl ( ConId tyc )
{
    List  nms;
    Tycon tc = findTycon(textOf(tyc));
#   ifdef DEBUG_IFACE
    printf ( "begin finishGHCDataDecl %s\n", textToStr(textOf(tyc)) );
#   endif
    if (isNull(tc)) internal("finishGHCDataDecl");
    
    for (nms=tycon(tc).defn; nonNull(nms); nms=tl(nms)) {
       Name n    = hd(nms);
       Int  line = name(n).line;
       assert(currentModule == name(n).mod);
       name(n).type = conidcellsToTycons(line,name(n).type);
    }
}


/* --------------------------------------------------------------------------
 * Newtype decls
 * ------------------------------------------------------------------------*/

Void startGHCNewType ( Int line, List ctx0, 
                       ConId tycon, List tvs, Cell constr )
{
    /* ctx0   :: [((QConId,VarId))]                */
    /* tycon  :: ConId                             */
    /* tvs    :: [((VarId,Kind))]                  */
    /* constr :: ((ConId,Type)) or NIL if abstract */
    List tmp;
    Type resTy;
    Text t = textOf(tycon);
#   ifdef DEBUG_IFACE
    fprintf(stderr, "begin startGHCNewType %s\n", textToStr(t) );
#   endif
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
           /* constr :: ((ConId,Type)) */
           Text con   = textOf(zfst(constr));
           Type type  = zsnd(constr);
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

           /* make resTy the result type of the constr, T v1 ... vn */
           resTy = tycon;
           for (tmp=tvs; nonNull(tmp); tmp=tl(tmp))
              resTy = ap(resTy,zfst(hd(tmp)));
           type = fn(type,resTy);
           if (nonNull(ctx0))
              type = ap(QUAL,pair(ctx0,type));
           type = tvsToOffsets(line,type,tvs);
           name(n).type   = type;
        }
    }
}


static Void finishGHCNewType ( ConId tyc )
{
    Tycon tc = findTycon(textOf(tyc));
#   ifdef DEBUG_IFACE
    printf ( "begin finishGHCNewType %s\n", textToStr(textOf(tyc)) );
#   endif
 
    if (isNull(tc)) internal("finishGHCNewType");

    if (isNull(tycon(tc).defn)) {
       /* it's an abstract type */
    }
    else if (length(tycon(tc).defn) == 1) {
       /* As we expect, has a single constructor */
       Name n    = hd(tycon(tc).defn);
       Int  line = name(n).line;
       assert(currentModule == name(n).mod);
       name(n).type = conidcellsToTycons(line,name(n).type);
    } else {
       internal("finishGHCNewType(2)");   
    }
}


/* --------------------------------------------------------------------------
 * Class declarations
 * ------------------------------------------------------------------------*/

Void startGHCClass(line,ctxt,tc_name,kinded_tvs,mems0)
Int   line;
List  ctxt;       /* [((QConId, VarId))]   */ 
ConId tc_name;    /* ConId                 */
List  kinded_tvs; /* [((VarId, Kind))]     */
List  mems0; {    /* [((VarId, Type))]     */

    List mems;    /* [((VarId, Type))]     */
    List tvsInT;  /* [VarId] and then [((VarId,Kind))] */
    List tvs;     /* [((VarId,Kind))]      */

    ZPair kinded_tv = hd(kinded_tvs);
    Text ct         = textOf(tc_name);
    Pair newCtx     = pair(tc_name, zfst(kinded_tv));
#   ifdef DEBUG_IFACE
    printf ( "begin startGHCClass %s\n", textToStr(ct) );
#   endif

    if (length(kinded_tvs) != 1) {
        ERRMSG(line) "Cannot presently handle multiparam type classes in ifaces"
        EEND;
    }

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

        cclass(nw).supers     = tvsToOffsets(line,ctxt,
                                             singleton(pair(tv,STAR)));
        */
        cclass(nw).supers     = tvsToOffsets(line,ctxt,
                                             singleton(kinded_tv));


        for (mems=mems0; nonNull(mems); mems=tl(mems)) {
           ZPair mem  = hd(mems);
           Type  memT = zsnd(mem);
           Text  mnt  = textOf(zfst(mem));
           Name  mn;

           /* Stick the new context on the member type */
           if (whatIs(memT)==POLYTYPE) internal("startGHCClass");
           if (whatIs(memT)==QUAL) {
              memT = pair(QUAL,
                          pair(cons(newCtx,fst(snd(memT))),snd(snd(memT))));
           } else {
              memT = pair(QUAL,
                          pair(singleton(newCtx),memT));
           }

           /* Cook up a kind for the type. */
           tvsInT = ifTyvarsIn(memT);
           /* tvsInT :: [VarId] */

           /* ToDo: maximally bogus */
           for (tvs=tvsInT; nonNull(tvs); tvs=tl(tvs))
              hd(tvs) = zpair(hd(tvs),STAR);
           /* tvsIntT :: [((VarId,STAR))] */

           memT = mkPolyType(tvsToKind(tvsInT),memT);
           memT = tvsToOffsets(line,memT,tvsInT);

           /* Park the type back on the member */
           mem = zpair(zfst(mem),memT);

           /* Bind code to the member */
           mn = findName(mnt);
           if (nonNull(mn)) {
              ERRMSG(line) 
                 "Repeated definition for class method \"%s\"",
                 textToStr(mnt)
              EEND;
           }
           mn = newName(mnt,NIL);

           hd(mems) = mem;
        }

        cclass(nw).members    = mems0;
        cclass(nw).numMembers = length(mems0);

        /* (ADR) ToDo: 
         * cclass(nw).dsels    = ?;
         * cclass(nw).dbuild   = ?;
         * cclass(nm).dcon     = ?;
         * cclass(nm).defaults = ?;
         */
    }
}


static Void finishGHCClass ( Tycon cls_tyc )
{
    List  mems;
    Int   line;
    Int   ctr;
    Class nw = findClass ( textOf(cls_tyc) );
#   ifdef DEBUG_IFACE
    printf ( "begin finishGHCClass %s\n", textToStr(cclass(nw).text) );
#   endif
    if (isNull(nw)) internal("finishGHCClass");

    line = cclass(nw).line;
    ctr  = - length(cclass(nw).members);
    assert (currentModule == cclass(nw).mod);

    cclass(nw).level   = 0;  /* (ADR) ToDo: 1 + max (map level supers) */
    cclass(nw).head    = conidcellsToTycons(line,cclass(nw).head);
    cclass(nw).supers  = conidcellsToTycons(line,cclass(nw).supers);
    cclass(nw).members = conidcellsToTycons(line,cclass(nw).members);

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
}


/* --------------------------------------------------------------------------
 * Instances
 * ------------------------------------------------------------------------*/

Inst startGHCInstance (line,ctxt0,cls,var)
Int   line;
List  ctxt0;  /* [((QConId, VarId))] */
Type  cls;    /* Type  */
VarId var; {  /* VarId */
    List tmp, tvs, ks;
    Inst in = newInst();
#   ifdef DEBUG_IFACE
    printf ( "begin startGHCInstance\n" );
#   endif

    /* Make tvs into a list of tyvars with bogus kinds. */
    tvs = ifTyvarsIn(cls);
    /* tvs :: [VarId] */

    ks = NIL;
    for (tmp = tvs; nonNull(tmp); tmp=tl(tmp)) {
       hd(tmp) = zpair(hd(tmp),STAR);
       ks = cons(STAR,ks);
    }
    /* tvs :: [((VarId,STAR))] */
    inst(in).line         = line;
    inst(in).implements   = NIL;
    inst(in).kinds        = ks;
    inst(in).specifics    = tvsToOffsets(line,ctxt0,tvs);
    inst(in).numSpecifics = length(ctxt0);
    inst(in).head         = tvsToOffsets(line,cls,tvs);

    /* Figure out the name of the class being instanced, and store it
       at inst(in).c.  finishGHCInstance will resolve it to a real Class. */
    { 
       Cell cl = inst(in).head;
       while (isAp(cl)) cl = arg(cl);
       assert(whatIs(cl)==DICTAP);
       cl = unap(DICTAP,cl);       
       cl = fst(cl);
       assert ( isQCon(cl) );
       inst(in).c = cl;
    }

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
    return in;
}


static Void finishGHCInstance ( Inst in )
{
    Int    line;
    Class  c;
    Type   cls;

#   ifdef DEBUG_IFACE
    printf ( "begin finishGHCInstance\n" );
#   endif

    assert (nonNull(in));
    line = inst(in).line;
    assert (currentModule==inst(in).mod);

    /* inst(in).c is, prior to finishGHCInstance, a ConId or Tuple,
       since beginGHCInstance couldn't possibly have resolved it to
       a Class at that point.  We convert it to a Class now.
    */
    c = inst(in).c;
    assert(isQCon(c));
    c = findQualClassWithoutConsultingExportList(c);
    assert(nonNull(c));
    inst(in).c = c;

    inst(in).head         = conidcellsToTycons(line,inst(in).head);
    inst(in).specifics    = conidcellsToTycons(line,inst(in).specifics);
    cclass(c).instances   = cons(in,cclass(c).instances);
}


/* --------------------------------------------------------------------------
 * Helper fns
 * ------------------------------------------------------------------------*/

/* This is called from the startGHC* functions.  It traverses a structure
   and converts varidcells, ie, type variables parsed by the interface
   parser, into Offsets, which is how Hugs wants to see them internally.
   The Offset for a type variable is determined by its place in the list
   passed as the second arg; the associated kinds are irrelevant.

   ((t1,t2)) denotes the typed (z-)pair of t1 and t2.
*/

/* tvsToOffsets :: LineNo -> Type -> [((VarId,Kind))] -> Type */
static Type tvsToOffsets(line,type,ktyvars)
Int  line;
Type type;
List ktyvars; { /* [((VarId,Kind))] */
   switch (whatIs(type)) {
      case NIL:
      case TUPLE:
      case QUALIDENT:
      case CONIDCELL:
      case TYCON:
         return type;
      case ZTUP2: /* convert to the untyped representation */
         return ap( tvsToOffsets(line,zfst(type),ktyvars),
                    tvsToOffsets(line,zsnd(type),ktyvars) );
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
      case DICTAP: /* bogus ?? */
         return ap(DICTAP, tvsToOffsets(line,snd(type),ktyvars));
      case UNBOXEDTUP:  /* bogus?? */
         return ap(UNBOXEDTUP, tvsToOffsets(line,snd(type),ktyvars));
      case BANG:  /* bogus?? */
         return ap(BANG, tvsToOffsets(line,snd(type),ktyvars));
      case VARIDCELL: /* Ha! some real work to do! */
       { Int i = 0;
         Text tv = textOf(type);
         for (; nonNull(ktyvars); i++,ktyvars=tl(ktyvars)) {
            Cell varid;
            Text tt;
            assert(isZPair(hd(ktyvars)));
            varid = zfst(hd(ktyvars));
            tt    = textOf(varid);
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
   assert(0);
   return NIL; /* NOTREACHED */
}


/* This is called from the finishGHC* functions.  It traverses a structure
   and converts conidcells, ie, type constructors parsed by the interface
   parser, into Tycons (or Classes), which is how Hugs wants to see them
   internally.  Calls to this fn have to be deferred to the second phase
   of interface loading (finishGHC* rather than startGHC*) so that all relevant
   Tycons or Classes have been loaded into the symbol tables and can be
   looked up.
*/
static Type conidcellsToTycons ( Int line, Type type )
{
   switch (whatIs(type)) {
      case NIL:
      case OFFSET:
      case TYCON:
      case CLASS:
      case VARIDCELL:
      case TUPLE:
      case STAR:
         return type;
      case QUALIDENT:
       { Cell t;  /* Tycon or Class */
         Text m     = qmodOf(type);
         Module mod = findModule(m);
         if (isNull(mod)) {
            ERRMSG(line)
               "Undefined module in qualified name \"%s\"",
               identToStr(type)
            EEND;
            return NIL;
         }
         t = findQualTyconWithoutConsultingExportList(type);
         if (nonNull(t)) return t;
         t = findQualClassWithoutConsultingExportList(type);
         if (nonNull(t)) return t;
         ERRMSG(line)
              "Undefined qualified class or type \"%s\"",
              identToStr(type)
         EEND;
         return NIL;
       }
      case CONIDCELL:
       { Tycon tc;
         Class cl;
         cl = findQualClass(type);
         if (nonNull(cl)) return cl;
         if (textOf(type)==findText("[]"))
            /* a hack; magically qualify [] into PrelBase.[] */
            return conidcellsToTycons(line, 
                                      mkQualId(mkCon(findText("PrelBase")),type));
         tc = findQualTycon(type);
         if (nonNull(tc)) return tc;
         ERRMSG(line)
             "Undefined class or type constructor \"%s\"",
             identToStr(type)
         EEND;
         return NIL;
       }
      case AP: 
         return ap( conidcellsToTycons(line,fun(type)),
                    conidcellsToTycons(line,arg(type)) );
      case ZTUP2: /* convert to std pair */
         return ap( conidcellsToTycons(line,zfst(type)),
                    conidcellsToTycons(line,zsnd(type)) );

      case POLYTYPE: 
         return mkPolyType ( 
                   polySigOf(type),
                   conidcellsToTycons(line,monotypeOf(type))
                );
         break;
      case QUAL:
         return pair(QUAL,pair(conidcellsToTycons(line,fst(snd(type))),
                               conidcellsToTycons(line,snd(snd(type)))));
      case DICTAP: /* bogus?? */
         return ap(DICTAP, conidcellsToTycons(line, snd(type)));
      case UNBOXEDTUP:
         return ap(UNBOXEDTUP, conidcellsToTycons(line, snd(type)));
      case BANG:
         return ap(BANG, conidcellsToTycons(line, snd(type)));
      default: 
         fprintf(stderr, "conidcellsToTycons: unknown stuff %d\n", 
                 whatIs(type));
         print(type,20);
         fprintf(stderr,"\n");
         assert(0);
   }
   assert(0);
   return NIL; /* NOTREACHED */
}


/* Find out if a type mentions a type constructor not present in 
   the supplied list of qualified tycons.
*/
static Bool allTypesKnown ( Type  type, 
                            List  aktys /* [QualId] */,
                            ConId thisMod )
{
   switch (whatIs(type)) {
      case NIL:
      case OFFSET:
      case VARIDCELL:
      case TUPLE:
         return TRUE;
      case AP:
         return allTypesKnown(fun(type),aktys,thisMod)
                && allTypesKnown(arg(type),aktys,thisMod);
      case ZTUP2:
         return allTypesKnown(zfst(type),aktys,thisMod)
                && allTypesKnown(zsnd(type),aktys,thisMod);
      case DICTAP: 
         return allTypesKnown(unap(DICTAP,type),aktys,thisMod);

      case CONIDCELL:
        if (textOf(type)==findText("[]"))
            /* a hack; magically qualify [] into PrelBase.[] */
            type = mkQualId(mkCon(findText("PrelBase")),type); else
            type = mkQualId(thisMod,type);
         /* fall through */
      case QUALIDENT:
         if (isNull(qualidIsMember(type,aktys))) goto missing;
         return TRUE;

      default: 
         fprintf(stderr, "allTypesKnown: unknown stuff %d\n", whatIs(type));
         print(type,10);printf("\n");
         internal("allTypesKnown");
         return TRUE; /*notreached*/
   }
  missing:
   printf ( "allTypesKnown: unknown " ); print(type,10); printf("\n");
   return FALSE;
}


/* --------------------------------------------------------------------------
 * Utilities
 *
 * None of these do lookups or require that lookups have been resolved
 * so they can be performed while reading interfaces.
 * ------------------------------------------------------------------------*/

/* tvsToKind :: [((VarId,Kind))] -> Kinds */
static Kinds tvsToKind(tvs)
List tvs; { /* [((VarId,Kind))] */
    List  rs;
    Kinds r  = STAR;
    for (rs=reverse(tvs); nonNull(rs); rs=tl(rs)) {
        if (whatIs(hd(rs)) != ZTUP2) internal("tvsToKind(1)");
        if (whatIs(zfst(hd(rs))) != VARIDCELL) internal("tvsToKind(2)");
        r = ap(zsnd(hd(rs)),r);
    }
    return r;
}


static Int arityInclDictParams ( Type type )
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
static Int arityFromType(type) 
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


/* ifTyvarsIn :: Type -> [VarId]
   The returned list has no duplicates -- is a set.
*/
static List ifTyvarsIn(type)
Type type; {
    List vs = typeVarsIn(type,NIL,NIL,NIL);
    List vs2 = vs;
    for (; nonNull(vs2); vs2=tl(vs2))
       if (whatIs(hd(vs2)) != VARIDCELL)
          internal("ifTyvarsIn");
    return vs;
}


/* --------------------------------------------------------------------------
 * ELF specifics
 * ------------------------------------------------------------------------*/

#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)

#include <elf.h>

static char* findElfSection ( void* objImage, Elf32_Word sh_type )
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


static Void resolveReferencesInObjectModule_elf ( Module m, 
                                                        Bool   verb )
{
   char symbol[1000]; // ToDo
   int i, j;
   Elf32_Sym*  stab = NULL;
   char* strtab;
   char* ehdrC = (char*)(module(m).oImage);
   Elf32_Ehdr* ehdr = (Elf32_Ehdr*) ehdrC;
   Elf32_Shdr* shdr = (Elf32_Shdr*) (ehdrC + ehdr->e_shoff);
   Elf32_Word* targ;
   // first find "the" symbol table
   // why is this commented out???
   stab = (Elf32_Sym*) findElfSection ( ehdrC, SHT_SYMTAB );

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


static Bool validateOImage_elf ( void*  imgV, 
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
      if (sh_strtab && verb) 
         fprintf ( stderr, "sname=%s", sh_strtab + shdr[i].sh_name );
      if (verb) fprintf ( stderr, "\n" );
   }

   if (verb) fprintf ( stderr, "\n\nString tables\n" );
   strtab = NULL;
   nstrtab = 0;
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB &&
          i !=  ehdr->e_shstrndx) {
         if (verb) 
            fprintf ( stderr, "   section %d is a normal string table\n", i );
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


static void readSyms_elf ( Module m, Bool verb )
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
                ELF32_ST_TYPE(stab[j].st_info)==STT_OBJECT ||
                ELF32_ST_TYPE(stab[j].st_info)==STT_NOTYPE)
	      ) {
            char* nm = strtab + stab[j].st_name;
            char* ad = ehdrC 
                       + shdr[ stab[j].st_shndx ].sh_offset
                       + stab[j].st_value;
            assert(nm);
            assert(ad);
            if (verb)
               fprintf(stderr, "addOTabName: %10p  %s %s\n",
                       ad, textToStr(module(m).text), nm );
            addOTabName ( m, nm, ad );
         }
	 //else fprintf(stderr, "skipping `%s'\n", strtab + stab[j].st_name );
      }

   }
}

#endif /* defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS) */


/* --------------------------------------------------------------------------
 * Arch-independent interface to the runtime linker
 * ------------------------------------------------------------------------*/

static Bool validateOImage ( void* img, Int size, Bool verb )
{
#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
   return
      validateOImage_elf ( img, size, verb );
#else
   internal("validateOImage: not implemented on this platform");
#endif
}


static Void resolveReferencesInObjectModule ( Module m, Bool verb )
{
#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
   resolveReferencesInObjectModule_elf ( m, verb );
#else
   internal("resolveReferencesInObjectModule: not implemented on this platform");
#endif
}


static Void readSyms ( Module m, Bool verb )
{
#if defined(linux_TARGET_OS) || defined(solaris2_TARGET_OS)
   readSyms_elf ( m, verb );
#else
   internal("readSyms: not implemented on this platform");
#endif
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
extern int CAF_BLACKHOLE_info;
extern int IND_STATIC_info;
extern int newCAF;

OSym rtsTab[] 
   = { 
       { "stg_gc_enter_1",        &stg_gc_enter_1     },
       { "stg_chk_0",             &stg_chk_0          },
       { "stg_chk_1",             &stg_chk_1          },
       { "stg_update_PAP",        &stg_update_PAP     },
       { "__ap_2_upd_info",       &__ap_2_upd_info    },
       { "MainRegTable",          &MainRegTable       },
       { "Upd_frame_info",        &Upd_frame_info     },
       { "CAF_BLACKHOLE_info",    &CAF_BLACKHOLE_info },
       { "IND_STATIC_info",       &IND_STATIC_info    },
       { "newCAF",                &newCAF             },
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
   t = unZcodeThenFindText(nm2);
   m = findModule(t);
   if (isNull(m)) goto not_found;
fprintf(stderr, "   %%%% %s\n", nm );
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
       case POSTPREL: break;

       case PREPREL:
       case RESET: 
          ifaces_outstanding  = NIL;
          break;
       case MARK: 
          mark(ifaces_outstanding);
          break;
    }
}

/*-------------------------------------------------------------------------*/


/* --------------------------------------------------------------------------
 * GHC interface file processing for Hugs
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: interface.c,v $
 * $Revision: 1.48 $
 * $Date: 2000/04/07 09:59:36 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "object.h"

#include "Assembler.h"  /* for wrapping GHC objects */


/*#define DEBUG_IFACE*/
#define VERBOSE FALSE

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

static Void startGHCValue       ( Int,VarId,Type );
static Void finishGHCValue      ( VarId );

static Void startGHCSynonym     ( Int,Cell,List,Type );
static Void finishGHCSynonym    ( Tycon ); 

static Void  startGHCClass      ( Int,List,Cell,List,List );
static Class finishGHCClass     ( Class ); 

static Inst startGHCInstance    ( Int,List,Pair,VarId );
static Void finishGHCInstance   ( Inst );

static Void startGHCImports     ( ConId,List );
static Void finishGHCImports    ( ConId,List );

static Void startGHCExports     ( ConId,List );
static Void finishGHCExports    ( ConId,List );

static Void finishGHCFixdecl    ( Cell prec, Cell assoc, ConVarId name );

static Void finishGHCModule     ( Cell );
static Void startGHCModule      ( Text );

static Void startGHCDataDecl    ( Int,List,Cell,List,List );
static List finishGHCDataDecl   ( ConId tyc );
/* Supporting stuff for {start|finish}GHCDataDecl */
static List startGHCConstrs     ( Int,List,List );
static Name startGHCSel         ( Int,Pair );
static Name startGHCConstr      ( Int,Int,Triple );

static Void startGHCNewType     ( Int,List,Cell,List,Cell );
static Void finishGHCNewType    ( ConId tyc );



static Kinds tvsToKind             ( List );
static Int   arityFromType         ( Type );
static Int   arityInclDictParams   ( Type );
static Bool  allTypesKnown         ( Type type, 
                                     List aktys /* [QualId] */,
                                     ConId thisMod );
                                         
static List  ifTyvarsIn            ( Type );
static Type  tvsToOffsets          ( Int,Type,List );
static Type  conidcellsToTycons    ( Int,Type );





/* --------------------------------------------------------------------------
 * Top-level interface processing
 * ------------------------------------------------------------------------*/

/* getIEntityName :: I_IMPORT..I_VALUE -> ConVarId | NIL */
static ConVarId getIEntityName ( Cell c )
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
static Cell filterInterface ( Cell root, 
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


List /* of CONID */ getInterfaceImports ( Cell iface )
{
    List  tops;
    List  imports = NIL;

    for (tops = zsnd(unap(I_INTERFACE,iface)); nonNull(tops); tops=tl(tops))
       if (whatIs(hd(tops)) == I_IMPORT) {
          ZPair imp_decl = unap(I_IMPORT,hd(tops));
          ConId m_to_imp = zfst(imp_decl);
          if (textOf(m_to_imp) != findText("PrelGHC")) {
             imports = cons(m_to_imp,imports);
#            ifdef DEBUG_IFACE
             fprintf(stderr, "add iface %s\n", 
                     textToStr(textOf(m_to_imp)));
#            endif
          }
       }
    return imports;
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


/* Does t start with "$dm" ? */
static Bool isIfaceDefaultMethodName ( Text t )
{
   String s = textToStr(t);
   return (s && s[0]=='$' && s[1]=='d' && s[2]=='m' && s[3]);
}
      

static Bool isExportedIFaceEntity ( Cell ife, List exlist_list )
{
   /* ife         :: I_IMPORT..I_VALUE                      */
   /* exlist_list :: [[ ConVarId | ((ConId, [ConVarId])) ]] */
   Text   tnm;
   List   exlist;
   List   t;
   String s;

   ConVarId ife_id = getIEntityName ( ife );

   if (isNull(ife_id)) return TRUE;

   tnm = textOf(ife_id);

   /* Don't junk default methods, even tho the export list doesn't
      mention them.
   */
   if (isIfaceDefaultMethodName(tnm)) goto retain;

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
#  ifdef DEBUG_IFACE
   fprintf ( stderr, "     dump %s\n", textToStr(tnm) );
#  endif
   return FALSE;

 retain:
#  ifdef DEBUG_IFACE
   fprintf ( stderr, "   retain %s\n", textToStr(tnm) );
#  endif
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

#  ifdef DEBUG_IFACE
   fprintf(stderr, "\ncleanIFace: %s\n", textToStr(textOf(iname)));
#  endif

   exlist_list = getExportDeclsInIFace ( root );
   /* exlist_list :: [I_EXPORT] */
   
   for (t=exlist_list; nonNull(t); t=tl(t))
      hd(t) = zsnd(unap(I_EXPORT,hd(t)));
   /* exlist_list :: [[ ConVarId | ((ConId, [ConVarId])) ]] */

#if 0
   if (isNull(exlist_list)) {
      ERRMSG(0) "Can't find any export lists in interface file"
      EEND;
   }
#endif

   return filterInterface ( root, isExportedIFaceEntity, 
                            exlist_list, NULL );
}


/* addTyconsAndClassesFromIFace :: I_INTERFACE -> [QualId] -> [QualId] */
static List addTyconsAndClassesFromIFace ( Cell root, List aktys )
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


static Void ifentityAllTypesKnown_dumpmsg ( Cell entity )
{
   ConVarId id = getIEntityName ( entity );
#  ifdef DEBUG_IFACE
   fprintf ( stderr, 
             "dumping %s because of unknown type(s)\n",
             isNull(id) ? "(nameless entity?!)" : textToStr(textOf(id)) );
#  endif
}


/* ifentityAllTypesKnown :: I_IMPORT..I_VALUE -> (([QualId], ConId)) -> Bool */
/* mod is the current module being processed -- so we can qualify unqual'd
   names.  Strange calling convention for aktys and mod is so we can call this
   from filterInterface.
*/
static Bool ifentityAllTypesKnown ( Cell entity, ZPair aktys_mod )
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


/* ifTypeDoesntRefUnknownTycon :: I_IMPORT..I_VALUE -> (([QualId], ConId)) -> Bool */
/* mod is the current module being processed -- so we can qualify unqual'd
   names.  Strange calling convention for aktys and mod is so we can call this
   from filterInterface.
*/
static Bool ifTypeDoesntRefUnknownTycon ( Cell entity, ZPair aktys_mod )
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


static Void ifTypeDoesntRefUnknownTycon_dumpmsg ( Cell entity )
{
   ConVarId id = getIEntityName ( entity );
   assert (whatIs(entity)==I_TYPE);
   assert (isCon(id));
#  ifdef DEBUG_IFACE
   fprintf ( stderr, 
             "dumping type %s because of unknown tycon(s)\n",
             textToStr(textOf(id)) );
#  endif
}


/* abstractifyExport :: I_EXPORT -> ConId -> I_EXPORT
*/
static List abstractifyExDecl ( Cell root, ConId toabs )
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


static Void ppModule ( Text modt )
{
#  ifdef DEBUG_IFACE
   fflush(stderr); fflush(stdout);
   fprintf(stderr, "---------------- MODULE %s ----------------\n", 
                   textToStr(modt) );
#  endif
}


static void* ifFindItblFor ( Name n )
{
   /* n is a constructor for which we want to find the GHC info table.
      First look for a _con_info symbol.  If that doesn't exist, _and_
      this is a nullary constructor, then it's safe to look for the
      _static_info symbol instead.
   */
   void* p;
   char  buf[1000];
   Text  t;

   sprintf ( buf, MAYBE_LEADING_UNDERSCORE_STR("%s_%s_con_info"), 
                  textToStr( module(name(n).mod).text ),
                  textToStr( name(n).text ) );
   t = enZcodeThenFindText(buf);
   p = lookupOTabName ( name(n).mod, textToStr(t) );

   if (p) return p;

   if (name(n).arity == 0) {
      sprintf ( buf, MAYBE_LEADING_UNDERSCORE_STR("%s_%s_static_info"), 
                     textToStr( module(name(n).mod).text ),
                     textToStr( name(n).text ) );
      t = enZcodeThenFindText(buf);
      p = lookupOTabName ( name(n).mod, textToStr(t) );
      if (p) return p;
   }

   ERRMSG(0) "Can't find info table %s", textToStr(t)
   EEND;
}


void ifLinkConstrItbl ( Name n )
{
   /* name(n) is either a constructor or a field name.  
      If the latter, ignore it.  If it is a non-nullary constructor,
      find its info table in the object code.  If it's nullary,
      we can skip the info table, since all accesses will go via
      the _closure label.
   */
   if (islower(textToStr(name(n).text)[0])) return;
   if (name(n).arity == 0) return;
   name(n).itbl = ifFindItblFor(n);
}


static void ifSetClassDefaultsAndDCon ( Class c )
{
   char   buf[100];
   char   buf2[1000];
   String s;
   Name   n;
   Text   t;
   void*  p;
   List   defs;   /* :: [Name] */
   List   mems;   /* :: [Name] */
   Module m;
   assert(isNull(cclass(c).defaults));

   /* Create the defaults list by more-or-less cloning the members list. */   
   defs = NIL;
   for (mems=cclass(c).members; nonNull(mems); mems=tl(mems)) {
      strcpy(buf, "$dm");
      s = textToStr( name(hd(mems)).text );
      assert(strlen(s) < 95);
      strcat(buf, s);
      n = findNameInAnyModule(findText(buf));
      assert (nonNull(n));
      defs = cons(n,defs);
   }
   defs = rev(defs);
   cclass(c).defaults = defs;

   /* Create a name table entry for the dictionary datacon.
      Interface files don't mention them, so it had better not
      already be present.
   */
   strcpy(buf, ":D");
   s = textToStr( cclass(c).text );
   assert( strlen(s) < 96 );
   strcat(buf, s);
   t = findText(buf);
   n = findNameInAnyModule(t);
   assert(isNull(n));

   m = cclass(c).mod;
   n = newName(t,NIL);
   name(n).mod    = m;
   name(n).arity  = cclass(c).numSupers + cclass(c).numMembers;
   name(n).number = cfunNo(0);
   cclass(c).dcon = n;

   /* And finally ... set name(n).itbl to Mod_:DClass_con_info.
      Because this happens right at the end of loading, we know
      that we should actually be able to find the symbol in this
      module's object symbol table.  Except that if the dictionary
      has arity 1, we don't bother, since it will be represented as
      a newtype and not as a data, so its itbl can remain NULL.
   */ 
   if (name(n).arity == 1) {
      name(n).itbl = NULL;
      name(n).defn = nameId;
   } else {
      p = ifFindItblFor ( n );
      name(n).itbl = p;
   }
}


void processInterfaces ( List /* of CONID */ iface_modnames )
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
    List    cls_list;         /* :: List Class */
    List    constructor_list; /* :: List Name */

    List ifaces       = NIL;  /* :: List I_INTERFACE */

    if (isNull(iface_modnames)) return;

#   ifdef DEBUG_IFACE
    fprintf ( stderr, 
              "processInterfaces: %d interfaces to process\n", 
              length(ifaces_outstanding) );
#   endif

    for (xs = iface_modnames; nonNull(xs); xs=tl(xs)) {
       mod = findModule(textOf(hd(xs)));
       assert(nonNull(mod));
       assert(module(mod).mode == FM_OBJECT);
       ifaces = cons ( module(mod).tree, ifaces );
    }
    ifaces = reverse(ifaces);

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
          all_known_types 
             = addTyconsAndClassesFromIFace ( hd(xs), all_known_types );

       /* Have we reached a fixed point? */
       i = length(all_known_types);
#      ifdef DEBUG_IFACE
       fprintf ( stderr,
                 "\n============= %d known types =============\n", i );
#      endif
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
#            ifdef DEBUG_IFACE
             fprintf ( stderr, 
                       "abstractifying %s because it uses an unknown type\n",
                       textToStr(textOf(getIEntityName(ent))) );
#            endif
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
#            ifdef DEBUG_IFACE
             fprintf(stderr, "abstractify data %s\n", 
                     textToStr(textOf(getIEntityName(ent))) );
#            endif
	  }
          else if (whatIs(ent)==I_NEWTYPE
              && isExportedAbstractly ( getIEntityName(ent), 
                                        exlist_list )) {
             Cell data = unap(I_NEWTYPE,ent);
             data = z5ble ( zsel15(data), zsel25(data), zsel35(data),
                            zsel45(data), NIL /* the constr-type pair */ );
             hd(es) = ap(I_NEWTYPE,data);
#            ifdef DEBUG_IFACE
             fprintf(stderr, "abstractify newtype %s\n", 
                     textToStr(textOf(getIEntityName(ent))) );
#            endif
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
#   ifdef DEBUG_IFACE
    fprintf(stderr,"\n");
#   endif
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
#      ifdef DEBUG_IFACE
       fprintf ( stderr,
                 "\n------------- %d known types -------------\n", i );
#      endif
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
    for (xs=ifaces; nonNull(xs); xs=tl(xs))
       startGHCModule ( textOf(zfst(unap(I_INTERFACE,hd(xs)))) );


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

#   ifdef DEBUG_IFACE
    fprintf(stderr, "\n============================"
                    "=============================\n");
    fprintf(stderr, "=============================="
                    "===========================\n");
#   endif

    /* Traverse again the decl lists of the modules, this time 
       calling the finishGHC* functions.  But don't process
       the export lists; those must wait for later.
    */
    cls_list         = NIL;
    constructor_list = NIL;
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
                Cell fixdecl = unap(I_FIXDECL,decl);
                finishGHCFixdecl ( zfst3(fixdecl), zsnd3(fixdecl), zthd3(fixdecl) );
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
                Cell ddecl   = unap(I_DATA,decl);
                List constrs = finishGHCDataDecl ( zsel35(ddecl) );
                constructor_list = dupOnto ( constrs, constructor_list );
                break;
             }
             case I_NEWTYPE: {
                Cell ntdecl = unap(I_NEWTYPE,decl);
                finishGHCNewType ( zsel35(ntdecl) );
                break;
             }
             case I_CLASS: {
                Cell  klass = unap(I_CLASS,decl);
                Class cls   = finishGHCClass ( zsel35(klass) );
                cls_list = cons(cls,cls_list);
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
#   ifdef DEBUG_IFACE
    fprintf(stderr, "\n+++++++++++++++++++++++++++++"
                    "++++++++++++++++++++++++++++\n");
    fprintf(stderr, "+++++++++++++++++++++++++++++++"
                    "++++++++++++++++++++++++++\n");
#   endif

    /* Build the module(m).export lists for each module, by running
       through the export lists in the iface.  Also, do the implicit
       'import Prelude' thing.  And finally, do the object code 
       linking.
    */
    for (xs = ifaces; nonNull(xs); xs = tl(xs))
       finishGHCModule(hd(xs));

    mapProc(visitClass,cls_list);
    mapProc(ifSetClassDefaultsAndDCon,cls_list);
    mapProc(ifLinkConstrItbl,constructor_list);

    /* Finished! */
    ifaces_outstanding = NIL;
}


/* --------------------------------------------------------------------------
 * Modules
 * ------------------------------------------------------------------------*/

static void startGHCModule_errMsg ( char* msg )
{
   fprintf ( stderr, "object error: %s\n", msg );
}

static void* startGHCModule_clientLookup ( char* sym )
{
#  ifdef DEBUG_IFACE
   /* fprintf ( stderr, "CLIENTLOOKUP %s\n", sym ); */
#  endif
   return lookupObjName ( sym );
}

static int /*Bool*/ startGHCModule_clientWantsSymbol ( char* sym )
{
   if (strcmp(sym,"ghc_cc_ID")==0) return 0;
   return 1;
}

static ObjectCode* startGHCModule_partial_load ( String objNm, Int objSz )
{
   ObjectCode* oc
      = ocNew ( startGHCModule_errMsg,
                startGHCModule_clientLookup,
                startGHCModule_clientWantsSymbol,
                objNm, objSz );
    
    if (!oc) {
       ERRMSG(0) "Storage allocation for object file \"%s\" failed", objNm
       EEND;
    }
    if (!ocLoadImage(oc,VERBOSE)) {
       ERRMSG(0) "Reading of object file \"%s\" failed", objNm
       EEND;
    }
    if (!ocVerifyImage(oc,VERBOSE)) {
       ERRMSG(0) "Validation of object file \"%s\" failed", objNm
       EEND;
    }
    if (!ocGetNames(oc,VERBOSE)) {
       ERRMSG(0) "Reading of symbol names in object file \"%s\" failed", objNm
       EEND;
    }
    return oc;
}

static Void startGHCModule ( Text mname )
{
   List   xts;
   Module m = findModule(mname);
   assert(nonNull(m));

#  ifdef DEBUG_IFACE
   fprintf ( stderr, "startGHCIface: name %16s   objsize %d\n", 
                      textToStr(mname), module(m).objSize );
#  endif
   if (module(m).fake)
      module(m).fake = FALSE;

   /* Get hold of the primary object for the module. */
   module(m).object
      = startGHCModule_partial_load ( textToStr(module(m).objName), 
                                      module(m).objSize );

   /* and any extras ... */
   for (xts = module(m).objectExtraNames; nonNull(xts); xts=tl(xts)) {
      Int         size;
      ObjectCode* oc;
      Text        xtt = hd(xts);
      String      nm  = getExtraObjectInfo (
                           textToStr(module(m).objName),
                           textToStr(xtt),
                           &size
                        );
      if (size == -1) {
         ERRMSG(0) "Can't find extra object file \"%s\"", nm
         EEND;
      }
      oc = startGHCModule_partial_load ( nm, size );
      oc->next = module(m).objectExtras;
      module(m).objectExtras = oc;
   }
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


static Void finishGHCModule ( Cell root ) 
{
   /* root :: I_INTERFACE */
   Cell        iface       = unap(I_INTERFACE,root);
   ConId       iname       = zfst(iface);
   Module      mod         = findModule(textOf(iname));
   List        exlist_list = NIL;
   List        t;
   ObjectCode* oc;

#  ifdef DEBUG_IFACE
   fprintf(stderr, "begin finishGHCModule %s\n", textToStr(textOf(iname)));
#  endif

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
#              ifdef DEBUG_IFACE
               fprintf(stderr, "   var %s\n", textToStr(textOf(ex)) );
#              endif
               module(mod).exports = cons(c, module(mod).exports);
               addName(c);
               break;

            case CONIDCELL: /* non data tycon */
               q = mkQualId(exmod,ex);
               c = findQualTyconWithoutConsultingExportList ( q );
               if (isNull(c)) goto notfound;
#              ifdef DEBUG_IFACE
               fprintf(stderr, "   type %s\n", textToStr(textOf(ex)) );
#              endif
               module(mod).exports = cons(pair(c,NIL), module(mod).exports);
               addTycon(c);
               break;

            case ZTUP2: /* data T = C1 ... Cn  or class C where f1 ... fn */
               subents = zsnd(ex);  /* :: [ConVarId] */
               ex      = zfst(ex);  /* :: ConId */
               q       = mkQualId(exmod,ex);
               c       = findQualTyconWithoutConsultingExportList ( q );

               if (nonNull(c)) { /* data */
#                 ifdef DEBUG_IFACE
                  fprintf(stderr, "   data/newtype %s = { ", 
                          textToStr(textOf(ex)) );
#                 endif
                  assert(tycon(c).what == DATATYPE || tycon(c).what==NEWTYPE);
                  abstract = isNull(tycon(c).defn);
                  /* This data/newtype could be abstract even tho the export list
                     says to export it non-abstractly.  That happens if it was 
                     imported from some other module and is now being re-exported,
                     and previous cleanup phases have abstractified it in the 
                     original (defining) module.
		  */
                  if (abstract) {
                     module(mod).exports = cons(pair(c,NIL), module(mod).exports);
                     addTycon(c);
#                    ifdef DEBUG_IFACE
                     fprintf ( stderr, "(abstract) ");
#                    endif
		  } else {
                     module(mod).exports = cons(pair(c,DOTDOT), module(mod).exports);
                     addTycon(c);
                     for (; nonNull(subents); subents = tl(subents)) {
                        Cell ent2 = hd(subents);
                        assert(isCon(ent2) || isVar(ent2)); 
                                              /* isVar since could be a field name */
                        q = mkQualId(exmod,ent2);
                        c = findQualNameWithoutConsultingExportList ( q );
#                       ifdef DEBUG_IFACE
                        fprintf(stderr, "%s ", textToStr(name(c).text));
#                       endif
                        assert(nonNull(c));
                        /* module(mod).exports = cons(c, module(mod).exports); */
                        addName(c);
                     }
                  }
#                 ifdef DEBUG_IFACE
                  fprintf(stderr, "}\n" );
#                 endif
               } else { /* class */
                  q = mkQualId(exmod,ex);
                  c = findQualClassWithoutConsultingExportList ( q );
                  if (isNull(c)) goto notfound;
#                 ifdef DEBUG_IFACE
                  fprintf(stderr, "   class %s { ", textToStr(textOf(ex)) );
#                 endif
                  module(mod).exports = cons(pair(c,DOTDOT), module(mod).exports);
                  addClass(c);
                  for (; nonNull(subents); subents = tl(subents)) {
                     Cell ent2 = hd(subents);
                     assert(isVar(ent2));
                     q = mkQualId(exmod,ent2);
                     c = findQualNameWithoutConsultingExportList ( q );
#                    ifdef DEBUG_IFACE
                     fprintf(stderr, "%s ", textToStr(name(c).text));
#                    endif
                     if (isNull(c)) goto notfound;
                     /* module(mod).exports = cons(c, module(mod).exports); */
                     addName(c);
                  }
#                 ifdef DEBUG_IFACE
                  fprintf(stderr, "}\n" );
#                 endif
               }
               break;

            default:
               internal("finishExports(2)");

         } /* switch */
         continue;  /* so notfound: can be placed after this */
  
        notfound:
         /* q holds what ain't found */
         assert(whatIs(q)==QUALIDENT);
#        ifdef DEBUG_IFACE
         fprintf( stderr, "   ------ IGNORED: %s.%s\n",
                  textToStr(qmodOf(q)), textToStr(qtextOf(q)) );
#        endif
         continue;
      }
   }

#if 0
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
#endif

   /* Last, but by no means least ... */
   if (!ocResolve(module(mod).object,VERBOSE))
      internal("finishGHCModule: object resolution failed");

   for (oc=module(mod).objectExtras; oc; oc=oc->next) {
      if (!ocResolve(oc, VERBOSE))
         internal("finishGHCModule: extra object resolution failed");
   }
}


/* --------------------------------------------------------------------------
 * Exports
 * ------------------------------------------------------------------------*/

static Void startGHCExports ( ConId mn, List exlist )
{
#   ifdef DEBUG_IFACE
    fprintf(stderr,"startGHCExports %s\n", textToStr(textOf(mn)) );
#   endif
   /* Nothing to do. */
}

static Void finishGHCExports ( ConId mn, List exlist )
{
#   ifdef DEBUG_IFACE
    fprintf(stderr,"finishGHCExports %s\n", textToStr(textOf(mn)) );
#   endif
   /* Nothing to do. */
}


/* --------------------------------------------------------------------------
 * Imports
 * ------------------------------------------------------------------------*/

static Void startGHCImports ( ConId mn, List syms )
/* nm     the module to import from */
/* syms   [ConId | VarId] -- the names to import */
{
#  ifdef DEBUG_IFACE
   fprintf(stderr,"startGHCImports %s\n", textToStr(textOf(mn)) );
#  endif
   /* Nothing to do. */
}


static Void finishGHCImports ( ConId nm, List syms )
/* nm     the module to import from */
/* syms   [ConId | VarId] -- the names to import */
{
#  ifdef DEBUG_IFACE
   fprintf(stderr,"finishGHCImports %s\n", textToStr(textOf(nm)) );
#  endif
  /* Nothing to do. */
}


/* --------------------------------------------------------------------------
 * Fixity decls
 * ------------------------------------------------------------------------*/

static Void finishGHCFixdecl ( Cell prec, Cell assoc, ConVarId name )
{
   Int  p = intOf(prec);
   Int  a = intOf(assoc);
   Name n = findName(textOf(name));
   assert (nonNull(n));
   name(n).syntax = mkSyntax ( a, p );
}


/* --------------------------------------------------------------------------
 * Vars (values)
 * ------------------------------------------------------------------------*/

/* convert a leading run of DICTAPs into Hugs' internal Qualtype form, viz:
   { C1 a } -> { C2 b } -> T            into
   ap(QUALTYPE, ( [(C1,a),(C2,b)], T ))
*/
static Type dictapsToQualtype ( Type ty )
{
   List pieces = NIL;
   List preds, dictaps;

   /* break ty into pieces at the top-level arrows */
   while (isAp(ty) && isAp(fun(ty)) && fun(fun(ty))==typeArrow) {
      pieces = cons ( arg(fun(ty)), pieces );
      ty     = arg(ty);
   }
   pieces = cons ( ty, pieces );
   pieces = reverse ( pieces );

   dictaps = NIL;
   while (nonNull(pieces) && whatIs(hd(pieces))==DICTAP) {
      dictaps = cons ( hd(pieces), dictaps );
      pieces = tl(pieces);
   }

   /* dictaps holds the predicates, backwards */
   /* pieces holds the remainder of the type, forwards */
   assert(nonNull(pieces));
   pieces = reverse(pieces);
   ty = hd(pieces);
   pieces = tl(pieces);
   for (; nonNull(pieces); pieces=tl(pieces)) 
      ty = fn(hd(pieces),ty);

   preds = NIL;
   for (; nonNull(dictaps); dictaps=tl(dictaps)) {
      Cell da = hd(dictaps);
      QualId cl = fst(unap(DICTAP,da));
      Cell   arg = snd(unap(DICTAP,da));
      preds = cons ( pair(cl,arg), preds );
   }

   if (nonNull(preds)) ty = ap(QUAL, pair(preds,ty));
   return ty;
}



static void startGHCValue ( Int line, VarId vid, Type ty )
{
    Name   n;
    List   tmp, tvs;
    Text   v = textOf(vid);

#   ifdef DEBUG_IFACE
    fprintf(stderr,"begin startGHCValue %s\n", textToStr(v));
#   endif

    line = intOf(line);
    n = findName(v);
    if (nonNull(n) && name(n).defn != PREDEFINED) {
        ERRMSG(line) "Attempt to redefine variable \"%s\"", textToStr(v)
        EEND;
    }
    if (isNull(n)) n = newName(v,NIL);

    ty = dictapsToQualtype(ty);

    tvs = ifTyvarsIn(ty);
    for (tmp=tvs; nonNull(tmp); tmp=tl(tmp))
       hd(tmp) = zpair(hd(tmp),STAR);
    if (nonNull(tvs))
       ty = mkPolyType(tvsToKind(tvs),ty);

    ty = tvsToOffsets(line,ty,tvs);
    name(n).type  = ty;
    name(n).arity = arityInclDictParams(ty);
    name(n).line  = line;
    name(n).defn  = NIL;
}


static void finishGHCValue ( VarId vid )
{
    Name n    = findName ( textOf(vid) );
    Int  line = name(n).line;
#   ifdef DEBUG_IFACE
    fprintf(stderr, "begin finishGHCValue %s\n", textToStr(name(n).text) );
#   endif
    assert(currentModule == name(n).mod);
    name(n).type = conidcellsToTycons(line,name(n).type);

    if (isIfaceDefaultMethodName(name(n).text)) {
       /* ... we need to set .parent to point to the class 
          ... once we figure out what the class actually is :-)
       */
       Type t = name(n).type;
       assert(isPolyType(t));
       if (isPolyType(t)) t = monotypeOf(t);
       assert(isQualType(t));
       t = fst(snd(t));       /* t :: [(Class,Offset)] */
       assert(nonNull(t));
       assert(nonNull(hd(t)));
       assert(isPair(hd(t)));
       t = fst(hd(t));        /* t :: Class */
       assert(isClass(t));
       
       name(n).parent = t;    /* phew! */
    }
}


/* --------------------------------------------------------------------------
 * Type synonyms
 * ------------------------------------------------------------------------*/

static Void startGHCSynonym ( Int line, ConId tycon, List tvs, Type ty )
{
    /* tycon :: ConId             */
    /* tvs   ::  [((VarId,Kind))] */
    /* ty    :: Type              */ 
    Text t = textOf(tycon);
#   ifdef DEBUG_IFACE
    fprintf(stderr, "begin startGHCSynonym %s\n", textToStr(t) );
#   endif
    line = intOf(line);
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

static Type qualifyIfaceType ( Type unqual, List ctx )
{
   /* ctx :: [((QConId,VarId))] */
   /* ctx is a list of (class name, tyvar) pairs.  
      Attach to unqual qualifiers taken from ctx
      for each tyvar which appears in unqual.
   */
   List tyvarsMentioned; /* :: [VarId] */
   List ctx2  = NIL;
   Cell kinds = NIL;

   if (isPolyType(unqual)) {
      kinds  = polySigOf(unqual);
      unqual = monotypeOf(unqual);
   }

   assert(!isQualType(unqual));
   tyvarsMentioned = ifTyvarsIn ( unqual );
   for (; nonNull(ctx); ctx=tl(ctx)) {
      ZPair ctxElem = hd(ctx); /* :: ((QConId, VarId)) */
      if (nonNull(varIsMember(textOf(zsnd(ctxElem)),tyvarsMentioned)))
         ctx2 = cons(ctxElem, ctx2);
   }
   if (nonNull(ctx2))
      unqual = ap(QUAL,pair(reverse(ctx2),unqual));
   if (nonNull(kinds))
      unqual = mkPolyType(kinds,unqual);
   return unqual;
}


static Void startGHCDataDecl(line,ctx0,tycon,ktyvars,constrs0)
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
    List    tmp, conArgs, sels, constrs, fields;
    Triple  constr;
    Cell    conid;
    Pair    conArg, ctxElem;
    Text    conArgNm;
    Int     conArgStrictness;
    Int     conStrictCompCount;

    Text t = textOf(tycon);
#   ifdef DEBUG_IFACE
    fprintf(stderr, "begin startGHCDataDecl %s\n",textToStr(t));
#   endif

    line = intOf(line);
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
           resTy = ap(resTy,zfst(hd(tmp)));

        /* for each constructor ... */
        for (constrs=constrs0; nonNull(constrs); constrs=tl(constrs)) {
           constr = hd(constrs);
           conid  = zfst(constr);
           fields = zsnd(constr);

           /* Build type of constr and handle any selectors found. */
           ty = resTy;

           conStrictCompCount = 0;
           conArgs = reverse(fields);
           for (; nonNull(conArgs); conArgs=tl(conArgs)) {
              conArg           = hd(conArgs); /* (Type,Text) */
              conArgTy         = zfst3(conArg);
              conArgNm         = zsnd3(conArg);
              conArgStrictness = intOf(zthd3(conArg));
              if (conArgStrictness > 0) conStrictCompCount++;
              ty = fn(conArgTy,ty);
              if (nonNull(conArgNm)) {
                 /* a field name is mentioned too */
                 selTy = fn(resTy,conArgTy);
                 if (whatIs(tycon(tc).kind) != STAR)
                    selTy = pair(POLYTYPE,pair(tycon(tc).kind, selTy));
                 selTy = qualifyIfaceType ( selTy, ctx0 );
                 selTy = tvsToOffsets(line,selTy, ktyvars);
                 sels = cons( zpair(conArgNm,selTy), sels);
              }
           }

           /* Now ty is the constructor's type, not including context.
              Throw away any parts of the context not mentioned in ty,
              and use it to qualify ty.
	   */
           ty = qualifyIfaceType ( ty, ctx0 );

           /* stick the tycon's kind on, if not simply STAR */
           if (whatIs(tycon(tc).kind) != STAR)
              ty = pair(POLYTYPE,pair(tycon(tc).kind, ty));

           ty = tvsToOffsets(line,ty, ktyvars);

           /* Finally, stick the constructor's type onto it. */
           hd(constrs) = z4ble(conid,fields,ty,mkInt(conStrictCompCount));
        }

        /* Final result is that 
           constrs :: [((ConId,[((Type,Text))],Type,Int))]   
                      lists the constructors, their types and # strict comps
           sels :: [((VarId,Type))]
                   lists the selectors and their types
	*/
        tycon(tc).defn = startGHCConstrs(line,constrs0,sels);
    }
}


static List startGHCConstrs ( Int line, List cons, List sels )
{
    /* cons :: [((ConId,[((Type,Text,Int))],Type,Int))] */
    /* sels :: [((VarId,Type))]                         */
    /* returns [Name]                                   */
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


static Name startGHCConstr ( Int line, Int conNo, Z4Ble constr )
{
    /* constr :: ((ConId,[((Type,Text,Int))],Type,Int)) */
    /* (ADR) ToDo: add rank2 annotation and existential annotation
     * these affect how constr can be used.
     */
    Text con     = textOf(zsel14(constr));
    Type type    = zsel34(constr);
    Int  arity   = arityFromType(type);
    Int  nStrict = intOf(zsel44(constr));
    Name n = findName(con);     /* Allocate constructor fun name   */
    if (isNull(n)) {
        n = newName(con,NIL);
    } else if (name(n).defn!=PREDEFINED) {
        ERRMSG(line) "Repeated definition for constructor \"%s\"",
            textToStr(con)
        EEND;
    }
    name(n).arity     = arity;     /* Save constructor fun details    */
    name(n).line      = line;
    name(n).number    = cfunNo(conNo);
    name(n).type      = type;
    name(n).hasStrict = nStrict > 0;
    return n;
}


static List finishGHCDataDecl ( ConId tyc )
{
    List  nms;
    Tycon tc = findTycon(textOf(tyc));
#   ifdef DEBUG_IFACE
    fprintf ( stderr, "begin finishGHCDataDecl %s\n", 
              textToStr(textOf(tyc)) );
#   endif
    if (isNull(tc)) internal("finishGHCDataDecl");
    
    for (nms=tycon(tc).defn; nonNull(nms); nms=tl(nms)) {
       Name n    = hd(nms);
       Int  line = name(n).line;
       assert(currentModule == name(n).mod);
       name(n).type   = conidcellsToTycons(line,name(n).type);
       name(n).parent = tc; //---????
    }

    return tycon(tc).defn;
}


/* --------------------------------------------------------------------------
 * Newtype decls
 * ------------------------------------------------------------------------*/

static Void startGHCNewType ( Int line, List ctx0, 
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

    line = intOf(line);

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
    fprintf ( stderr, "begin finishGHCNewType %s\n", 
              textToStr(textOf(tyc)) );
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

static Void startGHCClass(line,ctxt,tc_name,kinded_tvs,mems0)
Int   line;
List  ctxt;       /* [((QConId, VarId))]   */ 
ConId tc_name;    /* ConId                 */
List  kinded_tvs; /* [((VarId, Kind))]     */
List  mems0; {    /* [((VarId, Type))]     */

    List mems;    /* [((VarId, Type))]     */
    List tvsInT;  /* [VarId] and then [((VarId,Kind))] */
    List tvs;     /* [((VarId,Kind))]      */
    List ns;      /* [Name]                */
    Int  mno;

    ZPair kinded_tv = hd(kinded_tvs);
    Text ct         = textOf(tc_name);
    Pair newCtx     = pair(tc_name, zfst(kinded_tv));
#   ifdef DEBUG_IFACE
    fprintf ( stderr, "begin startGHCClass %s\n", textToStr(ct) );
#   endif

    line = intOf(line);
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
        cclass(nw).kinds      = singleton( zsnd(kinded_tv) );
        cclass(nw).instances  = NIL;
        cclass(nw).numSupers  = length(ctxt);

        /* Kludge to map the single tyvar in the context to Offset 0.
           Need to do something better for multiparam type classes.
        */
        cclass(nw).supers     = tvsToOffsets(line,ctxt,
                                             singleton(kinded_tv));


        for (mems=mems0; nonNull(mems); mems=tl(mems)) {
           ZPair mem  = hd(mems);
           Type  memT = zsnd(mem);
           Text  mnt  = textOf(zfst(mem));
           Name  mn;

           /* Stick the new context on the member type */
           memT = dictapsToQualtype(memT);
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

           /* ToDo: maximally bogus.  We allow the class tyvar to
              have the kind as supplied by the parser, but we just
              assume that all others have kind *.  It's a kludge.
           */
           for (tvs=tvsInT; nonNull(tvs); tvs=tl(tvs)) {
              Kind k;
              if (textOf(hd(tvs)) == textOf(zfst(kinded_tv)))
                 k = zsnd(kinded_tv); else
                 k = STAR;
              hd(tvs) = zpair(hd(tvs),k);
           }
           /* tvsIntT :: [((VarId,Kind))] */

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

        ns = NIL;
        for (mno=0; mno<cclass(nw).numSupers; mno++) {
           ns = cons(newDSel(nw,mno),ns);
        }
        cclass(nw).dsels = rev(ns);
    }
}


static Class finishGHCClass ( Tycon cls_tyc )
{
    List  mems;
    Int   line;
    Int   ctr;
    Class nw = findClass ( textOf(cls_tyc) );
#   ifdef DEBUG_IFACE
    fprintf ( stderr, "begin finishGHCClass %s\n", textToStr(cclass(nw).text) );
#   endif
    if (isNull(nw)) internal("finishGHCClass");

    line = cclass(nw).line;
    ctr = -2;
    assert (currentModule == cclass(nw).mod);

    cclass(nw).level   = 0;
    cclass(nw).head    = conidcellsToTycons(line,cclass(nw).head);
    cclass(nw).supers  = conidcellsToTycons(line,cclass(nw).supers);
    cclass(nw).members = conidcellsToTycons(line,cclass(nw).members);

    for (mems=cclass(nw).members; nonNull(mems); mems=tl(mems)) {
       Pair mem = hd(mems); /* (VarId, Type) */
       Text txt = textOf(fst(mem));
       Type ty  = snd(mem);
       Name n   = findName(txt);
       assert(nonNull(n));
       name(n).text   = txt;
       name(n).line   = cclass(nw).line;
       name(n).type   = ty;
       name(n).number = ctr--;
       name(n).arity  = arityInclDictParams(name(n).type);
       name(n).parent = nw;
       hd(mems) = n;
    }

    return nw;
}


/* --------------------------------------------------------------------------
 * Instances
 * ------------------------------------------------------------------------*/

static Inst startGHCInstance (line,ktyvars,cls,var)
Int   line;
List  ktyvars; /* [((VarId,Kind))] */
Type  cls;     /* Type  */
VarId var; {   /* VarId */
    List tmp, tvs, ks, spec;

    List xs1, xs2;
    Kind k;

    Inst in = newInst();
#   ifdef DEBUG_IFACE
    fprintf ( stderr, "begin startGHCInstance\n" );
#   endif

    line = intOf(line);

    tvs = ifTyvarsIn(cls);  /* :: [VarId] */
    /* tvs :: [VarId].
       The order of tvs is important for tvsToOffsets.
       tvs should be a permutation of ktyvars.  Fish the tyvar kinds
       out of ktyvars and attach them to tvs.
    */
    for (xs1=tvs; nonNull(xs1); xs1=tl(xs1)) {
       k = NIL;
       for (xs2=ktyvars; nonNull(xs2); xs2=tl(xs2))
          if (textOf(hd(xs1)) == textOf(zfst(hd(xs2))))
             k = zsnd(hd(xs2));
       if (isNull(k)) internal("startGHCInstance: finding kinds");
       hd(xs1) = zpair(hd(xs1),k);
    }

    cls = tvsToOffsets(line,cls,tvs);
    spec = NIL;
    while (isAp(cls)) {
       spec = cons(fun(cls),spec);
       cls  = arg(cls);
    }
    spec = reverse(spec);

    inst(in).line         = line;
    inst(in).implements   = NIL;
    inst(in).kinds        = simpleKind(length(tvs)); /* do this right */
    inst(in).specifics    = spec;
    inst(in).numSpecifics = length(spec);
    inst(in).head         = cls;

    /* Figure out the name of the class being instanced, and store it
       at inst(in).c.  finishGHCInstance will resolve it to a real Class. */
    { 
       Cell cl = inst(in).head;
       assert(whatIs(cl)==DICTAP);
       cl = unap(DICTAP,cl);       
       cl = fst(cl);
       assert ( isQCon(cl) );
       inst(in).c = cl;
    }

    {
        Name b         = newName( /*inventText()*/ textOf(var),NIL);
        name(b).line   = line;
        name(b).arity  = length(spec); /* unused? */ /* and surely wrong */
        name(b).number = DFUNNAME;
        name(b).parent = in;
        inst(in).builder = b;
        /* bindNameToClosure(b, lookupGHCClosure(inst(in).mod,var)); */
    }

    return in;
}


static Void finishGHCInstance ( Inst in )
{
    Int    line;
    Class  c;
    Type   cls;

#   ifdef DEBUG_IFACE
    fprintf ( stderr, "begin finishGHCInstance\n" );
#   endif

    assert (nonNull(in));
    line = inst(in).line;
    assert (currentModule==inst(in).mod);

    /* inst(in).c is, prior to finishGHCInstance, a ConId or Tuple,
       since startGHCInstance couldn't possibly have resolved it to
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
      case DICTAP: /* :: ap(DICTAP, pair(Class,Type))
                      Not sure if this is really the right place to
                      convert it to the form Hugs wants, but will do so anyway.
                    */
         /* return ap(DICTAP, conidcellsToTycons(line, snd(type))); */
	{
           Class cl   = fst(unap(DICTAP,type));
           List  args = snd(unap(DICTAP,type));
           return
              conidcellsToTycons(line,pair(cl,args));
        }
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
      case TYCON:
         return TRUE;

      default: 
         fprintf(stderr, "allTypesKnown: unknown stuff %d\n", whatIs(type));
         print(type,10);printf("\n");
         internal("allTypesKnown");
         return TRUE; /*notreached*/
   }
  missing:
#  ifdef DEBUG_IFACE
   fprintf ( stderr,"allTypesKnown: unknown " ); print(type,10); 
   fprintf(stderr,"\n");
#  endif
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
 * General object symbol query stuff
 * ------------------------------------------------------------------------*/

#define EXTERN_SYMS_ALLPLATFORMS     \
      Sym(MainRegTable)              \
      Sym(stg_gc_enter_1)            \
      Sym(stg_gc_noregs)             \
      Sym(stg_gc_seq_1)              \
      Sym(stg_gc_d1)                 \
      Sym(stg_gc_f1)                 \
      Sym(stg_chk_0)                 \
      Sym(stg_chk_1)                 \
      Sym(stg_gen_chk)               \
      Sym(stg_exit)                  \
      Sym(stg_update_PAP)            \
      Sym(stg_error_entry)           \
      Sym(__ap_2_upd_info)           \
      Sym(__ap_3_upd_info)           \
      Sym(__ap_4_upd_info)           \
      Sym(__ap_5_upd_info)           \
      Sym(__ap_6_upd_info)           \
      Sym(__ap_7_upd_info)           \
      Sym(__ap_8_upd_info)           \
      Sym(__sel_0_upd_info)          \
      Sym(__sel_1_upd_info)          \
      Sym(__sel_2_upd_info)          \
      Sym(__sel_3_upd_info)          \
      Sym(__sel_4_upd_info)          \
      Sym(__sel_5_upd_info)          \
      Sym(__sel_6_upd_info)          \
      Sym(__sel_7_upd_info)          \
      Sym(__sel_8_upd_info)          \
      Sym(__sel_9_upd_info)          \
      Sym(__sel_10_upd_info)         \
      Sym(__sel_11_upd_info)         \
      Sym(__sel_12_upd_info)         \
      Sym(Upd_frame_info)            \
      Sym(seq_frame_info)            \
      Sym(CAF_BLACKHOLE_info)        \
      Sym(IND_STATIC_info)           \
      Sym(EMPTY_MVAR_info)           \
      Sym(MUT_ARR_PTRS_FROZEN_info)  \
      Sym(newCAF)                    \
      Sym(putMVarzh_fast)            \
      Sym(newMVarzh_fast)            \
      Sym(takeMVarzh_fast)           \
      Sym(catchzh_fast)              \
      Sym(raisezh_fast)              \
      Sym(delayzh_fast)              \
      Sym(yieldzh_fast)              \
      Sym(killThreadzh_fast)         \
      Sym(waitReadzh_fast)           \
      Sym(waitWritezh_fast)          \
      Sym(CHARLIKE_closure)          \
      Sym(INTLIKE_closure)           \
      Sym(suspendThread)             \
      Sym(resumeThread)              \
      Sym(stackOverflow)             \
      Sym(int2Integerzh_fast)        \
      Sym(stg_gc_unbx_r1)            \
      Sym(ErrorHdrHook)              \
      Sym(makeForeignObjzh_fast)     \
      Sym(__encodeDouble)            \
      Sym(decodeDoublezh_fast)       \
      Sym(isDoubleNaN)               \
      Sym(isDoubleInfinite)          \
      Sym(isDoubleDenormalized)      \
      Sym(isDoubleNegativeZero)      \
      Sym(__encodeFloat)             \
      Sym(decodeFloatzh_fast)        \
      Sym(isFloatNaN)                \
      Sym(isFloatInfinite)           \
      Sym(isFloatDenormalized)       \
      Sym(isFloatNegativeZero)       \
      Sym(__int_encodeFloat)         \
      Sym(__int_encodeDouble)        \
      Sym(mpz_cmp_si)                \
      Sym(mpz_cmp)                   \
      Sym(__mpn_gcd_1)               \
      Sym(gcdIntegerzh_fast)         \
      Sym(newArrayzh_fast)           \
      Sym(unsafeThawArrayzh_fast)    \
      Sym(newDoubleArrayzh_fast)     \
      Sym(newFloatArrayzh_fast)      \
      Sym(newAddrArrayzh_fast)       \
      Sym(newWordArrayzh_fast)       \
      Sym(newIntArrayzh_fast)        \
      Sym(newCharArrayzh_fast)       \
      Sym(newMutVarzh_fast)          \
      Sym(quotRemIntegerzh_fast)     \
      Sym(quotIntegerzh_fast)        \
      Sym(remIntegerzh_fast)         \
      Sym(divExactIntegerzh_fast)    \
      Sym(divModIntegerzh_fast)      \
      Sym(timesIntegerzh_fast)       \
      Sym(minusIntegerzh_fast)       \
      Sym(plusIntegerzh_fast)        \
      Sym(addr2Integerzh_fast)       \
      Sym(mkWeakzh_fast)             \
      Sym(prog_argv)                 \
      Sym(prog_argc)                 \
      Sym(resetNonBlockingFd)        \
      Sym(getStablePtr)              \
      Sym(stable_ptr_table)          \
      Sym(createAdjThunk)            \
      Sym(shutdownHaskellAndExit)    \
      Sym(stg_enterStackTop)         \
      Sym(CAF_UNENTERED_entry)       \
      Sym(stg_yield_to_Hugs)         \
      Sym(StgReturn)                 \
      Sym(init_stack)                \
                                     \
      /* needed by libHS_cbits */    \
      SymX(malloc)                   \
      SymX(close)                    \
      Sym(mkdir)                     \
      SymX(close)                    \
      Sym(opendir)                   \
      Sym(closedir)                  \
      Sym(readdir)                   \
      Sym(tcgetattr)                 \
      Sym(tcsetattr)                 \
      SymX(isatty)                   \
      SymX(read)                     \
      SymX(lseek)                    \
      SymX(write)                    \
      Sym(getrusage)                 \
      Sym(gettimeofday)              \
      SymX(realloc)                  \
      SymX(getcwd)                   \
      SymX(free)                     \
      SymX(strcpy)                   \
      Sym(fcntl)                     \
      SymX(fprintf)                  \
      SymX(exit)                     \
      Sym(open)                      \
      SymX(unlink)                   \
      SymX(memcpy)                   \
      SymX(memchr)                   \
      SymX(rmdir)                    \
      SymX(rename)                   \
      SymX(chdir)                    \
      SymX(execl)                    \
      Sym(waitpid)                   \
      SymX(getenv)

#define EXTERN_SYMS_cygwin32         \
      SymX(GetCurrentProcess)        \
      SymX(GetProcessTimes)          \
      Sym(__udivdi3)                 \
      SymX(bzero)                    \
      Sym(select)                    \
      SymX(_impure_ptr)              \
      Sym(lstat)                     \
      Sym(setmode)                   \
      SymX(system)                   \
      SymX(sleep)                    \
      SymX(__imp__tzname)            \
      SymX(__imp__timezone)          \
      SymX(tzset)                    \
      Sym(log)                       \
      Sym(exp)                       \
      Sym(sqrt)                      \
      Sym(sin)                       \
      Sym(cos)                       \
      Sym(tan)                       \
      Sym(asin)                      \
      Sym(acos)                      \
      Sym(atan)                      \
      Sym(sinh)                      \
      Sym(cosh)                      \
      Sym(tanh)                      \
      Sym(pow)                       \
      Sym(__errno)                   \
      Sym(stat)                      \
      Sym(fstat)

#define EXTERN_SYMS_linux            \
      Sym(__errno_location)          \
      Sym(__xstat)                   \
      Sym(__fxstat)                  \
      Sym(__lxstat)                  \
      SymX(select)                   \
      SymX(stderr)                   \
      SymX(vfork)                    \
      SymX(_exit)                    \
      SymX(tzname)                   \
      SymX(localtime)                \
      SymX(strftime)                 \
      SymX(timezone)                 \
      SymX(mktime)                   \
      SymX(gmtime)                   \
      Sym(setitimer)                 \



#if defined(linux_TARGET_OS)
#define EXTERN_SYMS_THISPLATFORM EXTERN_SYMS_linux
#endif

#if defined(solaris2_TARGET_OS)
#define EXTERN_SYMS_THISPLATFORM EXTERN_SYMS_solaris2
#endif

#if defined(cygwin32_TARGET_OS)
#define EXTERN_SYMS_THISPLATFORM EXTERN_SYMS_cygwin32
#endif




/* entirely bogus claims about types of these symbols */
#define Sym(vvv)  extern void (vvv);
#define SymX(vvv) /**/
EXTERN_SYMS_ALLPLATFORMS
EXTERN_SYMS_THISPLATFORM
#undef Sym
#undef SymX


#define Sym(vvv)  { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
                    &(vvv) },
#define SymX(vvv) { MAYBE_LEADING_UNDERSCORE_STR(#vvv), \
                    &(vvv) },
OSym rtsTab[] 
   = { 
       EXTERN_SYMS_ALLPLATFORMS
       EXTERN_SYMS_THISPLATFORM
       {0,0} 
     };
#undef Sym
#undef SymX


void init_stack;


/* A kludge to assist Win32 debugging. */
char* nameFromStaticOPtr ( void* ptr )
{
   int k;
   for (k = 0; rtsTab[k].nm; k++)
      if (ptr == rtsTab[k].ad)
         return rtsTab[k].nm;
   return NULL;
}


void* lookupObjName ( char* nm )
{
   int    k;
   char*  pp;
   void*  a;
   Text   t;
   Module m;
   char   nm2[200];
   int    first_real_char;

   nm2[199] = 0;
   strncpy(nm2,nm,200);

   /*  first see if it's an RTS name */
   for (k = 0; rtsTab[k].nm; k++)
      if (0==strcmp(nm2,rtsTab[k].nm))
         return rtsTab[k].ad;

   /* perhaps an extra-symbol ? */
   a = lookupOExtraTabName ( nm );
   if (a) return a;

#  if LEADING_UNDERSCORE
   first_real_char = 1;
#  else
   first_real_char = 0;
#  endif

   /* Maybe it's an __init_Module thing? */
   if (strlen(nm2+first_real_char) > 7
       && strncmp(nm2+first_real_char, "__init_", 7)==0) {
      t = unZcodeThenFindText(nm2+first_real_char+7);
      if (t == findText("PrelGHC")) return (4+NULL); /* kludge */
      m = findModule(t);
      if (isNull(m)) goto not_found;
      a = lookupOTabName ( m, nm );
      if (a) return a;
      goto not_found;
   }

   /* if not an RTS name, look in the 
      relevant module's object symbol table
   */
   pp = strchr(nm2+first_real_char, '_');
   if (!pp || !isupper(nm2[first_real_char])) goto not_found;
   *pp = 0;
   t = unZcodeThenFindText(nm2+first_real_char);
   m = findModule(t);
   if (isNull(m)) goto not_found;

   a = lookupOTabName ( m, nm );  /* RATIONALISE */
   if (a) return a;

  not_found:
   fprintf ( stderr, 
             "lookupObjName: can't resolve name `%s'\n", 
             nm );
   assert(4-4);
   return NULL;
}


int is_dynamically_loaded_code_or_rodata_ptr ( char* p )
{
   OSectionKind sk = lookupSection(p);
   assert (sk != HUGS_SECTIONKIND_NOINFOAVAIL);
   return (sk == HUGS_SECTIONKIND_CODE_OR_RODATA);
}


int is_dynamically_loaded_rwdata_ptr ( char* p )
{
   OSectionKind sk = lookupSection(p);
   assert (sk != HUGS_SECTIONKIND_NOINFOAVAIL);
   return (sk == HUGS_SECTIONKIND_RWDATA);
}


int is_not_dynamically_loaded_ptr ( char* p )
{
   OSectionKind sk = lookupSection(p);
   assert (sk != HUGS_SECTIONKIND_NOINFOAVAIL);
   return (sk == HUGS_SECTIONKIND_OTHER);
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

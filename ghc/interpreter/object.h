
/* --------------------------------------------------------------------------
 * Machinery for dynamic loading and linking of object code.  Should be 
 * completely independent from the rest of Hugs so we can use it in
 * other applications if desired.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * ------------------------------------------------------------------------*/

#ifndef __HUGS_OBJECT_H
#define __HUGS_OBJECT_H

/* An entry in a very crude object symbol table */
typedef struct { char* nm; void* ad; } 
   OSym;


/* Indication of section kinds for loaded objects.  Needed by
   the GC for deciding whether or not a pointer on the stack
   is a code pointer.
*/
typedef enum { HUGS_SECTIONKIND_CODE_OR_RODATA,
               HUGS_SECTIONKIND_RWDATA,
               HUGS_SECTIONKIND_OTHER,
               HUGS_SECTIONKIND_NOINFOAVAIL } 
   OSectionKind;

typedef struct { void* start; void* end; OSectionKind kind; } 
   OSection;


/* Indication of the status of an ObjectCode structure.
   NOTINUSE  -- currently unused.
   OIMAGE    -- object image is in memory, but that's all.
   VERIFIED  -- OIMAGE + the loaded image has been verified as 
                a valid object file.
   HAVENAMES -- VERIFIED + names *defined* in this image have been 
                extracted from the image and placed in the oTab, 
                and also section info placed in sectionTab.
   RESOLVED  -- HAVENAMES + all names *used* in this image have
                successfully been resolved.
    
*/
typedef enum { OBJECT_NOTINUSE,
               OBJECT_OIMAGE,
               OBJECT_VERIFIED,
               OBJECT_HAVENAMES,
               OBJECT_RESOLVED }
   OStatus;


/* Top-level structure for an object module.  One of these is allocated
   for each object file in use.  This should really be an abstract type
   to clients.
*/
typedef
   struct __ObjectCode {
      OStatus   status;
      char*     objFileName;
      int       objFileSize;
      char*     formatName;            /* eg "ELF32", "DLL", "COFF", etc. */

      /* proc to call to deliver an error message to the client. */
      void      (*errMsg)(char*);

      /* proc to call to resolve symbols not defined in this module, 
         when asked to resolve symbols in this module */
      void*     (*clientLookup)(char*);

      /* ptr to malloc'd lump of memory holding the obj file */
      void*     oImage;

      /* ptr to object symbol table; lives in mallocville.  
         Dynamically expands. */
      OSym*     oTab;
      int       sizeoTab;
      int       usedoTab;

      /* The section-kind entries for this object module.  
         Dynamically expands. */    
      OSection* sectionTab;
      int       sizesectionTab;
      int       usedsectionTab;        

      /* Allow a chain of these things */
      struct __ObjectCode * next;
   }
   ObjectCode;


/* The API */
extern ObjectCode*  ocNew ( void  (*errMsg)(char*),
                            void* (*clientLookup)(char*),
                            char*  objFileName,
                            int    objFileSize );
                            
extern int /*Bool*/ ocLoadImage     ( ObjectCode* oc, int verb );
extern int /*Bool*/ ocVerifyImage   ( ObjectCode* oc, int verb );
extern int /*Bool*/ ocGetNames      ( ObjectCode* oc, int verb );
extern int /*Bool*/ ocResolve       ( ObjectCode* oc, int verb );
extern void         ocFree          ( ObjectCode* oc );

extern void*        ocLookupSym     ( ObjectCode* oc, char* sym );
extern char*        ocLookupAddr    ( ObjectCode* oc, void* addr );
extern OSectionKind ocLookupSection ( ObjectCode* oc, void* addr );

#endif

/*-------------------------------------------------------------------------*/


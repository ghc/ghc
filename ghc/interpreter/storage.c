
/* --------------------------------------------------------------------------
 * Primitives for manipulating global data structures
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: storage.c,v $
 * $Revision: 1.53 $
 * $Date: 2000/03/23 14:54:21 $
 * ------------------------------------------------------------------------*/

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "object.h"
#include <setjmp.h>

/*#define DEBUG_SHOWUSE*/

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/

static Int    local hash                ( String );
static Int    local saveText            ( Text );
static Module local findQualifier       ( Text );
static Void   local hashTycon           ( Tycon );
static List   local insertTycon         ( Tycon,List );
static Void   local hashName            ( Name );
static List   local insertName          ( Name,List );
static Void   local patternError        ( String );
static Bool   local stringMatch         ( String,String );
static Bool   local typeInvolves        ( Type,Type );
static Cell   local markCell            ( Cell );
static Void   local markSnd             ( Cell );
static Cell   local lowLevelLastIn      ( Cell );
static Cell   local lowLevelLastOut     ( Cell );


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
static  Text    savedText = TEXT_SIZE;  /* Start of saved portion of text  */
static  Text    nextNewText;            /* Next new text value             */
static  Text    nextNewDText;           /* Next new dict text value        */
static  char    text[TEXT_SIZE];        /* Storage of character strings    */
static  Text    textHash[TEXTHSZ][NUM_TEXTH]; /* Hash table storage        */

String textToStr(t)                    /* find string corresp to given Text*/
Text t; {
    static char newVar[16];

    if (isText(t))                              /* standard char string    */
        return text + t - TEXT_BASE_ADDR;
    if (isInventedDictVar(t)) {
        sprintf(newVar,"d%d",
                t-INDVAR_BASE_ADDR);            /* dictionary variable     */
        return newVar;
    }
    if (isInventedVar(t)) {
        sprintf(newVar,"v%d",
                t-INVAR_BASE_ADDR);             /* normal variable         */
       return newVar;
    }
    internal("textToStr");
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
    return 0; /* NOTREACHED */
}

Text inventText()     {                 /* return new unused variable name */
   if (nextNewText >= INVAR_BASE_ADDR+INVAR_MAX_AVAIL)
      internal("inventText: too many invented variables");
   return nextNewText++;
}

Text inventDictText() {                 /* return new unused dictvar name  */
   if (nextNewDText >= INDVAR_BASE_ADDR+INDVAR_MAX_AVAIL)
     internal("inventDictText: too many invented variables");
   return nextNewDText++;
}

Bool inventedText(t)                    /* Signal TRUE if text has been    */
Text t; {                               /* generated internally            */
    return isInventedVar(t) || isInventedDictVar(t);
}

#define MAX_FIXLIT 100
Text fixLitText(t)                /* fix literal text that might include \ */
Text t; {
    String   s = textToStr(t);
    char     p[MAX_FIXLIT];
    Int      i;
    for(i = 0;i < MAX_FIXLIT-2 && *s;s++) {
      p[i++] = *s;
      if (*s == '\\') {
	p[i++] = '\\';
      } 
    }
    if (i < MAX_FIXLIT-2) {
      p[i] = 0;
    } else {
	ERRMSG(0) "storage space exhausted for internal literal string"
	EEND;
    }
    return (findText(p));
}
#undef MAX_FIXLIT

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

#   define TryMatch     {   Text   originalTextPos = textPos;              \
                            String t;                                      \
                            for (t=s; *t==text[textPos]; textPos++,t++)    \
                                if (*t=='\0')                              \
                                    return originalTextPos+TEXT_BASE_ADDR; \
                        }
#   define Skip         while (text[textPos++]) ;

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

    return textPos+TEXT_BASE_ADDR;
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


static int fromHexDigit ( char c )
{
   switch (c) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
         return c - '0';
      case 'a': case 'A': return 10;
      case 'b': case 'B': return 11;
      case 'c': case 'C': return 12;
      case 'd': case 'D': return 13;
      case 'e': case 'E': return 14;
      case 'f': case 'F': return 15;
      default: return -1;
   }
}


/* returns findText (unZencode s) */
Text unZcodeThenFindText ( String s )
{
   unsigned char* p;
   Int            n, nn, i;
   Text           t;

   assert(s);
   nn = 100 + 10 * strlen(s);
   p = malloc ( nn );
   if (!p) internal ("unZcodeThenFindText: malloc failed");
   n = 0;

   while (1) {
      if (!(*s)) break;
      if (n > nn-90) internal ("unZcodeThenFindText: result is too big");
      if (*s != 'z' && *s != 'Z') {
         p[n] = *s; n++; s++; 
         continue;
      }
      s++;
      if (!(*s)) goto parse_error;
      switch (*s++) {
         case 'Z': p[n++] = 'Z'; break;
         case 'C': p[n++] = ':'; break;
         case 'L': p[n++] = '('; break;
         case 'R': p[n++] = ')'; break;
         case 'M': p[n++] = '['; break;
         case 'N': p[n++] = ']'; break;
         case 'z': p[n++] = 'z'; break;
         case 'a': p[n++] = '&'; break;
         case 'b': p[n++] = '|'; break;
         case 'd': p[n++] = '$'; break;
         case 'e': p[n++] = '='; break;
         case 'g': p[n++] = '>'; break;
         case 'h': p[n++] = '#'; break;
         case 'i': p[n++] = '.'; break;
         case 'l': p[n++] = '<'; break;
         case 'm': p[n++] = '-'; break;
         case 'n': p[n++] = '!'; break;
         case 'p': p[n++] = '+'; break;
         case 'q': p[n++] = '\\'; break;
         case 'r': p[n++] = '\''; break;
         case 's': p[n++] = '/'; break;
         case 't': p[n++] = '*'; break;
         case 'u': p[n++] = '^'; break;
         case 'v': p[n++] = '%'; break;
         case 'x':
            if (!s[0] || !s[1]) goto parse_error;
            if (fromHexDigit(s[0]) < 0 || fromHexDigit(s[1]) < 0) goto parse_error;
            p[n++] = 16 * fromHexDigit(s[0]) + fromHexDigit(s[1]);
            p += 2; s += 2;
            break;
         case '0': case '1': case '2': case '3': case '4':
         case '5': case '6': case '7': case '8': case '9':
            i = 0;
            s--;
            while (*s && isdigit((int)(*s))) {
               i = 10 * i + (*s - '0');
               s++;
            }
            if (*s != 'T') goto parse_error;
            s++;
            p[n++] = '(';
            while (i > 0) { p[n++] = ','; i--; };
            p[n++] = ')';
            break;
         default: 
            goto parse_error;
      }      
   }
   p[n] = 0;
   t = findText(p);
   free(p);
   return t;

  parse_error:
   free(p);
   fprintf ( stderr, "\nstring = `%s'\n", s );
   internal ( "unZcodeThenFindText: parse error on above string");
   return NIL; /*notreached*/
}


Text enZcodeThenFindText ( String s )
{
   unsigned char* p;
   Int            n, nn;
   Text           t;
   char toHex[16] = "0123456789ABCDEF";

   assert(s);
   nn = 100 + 10 * strlen(s);
   p = malloc ( nn );
   if (!p) internal ("enZcodeThenFindText: malloc failed");
   n = 0;
   while (1) {
      if (!(*s)) break;
      if (n > nn-90) internal ("enZcodeThenFindText: result is too big");
      if (*s != 'z' 
          && *s != 'Z'
          && (isalnum((int)(*s)) || *s == '_')) { 
         p[n] = *s; n++; s++;
         continue;
      }
      if (*s == '(') {
         int tup = 0;
         char num[12];
         s++;
         while (*s && *s==',') { s++; tup++; };
         if (*s != ')') internal("enZcodeThenFindText: invalid tuple type");
         s++;
         p[n++] = 'Z';
         sprintf(num,"%d",tup);
         p[n] = 0; strcat ( &(p[n]), num ); n += strlen(num);
         p[n++] = 'T';
         continue;         
      }
      switch (*s++) {
         case '(': p[n++] = 'Z'; p[n++] = 'L'; break;
         case ')': p[n++] = 'Z'; p[n++] = 'R'; break;
         case '[': p[n++] = 'Z'; p[n++] = 'M'; break;
         case ']': p[n++] = 'Z'; p[n++] = 'N'; break;
         case ':': p[n++] = 'Z'; p[n++] = 'C'; break;
         case 'Z': p[n++] = 'Z'; p[n++] = 'Z'; break;
         case 'z': p[n++] = 'z'; p[n++] = 'z'; break;
         case '&': p[n++] = 'z'; p[n++] = 'a'; break;
         case '|': p[n++] = 'z'; p[n++] = 'b'; break;
         case '$': p[n++] = 'z'; p[n++] = 'd'; break;
         case '=': p[n++] = 'z'; p[n++] = 'e'; break;
         case '>': p[n++] = 'z'; p[n++] = 'g'; break;
         case '#': p[n++] = 'z'; p[n++] = 'h'; break;
         case '.': p[n++] = 'z'; p[n++] = 'i'; break;
         case '<': p[n++] = 'z'; p[n++] = 'l'; break;
         case '-': p[n++] = 'z'; p[n++] = 'm'; break;
         case '!': p[n++] = 'z'; p[n++] = 'n'; break;
         case '+': p[n++] = 'z'; p[n++] = 'p'; break;
         case '\'': p[n++] = 'z'; p[n++] = 'q'; break;
         case '\\': p[n++] = 'z'; p[n++] = 'r'; break;
         case '/': p[n++] = 'z'; p[n++] = 's'; break;
         case '*': p[n++] = 'z'; p[n++] = 't'; break;
         case '^': p[n++] = 'z'; p[n++] = 'u'; break;
         case '%': p[n++] = 'z'; p[n++] = 'v'; break;
         default: s--; p[n++] = 'z'; p[n++] = 'x';
                       p[n++] = toHex[(int)(*s)/16];
                       p[n++] = toHex[(int)(*s)%16];
                  s++; break;
      }
   }
   p[n] = 0;
   t = findText(p);
   free(p);
   return t;
}


Text textOf ( Cell c )
{
   Int  wot = whatIs(c);
   Bool ok = 
          (wot==VARIDCELL
           || wot==CONIDCELL
           || wot==VAROPCELL
           || wot==CONOPCELL
           || wot==STRCELL
           || wot==DICTVAR
           || wot==IPCELL
           || wot==IPVAR
          );
   if (!ok) {
      fprintf(stderr, "\ntextOf: bad tag %d\n",wot );
      internal("textOf: bad tag");
   }
   return snd(c);
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
 * Expandable symbol tables.  A template, which is instantiated for the name, 
 * tycon, class, instance and module tables.  Also, potentially, TREX Exts.
 * ------------------------------------------------------------------------*/

#define EXPANDABLE_SYMBOL_TABLE(type_name,struct_name,                  \
                                proc_name,free_proc_name,               \
                                free_list,tab_name,tab_size,err_msg,    \
                                TAB_INIT_SIZE,TAB_MAX_SIZE,             \
                                TAB_BASE_ADDR)                          \
                                                                        \
             struct struct_name* tab_name  = NULL;                      \
             int                 tab_size  = 0;                         \
      static type_name           free_list = TAB_BASE_ADDR-1;           \
                                                                        \
      void free_proc_name ( type_name n )                               \
      {                                                                 \
         assert(TAB_BASE_ADDR <= n);                                    \
         assert(n < TAB_BASE_ADDR+tab_size);                            \
         assert(tab_name[n-TAB_BASE_ADDR].inUse);                       \
         tab_name[n-TAB_BASE_ADDR].inUse = FALSE;                      \
         /*tab_name[n-TAB_BASE_ADDR].nextFree = free_list; */               \
         /*free_list = n;*/                                                 \
      }                                                                 \
                                                                        \
      type_name proc_name ( void )                                      \
      {                                                                 \
         Int    i;                                                      \
         Int    newSz;                                                  \
         struct struct_name* newTab;                                    \
         struct struct_name* temp;                                      \
         try_again:                                                     \
         if (free_list != TAB_BASE_ADDR-1) {                            \
            type_name t = free_list;                                    \
            free_list = tab_name[free_list-TAB_BASE_ADDR].nextFree;     \
            assert (!(tab_name[t-TAB_BASE_ADDR].inUse));                \
            tab_name[t-TAB_BASE_ADDR].inUse = TRUE;                     \
            return t;                                                   \
         }                                                              \
                                                                        \
         newSz = (tab_size == 0 ? TAB_INIT_SIZE : 2 * tab_size);        \
         if (newSz > TAB_MAX_SIZE) goto cant_allocate;                  \
         newTab = malloc(newSz * sizeof(struct struct_name));           \
         if (!newTab) goto cant_allocate;                               \
         for (i = 0; i < tab_size; i++)                                 \
            newTab[i] = tab_name[i];                                    \
         for (i = tab_size; i < newSz; i++) {                           \
            newTab[i].inUse = FALSE;                                    \
            newTab[i].nextFree = i-1+TAB_BASE_ADDR;                     \
         }                                                              \
          fprintf(stderr, "Expanding " #type_name                     \
                    "table to size %d\n", newSz );                    \
         newTab[tab_size].nextFree = TAB_BASE_ADDR-1;                   \
         free_list = newSz-1+TAB_BASE_ADDR;                             \
         tab_size = newSz;                                              \
         temp = tab_name;                                               \
         tab_name = newTab;                                             \
         if (temp) free(temp);                                          \
         goto try_again;                                                \
                                                                        \
         cant_allocate:                                                 \
         ERRMSG(0) err_msg                                              \
         EEND;                                                          \
      }                                                                 \



EXPANDABLE_SYMBOL_TABLE(Name,strName,allocNewName,freeName,
                        nameFL,tabName,tabNameSz,
                        "Name storage space exhausted",
                        NAME_INIT_SIZE,NAME_MAX_SIZE,NAME_BASE_ADDR)


EXPANDABLE_SYMBOL_TABLE(Tycon,strTycon,allocNewTycon,freeTycon,
                        tyconFL,tabTycon,tabTyconSz,
                        "Type constructor storage space exhausted",
                        TYCON_INIT_SIZE,TYCON_MAX_SIZE,TYCON_BASE_ADDR)


EXPANDABLE_SYMBOL_TABLE(Class,strClass,allocNewClass,freeClass,
                        classFL,tabClass,tabClassSz,
                        "Class storage space exhausted",
                        CCLASS_INIT_SIZE,CCLASS_MAX_SIZE,CCLASS_BASE_ADDR)


EXPANDABLE_SYMBOL_TABLE(Inst,strInst,allocNewInst,freeInst,
                        instFL,tabInst,tabInstSz,
                        "Instance storage space exhausted",
                        INST_INIT_SIZE,INST_MAX_SIZE,INST_BASE_ADDR)


EXPANDABLE_SYMBOL_TABLE(Module,strModule,allocNewModule,freeModule,
                        moduleFL,tabModule,tabModuleSz,
                        "Module storage space exhausted",
                        MODULE_INIT_SIZE,MODULE_MAX_SIZE,MODULE_BASE_ADDR)

#ifdef DEBUG_STORAGE
struct strName* generate_name_ref ( Cell nm )
{
   assert(isName(nm));
   nm -= NAME_BASE_ADDR;
   assert(tabName[nm].inUse);
   assert(isModule(tabName[nm].mod));
   return & tabName[nm]; 
}
struct strTycon* generate_tycon_ref ( Cell tc )
{
   assert(isTycon(tc) || isTuple(tc));
   tc -= TYCON_BASE_ADDR;
   assert(tabTycon[tc].inUse);
   assert(isModule(tabTycon[tc].mod));
   return & tabTycon[tc]; 
}
struct strClass* generate_cclass_ref ( Cell cl )
{
   assert(isClass(cl));
   cl -= CCLASS_BASE_ADDR;
   assert(tabClass[cl].inUse);
   assert(isModule(tabClass[cl].mod));
   return & tabClass[cl]; 
}
struct strInst* generate_inst_ref ( Cell in )
{  
   assert(isInst(in));
   in -= INST_BASE_ADDR;
   assert(tabInst[in].inUse);
   assert(isModule(tabInst[in].mod));
   return & tabInst[in]; 
}
struct strModule* generate_module_ref ( Cell mo )
{  
   assert(isModule(mo));
   mo -= MODULE_BASE_ADDR;
   assert(tabModule[mo].inUse);
   return & tabModule[mo]; 
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
     //#define tHash(x) (((x)-TEXT_BASE_ADDR)%TYCONHSZ)/* Tycon hash function     */
static int tHash(Text x)
{
   int r;
   assert(isText(x) || inventedText(x));
   x -= TEXT_BASE_ADDR;
   if (x < 0) x = -x;
   r= x%TYCONHSZ;
   assert(r>=0);
   assert(r<TYCONHSZ);
   return r;
}
static  Tycon    tyconHash[TYCONHSZ];           /* Hash table storage      */
int RC_T ( int x ) 
{
   assert (x >= 0 && x < TYCONHSZ);
   return x;
}
Tycon newTycon ( Text t )               /* add new tycon to tycon table    */
{
    Int   h                      = tHash(t);
    Tycon tc                     = allocNewTycon();
    tabTycon
      [tc-TYCON_BASE_ADDR].tuple = -1;
    tabTycon
      [tc-TYCON_BASE_ADDR].mod   = currentModule;
    tycon(tc).text               = t;   /* clear new tycon record          */
    tycon(tc).kind               = NIL;
    tycon(tc).defn               = NIL;
    tycon(tc).what               = NIL;
    tycon(tc).conToTag           = NIL;
    tycon(tc).tagToCon           = NIL;
    tycon(tc).itbl               = NULL;
    tycon(tc).arity              = 0;
    module(currentModule).tycons = cons(tc,module(currentModule).tycons);
    tycon(tc).nextTyconHash      = tyconHash[RC_T(h)];
    tyconHash[RC_T(h)]                 = tc;
    return tc;
}

Tycon findTycon(t)                      /* locate Tycon in tycon table     */
Text t; {
    Tycon tc = tyconHash[RC_T(tHash(t))];
assert(isTycon(tc) || isTuple(tc) || isNull(tc));
    while (nonNull(tc) && tycon(tc).text!=t)
	tc = tycon(tc).nextTyconHash;
    return tc;
}

Tycon addTycon(tc)  /* Insert Tycon in tycon table - if no clash is caused */
Tycon tc; {
    Tycon oldtc; 
    assert(isTycon(tc) || isTuple(tc));
    oldtc = findTycon(tycon(tc).text);
    if (isNull(oldtc)) {
        hashTycon(tc);
        module(currentModule).tycons=cons(tc,module(currentModule).tycons);
        return tc;
    } else
        return oldtc;
}

static Void local hashTycon(tc)         /* Insert Tycon into hash table    */
Tycon tc; {
   Text t;
   Int  h;
   assert(isTycon(tc) || isTuple(tc));
   {int i; for (i = 0; i < TYCONHSZ; i++)
       assert (tyconHash[i] == 0 
               || isTycon(tyconHash[i])
               || isTuple(tyconHash[i]));
   }
   t = tycon(tc).text;
   h = tHash(t);
   tycon(tc).nextTyconHash = tyconHash[RC_T(h)];
   tyconHash[RC_T(h)]            = tc;
}

Tycon findQualTycon(id) /*locate (possibly qualified) Tycon in tycon table */
Cell id; {
    if (!isPair(id)) internal("findQualTycon");
    switch (fst(id)) {
        case CONIDCELL :
        case CONOPCELL :
            return findTycon(textOf(id));
        case QUALIDENT : {
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
        }
        default : internal("findQualTycon2");
    }
    return NIL; /* NOTREACHED */
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
    for (tc = TYCON_BASE_ADDR;
         tc < TYCON_BASE_ADDR+tabTyconSz; ++tc)
        if (tabTycon[tc-TYCON_BASE_ADDR].inUse)
           if (!pat || stringMatch(pat,textToStr(tycon(tc).text)))
               if (nonNull(tycon(tc).kind))
                  ts = insertTycon(tc,ts);
    return ts;
}

Text ghcTupleText_n ( Int n )
{
    Int i;
    Int x = 0; 
    char buf[104];
    if (n < 0 || n >= 100) internal("ghcTupleText_n");
    if (n == 1) internal("ghcTupleText_n==1");
    buf[x++] = '(';
    for (i = 1; i <= n-1; i++) buf[x++] = ',';
    buf[x++] = ')';
    buf[x++] = 0;
    return findText(buf);
}

Text ghcTupleText(tup)
Tycon tup; {
    if (!isTuple(tup)) {
       assert(isTuple(tup));
    }
    return ghcTupleText_n ( tupleOf(tup) );
}


Tycon mkTuple ( Int n )
{
   Int i;
   if (n >= NUM_TUPLES)
      internal("mkTuple: request for tuple of unsupported size");
   for (i = TYCON_BASE_ADDR;
        i < TYCON_BASE_ADDR+tabTyconSz; i++)
      if (tabTycon[i-TYCON_BASE_ADDR].inUse)
         if (tycon(i).tuple == n) return i;
   internal("mkTuple: request for non-existent tuple");
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
//#define nHash(x) (((x)-TEXT_BASE_ADDR)%NAMEHSZ) /* hash fn :: Text->Int    */
static int nHash(Text x)
{
   assert(isText(x) || inventedText(x));
   x -= TEXT_BASE_ADDR;
   if (x < 0) x = -x;
   return x%NAMEHSZ;
}
static  Name     nameHash[NAMEHSZ];             /* Hash table storage      */
int RC_N ( int x ) 
{
   assert (x >= 0 && x < NAMEHSZ);
   return x;
}
void hashSanity ( void )
{
   Int i, j;
   for (i = 0; i < TYCONHSZ; i++) {
      j = tyconHash[i];
      while (nonNull(j)) {
         assert(isTycon(j) || isTuple(j));
         j = tycon(j).nextTyconHash;
      }
   }
   for (i = 0; i < NAMEHSZ; i++) {
      j = nameHash[i];
      while (nonNull(j)) {
         assert(isName(j));
         j = name(j).nextNameHash;
      }
   }
}

Name newName ( Text t, Cell parent )    /* Add new name to name table      */
{
    Int h = nHash(t);
    Name nm = allocNewName();
    tabName
       [nm-NAME_BASE_ADDR].mod  = currentModule;
    name(nm).text               = t;    /* clear new name record           */
    name(nm).line               = 0;
    name(nm).syntax             = NO_SYNTAX;
    name(nm).parent             = parent;
    name(nm).arity              = 0;
    name(nm).number             = EXECNAME;
    name(nm).defn               = NIL;
    name(nm).stgVar             = NIL;
    name(nm).callconv           = NIL;
    name(nm).type               = NIL;
    name(nm).primop             = NULL;
    name(nm).itbl               = NULL;
    module(currentModule).names = cons(nm,module(currentModule).names);
    name(nm).nextNameHash       = nameHash[RC_N(h)];
    nameHash[RC_N(h)]                 = nm;
    return nm;
}

Name findName(t)                        /* Locate name in name table       */
Text t; {
    Name n = nameHash[RC_N(nHash(t))];
assert(isText(t));
assert(isName(n) || isNull(n));
    while (nonNull(n) && name(n).text!=t)
	n = name(n).nextNameHash;
    return n;
}

Name addName(nm)                        /* Insert Name in name table - if  */
Name nm; {                              /* no clash is caused              */
    Name oldnm; 
    assert(isName(nm));
    oldnm = findName(name(nm).text);
    if (isNull(oldnm)) {
        hashName(nm);
        module(currentModule).names=cons(nm,module(currentModule).names);
        return nm;
    } else
        return oldnm;
}

static Void local hashName(nm)          /* Insert Name into hash table	   */
Name nm; {
    Text t;
    Int  h;
    assert(isName(nm));
    t = name(nm).text;
    h = nHash(t);
    name(nm).nextNameHash = nameHash[RC_N(h)];
    nameHash[RC_N(h)]           = nm;
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
            Text   t  = qtextOf(id);
            Module m  = findQualifier(qmodOf(id));
            List   es = NIL;
            if (isNull(m)) return NIL;
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
                       if (!isName(hd(subentities)))
                            internal("findQualName3");
                        if (name(hd(subentities)).text == t)
                            return hd(subentities);
                    }
                }
            }
            return NIL;
        }
        default : internal("findQualName2");
    }
    return 0; /* NOTREACHED */
}


Name nameFromStgVar ( StgVar v )
{
   Int n;
   for (n = NAME_BASE_ADDR;
        n < NAME_BASE_ADDR+tabNameSz; n++)
      if (tabName[n-NAME_BASE_ADDR].inUse)
         if (name(n).stgVar == v) return n;
   return NIL;
}

void* getHugs_AsmObject_for ( char* s )
{
   StgVar v;
   Text   t = findText(s);
   Name   n = NIL;
   for (n = NAME_BASE_ADDR; 
        n < NAME_BASE_ADDR+tabNameSz; n++)
      if (tabName[n-NAME_BASE_ADDR].inUse)
         if (name(n).text == t) break;
   if (n == NAME_BASE_ADDR+tabNameSz) {
      fprintf ( stderr, "can't find `%s' in ...\n", s );
      internal("getHugs_AsmObject_for(1)");
   }
   v = name(n).stgVar;
   if (!isStgVar(v) || !isPtr(stgVarInfo(v)))
      internal("getHugs_AsmObject_for(2)");
   return ptrOf(stgVarInfo(v));
}

/* --------------------------------------------------------------------------
 * Primitive functions:
 * ------------------------------------------------------------------------*/

Module findFakeModule ( Text t )
{
   Module m = findModule(t);
   if (nonNull(m)) {
      if (!module(m).fake) internal("findFakeModule");
   } else {
      m = newModule(t);
      module(m).fake = TRUE;
   }
   return m;
}


Name addWiredInBoxingTycon
        ( String modNm, String typeNm, String constrNm,
          Int rep, Kind kind )
{
   Name   n;
   Tycon  t;
   Text   modT  = findText(modNm);
   Text   typeT = findText(typeNm);
   Text   conT  = findText(constrNm);
   Module m     = findFakeModule(modT);
   setCurrModule(m);
   
   n = newName(conT,NIL);
   name(n).arity  = 1;
   name(n).number = cfunNo(0);
   name(n).type   = NIL;
   name(n).primop = (void*)rep;

   t = newTycon(typeT);
   tycon(t).what = DATATYPE;
   tycon(t).kind = kind;
   return n;
}


Tycon addTupleTycon ( Int n )
{
   Int    i;
   Kind   k;
   Tycon  t;
   Module m;
   Name   nm;

   for (i = TYCON_BASE_ADDR; 
        i < TYCON_BASE_ADDR+tabTyconSz; i++)
      if (tabTycon[i-TYCON_BASE_ADDR].inUse)
         if (tycon(i).tuple == n) return i;

   if (combined)
      m = findFakeModule(findText(n==0 ? "PrelBase" : "PrelTup")); else
      m = findModule(findText("Prelude"));

   setCurrModule(m);
   k = STAR;
   for (i = 0; i < n; i++) k = ap(STAR,k);
   t = newTycon(ghcTupleText_n(n));
   tycon(t).kind  = k;
   tycon(t).tuple = n;
   tycon(t).what  = DATATYPE;

   if (n == 0) {
      /* maybe we want to do this for all n ? */
      nm = newName(ghcTupleText_n(n), t);
      name(nm).type = t;   /* ummm ... for n > 0 */
   }

   return t;
}


Tycon addWiredInEnumTycon ( String modNm, String typeNm, 
                            List /*of Text*/ constrs )
{
   Int    i;
   Tycon  t;
   Text   modT  = findText(modNm);
   Text   typeT = findText(typeNm);
   Module m     = findFakeModule(modT);
   setCurrModule(m);

   t             = newTycon(typeT);
   tycon(t).kind = STAR;
   tycon(t).what = DATATYPE;
   
   constrs = reverse(constrs);
   i       = length(constrs);
   for (; nonNull(constrs); constrs=tl(constrs),i--) {
      Text conT        = hd(constrs);
      Name con         = newName(conT,t);
      name(con).number = cfunNo(i);
      name(con).type   = t;
      name(con).parent = t;
      tycon(t).defn    = cons(con, tycon(t).defn);      
   }
   return t;
}


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
                                        /* or invented names are excluded) */
#if 1
    for (nm = NAME_BASE_ADDR;
         nm < NAME_BASE_ADDR+tabNameSz; ++nm)
       if (tabName[nm-NAME_BASE_ADDR].inUse) {
          if (!inventedText(name(nm).text) && nonNull(name(nm).type)) {
             String str = textToStr(name(nm).text);
             if (str[0]!='_' && (!pat || stringMatch(pat,str)))
                 ns = insertName(nm,ns);
          }
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

static List  classes;                  /* list of classes in current scope */

Class newClass ( Text t )              /* add new class to class table     */
{
    Class cl                     = allocNewClass();
    tabClass
      [cl-CCLASS_BASE_ADDR].mod  = currentModule;
    cclass(cl).text              = t;
    cclass(cl).arity             = 0;
    cclass(cl).kinds             = NIL;
    cclass(cl).head              = NIL;
    cclass(cl).fds               = NIL;
    cclass(cl).xfds              = NIL;
    cclass(cl).dcon              = NIL;
    cclass(cl).supers            = NIL;
    cclass(cl).dsels             = NIL;
    cclass(cl).members           = NIL;
    cclass(cl).defaults          = NIL;
    cclass(cl).instances         = NIL;
    classes                      = cons(cl,classes);
    module(currentModule).classes
       = cons(cl,module(currentModule).classes);
    return cl;
}

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
    Class oldc; 
    assert(whatIs(c)==CLASS);
    oldc = findClass(cclass(c).text);
    if (isNull(oldc)) {
        classes=cons(c,classes);
        module(currentModule).classes=cons(c,module(currentModule).classes);
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
    }
    return NIL;
}

Inst newInst() {                       /* Add new instance to table        */
    Inst in                    = allocNewInst();
    tabInst
       [in-INST_BASE_ADDR].mod = currentModule;
    inst(in).kinds             = NIL;
    inst(in).head              = NIL;
    inst(in).specifics         = NIL;
    inst(in).implements        = NIL;
    inst(in).builder           = NIL;
    return in;
}

#ifdef DEBUG_DICTS
extern Void printInst ( Inst));

Void printInst(in)
Inst in; {
    Class cl = inst(in).c;
    Printf("%s-", textToStr(cclass(cl).text));
    printType(stdout,inst(in).t);
}
#endif /* DEBUG_DICTS */

Inst findFirstInst(tc)                  /* look for 1st instance involving */
Tycon tc; {                             /* the type constructor tc         */
    return findNextInst(tc,INST_BASE_ADDR-1);
}

Inst findNextInst(tc,in)                /* look for next instance involving*/
Tycon tc;                               /* the type constructor tc         */
Inst  in; {                             /* starting after instance in      */
    Cell pi;
    while (++in < INST_BASE_ADDR+tabInstSz) {
        if (!tabInst[in-INST_BASE_ADDR].inUse) continue;
        assert(isModule(inst(in).mod));
        pi = inst(in).head;
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


/* Needed by finishGHCInstance to find classes, before the
   export list has been built -- so we can't use 
   findQualClass.
*/
Class findQualClassWithoutConsultingExportList ( QualId q )
{
   Class cl;
   Text t_mod;
   Text t_class;

   assert(isQCon(q));

   if (isCon(q)) {
      t_mod   = NIL;
      t_class = textOf(q);
   } else {
      t_mod   = qmodOf(q);
      t_class = qtextOf(q);
   }

   for (cl = CCLASS_BASE_ADDR; 
        cl < CCLASS_BASE_ADDR+tabClassSz; cl++) {
      if (tabClass[cl-CCLASS_BASE_ADDR].inUse)
         if (cclass(cl).text == t_class) {
            /* Class name is ok, but is this the right module? */
            if (isNull(t_mod)   /* no module name specified */
                || (nonNull(t_mod) 
                    && t_mod == module(cclass(cl).mod).text)
               )
               return cl;
         }
   }
   return NIL;
}

/* Same deal, except for Tycons. */
Tycon findQualTyconWithoutConsultingExportList ( QualId q )
{
   Tycon tc;
   Text t_mod;
   Text t_tycon;

   assert(isQCon(q));

   if (isCon(q)) {
      t_mod   = NIL;
      t_tycon = textOf(q);
   } else {
      t_mod   = qmodOf(q);
      t_tycon = qtextOf(q);
   }

   for (tc = TYCON_BASE_ADDR; 
        tc < TYCON_BASE_ADDR+tabTyconSz; tc++) {
      if (tabTycon[tc-TYCON_BASE_ADDR].inUse)
         if (tycon(tc).text == t_tycon) {
            /* Tycon name is ok, but is this the right module? */
            if (isNull(t_mod)   /* no module name specified */
                || (nonNull(t_mod) 
                    && t_mod == module(tycon(tc).mod).text)
               )
               return tc;
         }
   }
   return NIL;
}

/* Same deal, except for Names. */
Name findQualNameWithoutConsultingExportList ( QualId q )
{
   Name nm;
   Text t_mod;
   Text t_name;

   assert(isQVar(q) || isQCon(q));

   if (isCon(q) || isVar(q)) {
      t_mod  = NIL;
      t_name = textOf(q);
   } else {
      t_mod  = qmodOf(q);
      t_name = qtextOf(q);
   }

   for (nm = NAME_BASE_ADDR; 
        nm < NAME_BASE_ADDR+tabNameSz; nm++) {
      if (tabName[nm-NAME_BASE_ADDR].inUse)
         if (name(nm).text == t_name) {
            /* Name is ok, but is this the right module? */
            if (isNull(t_mod)   /* no module name specified */
                || (nonNull(t_mod) 
                    && t_mod == module(name(nm).mod).text)
               )
               return nm;
         }
   }
   return NIL;
}


Tycon findTyconInAnyModule ( Text t )
{
   Tycon tc;
   for (tc = TYCON_BASE_ADDR; 
        tc < TYCON_BASE_ADDR+tabTyconSz; tc++)
      if (tabTycon[tc-TYCON_BASE_ADDR].inUse)
         if (tycon(tc).text == t) return tc;
   return NIL;
}

Class findClassInAnyModule ( Text t )
{
   Class cc;
   for (cc = CCLASS_BASE_ADDR; 
        cc < CCLASS_BASE_ADDR+tabClassSz; cc++)
      if (tabClass[cc-CCLASS_BASE_ADDR].inUse)
         if (cclass(cc).text == t) return cc;
   return NIL;
}

Name findNameInAnyModule ( Text t )
{
   Name nm;
   for (nm = NAME_BASE_ADDR; 
        nm < NAME_BASE_ADDR+tabNameSz; nm++)
      if (tabName[nm-NAME_BASE_ADDR].inUse)
         if (name(nm).text == t) return nm;
   return NIL;
}


/* returns List of QualId */
List getAllKnownTyconsAndClasses ( void )
{
   Tycon tc;
   Class nw;
   List  xs = NIL;
   for (tc = TYCON_BASE_ADDR; 
        tc < TYCON_BASE_ADDR+tabTyconSz; tc++) {
      if (tabTycon[tc-TYCON_BASE_ADDR].inUse) {
         /* almost certainly undue paranoia about duplicate avoidance */
         QualId q = mkQCon( module(tycon(tc).mod).text, tycon(tc).text );
         if (!qualidIsMember(q,xs))
            xs = cons ( q, xs );
      }
   }
   for (nw = CCLASS_BASE_ADDR; 
        nw < CCLASS_BASE_ADDR+tabClassSz; nw++) {
      if (tabClass[nw-CCLASS_BASE_ADDR].inUse) {
         QualId q = mkQCon( module(cclass(nw).mod).text, cclass(nw).text );
         if (!qualidIsMember(q,xs))
            xs = cons ( q, xs );
      }
   }
   return xs;
}

/* Purely for debugging. */
void locateSymbolByName ( Text t )
{
   Int i;
   for (i = NAME_BASE_ADDR; 
        i < NAME_BASE_ADDR+tabNameSz; i++)
      if (tabName[i-NAME_BASE_ADDR].inUse && name(i).text == t)
         fprintf ( stderr, "name(%d)\n", i-NAME_BASE_ADDR);
   for (i = TYCON_BASE_ADDR; 
        i < TYCON_BASE_ADDR+tabTyconSz; i++)
      if (tabTycon[i-TYCON_BASE_ADDR].inUse && tycon(i).text == t)
         fprintf ( stderr, "tycon(%d)\n", i-TYCON_BASE_ADDR);
   for (i = CCLASS_BASE_ADDR; 
        i < CCLASS_BASE_ADDR+tabClassSz; i++)
      if (tabClass[i-CCLASS_BASE_ADDR].inUse && cclass(i).text == t)
         fprintf ( stderr, "class(%d)\n", i-CCLASS_BASE_ADDR);
}

/* --------------------------------------------------------------------------
 * Control stack:
 *
 * Various parts of the system use a stack of cells.  Most of the stack
 * operations are defined as macros, expanded inline.
 * ------------------------------------------------------------------------*/

Cell cellStack[NUM_STACK];          /* Storage for cells on stack          */
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

Module  currentModule;                  /* Module currently being processed*/

Bool isValidModule(m)                   /* is m a legitimate module id?    */
Module m; {
    return isModule(m);
}

Module newModule ( Text t )             /* add new module to module table  */
{
    Module mod                   = allocNewModule();
    module(mod).text             = t;      /* clear new module record      */

    module(mod).tycons           = NIL;
    module(mod).names            = NIL;
    module(mod).classes          = NIL;
    module(mod).exports          = NIL;
    module(mod).qualImports      = NIL;
    module(mod).fake             = FALSE;

    module(mod).tree             = NIL;
    module(mod).completed        = FALSE;
    module(mod).lastStamp        = 0; /* ???? */

    module(mod).fromSrc          = TRUE;
    module(mod).srcExt           = findText("");
    module(mod).uses             = NIL;

    module(mod).objName          = findText("");
    module(mod).objSize          = 0;

    module(mod).object           = NULL;
    module(mod).objectExtras     = NULL;
    module(mod).objectExtraNames = NIL;
    return mod;
}

void nukeModule ( Module m )
{
   ObjectCode* oc;
   ObjectCode* oc2;
   Int         i;
assert(isModule(m));
fprintf(stderr, "NUKEMODULE `%s'\n", textToStr(module(m).text));
   oc = module(m).object;
   while (oc) {
      oc2 = oc->next;
      ocFree(oc);
      oc = oc2;
   }
   oc = module(m).objectExtras;
   while (oc) {
      oc2 = oc->next;
      ocFree(oc);
      oc = oc2;
   }

   for (i = NAME_BASE_ADDR; i < NAME_BASE_ADDR+tabNameSz; i++)
      if (tabName[i-NAME_BASE_ADDR].inUse && name(i).mod == m) {
         if (name(i).itbl) free(name(i).itbl);
         name(i).itbl = NULL;
         freeName(i);
      }

   for (i = TYCON_BASE_ADDR; i < TYCON_BASE_ADDR+tabTyconSz; i++)
      if (tabTycon[i-TYCON_BASE_ADDR].inUse && tycon(i).mod == m) {
	 if (tycon(i).itbl) free(tycon(i).itbl);
         tycon(i).itbl = NULL;
         freeTycon(i);
      }

   for (i = CCLASS_BASE_ADDR; i < CCLASS_BASE_ADDR+tabClassSz; i++)
      if (tabClass[i-CCLASS_BASE_ADDR].inUse) {
         if (cclass(i).mod == m) {
            freeClass(i);
         } else {
            List /* Inst */ ins;
            List /* Inst */ ins2 = NIL;
            for (ins = cclass(i).instances; nonNull(ins); ins=tl(ins))
               if (inst(hd(ins)).mod != m) 
                  ins2 = cons(hd(ins),ins2);
            cclass(i).instances = ins2;
         }
      }


   for (i = INST_BASE_ADDR; i < INST_BASE_ADDR+tabInstSz; i++)
      if (tabInst[i-INST_BASE_ADDR].inUse && inst(i).mod == m)
         freeInst(i);

   freeModule(m);
   //for (i = 0; i < TYCONHSZ; i++) tyconHash[i] = 0;
   //for (i = 0; i < NAMEHSZ; i++)  nameHash[i] = 0;
   //classes = NIL;
   //hashSanity();
}

void ppModules ( void )
{
   Int i;
   fflush(stderr); fflush(stdout);
   printf ( "begin MODULES\n" );
   for (i  = MODULE_BASE_ADDR+tabModuleSz-1;
        i >= MODULE_BASE_ADDR; i--)
      if (tabModule[i-MODULE_BASE_ADDR].inUse)
         printf ( " %2d: %16s\n",
                  i-MODULE_BASE_ADDR, textToStr(module(i).text)
                );
   printf ( "end   MODULES\n" );
   fflush(stderr); fflush(stdout);
}


Module findModule(t)                    /* locate Module in module table  */
Text t; {
    Module m;
    for(m = MODULE_BASE_ADDR; 
        m < MODULE_BASE_ADDR+tabModuleSz; ++m) {
        if (tabModule[m-MODULE_BASE_ADDR].inUse)
            if (module(m).text==t)
                return m;
    }
    return NIL;
}

Module findModid(c)                    /* Find module by name or filename  */
Cell c; {
    switch (whatIs(c)) {
        case STRCELL   : internal("findModid-STRCELL unimp");
        case CONIDCELL : return findModule(textOf(c));
        default        : internal("findModid");
    }
    return NIL;/*NOTUSED*/
}

static local Module findQualifier(t)    /* locate Module in import list   */
Text t; {
    Module ms;
    for (ms=module(currentModule).qualImports; nonNull(ms); ms=tl(ms)) {
        if (textOf(fst(hd(ms)))==t)
            return snd(hd(ms));
    }
    if (module(currentModule).text==t)
        return currentModule;
    return NIL;
}

Void setCurrModule(m)              /* set lookup tables for current module */
Module m; {
    Int i;
    assert(isModule(m));
fprintf(stderr, "SET CURR MODULE %s\n", textToStr(module(m).text));
    {List t;
     for (t = module(m).names; nonNull(t); t=tl(t))
        assert(isName(hd(t)));
     for (t = module(m).tycons; nonNull(t); t=tl(t))
        assert(isTycon(hd(t)) || isTuple(hd(t)));
     for (t = module(m).classes; nonNull(t); t=tl(t))
        assert(isClass(hd(t)));
    }

    currentModule = m; /* This is the only assignment to currentModule */
    for (i=0; i<TYCONHSZ; ++i)
       tyconHash[RC_T(i)] = NIL;
    mapProc(hashTycon,module(m).tycons);
    for (i=0; i<NAMEHSZ; ++i)
       nameHash[RC_N(i)] = NIL;
    mapProc(hashName,module(m).names);
    classes = module(m).classes;
    hashSanity();
}

Name jrsFindQualName ( Text mn, Text sn )
{
   Module m;
   List   ns;

   for (m = MODULE_BASE_ADDR; 
        m < MODULE_BASE_ADDR+tabModuleSz; m++)
      if (tabModule[m-MODULE_BASE_ADDR].inUse 
          && module(m).text == mn) break;

   if (m == MODULE_BASE_ADDR+tabModuleSz) return NIL;
   
   for (ns = module(m).names; nonNull(ns); ns=tl(ns)) 
      if (name(hd(ns)).text == sn) return hd(ns);

   return NIL;
}


char* nameFromOPtr ( void* p )
{
   int i;
   Module m;
   for (m = MODULE_BASE_ADDR; 
        m < MODULE_BASE_ADDR+tabModuleSz; m++) {
      if (tabModule[m-MODULE_BASE_ADDR].inUse && module(m).object) {
         char* nm = ocLookupAddr ( module(m).object, p );
         if (nm) return nm;
      }
   }
#  if 0
   /* A kludge to assist Win32 debugging; not actually necessary. */
   { char* nm = nameFromStaticOPtr(p);
     if (nm) return nm;
   }
#  endif
   return NULL;
}


void* lookupOTabName ( Module m, char* sym )
{
   assert(isModule(m));
   if (module(m).object)
      return ocLookupSym ( module(m).object, sym );
   return NULL;
}


void* lookupOExtraTabName ( char* sym )
{
   ObjectCode* oc;
   Module      m;
   for (m = MODULE_BASE_ADDR; 
        m < MODULE_BASE_ADDR+tabModuleSz; m++) {
      if (tabModule[m-MODULE_BASE_ADDR].inUse)
         for (oc = module(m).objectExtras; oc; oc=oc->next) {
            void* ad = ocLookupSym ( oc, sym );
            if (ad) return ad;
         }
   }
   return NULL;
}


OSectionKind lookupSection ( void* ad )
{
   int          i;
   Module       m;
   ObjectCode*  oc;
   OSectionKind sect;

   for (m = MODULE_BASE_ADDR; 
        m < MODULE_BASE_ADDR+tabModuleSz; m++) {
      if (tabModule[m-MODULE_BASE_ADDR].inUse) {
         if (module(m).object) {
            sect = ocLookupSection ( module(m).object, ad );
            if (sect != HUGS_SECTIONKIND_NOINFOAVAIL)
               return sect;
         }
         for (oc = module(m).objectExtras; oc; oc=oc->next) {
            sect = ocLookupSection ( oc, ad );
            if (sect != HUGS_SECTIONKIND_NOINFOAVAIL)
               return sect;
         }
      }
   }
   return HUGS_SECTIONKIND_OTHER;
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
Long    numCells;
Int     numGcs;                         /* number of garbage collections   */
Int     cellsRecovered;                 /* number of cells recovered       */

static  Cell freeList;                  /* free list of unused cells       */
static  Cell lsave, rsave;              /* save components of pair         */

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
    numCells++;
    return c;
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

    /* STACK_CHECK: Avoid stack overflows during recursive marking. */
    if (isGenPair(fst(c))) {
	STACK_CHECK
        fst(c) = markCell(fst(c));
        markSnd(c);
    }
    else if (isNull(fst(c)) || isTagPtr(fst(c))) {
	STACK_CHECK
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
    else if (isNull(fst(c)) || isTagPtr(fst(c)))
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
fprintf ( stderr, "wa-hey!  garbage collection!  too difficult!  bye!\n" );
exit(0);
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

    everybody(GCDONE);

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
    savedText     = TEXT_SIZE;
    lastExprSaved = lowLevelLastIn(e);
}

static Cell local lowLevelLastIn(c)     /* Duplicate expression tree (i.e. */
Cell c; {                               /* acyclic graph) for later recall */
    if (isPair(c)) {                    /* Duplicating any text strings    */
        if (isTagNonPtr(fst(c)))        /* in case these are lost at some  */
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
        if (isTagNonPtr(fst(c)))        /* appropriate values              */
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

Cell whatIs ( register Cell c )
{
    if (isPair(c)) {
        register Cell fstc = fst(c);
        return isTag(fstc) ? fstc : AP;
    }
    if (isOffset(c))           return OFFSET;
    if (isChar(c))             return CHARCELL;
    if (isInt(c))              return INTCELL;
    if (isName(c))             return NAME;
    if (isTycon(c))            return TYCON;
    if (isTuple(c))            return TUPLE;
    if (isClass(c))            return CLASS;
    if (isInst(c))             return INSTANCE;
    if (isModule(c))           return MODULE;
    if (isText(c))             return TEXTCELL;
    if (isInventedVar(c))      return INVAR;
    if (isInventedDictVar(c))  return INDVAR;
    if (isSpec(c))             return c;
    if (isNull(c))             return c;
    fprintf ( stderr, "whatIs: unknown %d\n", c );
    internal("whatIs");
}


#if 0
Cell whatIs(c)                         /* identify type of cell            */
register Cell c; {
    if (isPair(c)) {
        register Cell fstc = fst(c);
        return isTag(fstc) ? fstc : AP;
    }
    if (c<OFFMIN)    return c;
#if TREX
    if (isExt(c))    return EXT;
#endif
    if (c>=INTMIN)   return INTCELL;

    if (c>=NAMEMIN){if (c>=CLASSMIN)   {if (c>=CHARMIN) return CHARCELL;
                                        else            return CLASS;}
                    else                if (c>=INSTMIN) return INSTANCE;
                                        else            return NAME;}
    else            if (c>=MODMIN)     {if (c>=TYCMIN)  return isTuple(c) ? TUPLE : TYCON;
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
#endif


/* A very, very simple printer.
 * Output is uglier than from printExp - but the printer is more
 * robust and can be used on any data structure irrespective of
 * its type.
 */
Void print ( Cell c, Int depth )
{
    if (0 == depth) {
        Printf("...");
    }
    else if (isNull(c)) {
       Printf("NIL");
    }
    else if (isTagPtr(c)) {
        Printf("TagP(%d)", c);
    }
    else if (isTagNonPtr(c)) {
        Printf("TagNP(%d)", c);
    }
    else if (isSpec(c)) {
        Printf("TagS(%d)", c);
    }
    else if (isText(c)) {
        Printf("text(%d)=\"%s\"",c-TEXT_BASE_ADDR,textToStr(c));
    }
    else if (isInventedVar(c)) {
        Printf("invented(%d)", c-INVAR_BASE_ADDR);
    }
    else if (isInventedDictVar(c)) {
        Printf("inventedDict(%d)",c-INDVAR_BASE_ADDR);
    }
    else {
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
                Printf("class(%d)", c-CCLASS_BASE_ADDR);
                Printf("=\"%s\"", textToStr(cclass(c).text));
                break;
        case INSTANCE:
                Printf("instance(%d)", c - INST_BASE_ADDR);
                break;
        case NAME:
                Printf("name(%d)", c-NAME_BASE_ADDR);
                Printf("=\"%s\"", textToStr(name(c).text));
                break;
        case TYCON:
                Printf("tycon(%d)", c-TYCON_BASE_ADDR);
                Printf("=\"%s\"", textToStr(tycon(c).text));
                break;
        case MODULE:
                Printf("module(%d)", c - MODULE_BASE_ADDR);
                Printf("=\"%s\"", textToStr(module(c).text));
                break;
        case OFFSET:
                Printf("Offset %d", offsetOf(c));
                break;
        case TUPLE:
                Printf("%s", textToStr(ghcTupleText(c)));
                break;
        case POLYTYPE:
                Printf("Polytype");
                print(snd(c),depth-1);
                break;
        case QUAL:
                Printf("Qualtype");
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
#if IPARAM
	  case IPCELL :
	      Printf("{ip %s}",textToStr(textOf(c)));
	      break;
	  case IPVAR :
	      Printf("?%s",textToStr(textOf(c)));
	      break;
#endif
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
        case DICTAP:
                Printf("(DICTAP,");
                print(snd(c),depth-1);
                Putchar(')');
                break;
        case UNBOXEDTUP:
                Printf("(UNBOXEDTUP,");
                print(snd(c),depth-1);
                Putchar(')');
                break;
        case ZTUP2:
                Printf("<ZPair ");
                print(zfst(c),depth-1);
                Putchar(' ');
                print(zsnd(c),depth-1);
                Putchar('>');
                break;
        case ZTUP3:
                Printf("<ZTriple ");
                print(zfst3(c),depth-1);
                Putchar(' ');
                print(zsnd3(c),depth-1);
                Putchar(' ');
                print(zthd3(c),depth-1);
                Putchar('>');
                break;
        case BANG:
                Printf("(BANG,");
                print(snd(c),depth-1);
                Putchar(')');
                break;
        default:
                if (isTagNonPtr(tag)) {
                    Printf("(TagNP=%d,%d)", c, tag);
                } else if (isTagPtr(tag)) {
                    Printf("(TagP=%d,",tag);
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

Bool eqQualIdent ( QualId c1, QualId c2 )
{
   assert(isQualIdent(c1));
   if (!isQualIdent(c2)) {
   assert(isQualIdent(c2));
   }
   return qmodOf(c1)==qmodOf(c2) &&
          qtextOf(c1)==qtextOf(c2);
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
    return isPair(c) ? (Int)(snd(c)) : (Int)(c-SMALL_INT_ZERO);
}

Cell mkInt(n)                          /* make cell representing integer   */
Int n; {
    return (SMALL_INT_MIN    <= SMALL_INT_ZERO+n &&
            SMALL_INT_ZERO+n <= SMALL_INT_MAX)
           ? SMALL_INT_ZERO+n
           : pair(INTCELL,n);
}

#if SIZEOF_VOID_P == SIZEOF_INT

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

Cell mkCPtr(p)
Ptr p;
{
    IntOrPtr x;
    x.p = p;
    return pair(CPTRCELL,x.i);
}

Ptr cptrOf(c)
Cell c;
{
    IntOrPtr x;
    assert(fst(c) == CPTRCELL);
    x.i = snd(c);
    return x.p;
}

#elif SIZEOF_VOID_P == 2*SIZEOF_INT

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

Cell mkCPtr(p)
Ptr p;
{
    IntOrPtr x;
    x.p = p;
    return pair(CPTRCELL,pair(mkInt(x.i.i1),mkInt(x.i.i2)));
}

Ptr cptrOf(c)
Cell c;
{
    IntOrPtr x;
    assert(fst(c) == CPTRCELL);
    x.i.i1 = intOf(fst(snd(c)));
    x.i.i2 = intOf(snd(snd(c)));
    return x.p;
}

#else

#error "Can't implement mkPtr/ptrOf on this architecture."

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

QualId qualidIsMember ( QualId q, List xs )
{
   for (; nonNull(xs); xs=tl(xs)) {
      if (eqQualIdent(q, hd(xs)))
         return hd(xs);
   }
   return NIL;
}  

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

List splitAt(n,xs)                      /* drop n things from front of list*/
Int  n;       
List xs; {
    for(; n>0; --n) {
        xs = tl(xs);
    }
    return xs;
}

Cell nth(n,xs)                          /* extract n'th element of list    */
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

List nubList(xs)                        /* nuke dups in list               */
List xs; {                              /* non destructive                 */
   List outs = NIL;
   for (; nonNull(xs); xs=tl(xs))
      if (isNull(cellIsMember(hd(xs),outs)))
         outs = cons(hd(xs),outs);
   outs = rev(outs);
   return outs;
}


/* --------------------------------------------------------------------------
 * Tagged tuples (experimental)
 * ------------------------------------------------------------------------*/

static void z_tag_check ( Cell x, int tag, char* caller )
{
   char buf[100];
   if (isNull(x)) {
      sprintf(buf,"z_tag_check(%s): null\n", caller);
      internal(buf);
   }
   if (whatIs(x) != tag) {
      sprintf(buf, 
          "z_tag_check(%s): tag was %d, expected %d\n",
          caller, whatIs(x), tag );
      internal(buf);
   }  
}

Cell zpair ( Cell x1, Cell x2 )
{ return ap(ZTUP2,ap(x1,x2)); }
Cell zfst ( Cell zpair )
{ z_tag_check(zpair,ZTUP2,"zfst"); return fst( snd(zpair) ); }
Cell zsnd ( Cell zpair )
{ z_tag_check(zpair,ZTUP2,"zsnd"); return snd( snd(zpair) ); }

Cell ztriple ( Cell x1, Cell x2, Cell x3 )
{ return ap(ZTUP3,ap(x1,ap(x2,x3))); }
Cell zfst3 ( Cell zpair )
{ z_tag_check(zpair,ZTUP3,"zfst3"); return fst( snd(zpair) ); }
Cell zsnd3 ( Cell zpair )
{ z_tag_check(zpair,ZTUP3,"zsnd3"); return fst(snd( snd(zpair) )); }
Cell zthd3 ( Cell zpair )
{ z_tag_check(zpair,ZTUP3,"zthd3"); return snd(snd( snd(zpair) )); }

Cell z4ble ( Cell x1, Cell x2, Cell x3, Cell x4 )
{ return ap(ZTUP4,ap(x1,ap(x2,ap(x3,x4)))); }
Cell zsel14 ( Cell zpair )
{ z_tag_check(zpair,ZTUP4,"zsel14"); return fst( snd(zpair) ); }
Cell zsel24 ( Cell zpair )
{ z_tag_check(zpair,ZTUP4,"zsel24"); return fst(snd( snd(zpair) )); }
Cell zsel34 ( Cell zpair )
{ z_tag_check(zpair,ZTUP4,"zsel34"); return fst(snd(snd( snd(zpair) ))); }
Cell zsel44 ( Cell zpair )
{ z_tag_check(zpair,ZTUP4,"zsel44"); return snd(snd(snd( snd(zpair) ))); }

Cell z5ble ( Cell x1, Cell x2, Cell x3, Cell x4, Cell x5 )
{ return ap(ZTUP5,ap(x1,ap(x2,ap(x3,ap(x4,x5))))); }
Cell zsel15 ( Cell zpair )
{ z_tag_check(zpair,ZTUP5,"zsel15"); return fst( snd(zpair) ); }
Cell zsel25 ( Cell zpair )
{ z_tag_check(zpair,ZTUP5,"zsel25"); return fst(snd( snd(zpair) )); }
Cell zsel35 ( Cell zpair )
{ z_tag_check(zpair,ZTUP5,"zsel35"); return fst(snd(snd( snd(zpair) ))); }
Cell zsel45 ( Cell zpair )
{ z_tag_check(zpair,ZTUP5,"zsel45"); return fst(snd(snd(snd( snd(zpair) )))); }
Cell zsel55 ( Cell zpair )
{ z_tag_check(zpair,ZTUP5,"zsel55"); return snd(snd(snd(snd( snd(zpair) )))); }


Cell unap ( int tag, Cell c )
{
   char buf[100];
   if (whatIs(c) != tag) {
      sprintf(buf, "unap: specified %d, actual %d\n",
                   tag, whatIs(c) );
      internal(buf);
   }
   return snd(c);
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
 * debugging support
 * ------------------------------------------------------------------------*/

static String maybeModuleStr ( Module m )
{
   if (isModule(m)) return textToStr(module(m).text); else return "??";
}

static String maybeNameStr ( Name n )
{
   if (isName(n)) return textToStr(name(n).text); else return "??";
}

static String maybeTyconStr ( Tycon t )
{
   if (isTycon(t)) return textToStr(tycon(t).text); else return "??";
}

static String maybeClassStr ( Class c )
{
   if (isClass(c)) return textToStr(cclass(c).text); else return "??";
}

static String maybeText ( Text t )
{
   if (isNull(t)) return "(nil)";
   return textToStr(t);
}

static void print100 ( Int x )
{
   print ( x, 100); printf("\n");
}

void dumpTycon ( Int t )
{
   if (isTycon(TYCON_BASE_ADDR+t) && !isTycon(t)) t += TYCON_BASE_ADDR;
   if (!isTycon(t)) {
      printf ( "dumpTycon %d: not a tycon\n", t);
      return;
   }
   printf ( "{\n" );
   printf ( "    text: %s\n",     textToStr(tycon(t).text) );
   printf ( "    line: %d\n",     tycon(t).line );
   printf ( "     mod: %s\n",     maybeModuleStr(tycon(t).mod));
   printf ( "   tuple: %d\n",     tycon(t).tuple);
   printf ( "   arity: %d\n",     tycon(t).arity);
   printf ( "    kind: ");        print100(tycon(t).kind);
   printf ( "    what: %d\n",     tycon(t).what);
   printf ( "    defn: ");        print100(tycon(t).defn);
   printf ( "    cToT: %d %s\n",  tycon(t).conToTag, 
                                  maybeNameStr(tycon(t).conToTag));
   printf ( "    tToC: %d %s\n",  tycon(t).tagToCon, 
                                  maybeNameStr(tycon(t).tagToCon));
   printf ( "    itbl: %p\n",     tycon(t).itbl);
   printf ( "  nextTH: %d %s\n",  tycon(t).nextTyconHash,
                                  maybeTyconStr(tycon(t).nextTyconHash));
   printf ( "}\n" );
}

void dumpName ( Int n )
{
   if (isName(NAME_BASE_ADDR+n) && !isName(n)) n += NAME_BASE_ADDR;
   if (!isName(n)) {
      printf ( "dumpName %d: not a name\n", n);
      return;
   }
   printf ( "{\n" );
   printf ( "    text: %s\n",     textToStr(name(n).text) );
   printf ( "    line: %d\n",     name(n).line );
   printf ( "     mod: %s\n",     maybeModuleStr(name(n).mod));
   printf ( "  syntax: %d\n",     name(n).syntax );
   printf ( "  parent: %d\n",     name(n).parent );
   printf ( "   arity: %d\n",     name(n).arity );
   printf ( "  number: %d\n",     name(n).number );
   printf ( "    type: ");        print100(name(n).type);
   printf ( "    defn: %d\n",     name(n).defn );
   printf ( "  stgVar: ");        print100(name(n).stgVar);
   printf ( "   cconv: %d\n",     name(n).callconv );
   printf ( "  primop: %p\n",     name(n).primop );
   printf ( "    itbl: %p\n",     name(n).itbl );
   printf ( "  nextNH: %d\n",     name(n).nextNameHash );
   printf ( "}\n" );
}


void dumpClass ( Int c )
{
   if (isClass(CCLASS_BASE_ADDR+c) && !isClass(c)) c += CCLASS_BASE_ADDR;
   if (!isClass(c)) {
      printf ( "dumpClass %d: not a class\n", c);
      return;
   }
   printf ( "{\n" );
   printf ( "    text: %s\n",     textToStr(cclass(c).text) );
   printf ( "    line: %d\n",     cclass(c).line );
   printf ( "     mod: %s\n",     maybeModuleStr(cclass(c).mod));
   printf ( "   arity: %d\n",     cclass(c).arity );
   printf ( "   level: %d\n",     cclass(c).level );
   printf ( "   kinds: ");        print100( cclass(c).kinds );
   printf ( "     fds: %d\n",     cclass(c).fds );
   printf ( "    xfds: %d\n",     cclass(c).xfds );
   printf ( "    head: ");        print100( cclass(c).head );
   printf ( "    dcon: ");        print100( cclass(c).dcon );
   printf ( "  supers: ");        print100( cclass(c).supers );
   printf ( " #supers: %d\n",     cclass(c).numSupers );
   printf ( "   dsels: ");        print100( cclass(c).dsels );
   printf ( " members: ");        print100( cclass(c).members );
   printf ( "#members: %d\n",     cclass(c).numMembers );
   printf ( "defaults: ");        print100( cclass(c).defaults );
   printf ( "   insts: ");        print100( cclass(c).instances );
   printf ( "}\n" );
}


void dumpInst ( Int i )
{
   if (isInst(INST_BASE_ADDR+i) && !isInst(i)) i += INST_BASE_ADDR;
   if (!isInst(i)) {
      printf ( "dumpInst %d: not an instance\n", i);
      return;
   }
   printf ( "{\n" );
   printf ( "   class: %s\n",     maybeClassStr(inst(i).c) );
   printf ( "    line: %d\n",     inst(i).line );
   printf ( "     mod: %s\n",     maybeModuleStr(inst(i).mod));
   printf ( "   kinds: ");        print100( inst(i).kinds );
   printf ( "    head: ");        print100( inst(i).head );
   printf ( "   specs: ");        print100( inst(i).specifics );
   printf ( "  #specs: %d\n",     inst(i).numSpecifics );
   printf ( "   impls: ");        print100( inst(i).implements );
   printf ( " builder: %s\n",     maybeNameStr( inst(i).builder ) );
   printf ( "}\n" );
}


/* --------------------------------------------------------------------------
 * storage control:
 * ------------------------------------------------------------------------*/

Void storage(what)
Int what; {
    Int i;

    switch (what) {
        case POSTPREL: break;

        case RESET   : clearStack();

                       /* the next 2 statements are particularly important
                        * if you are using GLOBALfst or GLOBALsnd since the
                        * corresponding registers may be reset to their
                        * uninitialised initial values by a longjump.
                        */
                       heapTopFst = heapFst + heapSize;
                       heapTopSnd = heapSnd + heapSize;
                       consGC = TRUE;
                       lsave  = NIL;
                       rsave  = NIL;
                       if (isNull(lastExprSaved))
                           savedText = TEXT_SIZE;
                       break;

        case MARK    : 
                       start();
                       for (i = NAME_BASE_ADDR; 
                            i < NAME_BASE_ADDR+tabNameSz; ++i) {
                          if (tabName[i-NAME_BASE_ADDR].inUse) {
                             mark(name(i).parent);
                             mark(name(i).defn);
                             mark(name(i).stgVar);
                             mark(name(i).type);
                          }
                       }
                       end("Names", nameHw-NAMEMIN);

                       start();
                       for (i = MODULE_BASE_ADDR; 
                            i < MODULE_BASE_ADDR+tabModuleSz; ++i) {
                          if (tabModule[i-MODULE_BASE_ADDR].inUse) {
                             mark(module(i).tycons);
                             mark(module(i).names);
                             mark(module(i).classes);
                             mark(module(i).exports);
                             mark(module(i).qualImports);
                             mark(module(i).objectExtraNames);
                          }
                       }
                       mark(moduleGraph);
                       mark(prelModules);
                       mark(targetModules);
                       end("Modules", moduleHw-MODMIN);

                       start();
                       for (i = TYCON_BASE_ADDR; 
                            i < TYCON_BASE_ADDR+tabTyconSz; ++i) {
                          if (tabTycon[i-TYCON_BASE_ADDR].inUse) {
                             mark(tycon(i).defn);
                             mark(tycon(i).kind);
                             mark(tycon(i).what);
                          }
                       }
                       end("Type constructors", tyconHw-TYCMIN);

                       start();
                       for (i = CCLASS_BASE_ADDR; 
                            i < CCLASS_BASE_ADDR+tabClassSz; ++i) {
                          if (tabModule[i-MODULE_BASE_ADDR].inUse) {
                             mark(cclass(i).head);
                             mark(cclass(i).kinds);
	  		     mark(cclass(i).fds);
		  	     mark(cclass(i).xfds);
                             mark(cclass(i).dsels);
                             mark(cclass(i).supers);
                             mark(cclass(i).members);
                             mark(cclass(i).defaults);
                             mark(cclass(i).instances);
                          }
                       }
                       mark(classes);
                       end("Classes", classHw-CLASSMIN);

                       start();
                       for (i = INST_BASE_ADDR; 
                            i < INST_BASE_ADDR+tabInstSz; ++i) {
                          if (tabInst[i-INST_BASE_ADDR].inUse) {
                             mark(inst(i).head);
                             mark(inst(i).kinds);
                             mark(inst(i).specifics);
                             mark(inst(i).implements);
                          }
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

        case PREPREL : heapFst = heapAlloc(heapSize);
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
                       numGcs    = 0;
                       consGC    = TRUE;
                       lsave     = NIL;
                       rsave     = NIL;

                       marksSize  = bitArraySize(heapSize);
                       if ((marks=(Int *)calloc(marksSize, sizeof(Int)))==0) {
                           ERRMSG(0) "Unable to allocate gc markspace"
                           EEND;
                       }

                       clearStack();

                       textHw        = 0;
                       nextNewText   = INVAR_BASE_ADDR;
                       nextNewDText  = INDVAR_BASE_ADDR;
                       lastExprSaved = NIL;
                       savedText     = TEXT_SIZE;

                       for (i=0; i<TEXTHSZ;  ++i) textHash[i][0] = NOTEXT;
                       for (i=0; i<TYCONHSZ; ++i) tyconHash[RC_T(i)] = NIL;
                       for (i=0; i<NAMEHSZ;  ++i) nameHash[RC_N(i)] = NIL;

                       break;
    }
}

/*-------------------------------------------------------------------------*/

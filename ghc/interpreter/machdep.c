
/* --------------------------------------------------------------------------
 * Machine dependent code
 * RISCOS specific code provided by Bryan Scatergood, JBS
 * Macintosh specific code provided by Hans Aberg (haberg@matematik.su.se)
 * HaskellScript code and recursive directory search provided by
 *  Daan Leijen (leijen@fwi.uva.nl)
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: machdep.c,v $
 * $Revision: 1.24 $
 * $Date: 2000/03/24 14:51:50 $
 * ------------------------------------------------------------------------*/

#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#else
# ifdef HAVE_TYPES_H
#  include <types.h>
# endif
#endif
#if HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#else
# ifdef HAVE_STAT_H
#  include <stat.h>
# endif
#endif
#ifdef HAVE_TIME_H
# include <time.h>
#endif

/* Windows/DOS include files */
#ifdef HAVE_DOS_H
# include <dos.h>
#endif
#if defined HAVE_CONIO_H
# include <conio.h>
#endif
#ifdef HAVE_IO_H
# include <io.h>
#endif
#ifdef HAVE_STD_H
# include <std.h>
#endif
#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

#if DOS
#include <mem.h>
extern unsigned _stklen = 8000;         /* Allocate an 8k stack segment    */
#endif

#if RISCOS
#include "swis.h"
#include "os.h"
#endif

/* Macintosh include files */
#ifdef HAVE_CONSOLE_H
# include <console.h>
#endif
#ifdef HAVE_PASCAL_H
# include <pascal.h>
#endif
#ifdef HAVE_FILES_H
# include <Files.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_UNIX_H
#include <unix.h>
#endif
#if SYMANTEC_C
int allow_break_count = 0;
#endif

/* --------------------------------------------------------------------------
 * Prototypes for registry reading
 * ------------------------------------------------------------------------*/

#if USE_REGISTRY

/* where have we hidden things in the registry? */
#if HSCRIPT
#define HScriptRoot ("SOFTWARE\\Haskell\\HaskellScript\\")
#endif

#define HugsRoot ("SOFTWARE\\Haskell\\Hugs\\" HUGS_VERSION "\\")
#define ProjectRoot ("SOFTWARE\\Haskell\\Projects\\")

static Bool   local createKey      ( HKEY, String, PHKEY, REGSAM );
static Bool   local queryValue     ( HKEY, String, String, LPDWORD, LPBYTE, DWORD );
static Bool   local setValue       ( HKEY, String, String, DWORD, LPBYTE, DWORD );
static String local readRegString  ( HKEY, String, String, String );
static Int    local readRegInt     ( String,Int );
static Bool   local writeRegString ( String,String );
static Bool   local writeRegInt    ( String,Int );

static String local readRegChildStrings ( HKEY, String, String, Char, String );
#endif /* USE_REGISTRY */

/* --------------------------------------------------------------------------
 * Find information about a file:
 * ------------------------------------------------------------------------*/

#include "machdep_time.h"

static Bool local readable      ( String );
static Void local getFileInfo   ( String, Time *, Long * );

static Void local getFileInfo(f,tm,sz)  /* find time stamp and size of file*/
String f;
Time   *tm;
Long   *sz; {
#if defined HAVE_SYS_STAT_H || defined HAVE_STAT_H || defined HAVE_UNIX_H
    struct stat scbuf;
    if (!stat(f,&scbuf)) {
        if (tm) *tm = scbuf.st_mtime;
        *sz = (Long)(scbuf.st_size);
    } else {
        if (tm) *tm = 0;
        *sz = 0;
    }
#else                                   /* normally just use stat()        */
    os_regset r;                        /* RISCOS PRM p.850 and p.837      */
    r.r[0] = 17;                        /* Read catalogue, no path         */
    r.r[1] = (int)s;
    os_swi(OS_File, &r);
    if(r.r[0] == 1 && (r.r[2] & 0xFFF00000) == 0xFFF00000) {
        if (tm) tm->hi = r.r[2] & 0xFF; /* Load address (high byte)        */
        if (tm) tm->lo = r.r[3];        /* Execution address (low 4 bytes) */
    } else {                            /* Not found, or not time-stamped  */
        if (tm) tm->hi = tm->lo = 0;
    }
    *sz = (Long)(r.r[0] == 1 ? r.r[4] : 0);
#endif
}

Void getFileSize ( String f, Long* sz )
{
   getFileInfo ( f, NULL, sz );
}

#if defined HAVE_GETFINFO               /* Mac971031 */
/* --------------------------------------------------------------------------
 * Define a MacOS version of access():
 *   If the file is not accessible, -1 is returned and errno is set to
 * the reason for the failure.
 *   If the file is accessible and the dummy is 0 (existence), 2 (write), 
 * or 4 (read), the return is 0.
 *   If the file is accessible, and the dummy is 1 (executable), then if
 * the file is a program (of type 'APPL'), the return is 0, otherwise -1.
 *   Warnings: Use with caution. UNIX access do no translate to Macs.
 * Check of write access is not implemented (same as read).
 * ------------------------------------------------------------------------*/

int access(char *fileName, int dummy) { 
        FInfo   fi;
        short   rc;
        
        errno = getfinfo(fileName, 0, &fi);
        if (errno != 0)  return -1;             /* Check file accessible. */
        
        /* Cases dummy = existence, read, write. */
        if (dummy == 0 || dummy & 0x6)  return 0;
        
        /* Case dummy = executable. */
        if (dummy == 1) { 
                if (fi.fdType == 'APPL')  return 0;
                errno = fi.fdType;
                return -1;
        }
        
        return 0;
}
#endif

static Bool local readable(f)           /* is f a regular, readable file   */
String f; {
#if DJGPP2 || defined HAVE_GETFINFO /* stat returns bogus mode bits on djgpp2 */
    return (0 == access(f,4));
#elif defined HAVE_SYS_STAT_H || defined HAVE_STAT_H
    struct stat scbuf;
    /* fprintf(stderr, "readable: %s\n", f ); */
    return (  !stat(f,&scbuf) 
           && (scbuf.st_mode & S_IREAD) /* readable     */
           && (scbuf.st_mode & S_IFREG) /* regular file */
           );
#elif defined HAVE_OS_SWI /* RISCOS specific */
    os_regset r;                        /* RISCOS PRM p.850     -- JBS     */
    assert(dummy == 0);
    r.r[0] = 17; /* Read catalogue, no path */
    r.r[1] = (int)f;
    os_swi(OS_File, &r);
    return r.r[0] != 1; /* Does this check it's a regular file? ADR */
#endif
}


/* --------------------------------------------------------------------------
 * Search for script files on the HUGS path:
 * ------------------------------------------------------------------------*/

static String local hugsdir       ( Void );
#if HSCRIPT
static String local hscriptDir    ( Void );
#endif
static int    local pathCmp       ( String, String );
static String local normPath      ( String );
static Void   local searchChr     ( Int );
static Void   local searchStr     ( String );
static Bool   local tryEndings    ( String );

#if (DOS_FILENAMES || __CYGWIN32__) 
# define SLASH                   '\\'
# define isSLASH(c)              ((c)=='\\' || (c)=='/')
# define PATHSEP                 ';'
# define PATHSEP_STR             ";"
# define DLL_ENDING              ".dll"
#elif MAC_FILENAMES
# define SLASH                   ':'
# define isSLASH(c)              ((c)==SLASH)
# define PATHSEP                 ';'
# define PATHSEP_STR             ";"
/* Mac PEF (Preferred Executable Format) file */
# define DLL_ENDING              ".pef" 
#else
# define SLASH                   '/'
# define isSLASH(c)              ((c)==SLASH)
# define PATHSEP                 ':'
# define PATHSEP_STR             ":"
# define DLL_ENDING              ".u_o"
#endif

static String local hugsdir() {     /* directory containing lib/Prelude.hs */
#if HSCRIPT
    /* In HaskellScript (Win32 only), we lookup InstallDir in the registry. */
    static char dir[FILENAME_MAX+1] = "";
    if (dir[0] == '\0') { /* not initialised yet */
        String s = readRegString(HKEY_LOCAL_MACHINE,HugsRoot,"InstallDir", 
                                 HUGSDIR);
        if (s) { 
            strcpy(dir,s); 
        }
    }
    return dir;
#elif HAVE_GETMODULEFILENAME && !DOS && !__CYGWIN32__
    /* On Windows, we can find the binary we're running and it's
     * conventional to put the libraries in the same place.
     */
    static char dir[FILENAME_MAX+1] = "";
    if (dir[0] == '\0') { /* not initialised yet */
        String slash = 0;
        GetModuleFileName((HMODULE)0,dir,FILENAME_MAX+1);
        if (dir[0] == '\0') { /* GetModuleFileName must have failed */
            return HUGSDIR;
        }
        slash = strrchr(dir,SLASH);
        if (slash) { /* truncate after directory name */
            *slash = '\0';
        }
    }
    return dir;
#else
    /* On Unix systems, we can't find the binary we're running and
     * the libraries may not be installed near the binary anyway.
     * This forces us to use a hardwired path which is set at 
     * configuration time (--datadir=...).
     */
    return HUGSDIR;
#endif
}

#if HSCRIPT    
static String local hscriptDir() {  /* Directory containing hscript.dll	   */
    static char dir[FILENAME_MAX+1] = "";
    if (dir[0] == '\0') { /* not initialised yet */
        String s = readRegString(HKEY_LOCAL_MACHINE,HScriptRoot,"InstallDir","");
        if (s) {
            strcpy(dir,s);
        }
    }
    return dir;
}
#endif

#if 0  /* apparently unused */
static String local RealPath(s)         /* Find absolute pathname of file  */
String s; {
#if HAVE__FULLPATH  /* eg DOS */
    static char path[FILENAME_MAX+1];
    _fullpath(path,s,FILENAME_MAX+1);
#elif HAVE_REALPATH /* eg Unix */
    static char path[MAXPATHLEN+1];
    realpath(s,path);                
#else
    static char path[FILENAME_MAX+1];
    strcpy(path,s);
#endif
    return path;
}
#endif


static int local pathCmp(p1,p2)       /* Compare paths after normalisation */
String p1;
String p2; {
#if HAVE__FULLPATH  /* eg DOS */
    static char path1[FILENAME_MAX+1];
    static char path2[FILENAME_MAX+1];
    _fullpath(path1,p1,FILENAME_MAX+1);
    _fullpath(path2,p2,FILENAME_MAX+1);
#elif HAVE_REALPATH /* eg Unix */
    static char path1[MAXPATHLEN+1];
    static char path2[MAXPATHLEN+1];
    realpath(p1,path1);                
    realpath(p2,path2);                
#else
    static char path1[FILENAME_MAX+1];
    static char path2[FILENAME_MAX+1];
    strcpy(path1,p1);
    strcpy(path2,p2);
#endif
#if CASE_INSENSITIVE_FILENAMES
    strlwr(path1);
    strlwr(path2);
#endif
    return filenamecmp(path1,path2);
}

static String local normPath(s) /* Try, as much as possible, to normalize  */
String s; {                     /* a pathname in some appropriate manner.  */
#if PATH_CANONICALIZATION
    String path = RealPath(s);
#if CASE_INSENSITIVE_FILENAMES
    strlwr(path);                       /* and convert to lowercase        */
#endif
    return path;
#else /* ! PATH_CANONICALIZATION */
    return s;
#endif /* ! PATH_CANONICALIZATION */
}

#if HSCRIPT
static String endings[] = { "", ".u_hi", ".hs", ".lhs", ".hsx", ".hash", 0 };
#else
static String endings[] = { "", ".u_hi", ".hs", ".lhs", 0 };
#endif
static char   searchBuf[FILENAME_MAX+1];
static Int    searchPos;

#define searchReset(n)          searchBuf[searchPos=(n)]='\0'

static Void local searchChr(c)  /* Add single character to search buffer   */
Int c; {
    if (searchPos<FILENAME_MAX) {
        searchBuf[searchPos++] = (char)c;
        searchBuf[searchPos]   = '\0';
    }
}

static Void local searchStr(s)  /* Add string to search buffer             */
String s; {
    while (*s && searchPos<FILENAME_MAX)
        searchBuf[searchPos++] = *s++;
    searchBuf[searchPos] = '\0';
}

static Bool local tryEndings(s) /* Try each of the listed endings          */
String s; {
    Int i = 0;
    searchStr(s);
    for (; endings[i]; ++i) {
        Int save = searchPos;
        searchStr(endings[i]);
        if (readable(searchBuf))
            return TRUE;
        searchReset(save);
    }
    return FALSE;
}



#if SEARCH_DIR

/* scandir, June 98 Daan Leijen
   searches the base directory and its direct subdirectories for a file

   input: searchbuf contains SLASH terminated base directory
          argument s contains the (base) filename
   output: TRUE: searchBuf contains the full filename
           FALSE: searchBuf is garbage, file not found
*/
          

#ifdef HAVE_WINDOWS_H

static Bool scanSubDirs(s)
String s;
{
    struct _finddata_t findInfo;
    long handle;
    int  save;
    
    save = searchPos;
    /* is it in the current directory ? */
    if (tryEndings(s)) return TRUE;

    searchReset(save);
    searchStr("*");
    
    /* initiate the search */
    handle = _findfirst( searchBuf, &findInfo );
    if (handle==-1) { errno = 0; return FALSE; }
    
    /* search all subdirectories */
    do {
        /* if we have a valid sub directory */
        if (((findInfo.attrib & _A_SUBDIR) == _A_SUBDIR) &&
            (findInfo.name[0] != '.')) {
            searchReset(save);
            searchStr(findInfo.name);
            searchChr(SLASH);
            if (tryEndings(s)) {
                return TRUE;
            }
        }
    } while (_findnext( handle, &findInfo ) == 0);
    
    _findclose( handle );
    return FALSE;
}

#elif defined(HAVE_FTW_H)

#include <ftw.h>

static char baseFile[FILENAME_MAX+1];
static char basePath[FILENAME_MAX+1];
static int  basePathLen;

static int scanitem( const char* path, 
                     const struct stat* statinfo, 
                     int info )
{
    if (info == FTW_D) { /* is it a directory */
        searchReset(0);
        searchStr(path);
        searchChr(SLASH);
        if (tryEndings(baseFile)) {
            return 1;
        }
    }
    return 0;
}

static Bool scanSubDirs(s)
String s;
{
    int r;
    strcpy(baseFile,s);
    strcpy(basePath,searchBuf);
    basePathLen = strlen(basePath);

    /* is it in the current directory ? */
    if (tryEndings(s)) return TRUE;
    
    /* otherwise scan the subdirectories */
    r = ftw( basePath, scanitem, 2 );
    errno = 0;
    return (r > 0);
}

#endif /* HAVE_WINDOWS_H || HAVE_FTW_H */
#endif /* SEARCH_DIR */

String findPathname(along,nm)   /* Look for a file along specified path    */
String along;                   /* Return NULL if file does not exist      */ 
String nm; {
    /* AC, 1/21/99: modified to search hugsPath first, then projectPath */
    String s = findMPathname(along,nm,hugsPath);
#if USE_REGISTRY
#if 0
 ToDo:
    if (s==NULL) {
        s = findMPathname(along,nm,projectPath);
    }
#endif /* 0 */
#endif /* USE_REGISTRY */
    return s ? s : normPath(searchBuf);
}

/* AC, 1/21/99: modified to pass in path to search explicitly */
String findMPathname(along,nm,path)/* Look for a file along specified path   */
String along;                   /* If nonzero, a path prefix from along is */
String nm;                      /* used as the first prefix in the search. */
String path; {
    String pathpt = path;

    searchReset(0);
    if (along) {                /* Was a path for an existing file given?  */
        Int last = (-1);
        Int i    = 0;
        for (; along[i]; i++) {
            searchChr(along[i]);
            if (isSLASH(along[i]))
                last = i;
        }
        searchReset(last+1);
    }
    if (tryEndings(nm))
        return normPath(searchBuf);

    if (pathpt && *pathpt) {    /* Otherwise, we look along the HUGSPATH   */
        Bool more = TRUE;
        do {
            Bool recurse = FALSE;   /* DL: shall we recurse ? */
            searchReset(0);
            if (*pathpt) {
                if (*pathpt!=PATHSEP) {
                    /* Pre-define one MPW-style "shell-variable" */
                    if (strncmp(pathpt,"{Hugs}",6)==0) {
                        searchStr(hugsdir());
                        pathpt += 6;
                    }
#if HSCRIPT
                    /* And another - we ought to generalise this stuff */
                    else if (strncmp(pathpt,"{HScript}",9)==0) {
                        searchStr(hscriptDir());
                        pathpt += 9;
                    }
#endif
                    do {
                        searchChr(*pathpt++);
                    } while (*pathpt && *pathpt!=PATHSEP);
                    recurse = (pathpt[-1] == SLASH);
                    if (!recurse) {
                        searchChr(SLASH);
                    }
                }
                if (*pathpt==PATHSEP)
                    pathpt++;
                else
                    more = FALSE;
            } else {
                more = FALSE;
            }
#if SEARCH_DIR
            if (recurse ? scanSubDirs(nm) : tryEndings(nm)) {
                return normPath(searchBuf);
            }
#else   
            if (tryEndings(nm)) {
                return normPath(searchBuf);
            }
#endif
        } while (more);
    }

    searchReset(0);  /* As a last resort, look for file in the current dir */
    return (tryEndings(nm) ? normPath(searchBuf) : 0);
}

/* --------------------------------------------------------------------------
 * New path handling stuff for the Combined System (tm)
 * ------------------------------------------------------------------------*/

char installDir[N_INSTALLDIR];

/* Sets installDir to $STGHUGSDIR, and ensures there is a trailing
   slash at the end.
*/
void setInstallDir ( String argv_0 )
{
   int   i;
   char* r = getenv("STGHUGSDIR");
   if (!r) {
      fprintf(stderr, 
          "%s: installation error: environment variable STGHUGSDIR is not set.\n",
          argv_0 );
      fprintf(stderr, 
          "%s: pls set it to be the directory where STGHugs98 is installed.\n\n",
          argv_0 );
      exit(2);

   }

   if (strlen(r) > N_INSTALLDIR-30 ) {
      fprintf(stderr, 
          "%s: environment variable STGHUGSDIR is suspiciously long; pls remedy\n\n",
          argv_0 );
      exit(2);
   }

   strcpy ( installDir, r );
   i = strlen(installDir);
   if (installDir[i-1] != SLASH) installDir[i++] = SLASH;
   installDir[i] = 0;
}


Bool findFilesForModule ( 
        String  modName,
        String* path,
        String* sExt,
        Bool* sAvail, Time* sTime, Long* sSize,
        Bool* iAvail, Time* iTime, Long* iSize,
        Bool* oAvail, Time* oTime, Long* oSize
     )
{
   /* Let the module name given be M.
      For each path entry P,
        a  s(rc)       file will be P/M.hs or P/M.lhs
        an i(nterface) file will be P/M.hi
        an o(bject)    file will be P/M.o
      If there is a s file or (both i and o files)
        use P to fill in the path names.
      Otherwise, move on to the next path entry.
      If all path entries are exhausted, return False.
   */
   Int    nPath;
   Bool   literate;
   String peStart, peEnd;
   String augdPath;       /* .:hugsPath:installDir/GhcPrel:installDir/lib */

   *path = *sExt = NULL;
   *sAvail = *iAvail = *oAvail = FALSE;
   *sSize  = *iSize  = *oSize  = 0;

   augdPath = malloc( 2*(10+3+strlen(installDir)) 
                      +strlen(hugsPath) +10/*paranoia*/);
   if (!augdPath)
      internal("moduleNameToFileNames: malloc failed(2)");

   augdPath[0] = 0;
   strcat(augdPath, ".");
   strcat(augdPath, PATHSEP_STR);

   strcat(augdPath, hugsPath);
   strcat(augdPath, PATHSEP_STR);

   if (combined) {
      strcat(augdPath, installDir);
      strcat(augdPath, "GhcPrel");
      strcat(augdPath, PATHSEP_STR);
   }

   strcat(augdPath, installDir);
   strcat(augdPath, "lib");
   strcat(augdPath, PATHSEP_STR);

   /*   fprintf ( stderr, "augdpath = `%s'\n", augdPath ); */

   peEnd = augdPath-1;
   while (1) {
      /* Advance peStart and peEnd very paranoically, giving up at
         the first sign of mutancy in the path string.
      */
      if (peEnd >= augdPath && !(*peEnd)) { free(augdPath); return FALSE; }
      peStart = peEnd+1;
      peEnd = peStart;
      while (*peEnd && *peEnd != PATHSEP) peEnd++;
      
      /* Now peStart .. peEnd-1 bracket the next path element. */
      nPath = peEnd-peStart;
      if (nPath + strlen(modName) + 10 /*slush*/ > FILENAME_MAX) {
         ERRMSG(0) "Hugs path \"%s\" contains excessively long component", 
                   hugsPath
         EEND;
         free(augdPath); 
         return FALSE;
      }

      strncpy(searchBuf, peStart, nPath); 
      searchBuf[nPath] = 0;
      if (nPath > 0 && !isSLASH(searchBuf[nPath-1])) 
         searchBuf[nPath++] = SLASH;

      strcpy(searchBuf+nPath, modName);
      nPath += strlen(modName);

      /* searchBuf now holds 'P/M'.  Try out the various endings. */
      *path = *sExt = NULL;
      *sAvail = *iAvail = *oAvail = FALSE;
      *sSize  = *iSize  = *oSize  = 0;

      strcpy(searchBuf+nPath, DLL_ENDING);
      if (readable(searchBuf)) {
         *oAvail = TRUE;
         getFileInfo(searchBuf, oTime, oSize);
      }

      strcpy(searchBuf+nPath, ".u_hi");
      if (readable(searchBuf)) {
         *iAvail = TRUE;
         getFileInfo(searchBuf, iTime, iSize);
      }

      strcpy(searchBuf+nPath, ".hs");
      if (readable(searchBuf)) {
         *sAvail = TRUE;
         literate = FALSE;
         getFileInfo(searchBuf, sTime, sSize);
         *sExt = ".hs";
      } else {
         strcpy(searchBuf+nPath, ".lhs");
         if (readable(searchBuf)) {
            *sAvail = TRUE;
            literate = TRUE;
            getFileInfo(searchBuf, sTime, sSize);
            *sExt = ".lhs";
         }
      }

      /* Success? */
      if (*sAvail || (*oAvail && *iAvail)) {
         nPath -= strlen(modName);
         *path = malloc(nPath+1);
         if (!(*path))
            internal("moduleNameToFileNames: malloc failed(1)");
         strncpy(*path, searchBuf, nPath);
         (*path)[nPath] = 0;
         free(augdPath); 
         return TRUE;
      }

   }
   
}


/* If the primaryObjectName is (eg)
     /foo/bar/PrelSwamp.o
   and the extraFileName is (eg)
     swampy_cbits
   and DLL_ENDING is set to .o
   return
     /foo/bar/swampy_cbits.o
     and set *extraFileSize to its size, or -1 if not avail
*/
String getExtraObjectInfo ( String primaryObjectName,
                            String extraFileName,
                            Int*   extraFileSize )
{
   Time   xTime;
   Long   xSize;
   String xtra;

   Int i = strlen(primaryObjectName)-1;
   while (i >= 0 && primaryObjectName[i] != SLASH) i--;
   if (i == -1) return extraFileName;
   i++;
   xtra = malloc ( i+3+strlen(extraFileName)+strlen(DLL_ENDING) );
   if (!xtra) internal("deriveExtraObjectName: malloc failed");
   strncpy ( xtra, primaryObjectName, i );
   xtra[i] = 0;
   strcat ( xtra, extraFileName );
   strcat ( xtra, DLL_ENDING );

   *extraFileSize = -1;
   if (readable(xtra)) {
      getFileInfo ( xtra, &xTime, &xSize );
      *extraFileSize = xSize;
   }
   return xtra;
}


/* --------------------------------------------------------------------------
 * Substitute old value of path into empty entries in new path
 * eg substPath("a:b:c::d:e","x:y:z") = "a:b:c:x:y:z:d:e"
 * ------------------------------------------------------------------------*/

static String local substPath ( String,String );

static String local substPath(new,sub) /* substitute sub path into new path*/
String new;
String sub; {
    Bool   substituted = FALSE;            /*   only allow one replacement */
    Int    maxlen      = strlen(sub) + strlen(new);    /* safe upper bound */
    String r = (String) malloc(maxlen+1);  /* result string                */
    String t = r;                          /* pointer into r               */
    String next = new;                     /* next uncopied char in new    */
    String start = next;                   /* start of last path component */
    if (r == 0) {
        ERRMSG(0) "String storage space exhausted"
        EEND;
    }
    do {
        if (*next == PATHSEP || *next == '\0') {
            if (!substituted && next == start) {
                String s = sub;
                for(; *s != '\0'; ++s) {
                    *t++ = *s;
                }
                substituted = TRUE;
            }
            start = next+1;
        }
    } while ((*t++ = *next++) != '\0');
    return r;
}


/* --------------------------------------------------------------------------
 * Garbage collection notification:
 * ------------------------------------------------------------------------*/

Bool gcMessages = FALSE;                /* TRUE => print GC messages       */

Void gcStarted() {                      /* Notify garbage collector start  */
    if (gcMessages) {
        Printf("{{Gc");
        FlushStdout();
    }
}

Void gcScanning() {                     /* Notify garbage collector scans  */
    if (gcMessages) {
        Putchar(':');
        FlushStdout();
    }
}

Void gcRecovered(recovered)             /* Notify garbage collection done  */
Int recovered; {
    if (gcMessages) {
        Printf("%d}}",recovered);
        FlushStdout();
    }
}

Cell *CStackBase;                       /* Retain start of C control stack */

#if RISCOS                              /* Stack traversal for RISCOS      */

/* Warning: The following code is specific to the Acorn ARM under RISCOS
   (and C4).  We must explicitly walk back through the stack frames, since
   the stack is extended from the heap. (see PRM pp. 1757).  gcCStack must
   not be modified, since the offset '5' assumes that only v1 is used inside
   this function. Hence we do all the real work in gcARM.
*/
                  
#define spreg 13 /* C3 has SP=R13 */

#define previousFrame(fp)       ((int *)((fp)[-3]))
#define programCounter(fp)      ((int *)((*(fp)-12) & ~0xFC000003))
#define isSubSPSP(w)            (((w)&dontCare) == doCare)
#define doCare                  (0xE24DD000)  /* SUB r13,r13,#0 */
#define dontCare                (~0x00100FFF) /* S and # bits   */
#define immediateArg(x)         ( ((x)&0xFF) << (((x)&0xF00)>>7) )

static void gcARM(int *fp) {
    int si = *programCounter(fp);       /* Save instruction indicates how */
                                        /* many registers in this frame   */
    int *regs = fp - 4;
    if (si & (1<<0)) markWithoutMove(*regs--);
    if (si & (1<<1)) markWithoutMove(*regs--);
    if (si & (1<<2)) markWithoutMove(*regs--);
    if (si & (1<<3)) markWithoutMove(*regs--);
    if (si & (1<<4)) markWithoutMove(*regs--);
    if (si & (1<<5)) markWithoutMove(*regs--);
    if (si & (1<<6)) markWithoutMove(*regs--);
    if (si & (1<<7)) markWithoutMove(*regs--);
    if (si & (1<<8)) markWithoutMove(*regs--);
    if (si & (1<<9)) markWithoutMove(*regs--);
    if (previousFrame(fp)) {
        /* The non-register stack space is for the previous frame is above
           this fp, and not below the previous fp, because of the way stack
           extension works. It seems the only way of discovering its size is
           finding the SUB sp, sp, #? instruction by walking through the code
           following the entry point.
        */
        int *oldpc = programCounter(previousFrame(fp));
        int fsize = 0, i;
        for(i = 1; i < 6; ++i)
            if(isSubSPSP(oldpc[i])) fsize += immediateArg(oldpc[i]) / 4;
        for(i=1; i<=fsize; ++i)
            markWithoutMove(fp[i]);
    }
}

void gcCStack() {
    int dummy;
    int *fp = 5 + &dummy;
    while (fp) {
        gcARM(fp);
        fp = previousFrame(fp);
    }
}

#else                   /* Garbage collection for standard stack machines  */

Void gcCStack() {                       /* Garbage collect elements off    */
    Cell stackTop = NIL;                /* C stack                         */
    Cell *ptr = &stackTop;
#if SIZEOF_VOID_P == 2
    if (((long)(ptr) - (long)(CStackBase))&1)
        fatal("gcCStack");
#elif STACK_ALIGNMENT == 2 /* eg Macintosh 68000 */
    if (((long)(ptr) - (long)(CStackBase))&1)
        fatal("gcCStack");
#else 
    if (((long)(ptr) - (long)(CStackBase))&3)
        fatal("gcCStack");
#endif

#define Blargh markWithoutMove(*ptr);
#if 0
               markWithoutMove((*ptr)/sizeof(Cell)); \
               markWithoutMove(( (void*)(*ptr)-(void*)heapTopFst)/sizeof(Cell));  \
               markWithoutMove(( (void*)(*ptr)-(void*)heapTopSnd)/sizeof(Cell))
#endif

#define StackGrowsDown  { while (ptr<=CStackBase) { Blargh; ptr++; }; }
#define StackGrowsUp    { while (ptr>=CStackBase) { Blargh; ptr--; }; }
#define GuessDirection  if (ptr>CStackBase) StackGrowsUp else StackGrowsDown

#if STACK_DIRECTION > 0
    StackGrowsUp;
#elif STACK_DIRECTION < 0
    StackGrowsDown;
#else
    GuessDirection;
#endif

#if SIZEOF_VOID_P==4 && STACK_ALIGNMENT == 2 /* eg Macintosh 68000 */
    ptr = (Cell *)((long)(&stackTop) + 2);
    StackGrowsDown;
#endif

#undef  StackGrowsDown
#undef  StackGrowsUp
#undef  GuessDirection
}
#endif

/* --------------------------------------------------------------------------
 * Terminal dependent stuff:
 * ------------------------------------------------------------------------*/

#if (HAVE_TERMIO_H | HAVE_SGTTY_H | HAVE_TERMIOS_H)

/* grab the varargs prototype for ioctl */
#if HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

/* The order of these three tests is very important because
 * some systems have more than one of the requisite header file
 * but only one of them seems to work.
 * Anyone changing the order of the tests should try enabling each of the
 * three branches in turn and write down which ones work as well as which
 * OS/compiler they're using.
 *
 * OS            Compiler      sgtty     termio  termios   notes
 * Linux 2.0.18  gcc 2.7.2     absent    works   works     1
 *
 * Notes:
 * 1) On Linux, termio.h just #includes termios.h and sgtty.h is
 *    implemented using termios.h.
 *    sgtty.h is in /usr/include/bsd which is not on my standard include
 *    path.  Adding it does no harm but you might as well use termios.
 *    --
 *    reid-alastair@cs.yale.edu
 */
#if HAVE_TERMIOS_H

#include <termios.h>
typedef  struct termios  TermParams;
#define  getTerminal(tp) tcgetattr(fileno(stdin), &tp)
#define  setTerminal(tp) tcsetattr(fileno(stdin), TCSAFLUSH, &tp)
#define  noEcho(tp)      tp.c_lflag    &= ~(ICANON | ECHO); \
                         tp.c_cc[VMIN]  = 1;                \
                         tp.c_cc[VTIME] = 0;

#elif HAVE_SGTTY_H

#include <sgtty.h>
typedef  struct sgttyb   TermParams;
#define  getTerminal(tp) ioctl(fileno(stdin),TIOCGETP,&tp)
#define  setTerminal(tp) ioctl(fileno(stdin),TIOCSETP,&tp)
#if HPUX
#define  noEcho(tp)      tp.sg_flags |= RAW; tp.sg_flags &= (~ECHO);
#else
#define  noEcho(tp)      tp.sg_flags |= CBREAK; tp.sg_flags &= (~ECHO);
#endif

#elif HAVE_TERMIO_H

#include <termio.h>
typedef  struct termio   TermParams;
#define  getTerminal(tp) ioctl(fileno(stdin),TCGETA,&tp)
#define  setTerminal(tp) ioctl(fileno(stdin),TCSETAF,&tp)
#define  noEcho(tp)      tp.c_lflag    &= ~(ICANON | ECHO); \
                         tp.c_cc[VMIN]  = 1;                \
                         tp.c_cc[VTIME] = 0;

#endif

static Bool messedWithTerminal = FALSE;
static TermParams originalSettings;

Void normalTerminal() {                 /* restore terminal initial state  */
    if (messedWithTerminal)
        setTerminal(originalSettings);
}

Void noechoTerminal() {                 /* set terminal into noecho mode   */
    TermParams settings;

    if (!messedWithTerminal) {
        getTerminal(originalSettings);
        messedWithTerminal = TRUE;
    }
    getTerminal(settings);
    noEcho(settings);
    setTerminal(settings);
}

Int getTerminalWidth() {                /* determine width of terminal     */
#ifdef TIOCGWINSZ
#ifdef _M_UNIX                          /* SCO Unix 3.2.4 defines TIOCGWINSZ*/
#include <sys/stream.h>                 /* Required by sys/ptem.h          */
#include <sys/ptem.h>                   /* Required to declare winsize     */
#endif
    static struct winsize terminalSize;
    ioctl(fileno(stdout),TIOCGWINSZ,&terminalSize);
    return (terminalSize.ws_col==0)? 80 : terminalSize.ws_col;
#else
    return 80;
#endif
}

Int readTerminalChar() {                /* read character from terminal    */
    return getchar();                   /* without echo, assuming that     */
}                                       /* noechoTerminal() is active...   */

#elif SYMANTEC_C

Int readTerminalChar() {                /* read character from terminal    */
    return getchar();                   /* without echo, assuming that     */
}                                       /* noechoTerminal() is active...   */
 
Int getTerminalWidth() {
    return console_options.ncols;
}

Void normalTerminal() {
    csetmode(C_ECHO, stdin);
}

Void noechoTerminal() {
    csetmode(C_NOECHO, stdin);
}

#else /* no terminal driver - eg DOS, RISCOS */

static Bool terminalEchoReqd = TRUE;

Int getTerminalWidth() {
#if RISCOS
    int dummy, width;
    (void) os_swi3r(OS_ReadModeVariable, -1, 1, 0, &dummy, &dummy, &width);
    return width+1;
#else
    return 80;
#endif
}

Void normalTerminal() {                 /* restore terminal initial state  */
    terminalEchoReqd = TRUE;
}

Void noechoTerminal() {                 /* turn terminal echo on/off       */
    terminalEchoReqd = FALSE;
}

Int readTerminalChar() {                /* read character from terminal    */
    if (terminalEchoReqd) {
        return getchar();
    } else {
#if IS_WIN32 && !__BORLANDC__
	/* When reading a character from the console/terminal, we want
	 * to operate in 'raw' mode (to use old UNIX tty parlance) and have
 	 * it return when a character is available and _not_ wait until
 	 * the next time the user hits carriage return. On Windows platforms,
 	 * this _can_ be done by reading directly from the console, using
	 * getch().  However, this doesn't sit well with programming
	 * environments such as Emacs which allow you to create sub-processes
	 * running Hugs, and then communicate with the running interpreter
	 * through its standard input and output handles. If you use getch()
	 * in that setting, you end up trying to read the (unused) console
	 * of the editor itself, through which not a lot of characters is
	 * bound to come out, since the editor communicates input to Hugs
	 * via the standard input handle.
 	 *
 	 * To avoid this rather unfortunate situation, we use the Win32
	 * console API and re-jig the input properties of the standard
	 * input handle before trying to read a character using stdio's
	 * getchar().
 	 * 
 	 * The 'cost' of this solution is that it is Win32 specific and
	 * won't work with Windows 3.1 + it is kind of ugly and verbose
	 * to have to futz around with the console properties on a
	 * per-char basis. Both of these disadvantages aren't in my
	 * opinion fatal.
 	 *
 	 * -- sof 5/99
 	 */
        Int c;
 	DWORD mo;
 	HANDLE hIn;
 
 	/* I don't quite understand why, but if the FILE*'s underlying file
	   descriptor is in text mode, we seem to lose the first carriage
	   return.
 	 */
 	setmode(fileno(stdin), _O_BINARY);
 	hIn = GetStdHandle(STD_INPUT_HANDLE);
 	GetConsoleMode(hIn, &mo);
 	SetConsoleMode(hIn, mo & ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT));
	/* 
	 * On Win9x, the first time you change the mode (as above) a
	 * raw '\n' is inserted.  Since enter maps to a raw '\r', and we
	 * map this (below) to '\n', we can just ignore all *raw* '\n's.
	 */
	do {
	  c = getc(stdin);
	} while (c == '\n');
 
 	/* Same as it ever was - revert back state of stdin. */
 	SetConsoleMode(hIn, mo);
 	setmode(fileno(stdin), _O_TEXT);
#else
	Int c = getch();
#endif
        return c=='\r' ? '\n' : c;      /* slight paranoia about CR-LF    */
    }
}

#endif /* no terminal driver */

/* --------------------------------------------------------------------------
 * Interrupt handling:
 * ------------------------------------------------------------------------*/

static Void installHandlers ( void ) { /* Install handlers for all fatal   */ 
                                      /* signals except SIGINT and SIGBREAK*/
#if IS_WIN32
    /* SetConsoleCtrlHandler(consoleHandler,TRUE); */
#endif
#if !DONT_PANIC && !DOS
# ifdef SIGABRT
    signal(SIGABRT,panic);
# endif
# ifdef SIGBUS
    signal(SIGBUS,panic);
# endif
# ifdef SIGFPE
    signal(SIGFPE,panic);
# endif
# ifdef SIGHUP
    signal(SIGHUP,panic);
# endif
# ifdef SIGILL
    signal(SIGILL,panic);
# endif
# ifdef SIGQUIT
    signal(SIGQUIT,panic);
# endif
# ifdef SIGSEGV
    signal(SIGSEGV,panic);
# endif
# ifdef SIGTERM
    signal(SIGTERM,panic);
# endif
#endif /* !DONT_PANIC && !DOS */
}

/* --------------------------------------------------------------------------
 * Shell escapes:
 * ------------------------------------------------------------------------*/

static Bool local startEdit(line,nm)    /* Start editor on file name at    */
Int    line;                            /* given line.  Both name and line */
String nm; {                            /* or just line may be zero        */
    static char editorCmd[FILENAME_MAX+1];

#if !SYMANTEC_C
    if (hugsEdit && *hugsEdit) {        /* Check that editor configured    */
#else
    /* On a Mac, files have creator information, telling which program
       to launch to, so an editor named to the empty string "" is often
       desirable. */
    if (hugsEdit) {        /* Check that editor configured    */
#endif
        Int n     = FILENAME_MAX;
        String he = hugsEdit;
        String ec = editorCmd;
        String rd = NULL;               /* Set to nonnull to redo ...      */

	for (; n>0 && *he && *he!=' ' && *he!='%'; n--)
            *ec++ = *he++;              /* Copy editor name to buffer      */
                                        /* assuming filename ends at space */

        if (nm && line && n>1 && *he){  /* Name, line, and enough space    */
            rd = ec;                    /* save, in case we don't find name*/
            while (n>0 && *he) {
                if (*he=='%') {
                    if (*++he=='d' && n>10) {
                        sprintf(ec,"%d",line);
                        he++;
                    }
                    else if (*he=='s' && (size_t)n>strlen(nm)) {
                        strcpy(ec,nm);
                        rd = NULL;
                        he++;
                    }
                    else if (*he=='%' && n>1) {
                        strcpy(ec,"%");
                        he++;
                    }
                    else                /* Ignore % char if not followed   */
                        *ec = '\0';     /* by one of d, s, or %,           */
                    for (; *ec && n>0; n--)
                        ec++;
                }   /* ignore % followed by anything other than d, s, or % */
                else {                  /* Copy other characters across    */
                    *ec++ = *he++;
                    n--;
                }
            }
        }
        else
            line = 0;

        if (rd) {                       /* If file name was not included   */
            ec   = rd;
            line = 0;
        }

        if (nm && line==0 && n>1) {     /* Name, but no line ...           */
            *ec++ = ' ';
            for (; n>0 && *nm; n--)     /* ... just copy file name         */
                *ec++ = *nm++;
        }

        *ec = '\0';                     /* Add terminating null byte       */
    }
    else {
        ERRMSG(0) "Hugs is not configured to use an editor"
        EEND;
    }

#if HAVE_WINEXEC
    WinExec(editorCmd, SW_SHOW);
    return FALSE;
#else
    if (shellEsc(editorCmd))
        Printf("Warning: Editor terminated abnormally\n");
    return TRUE;
#endif
}

Int shellEsc(s)                         /* run a shell command (or shell)  */
String s; {
#if HAVE_MACSYSTEM
    return macsystem(s);
#else
#if HAVE_BIN_SH
    if (s[0]=='\0') {
        s = fromEnv("SHELL","/bin/sh");
    }
#endif
    return system(s);
#endif
}

#if RISCOS                              /* RISCOS also needs a chdir()     */
int chdir(char *s) {                    /* RISCOS PRM p. 885    -- JBS     */
    return os_swi2(OS_FSControl + XOS_Bit, 0, (int)s) != NULL;
}
#elif defined HAVE_PBHSETVOLSYNC        /* Macintosh */
int chdir(const char *s) {      
    char* str;
    WDPBRec wd;
    wd.ioCompletion = 0;
    str = (char*)malloc(strlen(s) + 1);
    if (str == 0) return -1;
    strcpy(str, s);
    wd.ioNamePtr = C2PStr(str);
    wd.ioVRefNum = 0;
    wd.ioWDDirID = 0;
    errno = PBHSetVolSync(&wd);
    free(str);
    if (errno == 0) {
        return 0;
    } else {
        return -1;
    }
}
#endif


/*---------------------------------------------------------------------------
 * Printf-related operations:
 *-------------------------------------------------------------------------*/

#if !defined(HAVE_VSNPRINTF)
int vsnprintf(buffer, count, fmt, ap)
char*       buffer;
int         count;
const char* fmt;
va_list     ap; {
#if defined(HAVE__VSNPRINTF)
    return _vsnprintf(buffer, count, fmt, ap);
#else
    return 0;
#endif
}
#endif /* HAVE_VSNPRINTF */

#if !defined(HAVE_SNPRINTF)
int snprintf(char* buffer, int count, const char* fmt, ...) {
#if defined(HAVE__VSNPRINTF)
    int r;
    va_list ap;                    /* pointer into argument list           */
    va_start(ap, fmt);             /* make ap point to first arg after fmt */
    r = vsnprintf(buffer, count, fmt, ap);
    va_end(ap);                    /* clean up                             */
    return r;
#else
    return 0;
#endif
}
#endif /* HAVE_SNPRINTF */

/* --------------------------------------------------------------------------
 * Read/write values from/to the registry
 *
 * All reads are from either HUGS_CURRENT_USER\\hugs_ROOT\\key or 
 * HUGS_LOCAL_MACHINE\\hugs_ROOT\\key.  (Machine entry is only used if
 * user entry doesn't exist).
 *
 * All writes are to HUGS_CURRENT_USER\\HugsRoot\\key
 * ------------------------------------------------------------------------*/

#if USE_REGISTRY

#define HugsRoot ("SOFTWARE\\Haskell\\Hugs\\" HUGS_VERSION "\\")

static Bool   local createKey      ( HKEY, PHKEY, REGSAM );
static Bool   local queryValue     ( HKEY, String, LPDWORD, LPBYTE, DWORD );
static Bool   local setValue       ( HKEY, String, DWORD, LPBYTE, DWORD );

static Bool local createKey(hKey, phRootKey, samDesired)
HKEY    hKey;
PHKEY   phRootKey; 
REGSAM  samDesired; {
    DWORD  dwDisp;
    return RegCreateKeyEx(hKey, HugsRoot,
                          0, "", REG_OPTION_NON_VOLATILE,
                          samDesired, NULL, phRootKey, &dwDisp) 
           == ERROR_SUCCESS;
}

static Bool local queryValue(hKey, regPath, var, type, buf, bufSize)
HKEY    hKey;
String  regPath;
String  var;
LPDWORD type;
LPBYTE  buf;
DWORD   bufSize; {
    HKEY hRootKey;

    if (!createKey(hKey, regPath, &hRootKey, KEY_READ)) {
        return FALSE;
    } else {
        LONG res = RegQueryValueEx(hRootKey, var, NULL, type, buf, &bufSize);
        RegCloseKey(hRootKey);
        return (res == ERROR_SUCCESS);
    }
}

static Bool local setValue(hKey, regPath, var, type, buf, bufSize)
HKEY   hKey;
String regPath;
String var;
DWORD  type;
LPBYTE buf;
DWORD  bufSize; {
    HKEY hRootKey;

    if (!createKey(hKey, regPath, &hRootKey, KEY_WRITE)) {
        return FALSE;
    } else {
        LONG res = RegSetValueEx(hRootKey, var, 0, type, buf, bufSize);
        RegCloseKey(hRootKey);
        return (res == ERROR_SUCCESS);
    }
}

static String local readRegString(key,regPath,var,def) /* read String from registry */
HKEY   key;
String regPath;
String var; 
String def; {
    static char  buf[300];
    DWORD type;
    if (queryValue(key, regPath,var, &type, buf, sizeof(buf))
        && type == REG_SZ) {
        return (String)buf;
    } else {
        return def;
    }
}

static Int local readRegInt(var, def)            /* read Int from registry */
String var;
Int    def; {
    DWORD buf;
    DWORD type;

    if (queryValue(HKEY_CURRENT_USER, HugsRoot, var, &type, 
                   (LPBYTE)&buf, sizeof(buf))
        && type == REG_DWORD) {
        return (Int)buf;
    } else if (queryValue(HKEY_LOCAL_MACHINE, HugsRoot, var, &type, 
                          (LPBYTE)&buf, sizeof(buf))
               && type == REG_DWORD) {
        return (Int)buf;
    } else {
        return def;
    }
}

static Bool local writeRegString(var,val)      /* write String to registry */
String var;                        
String val; {
    if (NULL == val) {
        val = "";
    }
    return setValue(HKEY_CURRENT_USER, HugsRoot, var, 
                    REG_SZ, (LPBYTE)val, lstrlen(val)+1);
}

static Bool local writeRegInt(var,val)         /* write String to registry */
String var;                        
Int    val; {
    return setValue(HKEY_CURRENT_USER, HugsRoot, var, 
                    REG_DWORD, (LPBYTE)&val, sizeof(val));
}

#endif /* USE_REGISTRY */

/* --------------------------------------------------------------------------
 * Things to do with the argv/argc and the env
 * ------------------------------------------------------------------------*/

int nh_argc ( void )
{
  return prog_argc;
}

int nh_argvb ( int argno, int offset )
{
  return (int)(prog_argv[argno][offset]);
}

/* --------------------------------------------------------------------------
 * Machine dependent control:
 * ------------------------------------------------------------------------*/

Void machdep(what)                      /* Handle machine specific         */
Int what; {                             /* initialisation etc..            */
    switch (what) {
        case MARK    : break;
        case POSTPREL: break;
        case PREPREL : installHandlers();
                       break;
        case RESET   :
        case BREAK   :
        case EXIT    : normalTerminal();
                       break;
    }
}

/*-------------------------------------------------------------------------*/


/* --------------------------------------------------------------------------
 * Command interpreter
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: hugs.c,v $
 * $Revision: 1.39 $
 * $Date: 2000/02/15 13:16:19 $
 * ------------------------------------------------------------------------*/

#include <setjmp.h>
#include <ctype.h>
#include <stdio.h>

#include "prelude.h"
#include "storage.h"
#include "command.h"
#include "backend.h"
#include "connect.h"
#include "errors.h"
#include "version.h"
#include "link.h"

#include "Rts.h"
#include "RtsAPI.h"
#include "Schedule.h"
#include "Assembler.h"                                /* DEBUG_LoadSymbols */

Bool haskell98 = TRUE;                  /* TRUE => Haskell 98 compatibility*/

#if EXPLAIN_INSTANCE_RESOLUTION
Bool showInstRes = FALSE;
#endif
#if MULTI_INST
Bool multiInstRes = FALSE;
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local initialize        Args((Int,String []));
static Void   local promptForInput    Args((String));
static Void   local interpreter       Args((Int,String []));
static Void   local menu              Args((Void));
static Void   local guidance          Args((Void));
static Void   local forHelp           Args((Void));
static Void   local set               Args((Void));
static Void   local changeDir         Args((Void));
static Void   local load              Args((Void));
static Void   local project           Args((Void));
static Void   local readScripts       Args((Int));
static Void   local whatScripts       Args((Void));
static Void   local editor            Args((Void));
static Void   local find              Args((Void));
static Bool   local startEdit         Args((Int,String));
static Void   local runEditor         Args((Void));
static Void   local setModule         Args((Void));
static Module local findEvalModule    Args((Void));
static Void   local evaluator         Args((Void));
static Void   local stopAnyPrinting   Args((Void));
static Void   local showtype          Args((Void));
static String local objToStr          Args((Module, Cell));
static Void   local info              Args((Void));
static Void   local printSyntax       Args((Name));
static Void   local showInst          Args((Inst));
static Void   local describe          Args((Text));
static Void   local listNames         Args((Void));

static Void   local toggleSet         Args((Char,Bool));
static Void   local togglesIn         Args((Bool));
static Void   local optionInfo        Args((Void));
#if USE_REGISTRY || HUGS_FOR_WINDOWS
static String local optionsToStr      Args((Void));
#endif
static Void   local readOptions       Args((String));
static Bool   local processOption     Args((String));
static Void   local setHeapSize       Args((String));
static Int    local argToInt          Args((String));

static Void   local loadProject       Args((String));
static Void   local clearProject      Args((Void));
static Bool   local addScript         Args((Int));
static Void   local forgetScriptsFrom Args((Script));
static Void   local setLastEdit       Args((String,Int));
static Void   local failed            Args((Void));
static String local strCopy           Args((String));
static Void   local browseit	      Args((Module,String,Bool));
static Void   local browse	      Args((Void));

/* --------------------------------------------------------------------------
 * Machine dependent code for Hugs interpreter:
 * ------------------------------------------------------------------------*/

       Bool   combined      = TRUE;

#include "machdep.c"
#ifdef WANT_TIMER
#include "timer.c"
#endif

/* --------------------------------------------------------------------------
 * Local data areas:
 * ------------------------------------------------------------------------*/

static Bool   printing      = FALSE;    /* TRUE => currently printing value*/
static Bool   showStats     = FALSE;    /* TRUE => print stats after eval  */
static Bool   listScripts   = TRUE;   /* TRUE => list scripts after loading*/
static Bool   addType       = FALSE;    /* TRUE => print type with value   */
static Bool   useDots       = RISCOS;   /* TRUE => use dots in progress    */
static Bool   quiet         = FALSE;    /* TRUE => don't show progress     */
static Bool   lastWasObject = FALSE;
       Bool   preludeLoaded = FALSE;
       Bool   debugSC       = FALSE;

typedef 
   struct { 
      String modName;                   /* Module name                     */
      Bool   details;             /* FALSE => remaining fields are invalid */
      String path;                      /* Path to module                  */
      String srcExt;                    /* ".hs" or ".lhs" if fromSource   */
      Time   lastChange;                /* Time of last change to script   */
      Bool   fromSource;                /* FALSE => load object code       */
      Bool   postponed;                 /* Indicates postponed load        */
      Bool   objLoaded;
      Long   size;
      Long   oSize;
   }
   ScriptInfo;

static Void   local makeStackEntry    Args((ScriptInfo*,String));
static Void   local addStackEntry     Args((String));

static ScriptInfo scriptInfo[NUM_SCRIPTS];

static Int    numScripts;               /* Number of scripts loaded        */
static Int    nextNumScripts;
static Int    namesUpto;                /* Number of script names set      */
static Bool   needsImports;             /* set to TRUE if imports required */
       String scriptFile;               /* Name of current script (if any) */



static Text   evalModule  = 0;          /* Name of module we eval exprs in */
static String currProject = 0;          /* Name of current project file    */
static Bool   projectLoaded = FALSE;    /* TRUE => project file loaded     */

static Bool   autoMain   = FALSE;
static String lastEdit   = 0;           /* Name of script to edit (if any) */
static Int    lastEdLine = 0;           /* Editor line number (if possible)*/
static String prompt     = 0;           /* Prompt string                   */
static Int    hpSize     = DEFAULTHEAP; /* Desired heap size               */
       String hugsEdit   = 0;           /* String for editor command       */
       String hugsPath   = 0;           /* String for file search path     */

       List  ifaces_outstanding = NIL;

#if REDIRECT_OUTPUT
static Bool disableOutput = FALSE;      /* redirect output to buffer?      */
#endif

String bool2str ( Bool b )
{
   if (b) return "Yes"; else return "No ";
}

void ppSmStack ( String who )
{
   int i, j;
return;
   fflush(stdout);fflush(stderr);
   printf ( "\n" );
   printf ( "ppSmStack %s:  numScripts = %d   namesUpto = %d  needsImports = %s\n",
            who, numScripts, namesUpto, bool2str(needsImports) );
   assert (namesUpto >= numScripts);
   printf ( "     Det FrS Pst ObL           Module Ext   Size ModTime  Path\n" );
   for (i = namesUpto-1; i >= 0; i--) {
      printf ( "%c%2d: %3s %3s %3s %3s %16s %-4s %5ld %8lx %s\n",
               (i==numScripts ? '*' : ' '),
               i, bool2str(scriptInfo[i].details), 
                  bool2str(scriptInfo[i].fromSource),
                  bool2str(scriptInfo[i].postponed), 
                  bool2str(scriptInfo[i].objLoaded),
                  scriptInfo[i].modName, 
                  scriptInfo[i].fromSource ? scriptInfo[i].srcExt : "",
                  scriptInfo[i].size, 
                  scriptInfo[i].lastChange,
                  scriptInfo[i].path
             );
   }
   fflush(stdout);fflush(stderr);
   ppScripts();
   ppModules();
   printf ( "\n" );
}

/* --------------------------------------------------------------------------
 * Hugs entry point:
 * ------------------------------------------------------------------------*/

#ifndef NO_MAIN /* we omit main when building the "Hugs server" */
 
Main main Args((Int, String []));       /* now every func has a prototype  */

Main main(argc,argv)
int  argc;
char *argv[]; {
#ifdef HAVE_CONSOLE_H /* Macintosh port */
    _ftype = 'TEXT';
    _fcreator = 'R*ch';       /*  // 'KAHL';      //'*TEX';       //'ttxt'; */

    console_options.top = 50;
    console_options.left = 20;

    console_options.nrows = 32;
    console_options.ncols = 80;

    console_options.pause_atexit = 1;
    console_options.title = "\pHugs";

    console_options.procID = 5;
    argc = ccommand(&argv);
#endif

    CStackBase = &argc;                 /* Save stack base for use in gc   */

    /* If first arg is +Q or -Q, be entirely silent, and automatically run
       main after loading scripts.  Useful for running the nofib suite.    */
    if (argc > 1 && (strcmp(argv[1],"+Q") == 0 || strcmp(argv[1],"-Q")==0)) {
       autoMain = TRUE;
       if (strcmp(argv[1],"-Q") == 0) {
	 hugsEnableOutput(0);
       }
    }

    Printf("__   __ __  __  ____   ___      _________________________________________\n");
    Printf("||   || ||  || ||  || ||__      STGHugs: Based on the Haskell 98 standard\n");
    Printf("||___|| ||__|| ||__||  __||     Copyright (c) 1994-1999\n");
    Printf("||---||         ___||           World Wide Web: http://haskell.org/hugs\n");
    Printf("||   ||                         Report bugs to: hugs-bugs@haskell.org\n");
    Printf("||   || Version: %s _________________________________________\n\n",HUGS_VERSION);

    /* Get the absolute path to the directory containing the hugs 
       executable, so that we know where the Prelude and nHandle.so/.dll are.
       We do this by reading env var STGHUGSDIR.  This needs to succeed, so
       setInstallDir won't return unless it succeeds.
    */
    setInstallDir ( argv[0] );

#if SYMANTEC_C
    Printf("   Ported to Macintosh by Hans Aberg, compiled " __DATE__ ".\n\n");
#endif
    FlushStdout();
    interpreter(argc,argv);
    Printf("[Leaving Hugs]\n");
    everybody(EXIT);
    shutdownHaskell();
    FlushStdout();
    fflush(stderr);
    exit(0);
    MainDone();
}

#endif

/* --------------------------------------------------------------------------
 * Initialization, interpret command line args and read prelude:
 * ------------------------------------------------------------------------*/

static Void local initialize(argc,argv)/* Interpreter initialization       */
Int    argc;
String argv[]; {
    Script i;
    String proj        = 0;
    char argv_0_orig[1000];

    setLastEdit((String)0,0);
    lastEdit      = 0;
    scriptFile    = 0;
    numScripts    = 0;
    namesUpto     = 1;

#if HUGS_FOR_WINDOWS
    hugsEdit      = strCopy(fromEnv("EDITOR","c:\\windows\\notepad.exe"));
#elif SYMANTEC_C
    hugsEdit      = "";
#else
    hugsEdit      = strCopy(fromEnv("EDITOR",NULL));
#endif
    hugsPath      = strCopy(HUGSPATH);
    readOptions("-p\"%s> \" -r$$");
#if USE_REGISTRY
    projectPath   = strCopy(readRegChildStrings(HKEY_LOCAL_MACHINE,ProjectRoot,
                                                "HUGSPATH", PATHSEP, ""));
    readOptions(readRegString(HKEY_LOCAL_MACHINE,HugsRoot,"Options",""));
    readOptions(readRegString(HKEY_CURRENT_USER, HugsRoot,"Options",""));
#endif /* USE_REGISTRY */
    readOptions(fromEnv("STGHUGSFLAGS",""));

   strncpy(argv_0_orig,argv[0],1000);   /* startupHaskell mangles argv[0] */
   startupHaskell (argc,argv);
   argc = prog_argc; argv = prog_argv;

   namesUpto = numScripts = 0;

   /* Pre-scan flags to see if -c or +c is present.  This needs to
      precede adding the stack entry for Prelude.  On the other hand,
      that stack entry needs to be made before the cmd line args are
      properly examined.  Hence the following pre-scan of them.
   */
   for (i=1; i < argc; ++i) {
      if (strcmp(argv[i], "--")==0) break;
      if (strcmp(argv[i], "-c")==0) combined = FALSE;
      if (strcmp(argv[i], "+c")==0) combined = TRUE;
   }

   addStackEntry("Prelude");
   if (combined) addStackEntry("PrelHugs");

   for (i=1; i < argc; ++i) {            /* process command line arguments  */
        if (strcmp(argv[i], "--")==0) break;
        if (strcmp(argv[i],"+")==0 && i+1<argc) {
            if (proj) {
                ERRMSG(0) "Multiple project filenames on command line"
                EEND;
            } else {
                proj = argv[++i];
            }
        } else if (argv[i] && argv[i][0]/* workaround for /bin/sh silliness*/
                 && !processOption(argv[i])) {
            addStackEntry(argv[i]);
        }
    }

#if DEBUG
    { 
       char exe_name[N_INSTALLDIR + 6];
       strcpy(exe_name, installDir);
       strcat(exe_name, "hugs");
       DEBUG_LoadSymbols(exe_name);
    }
#endif


#if 0
    if (!scriptName[0]) {
        Printf("Prelude not found on current path: \"%s\"\n",
               hugsPath ? hugsPath : "");
        fatal("Unable to load prelude");
    }
#endif

    if (haskell98) {
        Printf("Haskell 98 mode: Restart with command line option -98 to enable extensions\n");
    } else {
        Printf("Hugs mode: Restart with command line option +98 for Haskell 98 mode\n");
    }

    if (combined) {
        Printf("Combined mode: Restart with command line -c for standalone mode\n\n" );
    } else {
        Printf("Standalone mode: Restart with command line +c for combined mode\n\n" );
    }
 
    everybody(PREPREL);

    evalModule = findText("");      /* evaluate wrt last module by default */
    if (proj) {
        if (namesUpto>1) {
            fprintf(stderr,
                    "\nUsing project file, ignoring additional filenames\n");
        }
        loadProject(strCopy(proj));
    }
    readScripts(0);
}

/* --------------------------------------------------------------------------
 * Command line options:
 * ------------------------------------------------------------------------*/

struct options {                        /* command line option toggles     */
    char   c;                           /* table defined in main app.      */
    int    h98;
    String description;
    Bool   *flag;
};
extern struct options toggle[];

static Void local toggleSet(c,state)    /* Set command line toggle         */
Char c;
Bool state; {
    Int i;
    for (i=0; toggle[i].c; ++i)
        if (toggle[i].c == c) {
            *toggle[i].flag = state;
            return;
        }
    ERRMSG(0) "Unknown toggle `%c'", c
    EEND;
}

static Void local togglesIn(state)      /* Print current list of toggles in*/
Bool state; {                           /* given state                     */
    Int count = 0;
    Int i;
    for (i=0; toggle[i].c; ++i)
 	if (*toggle[i].flag == state && (!haskell98 || toggle[i].h98)) {
            if (count==0)
                Putchar((char)(state ? '+' : '-'));
            Putchar(toggle[i].c);
            count++;
        }
    if (count>0)
        Putchar(' ');
}

static Void local optionInfo() {        /* Print information about command */
    static String fmts = "%-5s%s\n";    /* line settings                   */
    static String fmtc = "%-5c%s\n";
    Int    i;

    Printf("TOGGLES: groups begin with +/- to turn options on/off resp.\n");
    for (i=0; toggle[i].c; ++i) {
	if (!haskell98 || toggle[i].h98) {
  	    Printf(fmtc,toggle[i].c,toggle[i].description);
	}
    }

    Printf("\nOTHER OPTIONS: (leading + or - makes no difference)\n");
    Printf(fmts,"hnum","Set heap size (cannot be changed within Hugs)");
    Printf(fmts,"pstr","Set prompt string to str");
    Printf(fmts,"rstr","Set repeat last expression string to str");
    Printf(fmts,"Pstr","Set search path for modules to str");
    Printf(fmts,"Estr","Use editor setting given by str");
    Printf(fmts,"cnum","Set constraint cutoff limit");
#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
    Printf(fmts,"Fstr","Set preprocessor filter to str");
#endif

    Printf("\nCurrent settings: ");
    togglesIn(TRUE);
    togglesIn(FALSE);
    Printf("-h%d",heapSize);
    Printf(" -p");
    printString(prompt);
    Printf(" -r");
    printString(repeatStr);
    Printf(" -c%d",cutoff);
    Printf("\nSearch path     : -P");
    printString(hugsPath);
#if 0
ToDo
    if (projectPath!=NULL) {
        Printf("\nProject Path    : %s",projectPath);
    }
#endif
    Printf("\nEditor setting  : -E");
    printString(hugsEdit);
#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
    Printf("\nPreprocessor    : -F");
    printString(preprocessor);
#endif
    Printf("\nCompatibility   : %s", haskell98 ? "Haskell 98 (+98)"
 					       : "Hugs Extensions (-98)");
    Putchar('\n');
}

#if USE_REGISTRY || HUGS_FOR_WINDOWS
#define PUTC(c)                         \
    *next++=(c)

#define PUTS(s)                         \
    strcpy(next,s);                     \
    next+=strlen(next)

#define PUTInt(optc,i)                  \
    sprintf(next,"-%c%d",optc,i);       \
    next+=strlen(next)

#define PUTStr(c,s)                     \
    next=PUTStr_aux(next,c,s)

static String local PUTStr_aux Args((String,Char, String));

static String local PUTStr_aux(next,c,s)
String next;
Char   c;
String s; {
    if (s) { 
        String t = 0;
        sprintf(next,"-%c\"",c); 
        next+=strlen(next);      
        for(t=s; *t; ++t) {
            PUTS(unlexChar(*t,'"'));
        }
        next+=strlen(next);      
        PUTS("\" ");
    }
    return next;
}

static String local optionsToStr() {          /* convert options to string */
    static char buffer[2000];
    String next = buffer;

    Int i;
    for (i=0; toggle[i].c; ++i) {
        PUTC(*toggle[i].flag ? '+' : '-');
        PUTC(toggle[i].c);
        PUTC(' ');
    }
    PUTS(haskell98 ? "+98 " : "-98 ");
    PUTInt('h',hpSize);  PUTC(' ');
    PUTStr('p',prompt);
    PUTStr('r',repeatStr);
    PUTStr('P',hugsPath);
    PUTStr('E',hugsEdit);
    PUTInt('c',cutoff);  PUTC(' ');
#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
    PUTStr('F',preprocessor);
#endif
    PUTC('\0');
    return buffer;
}
#endif /* USE_REGISTRY */

#undef PUTC
#undef PUTS
#undef PUTInt
#undef PUTStr

static Void local readOptions(options)         /* read options from string */
String options; {
    String s;
    if (options) {
        stringInput(options);
        while ((s=readFilename())!=0) {
            if (*s && !processOption(s)) {
                ERRMSG(0) "Option string must begin with `+' or `-'"
                EEND;
            }
        }
    }
}

static Bool local processOption(s)      /* process string s for options,   */
String s; {                             /* return FALSE if none found.     */
    Bool state;

    if (s[0]=='-')
        state = FALSE;
    else if (s[0]=='+')
        state = TRUE;
    else
        return FALSE;

    while (*++s)
        switch (*s) {
            case 'Q' : break;                           /* already handled */

            case 'p' : if (s[1]) {
                           if (prompt) free(prompt);
                           prompt = strCopy(s+1);
                       }
                       return TRUE;

            case 'r' : if (s[1]) {
                           if (repeatStr) free(repeatStr);
                           repeatStr = strCopy(s+1);
                       }
                       return TRUE;

            case 'P' : {
                           String p = substPath(s+1,hugsPath ? hugsPath : "");
                           if (hugsPath) free(hugsPath);
                           hugsPath = p;
                           return TRUE;
                       }

            case 'E' : if (hugsEdit) free(hugsEdit);
                       hugsEdit = strCopy(s+1);
                       return TRUE;

#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
            case 'F' : if (preprocessor) free(preprocessor);
                       preprocessor = strCopy(s+1);
                       return TRUE;
#endif

            case 'h' : setHeapSize(s+1);
                       return TRUE;

            case 'c' : if (heapBuilt()) {
                          FPrintf(stderr, 
                                  "You can't enable/disable combined"
                                  " operation inside Hugs\n" );
                       } else {
 		          /* don't do anything, since pre-scan of args
                             will have got it already */
                       }
                       return TRUE;

            case 'D' : /* hack */
                {
                    extern void setRtsFlags( int x );
                    setRtsFlags(argToInt(s+1));
                    return TRUE;
                }

            default  : if (strcmp("98",s)==0) {
                           if (heapBuilt() && ((state && !haskell98) ||
                                               (!state && haskell98))) {
                               FPrintf(stderr,
                                       "Haskell 98 compatibility cannot be changed"
                                       " while the interpreter is running\n");
                           } else {
                               haskell98 = state;
                           }
                           return TRUE;
                       } else {
                           toggleSet(*s,state);
                       }
                       break;
        }
    return TRUE;
}

static Void local setHeapSize(s) 
String s; {
    if (s) {
        hpSize = argToInt(s);
        if (hpSize < MINIMUMHEAP)
            hpSize = MINIMUMHEAP;
        else if (MAXIMUMHEAP && hpSize > MAXIMUMHEAP)
            hpSize = MAXIMUMHEAP;
        if (heapBuilt() && hpSize != heapSize) {
            /* ToDo: should this use a message box in winhugs? */
#if USE_REGISTRY
            FPrintf(stderr,"Change to heap size will not take effect until you rerun Hugs\n");
#else
            FPrintf(stderr,"You cannot change heap size from inside Hugs\n");
#endif
        } else {
            heapSize = hpSize;
        }
    }
}

static Int local argToInt(s)            /* read integer from argument str  */
String s; {
    Int    n = 0;
    String t = s;

    if (*s=='\0' || !isascii((int)(*s)) || !isdigit((int)(*s))) {
        ERRMSG(0) "Missing integer in option setting \"%s\"", t
        EEND;
    }

    do {
        Int d = (*s++) - '0';
        if (n > ((MAXPOSINT - d)/10)) {
            ERRMSG(0) "Option setting \"%s\" is too large", t
            EEND;
        }
        n     = 10*n + d;
    } while (isascii((int)(*s)) && isdigit((int)(*s)));

    if (*s=='K' || *s=='k') {
        if (n > (MAXPOSINT/1000)) {
            ERRMSG(0) "Option setting \"%s\" is too large", t
            EEND;
        }
        n *= 1000;
        s++;
    }

#if MAXPOSINT > 1000000                 /* waste of time on 16 bit systems */
    if (*s=='M' || *s=='m') {
        if (n > (MAXPOSINT/1000000)) {
            ERRMSG(0) "Option setting \"%s\" is too large", t
            EEND;
        }
        n *= 1000000;
        s++;
    }
#endif

#if MAXPOSINT > 1000000000
    if (*s=='G' || *s=='g') {
        if (n > (MAXPOSINT/1000000000)) {
            ERRMSG(0) "Option setting \"%s\" is too large", t
            EEND;
        }
        n *= 1000000000;
        s++;
    }
#endif

    if (*s!='\0') {
        ERRMSG(0) "Unwanted characters after option setting \"%s\"", t
        EEND;
    }

    return n;
}

/* --------------------------------------------------------------------------
 * Print Menu of list of commands:
 * ------------------------------------------------------------------------*/

static struct cmd cmds[] = {
 {":?",      HELP},   {":cd",   CHGDIR},  {":also",    ALSO},
 {":type",   TYPEOF}, {":!",    SYSTEM},  {":load",    LOAD},
 {":reload", RELOAD}, {":gc",   COLLECT}, {":edit",    EDIT},
 {":quit",   QUIT},   {":set",  SET},     {":find",    FIND},
 {":names",  NAMES},  {":info", INFO},    {":project", PROJECT},
 {":dump",   DUMP},   {":ztats", STATS},
 {":module",SETMODULE}, 
 {":browse", BROWSE},
#if EXPLAIN_INSTANCE_RESOLUTION
 {":xplain", XPLAIN},
#endif
 {":version", PNTVER},
 {"",      EVAL},
 {0,0}
};

static Void local menu() {
    Printf("LIST OF COMMANDS:  Any command may be abbreviated to :c where\n");
    Printf("c is the first character in the full name.\n\n");
    Printf(":load <filenames>   load modules from specified files\n");
    Printf(":load               clear all files except prelude\n");
    Printf(":also <filenames>   read additional modules\n");
    Printf(":reload             repeat last load command\n");
    Printf(":project <filename> use project file\n");
    Printf(":edit <filename>    edit file\n");
    Printf(":edit               edit last module\n");
    Printf(":module <module>    set module for evaluating expressions\n");
    Printf("<expr>              evaluate expression\n");
    Printf(":type <expr>        print type of expression\n");
    Printf(":?                  display this list of commands\n");
    Printf(":set <options>      set command line options\n");
    Printf(":set                help on command line options\n");
    Printf(":names [pat]        list names currently in scope\n");
    Printf(":info <names>       describe named objects\n");
    Printf(":browse <modules>   browse names defined in <modules>\n");
#if EXPLAIN_INSTANCE_RESOLUTION
    Printf(":xplain <context>   explain instance resolution for <context>\n");
#endif
    Printf(":find <name>        edit module containing definition of name\n");
    Printf(":!command           shell escape\n");
    Printf(":cd dir             change directory\n");
    Printf(":gc                 force garbage collection\n");
    Printf(":version            print Hugs version\n");
    Printf(":dump <name>        print STG code for named fn\n");
#ifdef CRUDE_PROFILING
    Printf(":ztats <name>       print reduction stats\n");
#endif
    Printf(":quit               exit Hugs interpreter\n");
}

static Void local guidance() {
    Printf("Command not recognised.  ");
    forHelp();
}

static Void local forHelp() {
    Printf("Type :? for help\n");
}

/* --------------------------------------------------------------------------
 * Setting of command line options:
 * ------------------------------------------------------------------------*/

struct options toggle[] = {             /* List of command line toggles    */
    {'s', 1, "Print no. reductions/cells after eval", &showStats},
    {'t', 1, "Print type after evaluation",           &addType},
    {'g', 1, "Print no. cells recovered after gc",    &gcMessages},
    {'l', 1, "Literate modules as default",           &literateScripts},
    {'e', 1, "Warn about errors in literate modules", &literateErrors},
    {'.', 1, "Print dots to show progress",           &useDots},
    {'q', 1, "Print nothing to show progress",        &quiet},
    {'w', 1, "Always show which modules are loaded",  &listScripts},
    {'k', 1, "Show kind errors in full",              &kindExpert},
    {'o', 0, "Allow overlapping instances",           &allowOverlap},


#if DEBUG_CODE
    {'D', 1, "Debug: show generated code",            &debugCode},
#endif
#if EXPLAIN_INSTANCE_RESOLUTION
    {'x', 1, "Explain instance resolution",           &showInstRes},
#endif
#if MULTI_INST
    {'m', 0, "Use multi instance resolution",         &multiInstRes},
#endif
#if DEBUG_CODE
    {'D', 1, "Debug: show generated G code",          &debugCode},
#endif
    {'S', 1, "Debug: show generated SC code",         &debugSC},
    {0,   0, 0,                                       0}
};

static Void local set() {               /* change command line options from*/
    String s;                           /* Hugs command line               */

    if ((s=readFilename())!=0) {
        do {
            if (!processOption(s)) {
                ERRMSG(0) "Option string must begin with `+' or `-'"
                EEND;
            }
        } while ((s=readFilename())!=0);
#if USE_REGISTRY
        writeRegString("Options", optionsToStr());
#endif
    }
    else
        optionInfo();
}

/* --------------------------------------------------------------------------
 * Change directory command:
 * ------------------------------------------------------------------------*/

static Void local changeDir() {         /* change directory                */
    String s = readFilename();
    if (s && chdir(s)) {
        ERRMSG(0) "Unable to change to directory \"%s\"", s
        EEND;
    }
}

/* --------------------------------------------------------------------------
 * Loading project and script files:
 * ------------------------------------------------------------------------*/

static Void local loadProject(s)        /* Load project file               */
String s; {
    clearProject();
    currProject = s;
    projInput(currProject);
    scriptFile = currProject;
    forgetScriptsFrom(N_PRELUDE_SCRIPTS);
    while ((s=readFilename())!=0)
        addStackEntry(s);
    if (namesUpto<=1) {
        ERRMSG(0) "Empty project file"
        EEND;
    }
    scriptFile    = 0;
    projectLoaded = TRUE;
}

static Void local clearProject() {      /* clear name for current project  */
    if (currProject)
        free(currProject);
    currProject   = 0;
    projectLoaded = FALSE;
#if HUGS_FOR_WINDOWS
    setLastEdit((String)0,0);
#endif
}



static Void local makeStackEntry ( ScriptInfo* ent, String iname )
{
   Bool   ok, fromObj;
   Bool   sAvail, iAvail, oAvail;
   Time   sTime,  iTime,  oTime;
   Long   sSize,  iSize,  oSize;
   String path,   sExt;

   ok = findFilesForModule (
           iname,
           &path,
           &sExt,
           &sAvail, &sTime, &sSize,
           &iAvail, &iTime, &iSize,
           &oAvail, &oTime, &oSize
        );
   if (!ok) {
      ERRMSG(0) 
         "Can't find source or object+interface for module \"%s\"",
         /* "Can't find source for module \"%s\"", */
         iname
      EEND;
   }
   /* findFilesForModule should enforce this */
   if (!(sAvail || (oAvail && iAvail))) 
      internal("chase");
   /* Load objects in preference to sources if both are available */
   /* 11 Oct 99: disable object loading in the interim.
      Will probably only reinstate when HEP becomes available.
   */
   if (combined) {
      fromObj = sAvail
                ? (oAvail && iAvail && timeEarlier(sTime,oTime))
                : TRUE;
   } else {
      fromObj = FALSE;
   }

   /* ToDo: namesUpto overflow */
   ent->modName     = strCopy(iname);
   ent->details     = TRUE;
   ent->path        = path;
   ent->fromSource  = !fromObj;
   ent->srcExt      = sExt;
   ent->postponed   = FALSE;
   ent->lastChange  = sTime; /* ToDo: is this right? */
   ent->size        = fromObj ? iSize : sSize;
   ent->oSize       = fromObj ? oSize : 0;
   ent->objLoaded   = FALSE;
}



static Void nukeEnding( String s )
{
    Int l = strlen(s);
    if (l > 4 && strncmp(s+l-4,".u_o" ,4)==0) s[l-4] = 0; else
    if (l > 5 && strncmp(s+l-5,".u_hi",5)==0) s[l-5] = 0; else
    if (l > 3 && strncmp(s+l-3,".hs"  ,3)==0) s[l-3] = 0; else
    if (l > 4 && strncmp(s+l-4,".lhs" ,4)==0) s[l-4] = 0; else
    if (l > 4 && strncmp(s+l-4,".dll" ,4)==0) s[l-4] = 0; else
    if (l > 4 && strncmp(s+l-4,".DLL" ,4)==0) s[l-4] = 0;
}

static Void local addStackEntry(s)     /* Add script to list of scripts    */
String s; {                            /* to be read in ...                */
    String s2;
    Bool   found;
    Int    i;

    if (namesUpto>=NUM_SCRIPTS) {
        ERRMSG(0) "Too many module files (maximum of %d allowed)",
                  NUM_SCRIPTS
        EEND;
    }

    s = strCopy(s);
    nukeEnding(s);
    for (s2 = s; *s2; s2++)
       if (*s2 == SLASH && *(s2+1)) s = s2+1;

    found = FALSE;
    for (i = 0; i < namesUpto; i++)
       if (strcmp(scriptInfo[i].modName,s)==0)
          found = TRUE;

    if (!found) {
       makeStackEntry ( &scriptInfo[namesUpto], strCopy(s) );
       namesUpto++;
    }
    free(s);
}

/* Return TRUE if no imports were needed; FALSE otherwise. */
static Bool local addScript(stacknum)   /* read single file                */
Int stacknum; {
   Bool didPrelude;
   static char name[FILENAME_MAX+1];
   Int len = scriptInfo[stacknum].size;

#if HUGS_FOR_WINDOWS                    /* Set clock cursor while loading  */
    allowBreak();
    SetCursor(LoadCursor(NULL, IDC_WAIT));
#endif

    //   setLastEdit(name,0);

   strcpy(name, scriptInfo[stacknum].path);
   strcat(name, scriptInfo[stacknum].modName);
   if (scriptInfo[stacknum].fromSource)
      strcat(name, scriptInfo[stacknum].srcExt); else
      strcat(name, ".u_hi");

   scriptFile = name;

   if (scriptInfo[stacknum].fromSource) {
      if (lastWasObject) {
         didPrelude = processInterfaces();
         if (didPrelude) {
            preludeLoaded = TRUE;
            everybody(POSTPREL);
         }
      }
      lastWasObject = FALSE;
      Printf("Reading script \"%s\":\n",name);
      needsImports = FALSE;
      parseScript(name,len);
      if (needsImports) return FALSE;
      checkDefns();
      typeCheckDefns();
      compileDefns();
   } else {
      Cell    iface;
      List    imports;
      ZTriple iface_info;
      char    nameObj[FILENAME_MAX+1];
      Int     sizeObj;

      Printf("Reading  iface \"%s\":\n", name);
      scriptFile = name;
      needsImports = FALSE;

      // set nameObj for the benefit of openGHCIface
      strcpy(nameObj, scriptInfo[stacknum].path);
      strcat(nameObj, scriptInfo[stacknum].modName);
      strcat(nameObj, DLL_ENDING);
      sizeObj = scriptInfo[stacknum].oSize;

      iface = readInterface(name,len);
      imports = zsnd(iface); iface = zfst(iface);

      if (nonNull(imports)) chase(imports);
      scriptFile = 0;
      lastWasObject = TRUE;

      iface_info = ztriple(iface, findText(nameObj), mkInt(sizeObj) );
      ifaces_outstanding = cons(iface_info,ifaces_outstanding);

      if (needsImports) return FALSE;
   }
 
   scriptFile = 0;

   return TRUE;
}


Bool chase(imps)                        /* Process list of import requests */
List imps; {
    Int    dstPosn;
    ScriptInfo tmp;
    Int    origPos  = numScripts;       /* keep track of original position */
    String origName = scriptInfo[origPos].modName;
    for (; nonNull(imps); imps=tl(imps)) {
        String iname = textToStr(textOf(hd(imps)));
        Int    i     = 0;
        for (; i<namesUpto; i++)
            if (strcmp(scriptInfo[i].modName,iname)==0)
                break;
	//fprintf(stderr, "import name = %s   num = %d\n", iname, i );

        if (i<namesUpto) {
           /* We should have filled in the details of each module
              the first time we hear about it.
	   */
           assert(scriptInfo[i].details);
        }

        if (i>=origPos) {               /* Neither loaded or queued        */
            String theName;
            Time   theTime;
            Bool   thePost;
            Bool   theFS;

            needsImports = TRUE;
            if (scriptInfo[origPos].fromSource)
               scriptInfo[origPos].postponed  = TRUE;

            if (i==namesUpto) {         /* Name not found (i==namesUpto)   */
                 /* Find out where it lives, whether source or object, etc */
               makeStackEntry ( &scriptInfo[i], iname );
               namesUpto++;
            }
            else 
            if (scriptInfo[i].postponed && scriptInfo[i].fromSource) {
                                        /* Check for recursive dependency  */
                ERRMSG(0)
                  "Recursive import dependency between \"%s\" and \"%s\"",
                  scriptInfo[origPos].modName, iname
                EEND;
            }
            /* Move stack entry i to somewhere below origPos.  If i denotes 
             * an object, destination is immediately below origPos.  
             * Otherwise, it's underneath the queue of objects below origPos.
             */
            dstPosn = origPos-1;
            if (scriptInfo[i].fromSource)
               while (!scriptInfo[dstPosn].fromSource && dstPosn > 0)
                  dstPosn--;

            dstPosn++;
            tmp = scriptInfo[i];
            for (; i > dstPosn; i--) scriptInfo[i] = scriptInfo[i-1];
            scriptInfo[dstPosn] = tmp;
            if (dstPosn < nextNumScripts) nextNumScripts = dstPosn;
            origPos++;
        }
    }
    return needsImports;
}

static Void local forgetScriptsFrom(scno)/* remove scripts from system     */
Script scno; {
    Script i;
#if 0
    for (i=scno; i<namesUpto; ++i)
        if (scriptName[i])
            free(scriptName[i]);
#endif
    dropScriptsFrom(scno-1);
    namesUpto = scno;
    if (numScripts>namesUpto)
        numScripts = scno;
}

/* --------------------------------------------------------------------------
 * Commands for loading and removing script files:
 * ------------------------------------------------------------------------*/

static Void local load() {           /* read filenames from command line   */
    String s;                        /* and add to list of scripts waiting */
                                     /* to be read                         */
    while ((s=readFilename())!=0)
        addStackEntry(s);
    readScripts(N_PRELUDE_SCRIPTS);
}

static Void local project() {          /* read list of script names from   */
    String s;                          /* project file                     */

    if ((s=readFilename()) || currProject) {
        if (!s)
            s = strCopy(currProject);
        else if (readFilename()) {
            ERRMSG(0) "Too many project files"
            EEND;
        }
        else
            s = strCopy(s);
    }
    else {
        ERRMSG(0) "No project filename specified"
        EEND;
    }
    loadProject(s);
    readScripts(N_PRELUDE_SCRIPTS);
}

static Void local readScripts(n)        /* Reread current list of scripts, */
Int n; {                                /* loading everything after and    */
    Time timeStamp;                     /* including the first script which*/
    Long fileSize;                      /* has been either changed or added*/
    static char name[FILENAME_MAX+1];
    Bool didPrelude;

    lastWasObject = FALSE;
    ppSmStack("readscripts-begin");
#if HUGS_FOR_WINDOWS
    SetCursor(LoadCursor(NULL, IDC_WAIT));
#endif

#if 0
    for (; n<numScripts; n++) {         /* Scan previously loaded scripts  */
        ppSmStack("readscripts-loop1");
        getFileInfo(scriptName[n], &timeStamp, &fileSize);
        if (timeChanged(timeStamp,lastChange[n])) {
            dropScriptsFrom(n-1);
            numScripts = n;
            break;
        }
    }
    for (; n<NUM_SCRIPTS; n++)          /* No scripts have been postponed  */
        postponed[n] = FALSE;           /* at this stage                   */
    numScripts = 0;

    while (numScripts<namesUpto) {      /* Process any remaining scripts   */
        ppSmStack("readscripts-loop2");
        getFileInfo(scriptName[numScripts], &timeStamp, &fileSize);
        timeSet(lastChange[numScripts],timeStamp);
        if (numScripts>0)               /* no new script for prelude       */
            startNewScript(scriptName[numScripts]);
        if (addScript(scriptName[numScripts],fileSize))
            numScripts++;
        else
            dropScriptsFrom(numScripts-1);
    }
#endif

    interface(RESET);

    for (; n<numScripts; n++) {
        ppSmStack("readscripts-loop2");
        strcpy(name, scriptInfo[n].path);
        strcat(name, scriptInfo[n].modName);
        if (scriptInfo[n].fromSource)
           strcat(name, scriptInfo[n].srcExt); else
           strcat(name, ".u_hi");  //ToDo: should be .o
        getFileInfo(name,&timeStamp, &fileSize);
        if (timeChanged(timeStamp,scriptInfo[n].lastChange)) {
           dropScriptsFrom(n-1);
           numScripts = n;
           break;
        }
    }
    for (; n<NUM_SCRIPTS; n++)
        scriptInfo[n].postponed = FALSE;

    //numScripts = 0;

    while (numScripts < namesUpto) {
       ppSmStack ( "readscripts-loop2" );

       if (scriptInfo[numScripts].fromSource) {

          if (numScripts>0)
              startNewScript(scriptInfo[numScripts].modName);
          nextNumScripts = NUM_SCRIPTS; //bogus initialisation
          if (addScript(numScripts)) {
             numScripts++;
             assert(nextNumScripts==NUM_SCRIPTS);
          }
          else
             dropScriptsFrom(numScripts-1);

       } else {
      
          if (scriptInfo[numScripts].objLoaded) {
             numScripts++;
          } else {
             scriptInfo[numScripts].objLoaded = TRUE;
             /* new */
             if (numScripts>0)
                 startNewScript(scriptInfo[numScripts].modName);
	     /* end */
             nextNumScripts = NUM_SCRIPTS;
             if (addScript(numScripts)) {
                numScripts++;
                assert(nextNumScripts==NUM_SCRIPTS);
             } else {
	        //while (!scriptInfo[numScripts].fromSource && numScripts > 0)
	        //   numScripts--;
	        //if (scriptInfo[numScripts].fromSource)
	        //   numScripts++;
                numScripts = nextNumScripts;
                assert(nextNumScripts<NUM_SCRIPTS);
             }
          }
       }
       if (numScripts==namesUpto) ppSmStack( "readscripts-final") ;
    }

    didPrelude = processInterfaces();
    if (didPrelude) {
       preludeLoaded = TRUE;
       everybody(POSTPREL);
    }


    { Int  m     = namesUpto-1;
      Text mtext = findText(scriptInfo[m].modName);

      /* Hack to avoid starting up in PrelHugs */
      if (mtext == findText("PrelHugs")) mtext = findText("Prelude");


      /* Commented out till we understand what
       * this is trying to do.
       * Problem, you cant find a module till later.
       */
#if 0
       setCurrModule(findModule(mtext)); 
#endif
      evalModule = mtext;
    }

    

    if (listScripts)
        whatScripts();
    if (numScripts<=1)
        setLastEdit((String)0, 0);
    ppSmStack("readscripts-end  ");
}

static Void local whatScripts() {       /* list scripts in current session */
    int i;
    Printf("\nHugs session for:");
    if (projectLoaded)
        Printf(" (project: %s)",currProject);
    for (i=0; i<numScripts; ++i)
      Printf("\n%s%s",scriptInfo[i].path, scriptInfo[i].modName);
    Putchar('\n');
}

/* --------------------------------------------------------------------------
 * Access to external editor:
 * ------------------------------------------------------------------------*/

static Void local editor() {            /* interpreter-editor interface    */
    String newFile  = readFilename();
    if (newFile) {
        setLastEdit(newFile,0);
        if (readFilename()) {
            ERRMSG(0) "Multiple filenames not permitted"
            EEND;
        }
    }
    runEditor();
}

static Void local find() {              /* edit file containing definition */
#if 0
This just plain wont work no more.
ToDo: Fix!
    String nm = readFilename();         /* of specified name               */
    if (!nm) {
        ERRMSG(0) "No name specified"
        EEND;
    }
    else if (readFilename()) {
        ERRMSG(0) "Multiple names not permitted"
        EEND;
    }
    else {
        Text t;
        Cell c;
        setCurrModule(findEvalModule());
        startNewScript(0);
        if (nonNull(c=findTycon(t=findText(nm)))) {
            if (startEdit(tycon(c).line,scriptName[scriptThisTycon(c)])) {
                readScripts(N_PRELUDE_SCRIPTS);
            }
        } else if (nonNull(c=findName(t))) {
            if (startEdit(name(c).line,scriptName[scriptThisName(c)])) {
                readScripts(N_PRELUDE_SCRIPTS);
            }
        } else {
            ERRMSG(0) "No current definition for name \"%s\"", nm
            EEND;
        }
    }
#endif
}

static Void local runEditor() {         /* run editor on script lastEdit   */
    if (startEdit(lastEdLine,lastEdit)) /* at line lastEdLine              */
        readScripts(N_PRELUDE_SCRIPTS);
}

static Void local setLastEdit(fname,line)/* keep name of last file to edit */
String fname;
Int    line; {
    if (lastEdit)
        free(lastEdit);
    lastEdit = strCopy(fname);
    lastEdLine = line;
#if HUGS_FOR_WINDOWS
    DrawStatusLine(hWndMain);           /* Redo status line                */
#endif
}

/* --------------------------------------------------------------------------
 * Read and evaluate an expression:
 * ------------------------------------------------------------------------*/

static Void local setModule(){/*set module in which to evaluate expressions*/
    String s = readFilename();
    if (!s) s = "";              /* :m clears the current module selection */
    evalModule = findText(s);
    setLastEdit(fileOfModule(findEvalModule()),0);
}

static Module local findEvalModule() { /*Module in which to eval expressions*/
    Module m = findModule(evalModule); 
    if (isNull(m))
        m = lastModule();
    return m;
}

static Void local evaluator() {        /* evaluate expr and print value    */
    Type  type, bd;
    Kinds ks   = NIL;

    setCurrModule(findEvalModule());
    scriptFile = 0;
    startNewScript(0);                 /* Enables recovery of storage      */
                                       /* allocated during evaluation      */
    parseExp();
    checkExp();
    defaultDefns = combined ? stdDefaults : evalDefaults;
    type         = typeCheckExp(TRUE);

    if (isPolyType(type)) {
        ks = polySigOf(type);
        bd = monotypeOf(type);
    }
    else
        bd = type;

    if (whatIs(bd)==QUAL) {
        ERRMSG(0) "Unresolved overloading" ETHEN
        ERRTEXT   "\n*** Type       : "    ETHEN ERRTYPE(type);
        ERRTEXT   "\n*** Expression : "    ETHEN ERREXPR(inputExpr);
        ERRTEXT   "\n"
        EEND;
    }
  
#ifdef WANT_TIMER
    updateTimers();
#endif

#if 1
    if (isProgType(ks,bd)) {
        inputExpr = ap(nameRunIO_toplevel,inputExpr);
        evalExp();
        Putchar('\n');
    } else {
        Cell d = provePred(ks,NIL,ap(classShow,bd));
        if (isNull(d)) {
            ERRMSG(0) "Cannot find \"show\" function for:" ETHEN
            ERRTEXT   "\n*** expression : "   ETHEN ERREXPR(inputExpr);
            ERRTEXT   "\n*** of type    : "   ETHEN ERRTYPE(type);
            ERRTEXT   "\n"
            EEND;
        }
        inputExpr = ap2(nameShow,           d,inputExpr);
        inputExpr = ap (namePutStr,         inputExpr);
        inputExpr = ap (nameRunIO_toplevel, inputExpr);

        evalExp(); printf("\n");
        if (addType) {
            printf(" :: ");
            printType(stdout,type);
            Putchar('\n');
        }
    }

#else

   printf ( "result type is " );
   printType ( stdout, type );
   printf ( "\n" );
   evalExp();
   printf ( "\n" );

#endif

}

static Void local stopAnyPrinting() {  /* terminate printing of expression,*/
    if (printing) {                    /* after successful termination or  */
        printing = FALSE;              /* runtime error (e.g. interrupt)   */
        Putchar('\n');
        if (showStats) {
#define plural(v)   v, (v==1?"":"s")
            Printf("%lu cell%s",plural(numCells));
            if (numGcs>0)
                Printf(", %u garbage collection%s",plural(numGcs));
            Printf(")\n");
#undef plural
        }
        FlushStdout();
        garbageCollect();
    }
}

/* --------------------------------------------------------------------------
 * Print type of input expression:
 * ------------------------------------------------------------------------*/

static Void local showtype() {         /* print type of expression (if any)*/
    Cell type;

    setCurrModule(findEvalModule());
    startNewScript(0);                 /* Enables recovery of storage      */
                                       /* allocated during evaluation      */
    parseExp();
    checkExp();
    defaultDefns = evalDefaults;
    type = typeCheckExp(FALSE);
    printExp(stdout,inputExpr);
    Printf(" :: ");
    printType(stdout,type);
    Putchar('\n');
}


static Void local browseit(mod,t,all)
Module mod; 
String t;
Bool all; {
    if (nonNull(mod)) {
	Cell cs;
	if (nonNull(t))
	    Printf("module %s where\n",textToStr(module(mod).text));
	for (cs = module(mod).names; nonNull(cs); cs=tl(cs)) {
	    Name nm = hd(cs);
	    /* only look at things defined in this module,
 	       unless `all' flag is set */
	    if (all || name(nm).mod == mod) {
		/* unwanted artifacts, like lambda lifted values,
		   are in the list of names, but have no types */
		if (nonNull(name(nm).type)) {
		    printExp(stdout,nm);
		    Printf(" :: ");
		    printType(stdout,name(nm).type);
		    if (isCfun(nm)) {
			Printf("  -- data constructor");
		    } else if (isMfun(nm)) {
			Printf("  -- class member");
		    } else if (isSfun(nm)) {
			Printf("  -- selector function");
		    }
		    Printf("\n");
		}
	    }
	}
    } else {
      if (isNull(mod)) {
	Printf("Unknown module %s\n",t);
      }
    }
}

static Void local browse() {            /* browse modules                  */
    Int    count = 0;                   /* or give menu of commands        */
    String s;
    Bool all = FALSE;

    setCurrModule(findEvalModule());
    startNewScript(0);                  /* for recovery of storage         */
    for (; (s=readFilename())!=0; count++)
	if (strcmp(s,"all") == 0) {
	    all = TRUE;
	    --count;
	} else
	    browseit(findModule(findText(s)),s,all);
    if (count == 0) {
	browseit(findEvalModule(),NULL,all);
    }
}

#if EXPLAIN_INSTANCE_RESOLUTION
static Void local xplain() {         /* print type of expression (if any)*/
    Cell d;
    Bool sir = showInstRes;

    setCurrModule(findEvalModule());
    startNewScript(0);                 /* Enables recovery of storage      */
				       /* allocated during evaluation      */
    parseContext();
    checkContext();
    showInstRes = TRUE;
    d = provePred(NIL,NIL,hd(inputContext));
    if (isNull(d)) {
	fprintf(stdout, "not Sat\n");
    } else {
	fprintf(stdout, "Sat\n");
    }
    showInstRes = sir;
}
#endif

/* --------------------------------------------------------------------------
 * Enhanced help system:  print current list of scripts or give information
 * about an object.
 * ------------------------------------------------------------------------*/

static String local objToStr(m,c)
Module m;
Cell   c; {
#if 1 || DISPLAY_QUANTIFIERS
    static char newVar[60];
    switch (whatIs(c)) {
        case NAME  : if (m == name(c).mod) {
                         sprintf(newVar,"%s", textToStr(name(c).text));
                     } else {
                         sprintf(newVar,"%s.%s",
                                        textToStr(module(name(c).mod).text),
                                        textToStr(name(c).text));
                     }
                     break;

        case TYCON : if (m == tycon(c).mod) {
                         sprintf(newVar,"%s", textToStr(tycon(c).text));
                     } else {
                         sprintf(newVar,"%s.%s",
                                        textToStr(module(tycon(c).mod).text),
                                        textToStr(tycon(c).text));
                     }
                     break;

        case CLASS : if (m == cclass(c).mod) {
                         sprintf(newVar,"%s", textToStr(cclass(c).text));
                     } else {
                         sprintf(newVar,"%s.%s",
                                        textToStr(module(cclass(c).mod).text),
                                        textToStr(cclass(c).text));
                     }
                     break;

        default    : internal("objToStr");
    }
    return newVar;
#else
    static char newVar[33];
    switch (whatIs(c)) {
        case NAME  : sprintf(newVar,"%s", textToStr(name(c).text));
                     break;

        case TYCON : sprintf(newVar,"%s", textToStr(tycon(c).text));
                     break;

        case CLASS : sprintf(newVar,"%s", textToStr(cclass(c).text));
                     break;

        default    : internal("objToStr");
    }
    return newVar;
#endif
}

extern Name nameHw;

static Void dumpStg ( void )
{
   String s;
   Int i;
   setCurrModule(findEvalModule());
   startNewScript(0);
   s = readFilename();

   /* request to locate a symbol by name */
   if (s && (*s == '?')) {
      Text t = findText(s+1);
      locateSymbolByName(t);
      return;
   }

   /* request to dump a bit of the heap */
   if (s && (*s == '-' || isdigit(*s))) {
      int i = atoi(s);
      print(i,100);
      printf("\n");
      return;
   }

   /* request to dump a symbol table entry */
   if (!s 
       || !(*s == 't' || *s == 'n' || *s == 'c' || *s == 'i')
       || !isdigit(s[1])) {
      fprintf(stderr, ":d -- bad request `%s'\n", s );
      return;
   }
   i = atoi(s+1);
   switch (*s) {
      case 't': dumpTycon(i); break;
      case 'n': dumpName(i); break;
      case 'c': dumpClass(i); break;
      case 'i': dumpInst(i); break;
      default: fprintf(stderr, ":d -- `%c' not implemented\n", *s );
   }
}


#if 0
static Void local dumpStg( void ) {       /* print STG stuff                 */
    String s;
    Text   t;
    Name   n;
    Int    i;
    Cell   v;                           /* really StgVar */
    setCurrModule(findEvalModule());
    startNewScript(0);
    for (; (s=readFilename())!=0;) {
        t = findText(s);
        v = n = NIL;
        /* find the name while ignoring module scopes */
        for (i=NAMEMIN; i<nameHw; i++)
           if (name(i).text == t) n = i;

        /* perhaps it's an "idNNNNNN" thing? */
        if (isNull(n) &&
            strlen(s) >= 3 && 
            s[0]=='i' && s[1]=='d' && isdigit(s[2])) {
           v = 0;
           i = 2;
           while (isdigit(s[i])) {
              v = v * 10 + (s[i]-'0');
              i++;
           }
           v = -v;
           n = nameFromStgVar(v);
        }

        if (isNull(n) && whatIs(v)==STGVAR) {
           Printf ( "\n{- `%s' has no nametable entry -}\n", s );
           printStg(stderr, v );
        } else
        if (isNull(n)) {
           Printf ( "Unknown reference `%s'\n", s );
        } else
	if (!isName(n)) {
           Printf ( "Not a Name: `%s'\n", s );
        } else
        if (isNull(name(n).stgVar)) {
           Printf ( "Doesn't have a STG tree: %s\n", s );
        } else {
           Printf ( "\n{- stgVar of `%s' is id%d -}\n", s, -name(n).stgVar);
           printStg(stderr, name(n).stgVar);
        }
    }
}
#endif

static Void local info() {              /* describe objects                */
    Int    count = 0;                   /* or give menu of commands        */
    String s;

    setCurrModule(findEvalModule());
    startNewScript(0);                  /* for recovery of storage         */
    for (; (s=readFilename())!=0; count++) {
        describe(findText(s));
    }
    if (count == 0) {
        whatScripts();
    }
}


static Void local describe(t)           /* describe an object              */
Text t; {
    Tycon  tc  = findTycon(t);
    Class  cl  = findClass(t);
    Name   nm  = findName(t);

    if (nonNull(tc)) {                  /* as a type constructor           */
        Type t = tc;
        Int  i;
        Inst in;
        for (i=0; i<tycon(tc).arity; ++i) {
            t = ap(t,mkOffset(i));
        }
        Printf("-- type constructor");
        if (kindExpert) {
            Printf(" with kind ");
            printKind(stdout,tycon(tc).kind);
        }
        Putchar('\n');
        switch (tycon(tc).what) {
            case SYNONYM      : Printf("type ");
                                printType(stdout,t);
                                Printf(" = ");
                                printType(stdout,tycon(tc).defn);
                                break;

            case NEWTYPE      :
            case DATATYPE     : {   List cs = tycon(tc).defn;
                                    if (tycon(tc).what==DATATYPE) {
                                        Printf("data ");
                                    } else {
                                        Printf("newtype ");
                                    }
                                    printType(stdout,t);
                                    Putchar('\n');
                                    mapProc(printSyntax,cs);
                                    if (hasCfun(cs)) {
                                        Printf("\n-- constructors:");
                                    }
                                    for (; hasCfun(cs); cs=tl(cs)) {
                                        Putchar('\n');
                                        printExp(stdout,hd(cs));
                                        Printf(" :: ");
                                        printType(stdout,name(hd(cs)).type);
                                    }
                                    if (nonNull(cs)) {
                                        Printf("\n-- selectors:");
                                    }
                                    for (; nonNull(cs); cs=tl(cs)) {
                                        Putchar('\n');
                                        printExp(stdout,hd(cs));
                                        Printf(" :: ");
                                        printType(stdout,name(hd(cs)).type);
                                    }
                                }
                                break;

            case RESTRICTSYN  : Printf("type ");
                                printType(stdout,t);
                                Printf(" = <restricted>");
                                break;
        }
        Putchar('\n');
        if (nonNull(in=findFirstInst(tc))) {
            Printf("\n-- instances:\n");
            do {
                showInst(in);
                in = findNextInst(tc,in);
            } while (nonNull(in));
        }
        Putchar('\n');
    }

    if (nonNull(cl)) {                  /* as a class                      */
        List  ins = cclass(cl).instances;
        Kinds ks  = cclass(cl).kinds;
        if (nonNull(ks) && isNull(tl(ks)) && hd(ks)==STAR) {
            Printf("-- type class");
        } else {
            Printf("-- constructor class");
            if (kindExpert) {
                Printf(" with arity ");
                printKinds(stdout,ks);
            }
        }
        Putchar('\n');
        mapProc(printSyntax,cclass(cl).members);
        Printf("class ");
        if (nonNull(cclass(cl).supers)) {
            printContext(stdout,cclass(cl).supers);
            Printf(" => ");
        }
        printPred(stdout,cclass(cl).head);

	if (nonNull(cclass(cl).fds)) {
	    List   fds = cclass(cl).fds;
	    String pre = " | ";
	    for (; nonNull(fds); fds=tl(fds)) {
		Printf(pre);
		printFD(stdout,hd(fds));
		pre = ", ";
	    }
	}

        if (nonNull(cclass(cl).members)) {
            List ms = cclass(cl).members;
            Printf(" where");
            do {
		Type t = name(hd(ms)).type;
                if (isPolyType(t)) {
		    t = monotypeOf(t);
		}
                Printf("\n  ");
                printExp(stdout,hd(ms));
                Printf(" :: ");
                if (isNull(tl(fst(snd(t))))) {
                    t = snd(snd(t));
                } else {
                    t = ap(QUAL,pair(tl(fst(snd(t))),snd(snd(t))));
                }
                printType(stdout,t);
                ms = tl(ms);
            } while (nonNull(ms));
        }
        Putchar('\n');
        if (nonNull(ins)) {
            Printf("\n-- instances:\n");
            do {
                showInst(hd(ins));
                ins = tl(ins);
            } while (nonNull(ins));
        }
        Putchar('\n');
    }

    if (nonNull(nm)) {                  /* as a function/name              */
        printSyntax(nm);
        printExp(stdout,nm);
        Printf(" :: ");
        if (nonNull(name(nm).type)) {
            printType(stdout,name(nm).type);
        } else {
            Printf("<unknown type>");
        }
printf("\n");print(name(nm).type,10);printf("\n");
        if (isCfun(nm)) {
            Printf("  -- data constructor");
        } else if (isMfun(nm)) {
            Printf("  -- class member");
        } else if (isSfun(nm)) {
            Printf("  -- selector function");
        }
        Printf("\n\n");
    }


    if (isNull(tc) && isNull(cl) && isNull(nm)) {
        Printf("Unknown reference `%s'\n",textToStr(t));
    }
}

static Void local printSyntax(nm)
Name nm; {
    Syntax sy = syntaxOf(nm);
    Text   t  = name(nm).text;
    String s  = textToStr(t);
    if (sy != defaultSyntax(t)) {
        Printf("infix");
        switch (assocOf(sy)) {
            case LEFT_ASS  : Putchar('l'); break;
            case RIGHT_ASS : Putchar('r'); break;
            case NON_ASS   : break;
        }
        Printf(" %i ",precOf(sy));
        if (isascii((int)(*s)) && isalpha((int)(*s))) {
            Printf("`%s`",s);
        } else {
            Printf("%s",s);
        }
        Putchar('\n');
    }
}

static Void local showInst(in)          /* Display instance decl header    */
Inst in; {
    Printf("instance ");
    if (nonNull(inst(in).specifics)) {
        printContext(stdout,inst(in).specifics);
        Printf(" => ");
    }
    printPred(stdout,inst(in).head);
    Putchar('\n');
}

/* --------------------------------------------------------------------------
 * List all names currently in scope:
 * ------------------------------------------------------------------------*/

static Void local listNames() {         /* list names matching optional pat*/
    String pat   = readFilename();
    List   names = NIL;
    Int    width = getTerminalWidth() - 1;
    Int    count = 0;
    Int    termPos;
    Module mod   = findEvalModule();

    if (pat) {                          /* First gather names to list      */
        do {
            names = addNamesMatching(pat,names);
        } while ((pat=readFilename())!=0);
    } else {
        names = addNamesMatching((String)0,names);
    }
    if (isNull(names)) {                /* Then print them out             */
        ERRMSG(0) "No names selected"
        EEND;
    }
    for (termPos=0; nonNull(names); names=tl(names)) {
        String s = objToStr(mod,hd(names));
        Int    l = strlen(s);
        if (termPos+1+l>width) { 
            Putchar('\n');       
            termPos = 0;         
        } else if (termPos>0) {  
            Putchar(' ');        
            termPos++;           
        }
        Printf("%s",s);
        termPos += l;
        count++;
    }
    Printf("\n(%d names listed)\n", count);
}

/* --------------------------------------------------------------------------
 * print a prompt and read a line of input:
 * ------------------------------------------------------------------------*/

static Void local promptForInput(moduleName)
String moduleName; {
    char promptBuffer[1000];
#if 1
    /* This is portable but could overflow buffer */
    sprintf(promptBuffer,prompt,moduleName);
#else
    /* Works on ANSI C - but pre-ANSI compilers return a pointer to
     * promptBuffer instead.
     */
    if (sprintf(promptBuffer,prompt,moduleName) >= 1000) {
        /* Reset prompt to a safe default to avoid an infinite loop */
        free(prompt);
        prompt = strCopy("? ");
        internal("Combined prompt and evaluation module name too long");
    }
#endif
    if (autoMain)
       stringInput("main\0"); else
       consoleInput(promptBuffer);
}

/* --------------------------------------------------------------------------
 * main read-eval-print loop, with error trapping:
 * ------------------------------------------------------------------------*/

static jmp_buf catch_error;             /* jump buffer for error trapping  */

static Void local interpreter(argc,argv)/* main interpreter loop           */
Int    argc;
String argv[]; {
    Int errorNumber = setjmp(catch_error);

    if (errorNumber && autoMain) {
       fprintf(stderr, "hugs +Q: compilation failed -- can't run `main'\n" );
       exit(1);
    }

    breakOn(TRUE);                      /* enable break trapping           */
    if (numScripts==0) {                /* only succeeds on first time,    */
        if (errorNumber)                /* before prelude has been loaded  */
            fatal("Unable to load prelude");
        initialize(argc,argv);
        forHelp();
    }

    /* initialize calls startupHaskell, which trashes our signal handlers */
    breakOn(TRUE);

    for (;;) {
        Command cmd;
        everybody(RESET);               /* reset to sensible initial state */
        dropScriptsFrom(numScripts-1);  /* remove partially loaded scripts */
                                        /* not counting prelude as a script*/

        promptForInput(textToStr(module(findEvalModule()).text));

        cmd = readCommand(cmds, (Char)':', (Char)'!');
#ifdef WANT_TIMER
        updateTimers();
#endif
        switch (cmd) {
            case EDIT   : editor();
                          break;
            case FIND   : find();
                          break;
            case LOAD   : clearProject();
                          forgetScriptsFrom(N_PRELUDE_SCRIPTS);
                          load();
                          break;
            case ALSO   : clearProject();
                          forgetScriptsFrom(numScripts);
                          load();
                          break;
            case RELOAD : readScripts(N_PRELUDE_SCRIPTS);
                          break;
            case PROJECT: project();
                          break;
            case SETMODULE :
                          setModule();
                          break;
            case EVAL   : evaluator();
                          break;
            case TYPEOF : showtype();
                          break;
	    case BROWSE : browse();
			  break;
#if EXPLAIN_INSTANCE_RESOLUTION
	    case XPLAIN : xplain();
			  break;
#endif
            case NAMES  : listNames();
                          break;
            case HELP   : menu();
                          break;
            case BADCMD : guidance();
                          break;
            case SET    : set();
                          break;
            case STATS:
#ifdef CRUDE_PROFILING
                          cp_show();
#endif
                          break;
            case SYSTEM : if (shellEsc(readLine()))
                              Printf("Warning: Shell escape terminated abnormally\n");
                          break;
            case CHGDIR : changeDir();
                          break;
            case INFO   : info();
                          break;
	    case PNTVER: Printf("-- Hugs Version %s\n",
				 HUGS_VERSION);
 			  break;
            case DUMP   : dumpStg();
                          break;
            case QUIT   : return;
            case COLLECT: consGC = FALSE;
                          garbageCollect();
                          consGC = TRUE;
                          Printf("Garbage collection recovered %d cells\n",
                                 cellsRecovered);
                          break;
            case NOCMD  : break;
        }
#ifdef WANT_TIMER
        updateTimers();
        Printf("Elapsed time (ms): %ld (user), %ld (system)\n",
               millisecs(userElapsed), millisecs(systElapsed));
#endif
        if (autoMain) break;
    }
    breakOn(FALSE);
}

/* --------------------------------------------------------------------------
 * Display progress towards goal:
 * ------------------------------------------------------------------------*/

static Target currTarget;
static Bool   aiming = FALSE;
static Int    currPos;
static Int    maxPos;
static Int    charCount;

Void setGoal(what, t)                  /* Set goal for what to be t        */
String what;
Target t; {
    if (quiet)
      return;
#if EXPLAIN_INSTANCE_RESOLUTION
    if (showInstRes)
      return;
#endif
    currTarget = (t?t:1);
    aiming     = TRUE;
    if (useDots) {
        currPos = strlen(what);
        maxPos  = getTerminalWidth() - 1;
        Printf("%s",what);
    }
    else
        for (charCount=0; *what; charCount++)
            Putchar(*what++);
    FlushStdout();
}

Void soFar(t)                          /* Indicate progress towards goal   */
Target t; {                            /* has now reached t                */
    if (quiet)
      return;
#if EXPLAIN_INSTANCE_RESOLUTION
    if (showInstRes)
      return;
#endif
    if (useDots) {
        Int newPos = (Int)((maxPos * ((long)t))/currTarget);

        if (newPos>maxPos)
            newPos = maxPos;

        if (newPos>currPos) {
            do
                Putchar('.');
            while (newPos>++currPos);
            FlushStdout();
        }
        FlushStdout();
    }
}

Void done() {                          /* Goal has now been achieved       */
    if (quiet)
      return;
#if EXPLAIN_INSTANCE_RESOLUTION
    if (showInstRes)
      return;
#endif
    if (useDots) {
        while (maxPos>currPos++)
            Putchar('.');
        Putchar('\n');
    }
    else
        for (; charCount>0; charCount--) {
            Putchar('\b');
            Putchar(' ');
            Putchar('\b');
        }
    aiming = FALSE;
    FlushStdout();
}

static Void local failed() {           /* Goal cannot be reached due to    */
    if (aiming) {                      /* errors                           */
        aiming = FALSE;
        Putchar('\n');
        FlushStdout();
    }
}

/* --------------------------------------------------------------------------
 * Error handling:
 * ------------------------------------------------------------------------*/

Void errHead(l)                        /* print start of error message     */
Int l; {
    failed();                          /* failed to reach target ...       */
    stopAnyPrinting();
    FPrintf(errorStream,"ERROR");

    if (scriptFile) {
        FPrintf(errorStream," \"%s\"", scriptFile);
        setLastEdit(scriptFile,l);
        if (l) FPrintf(errorStream," (line %d)",l);
        scriptFile = 0;
    }
    FPrintf(errorStream,": ");
    FFlush(errorStream);
}

Void errFail() {                        /* terminate error message and     */
    Putc('\n',errorStream);             /* produce exception to return to  */
    FFlush(errorStream);                /* main command loop               */
    longjmp(catch_error,1);
}

Void errAbort() {                       /* altern. form of error handling  */
    failed();                           /* used when suitable error message*/
    stopAnyPrinting();                  /* has already been printed        */
    errFail();
}

Void internal(msg)                      /* handle internal error           */
String msg; {
#if HUGS_FOR_WINDOWS
    char buf[300];
    wsprintf(buf,"INTERNAL ERROR: %s",msg);
    MessageBox(hWndMain, buf, appName, MB_ICONHAND | MB_OK);
#endif
    failed();
    stopAnyPrinting();
    Printf("INTERNAL ERROR: %s\n",msg);
    FlushStdout();
    longjmp(catch_error,1);
}

Void fatal(msg)                         /* handle fatal error              */
String msg; {
#if HUGS_FOR_WINDOWS
    char buf[300];
    wsprintf(buf,"FATAL ERROR: %s",msg);
    MessageBox(hWndMain, buf, appName, MB_ICONHAND | MB_OK);
#endif
    FlushStdout();
    Printf("\nFATAL ERROR: %s\n",msg);
    everybody(EXIT);
    exit(1);
}

sigHandler(breakHandler) {              /* respond to break interrupt      */
#if HUGS_FOR_WINDOWS
    MessageBox(GetFocus(), "Interrupted!", appName, MB_ICONSTOP | MB_OK);
#endif
    Hilite();
    Printf("{Interrupted!}\n");
    Lolite();
    breakOn(TRUE);  /* reinstall signal handler - redundant on BSD systems */
                    /* but essential on POSIX (and other?) systems         */
    everybody(BREAK);
    failed();
    stopAnyPrinting();
    FlushStdout();
    clearerr(stdin);
    longjmp(catch_error,1);
    sigResume;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * Read value from environment variable or registry:
 * ------------------------------------------------------------------------*/

String fromEnv(var,def)         /* return value of:                        */
String var;                     /*     environment variable named by var   */
String def; {                   /* or: default value given by def          */
    String s = getenv(var);     
    return (s ? s : def);
}

/* --------------------------------------------------------------------------
 * String manipulation routines:
 * ------------------------------------------------------------------------*/

static String local strCopy(s)         /* make malloced copy of a string   */
String s; {
    if (s && *s) {
        char *t, *r;
        if ((t=(char *)malloc(strlen(s)+1))==0) {
            ERRMSG(0) "String storage space exhausted"
            EEND;
        }
        for (r=t; (*r++ = *s++)!=0; ) {
        }
        return t;
    }
    return NULL;
}

/* --------------------------------------------------------------------------
 * Compiler output
 * We can redirect compiler output (prompts, error messages, etc) by
 * tweaking these functions.
 * ------------------------------------------------------------------------*/

#if REDIRECT_OUTPUT && !HUGS_FOR_WINDOWS

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/* ----------------------------------------------------------------------- */

#define BufferSize 10000              /* size of redirected output buffer  */

typedef struct _HugsStream {
    char buffer[BufferSize];          /* buffer for redirected output      */
    Int  next;                        /* next space in buffer              */
} HugsStream;

static Void   local vBufferedPrintf  Args((HugsStream*, const char*, va_list));
static Void   local bufferedPutchar  Args((HugsStream*, Char));
static String local bufferClear      Args((HugsStream *stream));

static Void local vBufferedPrintf(stream, fmt, ap)
HugsStream* stream;
const char* fmt;
va_list     ap; {
    Int spaceLeft = BufferSize - stream->next;
    char* p = &stream->buffer[stream->next];
    Int charsAdded = vsnprintf(p, spaceLeft, fmt, ap);
    if (0 <= charsAdded && charsAdded < spaceLeft) 
        stream->next += charsAdded;
#if 1 /* we can either buffer the first n chars or buffer the last n chars */
    else
        stream->next = 0;
#endif
}

static Void local bufferedPutchar(stream, c)
HugsStream *stream;
Char        c; {
    if (BufferSize - stream->next >= 2) {
        stream->buffer[stream->next++] = c;
        stream->buffer[stream->next] = '\0';
    }
}    

static String local bufferClear(stream)
HugsStream *stream; {
    if (stream->next == 0) {
        return "";
    } else {
        stream->next = 0;
        return stream->buffer;
    }
}

/* ----------------------------------------------------------------------- */

static HugsStream outputStreamH;
/* ADR note: 
 * We rely on standard C semantics to initialise outputStreamH.next to 0.
 */

Void hugsEnableOutput(f) 
Bool f; {
    disableOutput = !f;
}

String hugsClearOutputBuffer() {
    return bufferClear(&outputStreamH);
}

#ifdef HAVE_STDARG_H
Void hugsPrintf(const char *fmt, ...) {
    va_list ap;                    /* pointer into argument list           */
    va_start(ap, fmt);             /* make ap point to first arg after fmt */
    if (!disableOutput) {
        vprintf(fmt, ap);
    } else {
        vBufferedPrintf(&outputStreamH, fmt, ap);
    }
    va_end(ap);                    /* clean up                             */
}
#else
Void hugsPrintf(fmt, va_alist) 
const char *fmt;
va_dcl {
    va_list ap;                    /* pointer into argument list           */
    va_start(ap);                  /* make ap point to first arg after fmt */
    if (!disableOutput) {
        vprintf(fmt, ap);
    } else {
        vBufferedPrintf(&outputStreamH, fmt, ap);
    }
    va_end(ap);                    /* clean up                             */
}
#endif

Void hugsPutchar(c)
int c; {
    if (!disableOutput) {
        putchar(c);
    } else {
        bufferedPutchar(&outputStreamH, c);
    }
}

Void hugsFlushStdout() {
    if (!disableOutput) {
        fflush(stdout);
    }
}

Void hugsFFlush(fp)
FILE* fp; {
    if (!disableOutput) {
        fflush(fp);
    }
}

#ifdef HAVE_STDARG_H
Void hugsFPrintf(FILE *fp, const char* fmt, ...) {
    va_list ap;             
    va_start(ap, fmt);      
    if (!disableOutput) {
        vfprintf(fp, fmt, ap);
    } else {
        vBufferedPrintf(&outputStreamH, fmt, ap);
    }
    va_end(ap);             
}
#else
Void hugsFPrintf(FILE *fp, const char* fmt, va_list)
FILE* fp;
const char* fmt;
va_dcl {
    va_list ap;             
    va_start(ap);      
    if (!disableOutput) {
        vfprintf(fp, fmt, ap);
    } else {
        vBufferedPrintf(&outputStreamH, fmt, ap);
    }
    va_end(ap);             
}
#endif

Void hugsPutc(c, fp)
int   c;
FILE* fp; {
    if (!disableOutput) {
        putc(c,fp);
    } else {
        bufferedPutchar(&outputStreamH, c);
    }
}
    
#endif /* REDIRECT_OUTPUT && !HUGS_FOR_WINDOWS */
/* --------------------------------------------------------------------------
 * Send message to each component of system:
 * ------------------------------------------------------------------------*/

Void everybody(what)            /* send command `what' to each component of*/
Int what; {                     /* system to respond as appropriate ...    */
#if 0
  fprintf ( stderr, "EVERYBODY %d\n", what );
#endif
    machdep(what);              /* The order of calling each component is  */
    storage(what);              /* important for the PREPREL command       */
    substitution(what);
    input(what);
    translateControl(what);
    linkControl(what);
    staticAnalysis(what);
    deriveControl(what);
    typeChecker(what);
    compiler(what);   
    codegen(what);
}

/* --------------------------------------------------------------------------
 * Hugs for Windows code (WinMain and related functions)
 * ------------------------------------------------------------------------*/

#if HUGS_FOR_WINDOWS
#include "winhugs.c"
#endif

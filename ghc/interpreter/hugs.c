
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
 * $Revision: 1.68 $
 * $Date: 2000/04/25 17:43:49 $
 * ------------------------------------------------------------------------*/

#include <setjmp.h>
#include <ctype.h>
#include <stdio.h>

#include "hugsbasictypes.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "version.h"

#include "Rts.h"
#include "RtsAPI.h"
#include "Schedule.h"
#include "Assembler.h"                                /* DEBUG_LoadSymbols */

Bool haskell98 = TRUE;                  /* TRUE => Haskell 98 compatibility*/
Bool initDone = FALSE;

#if EXPLAIN_INSTANCE_RESOLUTION
Bool showInstRes = FALSE;
#endif
#if MULTI_INST
Bool multiInstRes = FALSE;
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static List   local initialize        ( Int,String [] );
static Void   local promptForInput    ( String );
static Void   local interpreter       ( Int,String [] );
static Void   local menu              ( Void );
static Void   local guidance          ( Void );
static Void   local forHelp           ( Void );
static Void   local set               ( Void );
static Void   local changeDir         ( Void );
static Void   local load              ( Void );
static Void   local project           ( Void );
static Void   local editor            ( Void );
static Void   local find              ( Void );
static Bool   local startEdit         ( Int,String );
static Void   local runEditor         ( Void );
static Void   local setModule         ( Void );
static Void   local evaluator         ( Void );
static Void   local stopAnyPrinting   ( Void );
static Void   local showtype          ( Void );
static String local objToStr          ( Module, Cell );
static Void   local info              ( Void );
static Void   local printSyntax       ( Name );
static Void   local showInst          ( Inst );
static Void   local describe          ( Text );
static Void   local listNames         ( Void );

static Void   local toggleSet         ( Char,Bool );
static Void   local togglesIn         ( Bool );
static Void   local optionInfo        ( Void );
static Void   local readOptions       ( String );
static Bool   local processOption     ( String );
static Void   local setHeapSize       ( String );
static Int    local argToInt          ( String );

static Void   local setLastEdit       ( String,Int );
static Void   local failed            ( Void );
static String local strCopy           ( String );
static Void   local browseit	      ( Module,String,Bool );
static Void   local browse	      ( Void );
static void   local clearCurrentFile  ( void );


/* --------------------------------------------------------------------------
 * Machine dependent code for Hugs interpreter:
 * ------------------------------------------------------------------------*/

#include "machdep.c"

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

       Bool   flagAssert    = FALSE;    /* TRUE => assert False <e> causes
                                                   an assertion failure    */
       Bool   preludeLoaded = FALSE;
       Bool   debugSC       = FALSE;
       Bool   combined      = FALSE;

       Module moduleBeingParsed;        /* so the parser (topModule) knows */
static char*  currentFile;              /* Name of current file, or NULL   */       
static char   currentFileName[1000];    /* name is stored here if it exists*/

static Bool   autoMain   = FALSE;
static String lastEdit   = 0;           /* Name of script to edit (if any) */
static Int    lastEdLine = 0;           /* Editor line number (if possible)*/
static String prompt     = 0;           /* Prompt string                   */
static Int    hpSize     = DEFAULTHEAP; /* Desired heap size               */
static Bool   disableOutput = FALSE;    /* TRUE => quiet                   */
       String hugsEdit   = 0;           /* String for editor command       */
       String hugsPath   = 0;           /* String for file search path     */

       List  ifaces_outstanding = NIL;

static ConId currentModule_failed = NIL; /* Remember failed module from :r */



/* --------------------------------------------------------------------------
 * Hugs entry point:
 * ------------------------------------------------------------------------*/

#ifndef NO_MAIN /* we omit main when building the "Hugs server" */
 
Main main ( Int, String [] );       /* now every func has a prototype  */

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

#ifdef DEBUG
#if 0
    checkBytecodeCount();		/* check for too many bytecodes    */
#endif
#endif

    /* If first arg is +Q or -Q, be entirely silent, and automatically run
       main after loading scripts.  Useful for running the nofib suite.    */
    if (argc > 1 && (strcmp(argv[1],"+Q") == 0 || strcmp(argv[1],"-Q")==0)) {
       autoMain = TRUE;
       if (strcmp(argv[1],"-Q") == 0) {
	 EnableOutput(0);
       }
    }

    Printf("__   __ __  __  ____   ___      _________________________________________\n");
    Printf("||   || ||  || ||  || ||__      STGHugs: Based on the Haskell 98 standard\n");
    Printf("||___|| ||__|| ||__||  __||     Copyright (c) 1994-2000\n");
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

static List /*CONID*/ initialize ( Int argc, String argv[] )
{
   Int    i, j;
   List   initialModules;

   setLastEdit((String)0,0);
   lastEdit      = 0;
   currentFile   = NULL;

#if SYMANTEC_C
   hugsEdit      = "";
#else
   hugsEdit      = strCopy(fromEnv("EDITOR",NULL));
#endif
   hugsPath      = strCopy(HUGSPATH);
   readOptions("-p\"%s> \" -r$$");
   readOptions(fromEnv("STGHUGSFLAGS",""));

#  if DEBUG
   { 
      char exe_name[N_INSTALLDIR + 6];
      strcpy(exe_name, installDir);
      strcat(exe_name, "hugs");
      DEBUG_LoadSymbols(exe_name);
   }
#  endif

   /* startupHaskell extracts args between +RTS ... -RTS, and sets
      prog_argc/prog_argv to the rest.  We want to further process 
      the rest, so we then get hold of them again.
   */
   startupHaskell ( argc, argv, NULL );
   getProgArgv ( &argc, &argv );

   /* Find out early on if we're in combined mode or not.
      everybody(PREPREL) needs to know this.  Also, establish the
      heap size;
   */ 
   for (i = 1; i < argc; ++i) {
      if (strcmp(argv[i], "--")==0) break;
      if (strcmp(argv[i], "-c")==0) combined = FALSE;
      if (strcmp(argv[i], "+c")==0) combined = TRUE;

      if (strncmp(argv[i],"+h",2)==0 || strncmp(argv[i],"-h",2)==0)
         setHeapSize(&(argv[i][2]));
   }

   everybody(PREPREL);
   initialModules = NIL;

   for (i = 1; i < argc; ++i) {          /* process command line arguments  */
      if (strcmp(argv[i], "--")==0) 
         { argv[i] = NULL; break; }
      if (argv[i] && argv[i][0]/* workaround for /bin/sh silliness*/) {
         if (!processOption(argv[i]))
            initialModules
               = cons ( mkCon(findText(argv[i])), initialModules );
         argv[i] = NULL;
      }
   }

   if (haskell98) {
       Printf("Haskell 98 mode: Restart with command line option -98"
              " to enable extensions\n");
   } else {
       Printf("Hugs mode: Restart with command line option +98 for"
              " Haskell 98 mode\n");
   }

   if (combined) {
       Printf("Combined mode: Restart with command line -c for"
              " standalone mode\n\n" );
   } else {
       Printf("Standalone mode: Restart with command line +c for"
              " combined mode\n\n" );
   }

   /* slide args back over the deleted ones. */
   j = 1;
   for (i = 1; i < argc; i++)
      if (argv[i])
         argv[j++] = argv[i];

   argc = j;

   setProgArgv ( argc, argv );

   initDone = TRUE;
   return initialModules;
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
    clearCurrentFile();
    ERRMSG(0) "Unknown toggle `%c'", c
    EEND_NO_LONGJMP;
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

            case 'h' : /* don't do anything, since pre-scan of args
                       will have got it already */
                       return TRUE;

            case 'c' :  /* don't do anything, since pre-scan of args
                           will have got it already */
                       return TRUE;

            case 'D' : /* hack */
                {
                    extern void setRtsFlags( int x );
                    setRtsFlags(argToInt(s+1));
                    return TRUE;
                }

            default  : if (strcmp("98",s)==0) {
                           if (initDone && ((state && !haskell98) ||
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
        if (initDone && hpSize != heapSize) {
            /* ToDo: should this use a message box in winhugs? */
            FPrintf(stderr,"You cannot change heap size from inside Hugs\n");
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
    {'S', 1, "Debug: show generated SC code",         &debugSC},
    {'a', 1, "Raise exception on assert failure",     &flagAssert},
#if EXPLAIN_INSTANCE_RESOLUTION
    {'x', 1, "Explain instance resolution",           &showInstRes},
#endif
#if MULTI_INST
    {'m', 0, "Use multi instance resolution",         &multiInstRes},
#endif
    {0,   0, 0,                                       0}
};

static Void local set() {               /* change command line options from*/
    String s;                           /* Hugs command line               */

    if ((s=readFilename())!=0) {
        do {
            if (!processOption(s)) {
                ERRMSG(0) "Option string must begin with `+' or `-'"
                EEND_NO_LONGJMP;
            }
        } while ((s=readFilename())!=0);
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
        EEND_NO_LONGJMP;
    }
}


/* --------------------------------------------------------------------------
 * Interrupt handling
 * ------------------------------------------------------------------------*/

static jmp_buf catch_error;             /* jump buffer for error trapping  */

HugsBreakAction currentBreakAction = HugsIgnoreBreak;

static void handler_IgnoreBreak ( int sig )
{
   setHandler ( handler_IgnoreBreak );
}

static void handler_LongjmpOnBreak ( int sig )
{
   setHandler ( handler_LongjmpOnBreak );
   Printf("{Interrupted!}\n");
   longjmp(catch_error,1);
}

static void handler_RtsInterrupt ( int sig )
{
   setHandler ( handler_RtsInterrupt );
   interruptStgRts();
}

HugsBreakAction setBreakAction ( HugsBreakAction newAction )
{
   HugsBreakAction tmp = currentBreakAction;
   currentBreakAction = newAction;
   switch (newAction) {
      case HugsIgnoreBreak:
         setHandler ( handler_IgnoreBreak ); break;
      case HugsLongjmpOnBreak:
         setHandler ( handler_LongjmpOnBreak ); break;
      case HugsRtsInterrupt:
         setHandler ( handler_RtsInterrupt ); break;
      default:
         internal("setBreakAction");
   }
   return tmp;
}


/* --------------------------------------------------------------------------
 * The new module chaser, loader, etc
 * ------------------------------------------------------------------------*/

List    moduleGraph   = NIL;
List    prelModules   = NIL;
List    targetModules = NIL;

static String modeToString ( Cell mode )
{
   switch (mode) {
      case FM_SOURCE: return "source";
      case FM_OBJECT: return "object";
      case FM_EITHER: return "source or object";
      default: internal("modeToString");
   }
}

static Cell childMode ( Cell modeMeRequest, Cell modeMeActual )
{
   assert(modeMeActual == FM_SOURCE || 
          modeMeActual == FM_OBJECT);
   assert(modeMeRequest == FM_SOURCE || 
          modeMeRequest == FM_OBJECT ||
          modeMeRequest == FM_EITHER);
   if (modeMeRequest == FM_SOURCE) return modeMeRequest;
   if (modeMeRequest == FM_OBJECT) return modeMeRequest;
   if (modeMeActual == FM_OBJECT) return FM_OBJECT;
   if (modeMeActual == FM_SOURCE) return FM_EITHER;
   internal("childMode");
}

static Bool compatibleNewMode ( Cell modeNew, Cell modeExisting )
{
   if (modeNew == FM_OBJECT && modeExisting == FM_OBJECT) return TRUE;
   if (modeNew == FM_SOURCE && modeExisting == FM_SOURCE) return TRUE;
   if (modeNew == FM_EITHER && modeExisting == FM_OBJECT) return TRUE;
   if (modeNew == FM_EITHER && modeExisting == FM_SOURCE) return TRUE;
   return FALSE;
}

static void setCurrentFile ( Module mod )
{
   assert(isModule(mod));
   strncpy(currentFileName, textToStr(module(mod).text), 990);
   strcat(currentFileName, textToStr(module(mod).srcExt));
   currentFile       = currentFileName;
   moduleBeingParsed = mod;
}

static void clearCurrentFile ( void )
{
   currentFile       = NULL;
   moduleBeingParsed = NIL;
}

static void ppMG ( void )
{
   List t,u,v;
   for (t = moduleGraph; nonNull(t); t=tl(t)) {
      u = hd(t);
      switch (whatIs(u)) {
         case GRP_NONREC:
            FPrintf ( stderr, "  %s\n", textToStr(textOf(snd(u))));
            break;
         case GRP_REC:
            FPrintf ( stderr, "  {" );
            for (v = snd(u); nonNull(v); v=tl(v))
               FPrintf ( stderr, "%s ", textToStr(textOf(hd(v))) );
            FPrintf ( stderr, "}\n" );
            break;
         default:
            internal("ppMG");
      }
   }
}


static Bool elemMG ( ConId mod )
{
   List gs;
   for (gs = moduleGraph; nonNull(gs); gs=tl(gs))
     switch (whatIs(hd(gs))) {
        case GRP_NONREC: 
           if (textOf(mod)==textOf(snd(hd(gs)))) return TRUE;
           break;
        case GRP_REC: 
           if (varIsMember(textOf(mod),snd(hd(gs)))) return TRUE;
           break;
        default: 
           internal("elemMG");
     }
  return FALSE;
}


static ConId selectArbitrarilyFromGroup ( Cell group )
{
   switch (whatIs(group)) {
      case GRP_NONREC: return snd(group);
      case GRP_REC:    return hd(snd(group));
      default:         internal("selectArbitrarilyFromGroup");
   }
}

static ConId selectLatestMG ( void )
{
   List gs = moduleGraph;
   if (isNull(gs)) internal("selectLatestMG(1)");
   while (nonNull(gs) && nonNull(tl(gs))) gs = tl(gs);
   return selectArbitrarilyFromGroup(hd(gs));
}


static List /* of CONID */ listFromSpecifiedMG ( List mg )
{
   List gs;
   List cs = NIL;
   for (gs = mg; nonNull(gs); gs=tl(gs)) {
      switch (whatIs(hd(gs))) {
        case GRP_REC:    cs = appendOnto(cs,snd(hd(gs))); break;
        case GRP_NONREC: cs = cons(snd(hd(gs)),cs); break;
        default:         internal("listFromSpecifiedMG");
      }
   }
   return cs;
}

static List /* of CONID */ listFromMG ( void )
{
   return listFromSpecifiedMG ( moduleGraph );
}


/* Calculate the strongly connected components of modgList
   and assign them to moduleGraph.  Uses the .uses field of
   each of the modules to build the graph structure.
*/
#define  SCC             modScc          /* make scc algorithm for StgVars */
#define  LOWLINK         modLowlink
#define  DEPENDS(t)      snd(t)
#define  SETDEPENDS(c,v) snd(c)=v
#include "scc.c"
#undef   SETDEPENDS
#undef   DEPENDS
#undef   LOWLINK
#undef   SCC

static void mgFromList ( List /* of CONID */ modgList )
{
   List   t;
   List   u;
   Text   mT;
   List   usesT;
   List   adjList; /* :: [ (Text, [Text]) ] */
   Module mod;
   List   scc;
   Bool   isRec;

   adjList = NIL;
   for (t = modgList; nonNull(t); t=tl(t)) {
      mT = textOf(hd(t));
      mod = findModule(mT);
      assert(nonNull(mod));
      usesT = NIL;
      for (u = module(mod).uses; nonNull(u); u=tl(u))
         usesT = cons(textOf(hd(u)),usesT);

      /* artificially give all modules a dependency on Prelude */
      if (mT != textPrelude && mT != textPrelPrim)
         usesT = cons(textPrelude,usesT);
      adjList = cons(pair(mT,usesT),adjList);
   }

   /* adjList is now [ (module-text, [modules-which-i-import-text]) ].
      Modify this so that the adjacency list is a list of pointers
      back to bits of adjList -- that's what modScc needs.
   */
   for (t = adjList; nonNull(t); t=tl(t)) {
      List adj = NIL;
      /* for each elem of the adjacency list ... */
      for (u = snd(hd(t)); nonNull(u); u=tl(u)) {
         List v;
         Text a = hd(u);
         /* find the element of adjList whose fst is a */
         for (v = adjList; nonNull(v); v=tl(v)) {
            assert(isText(a));
            assert(isText(fst(hd(v))));
            if (fst(hd(v))==a) break;
         }
         if (isNull(v)) internal("mgFromList");
         adj = cons(hd(v),adj);
      }
      snd(hd(t)) = adj;
   }

   adjList = modScc ( adjList );
   /* adjList is now [ [(module-text, aux-info-field)] ] */

   moduleGraph = NIL;

   for (t = adjList; nonNull(t); t=tl(t)) {

      scc = hd(t);
      /* scc :: [ (module-text, aux-info-field) ] */
      for (u = scc; nonNull(u); u=tl(u))
         hd(u) = mkCon(fst(hd(u)));

      /* scc :: [CONID] */
      if (length(scc) > 1) {
         isRec = TRUE;
      } else {
         /* singleton module in scc; does it import itself? */
         mod = findModule ( textOf(hd(scc)) );
         assert(nonNull(mod));
         isRec = FALSE;
         for (u = module(mod).uses; nonNull(u); u=tl(u))
            if (textOf(hd(u))==textOf(hd(scc)))
               isRec = TRUE;
      }

      if (isRec)
         moduleGraph = cons( ap(GRP_REC,scc), moduleGraph ); else
         moduleGraph = cons( ap(GRP_NONREC,hd(scc)), moduleGraph );
   }
   moduleGraph = reverse(moduleGraph);
}


static List /* of CONID */ getModuleImports ( Cell tree )
{
   Cell  te;
   List  tes;
   ConId use;
   List  uses = NIL;
   for (tes = zthd3(unap(M_MODULE,tree)); nonNull(tes); tes=tl(tes)) {
      te = hd(tes);
      switch(whatIs(te)) {
         case M_IMPORT_Q:
            use = zfst(unap(M_IMPORT_Q,te));
            assert(isCon(use));
            if (!varIsMember(textOf(use),uses)) uses = cons ( use, uses );
            break;
         case M_IMPORT_UNQ:
            use = zfst(unap(M_IMPORT_UNQ,te));
            assert(isCon(use));
            if (!varIsMember(textOf(use),uses)) uses = cons ( use, uses );
            break;
         default:
            break;
      }
   }
   return uses;
}


static void processModule ( Module m )
{
   Cell  tree;
   ConId modNm;
   List  topEnts;
   List  tes;
   Cell  te;
   Cell  te2;

   tyconDefns     = NIL;
   typeInDefns    = NIL;
   valDefns       = NIL;
   classDefns     = NIL;
   instDefns      = NIL;
   selDefns       = NIL;
   genDefns       = NIL;
   unqualImports  = NIL;
   foreignImports = NIL;
   foreignExports = NIL;
   defaultDefns   = NIL;
   defaultLine    = 0;
   inputExpr      = NIL;

   setCurrentFile(m);
   startModule(m);
   tree = unap(M_MODULE,module(m).tree);
   modNm = zfst3(tree);

   if (textOf(modNm) != module(m).text) {
      ERRMSG(0) "Module name \"%s\" does not match file name \"%s%s\"",
                textToStr(textOf(modNm)), 
                textToStr(module(m).text),
                textToStr(module(m).srcExt)
      EEND;
   }

   setExportList(zsnd3(tree));
   topEnts = zthd3(tree);

   for (tes = topEnts; nonNull(tes); tes=tl(tes)) {
      te  = hd(tes);
      assert(isGenPair(te));
      te2 = snd(te);
      switch(whatIs(te)) {
         case M_IMPORT_Q: 
            addQualImport(zfst(te2),zsnd(te2));
            break;
         case M_IMPORT_UNQ:
            addUnqualImport(zfst(te2),zsnd(te2));
            break;
         case M_TYCON:
            tyconDefn(intOf(zsel14(te2)),zsel24(te2),zsel34(te2),zsel44(te2));
            break;
         case M_CLASS:
            classDefn(intOf(zsel14(te2)),zsel24(te2),zsel34(te2),zsel44(te2));
            break;
         case M_INST:
            instDefn(intOf(zfst3(te2)),zsnd3(te2),zthd3(te2));
            break;
         case M_DEFAULT:
            defaultDefn(intOf(zfst(te2)),zsnd(te2));
            break;
         case M_FOREIGN_IM:
            foreignImport(intOf(zsel15(te2)),zsel25(te2),zsel35(te2),
                          zsel45(te2),zsel55(te2));
            break;
         case M_FOREIGN_EX:
            foreignExport(intOf(zsel15(te2)),zsel25(te2),zsel35(te2),
                          zsel45(te2),zsel55(te2));
         case M_VALUE:
            valDefns = cons(te2,valDefns);
            break;
         default:
            internal("processModule");
      }
   }
   checkDefns(m);
   typeCheckDefns();
   compileDefns();
}


static Module parseModuleOrInterface ( ConId mc, Cell modeRequest )
{
   /* Allocate a module-table entry. */
   /* Parse the entity and fill in the .tree and .uses entries. */
   String path;
   String sExt;
   Bool sAvail;  Time sTime;  Long sSize;
   Bool oiAvail; Time oiTime; Long oSize; Long iSize;
   Bool ok;
   Bool useSource;
   char name[10000];

   Text   mt  = textOf(mc);
   Module mod = findModule ( mt );

   /* fprintf ( stderr, "parseModuleOrInterface `%s' == %d\n",
                textToStr(mt),mod); */
   if (nonNull(mod) && !module(mod).fake)
      internal("parseModuleOrInterface");
   if (nonNull(mod)) 
      module(mod).fake = FALSE;

   if (isNull(mod)) 
      mod = newModule(mt);

   /* This call malloc-ates path; we should deallocate it. */
   ok = findFilesForModule (
           textToStr(module(mod).text),
           &path,
           &sExt,
           &sAvail,  &sTime,  &sSize,
           &oiAvail, &oiTime, &oSize, &iSize
        );

   if (!ok) goto cant_find;
   if (!sAvail && !oiAvail) goto cant_find;

   /* Find out whether to use source or object. */
   switch (modeRequest) {
      case FM_SOURCE:
         if (!sAvail) goto cant_find;
         useSource = TRUE;
         break;
      case FM_OBJECT:
         if (!oiAvail) goto cant_find;
         useSource = FALSE;
         break;
      case FM_EITHER:
         if ( sAvail && !oiAvail) { useSource = TRUE; break; }
         if (!sAvail &&  oiAvail) { useSource = FALSE; break; }
         useSource = firstTimeIsLater ( sTime, oiTime ) ? TRUE : FALSE;
         break;
      default:
         internal("parseModuleOrInterface");
   }

   /* Actually do the parsing. */
   if (useSource) {
      module(mod).srcExt = findText(sExt);
      setCurrentFile(mod);
      strcpy(name, path);
      strcat(name, textToStr(mt));
      strcat(name, sExt);
      module(mod).tree      = parseModule(name,sSize);
      module(mod).uses      = getModuleImports(module(mod).tree);
      module(mod).mode      = FM_SOURCE;
      module(mod).lastStamp = sTime;
   } else {
      module(mod).srcExt = findText(HI_ENDING);
      setCurrentFile(mod);
      strcpy(name, path);
      strcat(name, textToStr(mt));
      strcat(name, DLL_ENDING);
      module(mod).objName = findText(name);
      module(mod).objSize = oSize;
      strcpy(name, path);
      strcat(name, textToStr(mt));
      strcat(name, ".u_hi");
      module(mod).tree      = parseInterface(name,iSize);
      module(mod).uses      = getInterfaceImports(module(mod).tree);
      module(mod).mode      = FM_OBJECT;
      module(mod).lastStamp = oiTime;
   }

   if (path) free(path);
   return mod;

  cant_find:
   if (path) free(path);
   clearCurrentFile();
   ERRMSG(0) 
      "Can't find %s for module \"%s\"",
      modeToString(modeRequest), textToStr(mt)
   EEND;
}


static void tryLoadGroup ( Cell grp )
{
   Module m;
   List   t;
   switch (whatIs(grp)) {
      case GRP_NONREC:
         m = findModule(textOf(snd(grp)));
         assert(nonNull(m));
         if (module(m).mode == FM_SOURCE) {
            processModule ( m );
            module(m).tree = NIL;
         } else {
            processInterfaces ( singleton(snd(grp)) );
            m = findModule(textOf(snd(grp)));
            assert(nonNull(m));
            module(m).tree = NIL;
         }
         break;
      case GRP_REC:
	 for (t = snd(grp); nonNull(t); t=tl(t)) {
            m = findModule(textOf(hd(t)));
            assert(nonNull(m));
            if (module(m).mode == FM_SOURCE) {
               ERRMSG(0) "Source module \"%s\" imports itself recursively",
                         textToStr(textOf(hd(t)))
               EEND;
            }
	 }
         processInterfaces ( snd(grp) );
	 for (t = snd(grp); nonNull(t); t=tl(t)) {
            m = findModule(textOf(hd(t)));
            assert(nonNull(m));
            module(m).tree = NIL;
         }
         break;
      default:
         internal("tryLoadGroup");
   }
}


static void fallBackToPrelModules ( void )
{
   Module m;
   for (m = MODULE_BASE_ADDR;
        m < MODULE_BASE_ADDR+tabModuleSz; m++)
      if (module(m).inUse
          && !varIsMember(module(m).text, prelModules))
         nukeModule(m);
}


/* This function catches exceptions in most of the system.
   So it's only ok for procedures called from this one
   to do EENDs (ie, write error messages).  Others should use
   EEND_NO_LONGJMP.
*/
static void achieveTargetModules ( Bool loadingThePrelude )
{
   volatile List   ood;
   volatile List   modgList;
   volatile List   t;
   volatile Module mod;
   volatile Bool   ok;

   String path = NULL;
   String sExt = NULL;
   Bool sAvail;  Time sTime;  Long sSize;
   Bool oiAvail; Time oiTime; Long oSize; Long iSize;

   volatile Time oisTime;
   volatile Bool out_of_date;
   volatile List ood_new;
   volatile List us;
   volatile List modgList_new;
   volatile List parsedButNotLoaded;
   volatile List toChase;
   volatile List trans_cl;
   volatile List trans_cl_new;
   volatile List u;
   volatile List mg;
   volatile List mg2;
   volatile Cell grp;
   volatile List badMods;

   setBreakAction ( HugsIgnoreBreak );

   /* First, examine timestamps to find out which modules are
      out of date with respect to the source/interface/object files.
   */
   ood      = NIL;
   modgList = listFromMG();

   for (t = modgList; nonNull(t); t=tl(t)) {

      if (varIsMember(textOf(hd(t)),prelModules))
         continue;

      mod = findModule(textOf(hd(t)));
      if (isNull(mod)) internal("achieveTargetSet(1)");
      
      /* In standalone mode, only succeeds for source modules. */
      ok = findFilesForModule (
              textToStr(module(mod).text),
              &path,
              &sExt,
              &sAvail,  &sTime,  &sSize,
              &oiAvail, &oiTime, &oSize, &iSize
           );

      if (!combined && !sAvail) ok = FALSE;
      if (!ok) {
         fallBackToPrelModules();
         ERRMSG(0) 
            "Can't find source or object+interface for module \"%s\"",
            textToStr(module(mod).text)
         EEND_NO_LONGJMP;
         if (path) free(path);
         return;
      }

      if (sAvail && oiAvail) {
         oisTime = whicheverIsLater(sTime,oiTime);
      } 
      else if (sAvail && !oiAvail) {
         oisTime = sTime;
      } 
      else if (!sAvail && oiAvail) {
         oisTime = oiTime;
      }
      else {
         internal("achieveTargetSet(2)");
      }

      out_of_date = firstTimeIsLater(oisTime,module(mod).lastStamp);
      if (out_of_date) {
         assert(!varIsMember(textOf(hd(t)),ood));
         ood = cons(hd(t),ood);
      }

      if (path) { free(path); path = NULL; };
   }

   /* Second, form a simplistic transitive closure of the out-of-date
      modules: a module is out of date if it imports an out-of-date
      module. 
   */
   while (1) {
      ood_new = NIL;
      for (t = modgList; nonNull(t); t=tl(t)) {
         mod = findModule(textOf(hd(t)));
         assert(nonNull(mod));
         for (us = module(mod).uses; nonNull(us); us=tl(us))
            if (varIsMember(textOf(hd(us)),ood))
               break;
         if (nonNull(us)) {
            if (varIsMember(textOf(hd(t)),prelModules))
               Printf ( "warning: prelude module \"%s\" is out-of-date\n",
                        textToStr(textOf(hd(t))) );
            else
               if (!varIsMember(textOf(hd(t)),ood_new) &&
                   !varIsMember(textOf(hd(t)),ood))
                  ood_new = cons(hd(t),ood_new);
         }
      }
      if (isNull(ood_new)) break;
      ood = appendOnto(ood_new,ood);            
   }

   /* Now ood holds the entire set of modules which are out-of-date.
      Throw them out of the system, yielding a "reduced system",
      in which the remaining modules are in-date.
   */
   for (t = ood; nonNull(t); t=tl(t)) {
      mod = findModule(textOf(hd(t)));
      assert(nonNull(mod));
      nukeModule(mod);      
   }
   modgList_new = NIL;
   for (t = modgList; nonNull(t); t=tl(t))
      if (!varIsMember(textOf(hd(t)),ood))
         modgList_new = cons(hd(t),modgList_new);
   modgList = modgList_new;

   /* Update the module group list to reflect the reduced system.
      We do this so that if the following parsing phases fail, we can 
      safely fall back to the reduced system.
   */
   mgFromList ( modgList );

   /* Parse modules/interfaces, collecting parse trees and chasing
      imports, starting from the target set. 
   */
   toChase = dupList(targetModules);
   for (t = toChase; nonNull(t); t=tl(t)) {
      Cell mode = (!combined) 
                  ? FM_SOURCE
                  : ( (loadingThePrelude && combined) 
                      ? FM_OBJECT
                      : FM_EITHER );
      hd(t) = zpair(hd(t), mode);
   } 

   /* toChase :: [ ((ConId, {FM_SOURCE|FM_OBJECT|FM_EITHER} )) ] */

   parsedButNotLoaded = NIL;

   
   while (nonNull(toChase)) {
      ConId mc   = zfst(hd(toChase));
      Cell  mode = zsnd(hd(toChase));
      toChase    = tl(toChase);
      if (varIsMember(textOf(mc),modgList)
          || varIsMember(textOf(mc),parsedButNotLoaded)) {
         /* either exists fully, or is at least parsed */
         mod = findModule(textOf(mc));
         assert(nonNull(mod));
         if (!compatibleNewMode(mode,module(mod).mode)) {
            clearCurrentFile();
            ERRMSG(0)
               "module %s: %s required, but %s is more recent",
               textToStr(textOf(mc)), modeToString(mode),
               modeToString(module(mod).mode)
            EEND_NO_LONGJMP;
            goto parseException;
         }
      } else {

         setBreakAction ( HugsLongjmpOnBreak );
         if (setjmp(catch_error)==0) {
            /* try this; it may throw an exception */
            mod = parseModuleOrInterface ( mc, mode );
         } else {
            /* here's the exception handler, if parsing fails */
            /* A parse error (or similar).  Clean up and abort. */
           parseException:
            setBreakAction ( HugsIgnoreBreak );
            mod = findModule(textOf(mc));
            if (nonNull(mod)) nukeModule(mod);
            for (t = parsedButNotLoaded; nonNull(t); t=tl(t)) {
               mod = findModule(textOf(hd(t)));
               assert(nonNull(mod));
               if (nonNull(mod)) nukeModule(mod);
            }
            return;
            /* end of the exception handler */
         }
         setBreakAction ( HugsIgnoreBreak );

         parsedButNotLoaded = cons(mc, parsedButNotLoaded);
         for (t = module(mod).uses; nonNull(t); t=tl(t))
            toChase = cons(
                        zpair( hd(t), childMode(mode,module(mod).mode) ),
                        toChase);
      }
   }

   modgList = dupOnto(parsedButNotLoaded, modgList);

   /* We successfully parsed all modules reachable from the target
      set which were not part of the reduced system.  However, there
      may be modules in the reduced system which are not reachable from
      the target set.  We detect these now by building the transitive
      closure of the target set, and nuking modules in the reduced
      system which are not part of that closure. 
   */
   trans_cl = dupList(targetModules);
   while (1) {
      trans_cl_new = NIL;
      for (t = trans_cl; nonNull(t); t=tl(t)) {
         mod = findModule(textOf(hd(t)));
         assert(nonNull(mod));
         for (u = module(mod).uses; nonNull(u); u=tl(u))
            if (!varIsMember(textOf(hd(u)),trans_cl)
                && !varIsMember(textOf(hd(u)),trans_cl_new)
                && !varIsMember(textOf(hd(u)),prelModules))
               trans_cl_new = cons(hd(u),trans_cl_new);
      }
      if (isNull(trans_cl_new)) break;
      trans_cl = appendOnto(trans_cl_new,trans_cl);
   }
   modgList_new = NIL;
   for (t = modgList; nonNull(t); t=tl(t)) {
      if (varIsMember(textOf(hd(t)),trans_cl)) {
         modgList_new = cons(hd(t),modgList_new);
      } else {
         mod = findModule(textOf(hd(t)));
         assert(nonNull(mod));
         nukeModule(mod);
      }
   }
   modgList = modgList_new;
   
   /* Now, the module symbol tables hold exactly the set of
      modules reachable from the target set, and modgList holds
      their names.   Calculate the scc-ified module graph, 
      since we need that to guide the next stage, that of
      Actually Loading the modules. 

      If no errors occur, moduleGraph will reflect the final graph
      loaded.  If an error occurs loading a group, we nuke 
      that group, truncate the moduleGraph just prior to that 
      group, and exit.  That leaves the system having successfully
      loaded all groups prior to the one which failed.
   */
   mgFromList ( modgList );

   for (mg = moduleGraph; nonNull(mg); mg=tl(mg)) {
      grp = hd(mg);
      
      if (!varIsMember(textOf(selectArbitrarilyFromGroup(grp)),
                       parsedButNotLoaded)) continue;

      setBreakAction ( HugsLongjmpOnBreak );
      if (setjmp(catch_error)==0) {
         /* try this; it may throw an exception */
         tryLoadGroup(grp);
      } else {
         /* here's the exception handler, if static/typecheck etc fails */
         /* nuke the entire rest (ie, the unloaded part)
            of the module graph */
         setBreakAction ( HugsIgnoreBreak );
         badMods = listFromSpecifiedMG ( mg );
         for (t = badMods; nonNull(t); t=tl(t)) {
            mod = findModule(textOf(hd(t)));
            if (nonNull(mod)) nukeModule(mod);
         }
         /* truncate the module graph just prior to this group. */
         mg2 = NIL;
         mg = moduleGraph;
         while (TRUE) {
            if (isNull(mg)) break;
            if (hd(mg) == grp) break;
            mg2 = cons ( hd(mg), mg2 );
            mg = tl(mg);
         }
         moduleGraph = reverse(mg2);
         return;
         /* end of the exception handler */
      }
      setBreakAction ( HugsIgnoreBreak );
   }

   /* Err .. I think that's it.  If we get here, we've successfully
      achieved the target set.  Phew!
   */
   setBreakAction ( HugsIgnoreBreak );
}


static Bool loadThePrelude ( void )
{
   Bool ok;
   ConId conPrelude;
   ConId conPrelHugs;
   moduleGraph = prelModules = NIL;

   if (combined) {
      conPrelude    = mkCon(findText("Prelude"));
      conPrelHugs   = mkCon(findText("PrelHugs"));
      targetModules = doubleton(conPrelude,conPrelHugs);
      achieveTargetModules(TRUE);
      ok = elemMG(conPrelude) && elemMG(conPrelHugs);
   } else {
      conPrelude    = mkCon(findText("Prelude"));
      targetModules = singleton(conPrelude);
      achieveTargetModules(TRUE);
      ok = elemMG(conPrelude);
   }

   if (ok) prelModules = listFromMG();
   return ok;
}


/* Refresh the current target modules, and attempt to set the
   current module to what it was before (ie currentModule):
     if currentModule_failed is different from currentModule,
        use that instead
     if nextCurrMod is non null, try to set it to that instead
     if the one we're after insn't available, select a target
       from the end of the module group list.
*/
static void refreshActions ( ConId nextCurrMod, Bool cleanAfter )
{
   List t;
   ConId tryFor; 

   /* Remember what the old current module was. */
   tryFor = mkCon(module(currentModule).text);

   /* Do the Real Work. */
   achieveTargetModules(FALSE);

   /* Remember if the current module was invalidated by this
      refresh, so later refreshes can attempt to reload it. */
   if (!elemMG(tryFor))
      currentModule_failed = tryFor;

   /* If a previous refresh failed to get an old current module, 
      try for that instead. */
   if (nonNull(currentModule_failed) 
       && textOf(currentModule_failed) != textOf(tryFor)
       && elemMG(currentModule_failed))
      tryFor = currentModule_failed;
   /* If our caller specified a new current module, that overrides
      all historical settings. */
   if (nonNull(nextCurrMod))
      tryFor = nextCurrMod;
   /* Finally, if we can't actually get hold of whatever it was we
      were after, select something which is possible. */
   if (!elemMG(tryFor))
      tryFor = selectLatestMG();

   /* combined mode kludge, to get Prelude rather than PrelHugs */
   if (combined && textOf(tryFor)==findText("PrelHugs"))
      tryFor = mkCon(findText("Prelude"));

   if (cleanAfter) {
      /* delete any targetModules which didn't actually get loaded  */
      t = targetModules;
      targetModules = NIL;
      for (; nonNull(t); t=tl(t))
         if (elemMG(hd(t)))
            targetModules = cons(hd(t),targetModules);
   }

   setCurrModule ( findModule(textOf(tryFor)) );
   Printf("Hugs session for:\n");
   ppMG();
}


static void addActions ( List extraModules /* :: [CONID] */ )
{
   List t;
   for (t = extraModules; nonNull(t); t=tl(t)) {
      ConId extra = hd(t);
      if (!varIsMember(textOf(extra),targetModules))
         targetModules = cons(extra,targetModules);
   }
   refreshActions ( isNull(extraModules) 
                       ? NIL 
                       : hd(reverse(extraModules)),
                    TRUE
                  );
}


static void loadActions ( List loadModules /* :: [CONID] */ )
{
   List t;
   targetModules = dupList ( prelModules );   

   for (t = loadModules; nonNull(t); t=tl(t)) {
      ConId load = hd(t);
      if (!varIsMember(textOf(load),targetModules))
         targetModules = cons(load,targetModules);
   }
   refreshActions ( isNull(loadModules) 
                       ? NIL 
                       : hd(reverse(loadModules)),
                    TRUE
                  );
}


/* --------------------------------------------------------------------------
 * Access to external editor:
 * ------------------------------------------------------------------------*/

/* ToDo: All this editor stuff needs fixing. */

static Void local editor() {            /* interpreter-editor interface    */
#if 0
    String newFile  = readFilename();
    if (newFile) {
        setLastEdit(newFile,0);
        if (readFilename()) {
            ERRMSG(0) "Multiple filenames not permitted"
            EEND;
        }
    }
    runEditor();
#endif
}

static Void local find() {              /* edit file containing definition */
#if 0
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
#if 0
    if (startEdit(lastEdLine,lastEdit)) /* at line lastEdLine              */
        readScripts(N_PRELUDE_SCRIPTS);
#endif
}

static Void local setLastEdit(fname,line)/* keep name of last file to edit */
String fname;
Int    line; {
#if 0
    if (lastEdit)
        free(lastEdit);
    lastEdit = strCopy(fname);
    lastEdLine = line;
#endif
}

/* --------------------------------------------------------------------------
 * Read and evaluate an expression:
 * ------------------------------------------------------------------------*/

static Void setModule ( void ) {
                              /*set module in which to evaluate expressions*/
   Module m;
   ConId  mc = NIL;
   String s  = readFilename();
   if (!s) {
      mc = selectLatestMG();
      if (combined && textOf(mc)==findText("PrelHugs"))
         mc = mkCon(findText("Prelude"));
      m = findModule(textOf(mc));
      assert(nonNull(m));
   } else {
      m = findModule(findText(s));
      if (isNull(m)) {
         ERRMSG(0) "Cannot find module \"%s\"", s
         EEND_NO_LONGJMP;
         return;
      }
   }
   setCurrModule(m);          
}

static Module allocEvalModule ( void )
{
   Module evalMod = newModule( findText("_Eval_Module_") );
   module(evalMod).names   = module(currentModule).names;
   module(evalMod).tycons  = module(currentModule).tycons;
   module(evalMod).classes = module(currentModule).classes;
   module(evalMod).qualImports 
     = singleton(pair(mkCon(textPrelude),modulePrelude));
   return evalMod;
}

static Void local evaluator() {        /* evaluate expr and print value    */
    volatile Type   type;
    volatile Type   bd;
    volatile Kinds  ks      = NIL;
    volatile Module evalMod = allocEvalModule();
    volatile Module currMod = currentModule;
    setCurrModule(evalMod);
    currentFile = NULL;

    defaultDefns = combined ? stdDefaults : evalDefaults;

    setBreakAction ( HugsLongjmpOnBreak );
    if (setjmp(catch_error)==0) {
       /* try this */
       parseExp();
       checkExp();
       type = typeCheckExp(TRUE);
    } else {
       /* if an exception happens, we arrive here */
       setBreakAction ( HugsIgnoreBreak );
       goto cleanup_and_return;
    }

    setBreakAction ( HugsIgnoreBreak );
    if (isPolyType(type)) {
        ks = polySigOf(type);
        bd = monotypeOf(type);
    }
    else
        bd = type;

    if (whatIs(bd)==QUAL) {
	printing = FALSE;
       clearCurrentFile();
       ERRMSG(0) "Unresolved overloading" ETHEN
       ERRTEXT   "\n*** Type       : "    ETHEN ERRTYPE(type);
       ERRTEXT   "\n*** Expression : "    ETHEN ERREXPR(inputExpr);
       ERRTEXT   "\n"
       EEND_NO_LONGJMP;
       goto cleanup_and_return;
    }
  
#if 1
    printing      = TRUE;
    numEnters     = 0;
    if (isProgType(ks,bd)) {
        inputExpr = ap(nameRunIO_toplevel,inputExpr);
        evalExp();
        Putchar('\n');
    } else {
        Cell d = provePred(ks,NIL,ap(classShow,bd));
        if (isNull(d)) {
	    clearCurrentFile();
	    printing = FALSE;
           ERRMSG(0) "Cannot find \"show\" function for:" ETHEN
           ERRTEXT   "\n*** expression : "   ETHEN ERREXPR(inputExpr);
           ERRTEXT   "\n*** of type    : "   ETHEN ERRTYPE(type);
           ERRTEXT   "\n"
           EEND_NO_LONGJMP;
           goto cleanup_and_return;
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

  cleanup_and_return:
   setBreakAction ( HugsIgnoreBreak );
   nukeModule(evalMod);
   setCurrModule(currMod);
   setCurrentFile(currMod);
   stopAnyPrinting();
}



/* --------------------------------------------------------------------------
 * Print type of input expression:
 * ------------------------------------------------------------------------*/

static Void showtype ( void ) {        /* print type of expression (if any)*/

    volatile Cell   type;
    volatile Module evalMod = allocEvalModule();
    volatile Module currMod = currentModule;
    setCurrModule(evalMod);

    if (setjmp(catch_error)==0) {
       /* try this */
       parseExp();
       checkExp();
       defaultDefns = evalDefaults;
       type = typeCheckExp(FALSE);
       printExp(stdout,inputExpr);
       Printf(" :: ");
       printType(stdout,type);
       Putchar('\n');
    } else {
       /* if an exception happens, we arrive here */
    }
 
    nukeModule(evalMod);
    setCurrModule(currMod);
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

    for (; (s=readFilename())!=0; count++)
	if (strcmp(s,"all") == 0) {
	    all = TRUE;
	    --count;
	} else
	    browseit(findModule(findText(s)),s,all);
    if (count == 0) {
	browseit(currentModule,NULL,all);
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
#if 0
   Whats this for?
   setCurrModule(findEvalModule());
   startNewScript(0);
#endif
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

    for (; (s=readFilename())!=0; count++) {
        describe(findText(s));
    }
    if (count == 0) {
       /* whatScripts(); */
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
    Module mod   = currentModule;

    if (pat) {                          /* First gather names to list      */
        do {
            names = addNamesMatching(pat,names);
        } while ((pat=readFilename())!=0);
    } else {
        names = addNamesMatching((String)0,names);
    }
    if (isNull(names)) {                /* Then print them out             */
        clearCurrentFile();
        ERRMSG(0) "No names selected"
        EEND_NO_LONGJMP;
        return;
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

static Void local interpreter(argc,argv)/* main interpreter loop           */
Int    argc;
String argv[]; {

    List   modConIds; /* :: [CONID] */
    Bool   prelOK;
    String s;

    setBreakAction ( HugsIgnoreBreak );
    modConIds = initialize(argc,argv);  /* the initial modules to load     */
    setBreakAction ( HugsIgnoreBreak );
    prelOK    = loadThePrelude();

    if (!prelOK) {
       if (autoMain)
          fprintf(stderr, "hugs +Q: fatal error: can't load the Prelude.\n" );
       else
          fprintf(stderr, "hugs: fatal error: can't load the Prelude.\n" );
       exit(1);
    }    

    if (combined) everybody(POSTPREL);
    loadActions(modConIds);

    if (autoMain) {
       for (; nonNull(modConIds); modConIds=tl(modConIds))
          if (!elemMG(hd(modConIds))) {
             fprintf(stderr,
                     "hugs +Q: compilation failed -- can't run `main'\n" );
             exit(1);
          }
    }

    modConIds = NIL;

    /* initialize calls startupHaskell, which trashes our signal handlers */
    setBreakAction ( HugsIgnoreBreak );
    forHelp();

    for (;;) {
        Command cmd;
        everybody(RESET);               /* reset to sensible initial state */

        promptForInput(textToStr(module(currentModule).text));

        cmd = readCommand(cmds, (Char)':', (Char)'!');
        switch (cmd) {
            case EDIT   : editor();
                          break;
            case FIND   : find();
                          break;
            case LOAD   : modConIds = NIL;
		while ((s=readFilename())!=0) {
                          modConIds = cons(mkCon(findText(s)),modConIds);

		}
                          loadActions(modConIds);
                          modConIds = NIL;
                          break;
            case ALSO   : modConIds = NIL;
                          while ((s=readFilename())!=0)
                             modConIds = cons(mkCon(findText(s)),modConIds);
                          addActions(modConIds);
                          modConIds = NIL;
                          break;
            case RELOAD : refreshActions(NIL,FALSE);
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

        if (autoMain) break;
    }
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

static Void local stopAnyPrinting() {  /* terminate printing of expression,*/
    if (printing) {                    /* after successful termination or  */
        printing = FALSE;              /* runtime error (e.g. interrupt)   */
        Putchar('\n');
        if (showStats) {
#define plural(v)   v, (v==1?"":"s")
	    Printf("(%lu enter%s)\n",plural(numEnters));
#undef plural
        }
        FlushStdout();
        garbageCollect();
    }
}

Cell errAssert(l)   /* message to use when raising asserts, etc */
Int l; {
  Cell str;
  if (currentFile) {
    str = mkStr(findText(currentFile));
  } else {
    str = mkStr(findText(""));
  }
  return (ap2(nameTangleMessage,str,mkInt(l)));
}

Void errHead(l)                        /* print start of error message     */
Int l; {
    failed();                          /* failed to reach target ...       */
    stopAnyPrinting();
    FPrintf(errorStream,"ERROR");

    if (currentFile) {
        FPrintf(errorStream," \"%s\"", currentFile);
        setLastEdit(currentFile,l);
        if (l) FPrintf(errorStream," (line %d)",l);
        currentFile = NULL;
    }
    FPrintf(errorStream,": ");
    FFlush(errorStream);
}

Void errFail() {                        /* terminate error message and     */
    Putc('\n',errorStream);             /* produce exception to return to  */
    FFlush(errorStream);                /* main command loop               */
    longjmp(catch_error,1);
}

Void errFail_no_longjmp() {             /* terminate error message but     */
    Putc('\n',errorStream);             /* don't produce an exception      */
    FFlush(errorStream);
}

Void errAbort() {                       /* altern. form of error handling  */
    failed();                           /* used when suitable error message*/
    stopAnyPrinting();                  /* has already been printed        */
    errFail();
}

Void internal(msg)                      /* handle internal error           */
String msg; {
    failed();
    stopAnyPrinting();
    Printf("INTERNAL ERROR: %s\n",msg);
    FlushStdout();
exit(9);
    longjmp(catch_error,1);
}

Void fatal(msg)                         /* handle fatal error              */
String msg; {
    FlushStdout();
    Printf("\nFATAL ERROR: %s\n",msg);
    everybody(EXIT);
    exit(1);
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

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

Void hugsEnableOutput(f) 
Bool f; {
    disableOutput = !f;
}

#ifdef HAVE_STDARG_H
Void hugsPrintf(const char *fmt, ...) {
    va_list ap;                    /* pointer into argument list           */
    va_start(ap, fmt);             /* make ap point to first arg after fmt */
    if (!disableOutput) {
        vprintf(fmt, ap);
    } else {
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
    }
    va_end(ap);                    /* clean up                             */
}
#endif

Void hugsPutchar(c)
int c; {
    if (!disableOutput) {
        putchar(c);
    } else {
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
    }
}

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

    if (what == MARK) {
       mark(moduleGraph);
       mark(prelModules);
       mark(targetModules);
       mark(daSccs);
       mark(currentModule_failed);
    }
}

/*-------------------------------------------------------------------------*/

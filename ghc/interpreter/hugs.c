/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Command interpreter
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: hugs.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:09 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "version.h"
#include "storage.h"
#include "command.h"
#include "connect.h"
#include "charset.h"
#include "input.h"
#include "type.h"
#include "subst.h"  /* for typeMatches                        */
#include "link.h"   /* for classShow, nameRunIO and namePrint */
#include "static.h"
#include "compiler.h"
#include "interface.h"
#include "hugs.h"
#include "errors.h"
#include <setjmp.h>
#include <ctype.h>

#include <stdio.h>

#include "machdep.h"

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
static Void   local runEditor         Args((Void));
static Void   local setModule         Args((Void));
static Module local findEvalModule    Args((Void));
static Void   local evaluator         Args((Void));
static Void   local showtype          Args((Void));
static Void   local info              Args((Void));
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
static Void   local addScriptName     Args((String,Bool));
static Bool   local addScript         Args((String,Long));
static Void   local forgetScriptsFrom Args((Script));
static Void   local setLastEdit       Args((String,Int));
static Void   local failed            Args((Void));
static String local strCopy           Args((String));

/* --------------------------------------------------------------------------
 * Machine dependent code for Hugs interpreter:
 * ------------------------------------------------------------------------*/

#ifdef WANT_TIMER
#include "timer.c"
#endif

/* --------------------------------------------------------------------------
 * Local data areas:
 * ------------------------------------------------------------------------*/

static Bool   listScripts  = TRUE;      /* TRUE => list scripts after loading*/
static Bool   addType	   = FALSE; 	/* TRUE => print type with value   */
static Bool   chaseImports = TRUE;      /* TRUE => chase imports on load   */
static Bool   useDots      = RISCOS;    /* TRUE => use dots in progress    */
static Bool   quiet        = FALSE;     /* TRUE => don't show progress     */

static String scriptName[NUM_SCRIPTS];  /* Script file names               */
static Time   lastChange[NUM_SCRIPTS];  /* Time of last change to script   */
static Bool   postponed[NUM_SCRIPTS];   /* Indicates postponed load        */
static Int    scriptBase;               /* Number of scripts in Prelude    */
static Int    numScripts;               /* Number of scripts loaded        */
static Int    namesUpto;                /* Number of script names set      */
static Bool   needsImports;             /* set to TRUE if imports required */
       String scriptFile;               /* Name of current script (if any) */

static Text   evalModule  = 0;          /* Name of module we eval exprs in */
static String currProject = 0;          /* Name of current project file    */
static Bool   projectLoaded = FALSE;    /* TRUE => project file loaded     */

static String lastEdit   = 0;           /* Name of script to edit (if any) */
static Int    lastLine   = 0;           /* Editor line number (if possible)*/
static String prompt     = 0;           /* Prompt string                   */
static Int    hpSize     = DEFAULTHEAP; /* Desired heap size               */
String hugsEdit = 0;                    /* String for editor command       */
String hugsPath = 0;                    /* String for file search path     */

#if REDIRECT_OUTPUT
static Bool disableOutput = FALSE;      /* redirect output to buffer?      */
#endif

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

    /* The startup banner now includes my name.  Hugs is provided free of  */
    /* charge.  I ask however that you show your appreciation for the many */
    /* hours of work involved by retaining my name in the banner.  Thanks! */

#if SMALL_BANNER
    Printf("Hugs 1.4, %s release.\n", HUGS_VERSION);
    Printf("Copyright (c) The University of Nottingham and Yale University, 1994-1998.\n");
    Printf("Home page: http://haskell.org/hugs.  Bug reports: hugs-bugs@haskell.org.\n");
#else
#ifdef OLD_LOGO
    Printf("      ___    ___   ___    ___   __________   __________                        \n");
    Printf("     /  /   /  /  /  /   /  /  /  _______/  /  _______/         Hugs 1.4       \n");
    Printf("    /  /___/  /  /  /   /  /  /  / _____   /  /______                          \n"); 
    Printf("   /  ____   /  /  /   /  /  /  / /_   /  /______   /  The Nottingham and Yale\n");
    Printf("  /  /   /  /  /  /___/  /  /  /___/  /  _______/  /    Haskell User's System \n");     
    Printf(" /__/   /__/  /_________/  /_________/  /_________/         %s\n\n", HUGS_VERSION);
    Printf("Copyright (c) The University of Nottingham and Yale University, 1994-1998.\n");
    Printf("Home page: http://haskell.org/hugs.  Bug reports: hugs-bugs@haskell.org.\n");
#else
    /* There is now a new banner, designed to draw attention to the fact   */
    /* that the version of Hugs being used is substantially different from */
    /* previous releases (and to correct the mistaken view that Hugs is    */
    /* written in capitals).  If you really prefer the old style banner,   */
    /* you can still get it by compiling with -DOLD_LOGO.                  */

    printf("  __   __ __  __  ____   ___     __________________________________________\n");
    printf("  ||   || ||  || ||  || ||__     Hugs 1.4: The Haskell User's Gofer System\n");
    printf("  ||___|| ||__|| ||__||  __||    (c) The University of Nottingham\n");
    printf("  ||---||         ___||              and Yale University, 1994-1998.\n");
    printf("  ||   ||                        Report bugs to hugs-bugs@haskell.org\n");
    printf("  ||   ||     "HUGS_VERSION"      __________________________________________\n\n");
#endif
#endif
#if SYMANTEC_C
    Printf("   Ported to Macintosh by Hans Aberg, compiled " __DATE__ ".\n\n");
#endif
    FlushStdout();
    interpreter(argc,argv);
    Printf("[Leaving Hugs]\n");
    everybody(EXIT);
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
    String proj = 0;

    setLastEdit((String)0,0);
    lastEdit      = 0;
    scriptFile    = 0;
    numScripts    = 0;
    namesUpto     = 1;
    initCharTab();

#if HUGS_FOR_WINDOWS
    hugsEdit      = strCopy(fromEnv("EDITOR","c:\\windows\notepad.exe"));
#elif SYMANTEC_C
    hugsEdit      = "";
#else
    hugsEdit      = strCopy(fromEnv("EDITOR",NULL));
#endif
    hugsPath      = strCopy(HUGSPATH);
    readOptions("-p\"%s> \" -r$$");
#if USE_REGISTRY
    readOptions(readRegString("Options",""));
#endif
    readOptions(fromEnv("HUGSFLAGS",""));

    for (i=1; i<argc; ++i) {            /* process command line arguments  */
        if (strcmp(argv[i],"+")==0 && i+1<argc) {
            if (proj) {
                ERRMSG(0) "Multiple project filenames on command line"
                EEND;
            } else {
                proj = argv[++i];
            }
        } else if (!processOption(argv[i])) {
            addScriptName(argv[i],TRUE);
        }
    }
    /* ToDo: clean up this hack */
    { 
        static char* my_argv[] = {"Hugs"};
        startupHaskell(sizeof(my_argv)/sizeof(char*),my_argv);
    }
#ifdef DEBUG
    DEBUG_LoadSymbols(argv[0]);
#endif

    scriptName[0] = strCopy(findMPathname(NULL,STD_PRELUDE));
    if (!scriptName[0]) {
        Printf("Prelude not found on current path: \"%s\"\n",
               hugsPath ? hugsPath : "");
        fatal("Unable to load prelude");
    }

    everybody(INSTALL);
    evalModule = findText("");      /* evaluate wrt last module by default */
    if (proj) {
        if (namesUpto>1) {
            fprintf(stderr,
                    "\nUsing project file, ignoring additional filenames\n");
        }
        loadProject(strCopy(proj));
    }
    readScripts(0);
    scriptBase = numScripts;
}

/* --------------------------------------------------------------------------
 * Command line options:
 * ------------------------------------------------------------------------*/

struct options {                        /* command line option toggles     */
    char   c;                           /* table defined in main app.      */
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
        if (*toggle[i].flag == state) {
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
    for (i=0; toggle[i].c; ++i)
        Printf(fmtc,toggle[i].c,toggle[i].description);

    Printf("\nOTHER OPTIONS: (leading + or - makes no difference)\n");
    Printf(fmts,"hnum","Set heap size (cannot be changed within Hugs)");
    Printf(fmts,"pstr","Set prompt string to str");
    Printf(fmts,"rstr","Set repeat last expression string to str");
    Printf(fmts,"Pstr","Set search path for modules to str");
    Printf(fmts,"Estr","Use editor setting given by str");
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
    Printf("\nSearch path     : -P");
    printString(hugsPath);
    Printf("\nEditor setting  : -E");
    printString(hugsEdit);
#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
    Printf("\nPreprocessor    : -F");
    printString(preprocessor);
#endif
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
    PUTInt('h',hpSize);  PUTC(' ');
    PUTStr('p',prompt);
    PUTStr('r',repeatStr);
    PUTStr('P',hugsPath);
    PUTStr('E',hugsEdit);
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

            case 'd' : /* hack */
                {
                    extern void setRtsFlags( int x );
                    setRtsFlags(argToInt(s+1));
                    return TRUE;
                }

            default  : toggleSet(*s,state);
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
            FPrintf(stderr,"Cannot change heap size\n");
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

    if (*s=='\0' || !isascii(*s) || !isdigit(*s)) {
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
    } while (isascii(*s) && isdigit(*s));

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
 {":module", SETMODULE}, 
 {":version", SHOWVERSION}, 
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
    Printf(":version            show Hugs version\n");
    Printf(":?                  display this list of commands\n");
    Printf(":set <options>      set command line options\n");
    Printf(":set                help on command line options\n");
    Printf(":names [pat]        list names currently in scope\n");
    Printf(":info <names>       describe named objects\n");
    Printf(":find <name>        edit module containing definition of name\n");
    Printf(":!command           shell escape\n");
    Printf(":cd dir             change directory\n");
    Printf(":gc                 force garbage collection\n");
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
    {'t', "Print type after evaluation",	   &addType},
    {'g', "Print no. cells recovered after gc",    &gcMessages},
    {'l', "Literate modules as default",           &literateScripts},
    {'e', "Warn about errors in literate modules", &literateErrors},
    {'.', "Print dots to show progress",           &useDots},
    {'q', "Print nothing to show progress",        &quiet},
    {'w', "Always show which modules are loaded",  &listScripts},
    {'k', "Show kind errors in full",              &kindExpert},
    {'o', "Allow overlapping instances",           &allowOverlap},
    {'i', "Chase imports while loading modules",   &chaseImports},
#if DEBUG_CODE
    {'D', "Debug: show generated code",            &debugCode},
#endif
    {0,   0,                                       0}
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
    forgetScriptsFrom(scriptBase);
    while ((s=readFilename())!=0)
        addScriptName(s,TRUE);
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

static Void local addScriptName(s,sch)  /* Add script to list of scripts   */
String s;                               /* to be read in ...               */
Bool   sch; {                           /* TRUE => requires pathname search*/
    if (namesUpto>=NUM_SCRIPTS) {
        ERRMSG(0) "Too many module files (maximum of %d allowed)",
                  NUM_SCRIPTS
        EEND;
    }
    else
        scriptName[namesUpto++] = strCopy(sch ? findPathname(NULL,s) : s);
}

static Bool local addScript(fname,len)  /* read single script file         */
String fname;                           /* name of script file             */
Long   len; {                           /* length of script file           */
    scriptFile = fname;

#if HUGS_FOR_WINDOWS                    /* Set clock cursor while loading  */
    allowBreak();
    SetCursor(LoadCursor(NULL, IDC_WAIT));
#endif

    Printf("Reading file \"%s\":\n",fname);
    setLastEdit(fname,0);

    if (isInterfaceFile(fname)) {
        loadInterface(fname);
    } else {
        needsImports = FALSE;
        parseScript(fname,len);         /* process script file             */
        if (needsImports)
            return FALSE;
        checkDefns();
        typeCheckDefns();
        compileDefns();
    }
    scriptFile = 0;
    return TRUE;
}

Bool chase(imps)                        /* Process list of import requests */
List imps; {
    if (chaseImports) {
        Int    origPos  = numScripts;   /* keep track of original position */
        String origName = scriptName[origPos];
        for (; nonNull(imps); imps=tl(imps)) {
            String iname = findPathname(origName,textToStr(textOf(hd(imps))));
            Int    i     = 0;
            for (; i<namesUpto; i++)
                if (pathCmp(scriptName[i],iname)==0)
                    break;
            if (i>=origPos) {           /* Neither loaded or queued        */
                String theName;
                Time   theTime;
                Bool   thePost;

                postponed[origPos] = TRUE;
                needsImports       = TRUE;

                if (i>=namesUpto)       /* Name not found (i==namesUpto)   */
                    addScriptName(iname,FALSE);
                else if (postponed[i]) {/* Check for recursive dependency  */
                    ERRMSG(0)
                      "Recursive import dependency between \"%s\" and \"%s\"",
                      scriptName[origPos], iname
                    EEND;
                }
                /* Right rotate section of tables between numScripts and i so
                 * that i ends up with other imports in front of orig. script
                 */
                theName = scriptName[i];
                thePost = postponed[i];
                timeSet(theTime,lastChange[i]);
                for (; i>numScripts; i--) {
                    scriptName[i] = scriptName[i-1];
                    postponed[i]  = postponed[i-1];
                    timeSet(lastChange[i],lastChange[i-1]);
                }
                scriptName[numScripts] = theName;
                postponed[numScripts]  = thePost;
                timeSet(lastChange[numScripts],theTime);
                origPos++;
            }
        }
        return needsImports;
    }
    return FALSE;
}

static Void local forgetScriptsFrom(scno)/* remove scripts from system     */
Script scno; {
    Script i;
    for (i=scno; i<namesUpto; ++i)
        if (scriptName[i])
            free(scriptName[i]);
    dropScriptsFrom(scno);
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
        addScriptName(s,TRUE);
    readScripts(scriptBase);
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
    readScripts(scriptBase);
}

static Void local readScripts(n)        /* Reread current list of scripts, */
Int n; {                                /* loading everything after and    */
    Time timeStamp;                     /* including the first script which*/
    Long fileSize;                      /* has been either changed or added*/

#if HUGS_FOR_WINDOWS
    SetCursor(LoadCursor(NULL, IDC_WAIT));
#endif

    for (; n<numScripts; n++) {         /* Scan previously loaded scripts  */
        getFileInfo(scriptName[n], &timeStamp, &fileSize);
        if (timeChanged(timeStamp,lastChange[n])) {
            dropScriptsFrom(n);
            numScripts = n;
            break;
        }
    }
    for (; n<NUM_SCRIPTS; n++)          /* No scripts have been postponed  */
        postponed[n] = FALSE;           /* at this stage                   */

    while (numScripts<namesUpto) {      /* Process any remaining scripts   */
        getFileInfo(scriptName[numScripts], &timeStamp, &fileSize);
        timeSet(lastChange[numScripts],timeStamp);
        startNewScript(scriptName[numScripts]);
        if (addScript(scriptName[numScripts],fileSize))
            numScripts++;
        else
            dropScriptsFrom(numScripts);
    }

    if (listScripts)
        whatScripts();
    if (numScripts<=scriptBase)
        setLastEdit((String)0, 0);
}

static Void local whatScripts() {       /* list scripts in current session */
    int i;
    Printf("\nHugs session for:");
    if (projectLoaded)
        Printf(" (project: %s)",currProject);
    for (i=0; i<numScripts; ++i)
        Printf("\n%s",scriptName[i]);
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
                readScripts(scriptBase);
            }
        } else if (nonNull(c=findName(t))) {
            if (startEdit(name(c).line,scriptName[scriptThisName(c)])) {
                readScripts(scriptBase);
            }
        } else {
            ERRMSG(0) "No current definition for name \"%s\"", nm
            EEND;
        }
    }
}

static Void local runEditor() {         /* run editor on script lastEdit   */
    if (startEdit(lastLine,lastEdit))   /* at line lastLine                */
        readScripts(scriptBase);
}

static Void local setLastEdit(fname,line)/* keep name of last file to edit */
String fname;
Int    line; {
    if (lastEdit)
        free(lastEdit);
    lastEdit = strCopy(fname);
    lastLine = line;
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
    if (isNull(m)) {
        m = lastModule();
    }
    return m;
}

static Void local evaluator() {        /* evaluate expr and print value    */
    Type  type, bd;
    Kinds ks = NIL;

    setCurrModule(findEvalModule());
    scriptFile = 0;
    startNewScript(0);                 /* Enables recovery of storage      */
                                       /* allocated during evaluation      */
    parseExp();
    checkExp();
    defaultDefns = evalDefaults;
    type         = typeCheckExp(TRUE);
    if (isPolyType(type)) {
        ks = polySigOf(type);
        bd = monotypeOf(type);
    }
    else
        bd = type;

    if (whatIs(bd)==QUAL) {
        ERRMSG(0) "Unresolved overloading" ETHEN
        ERRTEXT   "\n*** type       : "    ETHEN ERRTYPE(type);
        ERRTEXT   "\n*** expression : "    ETHEN ERREXPR(inputExpr);
        ERRTEXT   "\n"
        EEND;
    }
    
    /* ToDo: restore the code to print types, use show, etc */

#ifdef WANT_TIMER
    updateTimers();
#endif
    if (typeMatches(type,ap(typeIO,typeUnit))) {
        inputExpr = ap(nameRunIO,inputExpr);
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
        inputExpr = ap2(namePrint,d,inputExpr);
        inputExpr = ap(nameRunIO,inputExpr);
        evalExp();
        if (addType) {
            printf(" :: ");
            printType(stdout,type);
            Putchar('\n');
        }
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

/* --------------------------------------------------------------------------
 * Enhanced help system:  print current list of scripts or give information
 * about an object.
 * ------------------------------------------------------------------------*/

static String local objToStr Args((Module, Cell));

static String local objToStr(m,c)
Module m;
Cell   c; {
#if DISPLAY_QUANTIFIERS
    static char newVar[60];
    switch (whatIs(c)) {
    case NAME  : if (m == name(c).mod) {
                     sprintf(newVar,"%s",   textToStr(name(c).text));
                 } else {
                     sprintf(newVar,"%s.%s",textToStr(module(name(c).mod).text),
                                            textToStr(name(c).text));
                 }
                 break;
    case TYCON : if (m == tycon(c).mod) {
                     sprintf(newVar,"%s",   textToStr(tycon(c).text));
                 } else {
                     sprintf(newVar,"%s.%s",textToStr(module(tycon(c).mod).text),
                                            textToStr(tycon(c).text));
                 }
                 break;
    case CLASS : if (m == cclass(c).mod) {
                     sprintf(newVar,"%s",   textToStr(cclass(c).text));
                 } else {
                     sprintf(newVar,"%s.%s",textToStr(module(cclass(c).mod).text),
                                            textToStr(cclass(c).text));
                 }
                 break;
    default    : internal("objToStr");
    }
    return newVar;
#else
    static char newVar[33];
    switch (whatIs(c)) {
    case NAME  : sprintf(newVar,"%s",   textToStr(name(c).text));
                 break;
    case TYCON : sprintf(newVar,"%s",   textToStr(tycon(c).text));
                 break;
    case CLASS : sprintf(newVar,"%s",   textToStr(cclass(c).text));
    default    : internal("objToStr");
    }
    return newVar;
#endif
}

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
    Tycon tc = findTycon(t);
    Class cl = findClass(t);
    Name  nm = findName(t);
    Module mod = findEvalModule();

    if (nonNull(tc)) {                  /* as a type constructor           */
        Type ty = tc;
        Int  i;
        Inst in;
        for (i=0; i<tycon(tc).arity; ++i) {
            ty = ap(ty,mkOffset(i));
        }
        Printf("-- type constructor");
        if (kindExpert) {
            Printf(" with kind ");
            printKind(stdout,tycon(tc).kind);
        }
        Putchar('\n');
        switch (tycon(tc).what) {
            case SYNONYM      : Printf("type ");
                                printType(stdout,ty);
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
                                    printType(stdout,ty);
                                    if (hasCfun(cs)) {
                                        Printf("\n\n-- constructors:");
                                    }
                                    for (; hasCfun(cs); cs=tl(cs)) {
                                        Putchar('\n');
                                        printExp(stdout,hd(cs));
                                        Printf(" :: ");
                                        printType(stdout,name(hd(cs)).type);
                                    }
                                    if (nonNull(cs)) {
                                        Printf("\n\n-- selectors:");
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
                                printType(stdout,ty);
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
            printf("-- type class");
        } else {
            printf("-- constructor class");
            if (kindExpert) {
                printf(" with arity ");
                printKinds(stdout,ks);
            }
        }
        printf("\nclass ");
        if (nonNull(cclass(cl).supers)) {
            printContext(stdout,cclass(cl).supers);
            printf(" => ");
        }
        printPred(stdout,cclass(cl).head);
        if (nonNull(cclass(cl).members)) {
            List ms = cclass(cl).members;
            printf(" where");
            do {
                Type t = monotypeOf(name(hd(ms)).type);
                printf("\n  ");
                printExp(stdout,hd(ms));
                printf(" :: ");
                if (isNull(tl(fst(snd(t))))) {
                    t = snd(snd(t));
                } else {
                    t = ap(QUAL,pair(tl(fst(snd(t))),snd(snd(t))));
                }
                printType(stdout,t);
                ms = tl(ms);
            } while (nonNull(ms));
        }
        putchar('\n');
        if (nonNull(ins)) {
            printf("\n-- instances:\n");
            do {
                showInst(hd(ins));
                ins = tl(ins);
            } while (nonNull(ins));
        }
        putchar('\n');
    }

    if (nonNull(nm)) {                  /* as a function/name              */
        printExp(stdout,nm);
        printf(" :: ");
        if (nonNull(name(nm).type)) {
            printType(stdout,name(nm).type);
        } else {
            printf("<unknown type>");
        }

        if (isCfun(nm)) {
            printf("  -- data constructor");
        } else if (isMfun(nm)) {
            printf("  -- class member");
        } else if (isSfun(nm)) {
            printf("  -- selector function");
        }
        if (name(nm).primop) {
            printf("   -- primitive");
        }
        printf("\n\n");
    }

    if (isNull(tc) && isNull(cl) && isNull(nm)) {
        Printf("Unknown reference `%s'\n",textToStr(t));
    }
}

static Void local showInst(in)          /* Display instance decl header    */
Inst in; {
    printf("instance ");
    if (nonNull(inst(in).specifics)) {
        printContext(stdout,inst(in).specifics);
        printf(" => ");
    }
    printPred(stdout,inst(in).head);
    putchar('\n');
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

    breakOn(TRUE);                      /* enable break trapping           */
    if (numScripts==0) {                /* only succeeds on first time,    */
        if (errorNumber)                /* before prelude has been loaded  */
            fatal("Unable to load prelude");
        initialize(argc,argv);
        forHelp();
    }

    for (;;) {
        Command cmd;
        everybody(RESET);               /* reset to sensible initial state */
        dropScriptsFrom(numScripts);    /* remove partially loaded scripts */

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
                          forgetScriptsFrom(scriptBase);
                          load();
                          break;
            case ALSO   : clearProject();
                          forgetScriptsFrom(numScripts);
                          load();
                          break;
            case RELOAD : readScripts(scriptBase);
                          break;
            case PROJECT: project();
                          break;
            case SETMODULE :
                          setModule();
                          break;
            case SHOWVERSION :
                          Printf("Hugs 1.4, %s release.\n", HUGS_VERSION);
                          break;
            case EVAL   : evaluator();
                          break;
            case TYPEOF : showtype();
                          break;
            case NAMES  : listNames();
                          break;
            case HELP   : menu();
                          break;
            case BADCMD : guidance();
                          break;
            case SET    : set();
                          break;
            case SYSTEM : if (shellEsc(readLine())) 
                              Printf("Warning: Shell escape terminated abnormally\n");
                          break;
            case CHGDIR : changeDir();
                          break;
            case INFO   : info();
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
    if (quiet) return;
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
    if (quiet) return;
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
    if (quiet) return;
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
    errFail();                          /* has already been printed        */
}

Void internal(msg)                      /* handle internal error           */
String msg; {
#if HUGS_FOR_WINDOWS
    char buf[300];
    wsprintf(buf,"INTERNAL ERROR: %s",msg);
    MessageBox(hWndMain, buf, appName, MB_ICONHAND | MB_OK);
#endif
    failed();
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
    breakOn(TRUE);
    everybody(BREAK);
    failed();
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

#define BufferSize 5000               /* size of redirected output buffer  */

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

static HugsStream outputStream;
/* ADR note: 
 * We rely on standard C semantics to initialise outputStream.next to 0.
 */

Void hugsEnableOutput(f) 
Bool f; {
    disableOutput = !f;
}

String hugsClearOutputBuffer() {
    return bufferClear(&outputStream);
}

#ifdef HAVE_STDARG_H
Void hugsPrintf(const char *fmt, ...) {
    va_list ap;                    /* pointer into argument list           */
    va_start(ap, fmt);             /* make ap point to first arg after fmt */
    if (!disableOutput) {
        vprintf(fmt, ap);
    } else {
        vBufferedPrintf(&outputStream, fmt, ap);
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
        vBufferedPrintf(&outputStream, fmt, ap);
    }
    va_end(ap);                    /* clean up                             */
}
#endif

Void hugsPutchar(c)
int c; {
    if (!disableOutput) {
        putchar(c);
    } else {
        bufferedPutchar(&outputStream, c);
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
        vBufferedPrintf(&outputStream, fmt, ap);
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
        vBufferedPrintf(&outputStream, fmt, ap);
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
        bufferedPutchar(&outputStream, c);
    }
}
    
#endif /* REDIRECT_OUTPUT && !HUGS_FOR_WINDOWS */

/* --------------------------------------------------------------------------
 * Hugs for Windows code (WinMain and related functions)
 * ------------------------------------------------------------------------*/

#if HUGS_FOR_WINDOWS
#include "winhugs.c"
#endif

/*-------------------------------------------------------------------------*/


extern String repeatStr;                /* Repeat last command string      */

extern List  tyconDefns;                /* list of type constructor defns  */
extern List  typeInDefns;               /* list of synonym restrictions    */
extern List  valDefns;                  /* list of value definitions       */
extern List  opDefns;                   /* list of operator definitions    */
extern List  classDefns;                /* list of class definitions       */
extern List  instDefns;                 /* list of instance definitions    */
extern List  selDefns;                  /* list of selector lists          */
extern List  genDefns;                  /* list of generated defns         */
extern List  foreignImports;            /* foreign import declarations     */
extern List  foreignExports;            /* foreign export declarations     */
extern List  defaultDefns;              /* default definitions (if any)    */
extern Int   defaultLine;               /* line in which default defs occur*/
extern List  evalDefaults;              /* defaults for evaluator          */
extern Cell  inputExpr;                 /* evaluator input expression      */

extern Bool  literateScripts;           /* TRUE => default lit scripts     */
extern Bool  literateErrors;            /* TRUE => report errs in lit scrs */
                                        /*         termination             */
#if USE_PREPROCESSOR
extern String preprocessor;             /* preprocessor command            */
#endif

extern Cell  conPrelude;                /* Prelude                         */
#if    NPLUSK
extern Text  textPlus;                  /* Used to recognise n+k patterns  */
#endif

extern  String unlexChar        Args((Char,Char));
extern  Void   printString      Args((String));

extern  Void   consoleInput     Args((String));
extern  Void   projInput        Args((String));
extern  Void   stringInput      Args((String));
extern  Void   parseScript      Args((String,Long));
extern  Void   parseInterface   Args((String,Long));
extern  Void   parseExp         Args((Void));
extern  String readFilename     Args((Void));
extern  String readLine         Args((Void));

extern  Bool   isInterfaceFile  Args((String));

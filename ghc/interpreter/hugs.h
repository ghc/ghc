typedef long   Target;
extern  Void   setGoal          Args((String, Target));
extern  Void   soFar            Args((Target));
extern  Void   done             Args((Void));

extern  String fromEnv          Args((String,String));
extern  Bool   chase            Args((List));


extern String hugsEdit;                 /* String for editor command       */
extern String hugsPath;                 /* String for file search path     */

extern Cell  *CStackBase;               /* pointer to base of C stack      */



extern Bool  gcMessages;                /* TRUE => print GC messages       */
#if DEBUG_CODE
extern Bool  debugCode;                 /* TRUE => print G-code to screen  */
#endif
extern Bool  kindExpert;                /* TRUE => display kind errors in  */
                                        /*         full detail             */
extern Bool  allowOverlap;              /* TRUE => allow overlapping insts */


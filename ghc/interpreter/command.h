/* --------------------------------------------------------------------------
 * Interpreter command structure
 *
 * Hugs 98 is Copyright (c) Mark P Jones, Alastair Reid and the Yale
 * Haskell Group 1994-99, and is distributed as Open Source software
 * under the Artistic License; see the file "Artistic" that is included
 * in the distribution for details.
 *
 * $RCSfile: command.h,v $
 * $Revision: 1.4 $
 * $Date: 1999/04/27 10:06:48 $
 * ------------------------------------------------------------------------*/

typedef Int Command;

struct cmd {
    String cmdString;
    Command cmdCode;
};

extern Command readCommand Args((struct cmd *, Char, Char));

#define EDIT    0
#define FIND    1
#define LOAD    2
#define ALSO    3
#define PROJECT 4
#define RELOAD  5
#define EVAL    6
#define TYPEOF  7
#define HELP    8
#define NAMES   9
#define BADCMD  10
#define SET     11
#define QUIT    12
#define SYSTEM  13
#define CHGDIR  14
#define INFO    15
#define COLLECT 16
#define SETMODULE 17
#define DUMP    18
#define STATS   19
#define NOCMD   20

/*-------------------------------------------------------------------------*/

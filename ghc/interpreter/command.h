/* --------------------------------------------------------------------------
 * Interpreter command structure
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: command.h,v $
 * $Revision: 1.5 $
 * $Date: 1999/10/15 21:41:03 $
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

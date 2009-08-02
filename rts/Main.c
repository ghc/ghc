/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2009
 *
 * The C main() function for a standalone Haskell program.
 *
 * Note that this is not part of the RTS. It calls into the RTS to get things
 * going. It is compiled to a separate Main.o which is linked into every
 * standalone Haskell program that uses a Haskell Main.main function
 * (as opposed to a mixed Haskell C program using a C main function).
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsMain.h"

/* The symbol for the Haskell Main module's init function. It is safe to refer
 * to it here because this Main.o object file will only be linked in if we are
 * linking a Haskell program that uses a Haskell Main.main function.
 */
extern void __stginit_ZCMain(void);

/* Similarly, we can refer to the ZCMain_main_closure here */
extern StgClosure ZCMain_main_closure;

int main(int argc, char *argv[])
{
    return hs_main(argc, argv, &__stginit_ZCMain, &ZCMain_main_closure);
}

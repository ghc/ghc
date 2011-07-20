#include "Rts.h"

int main (int argc, char *argv[])
{
    // should fail: RTS is not initialised
    rts_lock();
}

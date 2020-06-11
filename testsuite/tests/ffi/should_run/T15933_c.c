#include "T15933.h"

void function_in_c(hs_callback cb)
{
    int x = 10;
    cb(x);
}

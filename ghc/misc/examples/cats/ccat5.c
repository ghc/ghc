#include <stdio.h>

main ()
{
    int c;
    while ((c = getchar()) != EOF) {
        putchar(c);
    }
    exit(0);
}

/*
-- 2,085,477 bytes/sec ( 600KB input)
-- 2,320,718 bytes/sec ( 9.3MB input)
-- 2,130,143 bytes/sec (25.5MB input)
*/

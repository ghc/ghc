#include <stdio.h>

main ()
{
    char c[4096];
    int  n;

    while ((n=fread(c,1,4096,stdin)) > 0) {
        fwrite(c,1,n,stdout);
    }
    exit(0);
}

/*
--  8,937,757 bytes/sec ( 600KB input)
-- 12,146,094 bytes/sec ( 9.3MB input)
--  8,658,233 bytes/sec (25.5MB input)
*/

#include <stdio.h>

main ()
{
    int n;
    
    scanf("%d",&n);
    n = nfib(n);
    printf("nfibs=%d\n",n);
    exit(0);
}

nfib (n)
{
    return(n <= 1 ? 1 : nfib(n-1) + nfib(n-2) + 1);
}

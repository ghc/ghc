#include <stdio.h>
#include <stdlib.h>

void expect_999(int p) {
    if (p != 999) {
        printf("Error: received %d\n",p);
        exit(1);
    }
}

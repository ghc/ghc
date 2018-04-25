#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "HsFFI.h"

#include "HaskelineExport_stub.h"

extern void __stginit_HaskelineExport();

void *hdata;

void catch_signal(int signo) {
    cancel_input(hdata);
    hs_exit();
    exit(0);
}

int main(int argc, char *argv[]) {
    hs_init(&argc, &argv);
    hs_add_root(__stginit_HaskelineExport);

    // TODO: block signals at certain points of this program 
    // in order to avoid race conditions.
    hdata = initialize_input();

    signal(SIGINT, catch_signal);

    char* str1 = get_input_line(hdata,"first:");
    char* str2 = get_input_line(hdata,"second:");

    if (str1!=NULL && str2 != NULL) {
        printf("The strings were:\n%s\n%s\n",str1,str2);
    }

    close_input(hdata);

    hs_exit();
    return 0;
}

#include "Rts.h"

void printObj( StgClosure *obj );
void print_obj( StgClosure *obj );
void rs_collect_pointers( StgClosure *obj);

void c_printClosure(StgClosure *p) {
    printObj(p);
    // print_obj(p);
    // collect_pointers(p);
    rs_collect_pointers(p);
}

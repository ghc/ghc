
#include "HsFFI.h"

typedef void (* funtype8)(HsInt8);
typedef void (* funtype16)(HsInt16);
typedef void (* funtype32)(HsInt32);
typedef void (* funtype64)(HsInt64);

void call8(funtype8 fun);
void call16(funtype16 fun);
void call32(funtype32 fun);
void call64(funtype64 fun);

int cmain();


/* --------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1992-2004
 *
 * mkDerivedConstants.c
 *
 * Basically this is a C program that extracts information from the C
 * declarations in the header files (primarily struct field offsets)
 * and generates a header file that can be #included into non-C source
 * containing this information.
 *
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include "gmp.h"


#define str(a,b) #a "_" #b

#define OFFSET(s_type, field) ((size_t)&(((s_type*)0)->field))

/* struct_size(TYPE)
 *
 */
#define def_size(str, size) \
    printf("#define SIZEOF_" str " %lu\n", (unsigned long)size);

#define struct_size(s_type) \
    def_size(#s_type, sizeof(s_type));



/* struct_field(TYPE, FIELD)
 *
 */
#define def_offset(str, offset) \
    printf("#define OFFSET_" str " %d\n", (int)(offset));

#define field_offset_(str, s_type, field) \
    def_offset(str, OFFSET(s_type,field));

#define field_offset(s_type, field) \
    field_offset_(str(s_type,field),s_type,field);

#define field_type_(str, s_type, field) \
    printf("#define REP_" str " b"); \
    printf("%lu\n", (unsigned long)sizeof (__typeof__(((((s_type*)0)->field)))) * 8);

#define field_type(s_type, field) \
    field_type_(str(s_type,field),s_type,field);

/* An access macro for use in C-- sources. */
#define struct_field_macro(str) \
    printf("#define " str "(__ptr__)  REP_" str "[__ptr__+OFFSET_" str "]\n");

/* Outputs the byte offset and MachRep for a field */
#define struct_field(s_type, field)		\
    field_offset(s_type, field);		\
    field_type(s_type, field);			\
    struct_field_macro(str(s_type,field))


int
main(int argc, char *argv[])
{
    printf("/* This file is created automatically.  Do not edit by hand.*/\n\n");

    struct_size(MP_INT);
    struct_field(MP_INT,_mp_alloc);
    struct_field(MP_INT,_mp_size);
    struct_field(MP_INT,_mp_d);

    return 0;
}

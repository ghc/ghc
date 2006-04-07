#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409
#include <Rts.h>
#endif
#include <HsFFI.h>

#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

#ifndef offsetof
#define offsetof(t, f) ((size_t) &((t *)0)->f)
#endif

#if __NHC__
#define hsc_line(line, file) \
    printf ("# %d \"%s\"\n", line, file);
#else
#define hsc_line(line, file) \
    printf ("{-# LINE %d \"%s\" #-}\n", line, file);
#endif

#define hsc_const(x)                        \
    if ((x) < 0)                            \
        printf ("%ld", (long)(x));          \
    else                                    \
        printf ("%lu", (unsigned long)(x));

#define hsc_const_str(x)                                          \
    {                                                             \
        const char *s = (x);                                      \
        printf ("\"");                                            \
        while (*s != '\0')                                        \
        {                                                         \
            if (*s == '"' || *s == '\\')                          \
                printf ("\\%c", *s);                              \
            else if (*s >= 0x20 && *s <= 0x7E)                    \
                printf ("%c", *s);                                \
            else                                                  \
                printf ("\\%d%s",                                 \
                        (unsigned char) *s,                       \
                        s[1] >= '0' && s[1] <= '9' ? "\\&" : ""); \
            ++s;                                                  \
        }                                                         \
        printf ("\"");                                            \
    }

#define hsc_type(t)                                         \
    if ((t)(int)(t)1.4 == (t)1.4)                           \
        printf ("%s%d",                                     \
                (t)(-1) < (t)0 ? "Int" : "Word",            \
                sizeof (t) * 8);                            \
    else                                                    \
        printf ("%s",                                       \
                sizeof (t) >  sizeof (double) ? "LDouble" : \
                sizeof (t) == sizeof (double) ? "Double"  : \
                "Float");

#define hsc_peek(t, f) \
    printf ("(\\hsc_ptr -> peekByteOff hsc_ptr %ld)", (long) offsetof (t, f));

#define hsc_poke(t, f) \
    printf ("(\\hsc_ptr -> pokeByteOff hsc_ptr %ld)", (long) offsetof (t, f));

#define hsc_ptr(t, f) \
    printf ("(\\hsc_ptr -> hsc_ptr `plusPtr` %ld)", (long) offsetof (t, f));

#define hsc_offset(t, f) \
    printf("(%ld)", (long) offsetof (t, f));

#define hsc_size(t) \
    printf("(%ld)", (long) sizeof(t));

#define hsc_enum(t, f, print_name, x)         \
    print_name;                               \
    printf (" :: %s\n", #t);                  \
    print_name;                               \
    printf (" = %s ", #f);                    \
    if ((x) < 0)                              \
        printf ("(%ld)\n", (long)(x));        \
    else                                      \
        printf ("%lu\n", (unsigned long)(x));

#define hsc_haskellize(x)                                          \
    {                                                              \
        const char *s = (x);                                       \
        int upper = 0;                                             \
        if (*s != '\0')                                            \
        {                                                          \
            putchar (tolower (*s));                                \
            ++s;                                                   \
            while (*s != '\0')                                     \
            {                                                      \
                if (*s == '_')                                     \
                    upper = 1;                                     \
                else                                               \
                {                                                  \
                    putchar (upper ? toupper (*s) : tolower (*s)); \
                    upper = 0;                                     \
                }                                                  \
                ++s;                                               \
            }                                                      \
        }                                                          \
    }

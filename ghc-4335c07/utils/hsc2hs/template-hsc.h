
/* We need stddef to be able to use size_t. Hopefully this won't cause
   any problems along the lines of ghc trac #2897. */
#include <stddef.h>

/* hsc_* are defined in the generated utils.c */
int hsc_printf(const char *format, ...);
int hsc_toupper(int c);
int hsc_tolower(int c);
int hsc_putchar(int c);
/* "void" should really be "FILE", but we aren't able to refer to "FILE"
   as we don't want to include <stdio.h> here */
int hsc_fputs(const char *s, void *stream);
/* "void" should really be "FILE", but we aren't able to refer to "FILE"
   as we don't want to include <stdio.h> here */
void *hsc_stdout(void);

/* For the single-argument macros we make the macros variadic (the
   argument is x... rather than simply x) so that arguments containing
   commas work. See trac #590. */

#ifndef offsetof
#define offsetof(t, f) ((size_t) &((t *)0)->f)
#endif

#if __NHC__
#define hsc_line(line, file) \
    hsc_printf ("# %d \"%s\"\n", line, file);
#define hsc_column(column)
#else
#define hsc_line(line, file) \
    hsc_printf ("{-# LINE %d \"%s\" #-}\n", line, file);
#define hsc_column(column) \
    hsc_printf ("{-# COLUMN %d #-}", column);
#endif

#define hsc_const(x...)                               \
    if ((x) < 0)                                      \
        hsc_printf ("%lld", (long long)(x));          \
    else                                              \
        hsc_printf ("%llu", (unsigned long long)(x));

#define hsc_const_str(x...)                                       \
    {                                                             \
        const char *s = (x);                                      \
        hsc_printf ("\"");                                        \
        while (*s != '\0')                                        \
        {                                                         \
            if (*s == '"' || *s == '\\')                          \
                hsc_printf ("\\%c", *s);                          \
            else if (*s >= 0x20 && *s <= 0x7E)                    \
                hsc_printf ("%c", *s);                            \
            else                                                  \
                hsc_printf ("\\%d%s",                             \
                        (unsigned char) *s,                       \
                        s[1] >= '0' && s[1] <= '9' ? "\\&" : ""); \
            ++s;                                                  \
        }                                                         \
        hsc_printf ("\"");                                        \
    }

#define hsc_type(t...)                                      \
    if ((t)(int)(t)1.4 == (t)1.4)                           \
        hsc_printf ("%s%lu",                                \
                (t)(-1) < (t)0 ? "Int" : "Word",            \
                (unsigned long)sizeof (t) * 8);             \
    else                                                    \
        hsc_printf ("%s",                                   \
                sizeof (t) >  sizeof (double) ? "LDouble" : \
                sizeof (t) == sizeof (double) ? "Double"  : \
                "Float");

#define hsc_peek(t, f) \
    hsc_printf ("(\\hsc_ptr -> peekByteOff hsc_ptr %ld)", \
                (long) offsetof (t, f));

#define hsc_poke(t, f) \
    hsc_printf ("(\\hsc_ptr -> pokeByteOff hsc_ptr %ld)", \
                (long) offsetof (t, f));

#define hsc_ptr(t, f) \
    hsc_printf ("(\\hsc_ptr -> hsc_ptr `plusPtr` %ld)", \
                (long) offsetof (t, f));

#define hsc_offset(t, f) \
    hsc_printf("(%ld)", (long) offsetof (t, f));

#define hsc_size(t...) \
    hsc_printf("(%ld)", (long) sizeof(t));

#define hsc_alignment(x...)                                           \
  do {                                                                \
    struct __anon_x__ {                                               \
      char a;                                                         \
      x b;                                                            \
    };                                                                \
    hsc_printf("%lu", (unsigned long)offsetof(struct __anon_x__, b)); \
  } while (0)

#define hsc_enum(t, f, print_name, x)                   \
    print_name;                                         \
    hsc_printf (" :: %s\n", #t);                        \
    print_name;                                         \
    hsc_printf (" = %s ", #f);                          \
    if ((x) < 0)                                        \
        hsc_printf ("(%lld)\n", (long long)(x));        \
    else                                                \
        hsc_printf ("%llu\n", (unsigned long long)(x));

#define hsc_haskellize(x...)                                       \
    {                                                              \
        const char *s = (x);                                       \
        int upper = 0;                                             \
        if (*s != '\0')                                            \
        {                                                          \
            hsc_putchar (hsc_tolower (*s));                        \
            ++s;                                                   \
            while (*s != '\0')                                     \
            {                                                      \
                if (*s == '_')                                     \
                    upper = 1;                                     \
                else                                               \
                {                                                  \
                    hsc_putchar (upper ? hsc_toupper (*s)          \
                                       : hsc_tolower (*s));        \
                    upper = 0;                                     \
                }                                                  \
                ++s;                                               \
            }                                                      \
        }                                                          \
    }

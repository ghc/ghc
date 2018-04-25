/* Coverity Scan model
 * This is a modeling file for Coverity Scan. Modeling helps to avoid false
 * positives.
 *
 * - A model file can't import any header files.  Some built-in primitives are
 *   available but not wchar_t, NULL etc.
 * - Modeling doesn't need full structs and typedefs. Rudimentary structs
 *   and similar types are sufficient.
 * - An uninitialized local variable signifies that the variable could be
 *   any value.
 *
 * The model file must be uploaded by an admin in the analysis settings of
 * http://scan.coverity.com/projects/1919
 */

#define NULL ((void*)0)
#define assert(x) if (!(x)) __coverity_panic__();

/* type decls */
typedef struct {} va_list;

/* glibc functions */
void *malloc (size_t);
void *calloc (size_t, size_t);
void *realloc (void *, size_t);
void free (void *);

/* rts allocation functions */

void* stgMallocBytes(int n, char *msg)
{
  void *mem;
  __coverity_negative_sink__((size_t)n);
  mem = malloc((size_t)n);
  assert(mem != NULL);
  return mem;
}

void* stgReallocBytes(void *p, int n, char *msg)
{
  void *mem;
  __coverity_negative_sink__((size_t)n);

  /* man 3 realloc: if p == NULL, then realloc is equivalent to malloc() */
  if (p == NULL) {
    mem = malloc((size_t)n);
    assert(mem != NULL);
    return mem;
  }

  /* man 3 realloc: if n == 0, then realloc is equivalent to free() */
  if (n == 0) {
    free(p);
    return NULL;
  } else {
    mem = realloc(p, (size_t)n);
    assert(mem != NULL);
    return mem;
  }
}

void* stgCallocBytes(int n, int m, char *msg)
{
  void *mem;
  __coverity_negative_sink__((size_t)n);
  __coverity_negative_sink__((size_t)m);
  mem = calloc(n, m);
  assert(mem != NULL);
  return mem;
}

void stgFree(void* p)
{
  free(p);
}

/* Kill paths */

void stg_exit(int n)
{
  __coverity_panic__();
}

void shutdownThread(void)
{
  __coverity_panic__();
}

void shutdownHaskellAndExit(int exitCode, int fastExit)
{
  __coverity_panic__();
}

void shutdownHaskellAndSignal(int sig, int fastExit)
{
  __coverity_panic__();
}

void _assertFail(const char *filename, unsigned int linenum)
{
  __coverity_panic__();
}

void barf(const char *s, ...)
{
  __coverity_panic__();
}

void vbarf(const char *s, va_list ap)
{
  __coverity_panic__();
}

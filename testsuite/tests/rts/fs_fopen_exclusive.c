#include <errno.h>
#include <stdio.h>
#include <string.h>

extern FILE *__rts_fopen(const char *, const char *);

static int check(const char *filename, const char *mode) {
  const char contents[] = "foo";
  char buf[sizeof(contents)];

  remove(filename);

  FILE *f = __rts_fopen(filename, mode);
  if (f == NULL) {
    perror("exclusive creation");
    return 1;
  }

  int ok = fwrite(contents, 1, sizeof(contents), f) == sizeof(contents);
  if (fclose(f) != 0)
    ok = 0;
  if (!ok) {
    fprintf(stderr, "Failed writing %s\n", filename);
    return 1;
  }

  f = __rts_fopen(filename, mode);
  if (f != NULL) {
    fclose(f);
    fprintf(stderr, "%s unexpectedly succeeded on existing file\n", mode);
    return 1;
  }
  if (errno != EEXIST) {
    fprintf(stderr, "%s failed with errno %d instead of EEXIST\n", mode, errno);
    return 1;
  }

  f = __rts_fopen(filename, "r");
  if (f == NULL) {
    perror("reopen");
    return 1;
  }

  ok = fread(buf, 1, sizeof(buf), f) == sizeof(buf)
       && memcmp(buf, contents, sizeof(contents)) == 0;
  fclose(f);

  if (!ok) {
    fprintf(stderr, "%s changed the existing file\n", mode);
    return 1;
  }

  remove(filename);
  return 0;
}

int main(void) {
  return check("fs_exclusive_w.tmp", "wx") || check("fs_exclusive_wplus.tmp", "w+x");
}

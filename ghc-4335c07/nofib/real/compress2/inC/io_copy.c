/*
 * file_io.c - copy stdin to stdio
 *
 */

#include <stdio.h>

main ()
{
  register int c;

  while ((c = getchar ()) != EOF) {
    putchar (c);
  };
}

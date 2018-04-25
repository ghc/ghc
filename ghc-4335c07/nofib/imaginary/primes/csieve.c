# include <stdio.h>


# define MAX_SIEVE 100000

int sieve_array[MAX_SIEVE];


void primes()
{
  int p = 1,
      i;

  while (1)
  {
    p++;
    while (p<MAX_SIEVE && sieve_array[p]) p++;
    if (p>=MAX_SIEVE) break;
    printf(" %d,", p);

    for (i = 2*p; i<MAX_SIEVE; i += p) sieve_array[i] = 1;
  }
}

int main ()
{
  primes ();
}

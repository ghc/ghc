/*  Ensnaffled by SLPJ from MIT, 93/08/26;
    via shail@au-bon-pain.lcs.mit.edu (Shail Aditya).
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/times.h>
/* #include "cqc.h" */
/* #include "paraffins.h" */

/* ======================================================================== */

/* cqc's favoriate primitive types */

enum boolean { FALSE, TRUE };
typedef enum boolean bool;

/* cqc's favoriate primitive macros */

#define NEW_STRUCT(name) ((name *)(calloc(1, sizeof(name))))

/* misc. */

/* enum exit_values { WELL_DONE, SOMETHING_WRONG } for the system call, "exit" 
*/

/* ======================================================================== */

#define NUM_SUB_RADICALS  3
#define MAX_NUM_RADICALS  4
#define MAX_SIZE_RADICAL  30
#define MAX_SIZE_PARAFFIN 30
#define HEAPSIZE 0x06000000

enum kind_radical { H, C };
typedef enum kind_radical KIND_RADICAL;

enum kind_paraffin { BCP, CCP };
typedef enum kind_paraffin KIND_PARAFFIN;

typedef
  struct radical
    {
      KIND_RADICAL   kind;
      struct radical *sub_radicals[NUM_SUB_RADICALS];
      struct radical *next;
    }
  RADICAL;

typedef
  struct paraffin
    {
      KIND_PARAFFIN   kind;
      RADICAL         *radicals[MAX_NUM_RADICALS];
      struct paraffin *next;
    }
  PARAFFIN;


typedef
  struct tuple
    {
	PARAFFIN     *bcp;
	PARAFFIN     *ccp;
    }
  TUPLE;

typedef
  struct three_parts
    {
      int nc[3];
      struct three_parts *next;
    }
  THREE_PARTS;

typedef
  struct four_parts
    {
      int nc[4];
      struct four_parts *next;
    }
  FOUR_PARTS;

#define U_NEW_STRUCT(name) ((name *)(halloc(sizeof(name))))
#define max(a,b) ((a) > (b) ? (a) : (b))

/* ======================================================================== */
/*
extern int  get_size();
extern void clear_tables(int n);
extern void free_four_partitions(FOUR_PARTS *parts);
extern void free_three_partitions(THREE_PARTS *parts);
extern void print_paraffin_counts(int n);
extern void print_radical_counts(int n);
extern void print_radicals(int n);
*/

RADICAL  *radicals[MAX_SIZE_RADICAL];
int      paraffins_counts[MAX_SIZE_RADICAL];
int      BCP_counts[MAX_SIZE_RADICAL];
int      CCP_counts[MAX_SIZE_RADICAL];
PARAFFIN *BCP_array[MAX_SIZE_PARAFFIN];
PARAFFIN *CCP_array[MAX_SIZE_PARAFFIN];
TUPLE    *paraffins_array[MAX_SIZE_PARAFFIN];
int      num_paraffins[MAX_SIZE_PARAFFIN];
PARAFFIN *paraffins;
static char *free, *limit;

main(argc, argv)
     int argc;
     char *argv[];
{
  int n, i;
  
/*
  printf("\nPARAFFINS Problem: Version 4\n");
  printf("\t(No Garbage Collection, User-Managed Heap, No Copying for Partitions)\n\n");
*/
  if (argc != 2) {
    printf("Usage: paraffins size\n");
    exit(0);
  }
  n = atoi(argv[1]);
  init_heap();
  paraffins_until(n);
  /* print the information about results */

  print_paraffin_counts(n);


}

/* Printout Functions */

print_paraffin_counts(n)
     int n;
{
  PARAFFIN *p;
  int num_paraffins, i;

  printf("\nParaffin Counts\n");
  for (i = 1; i <= n; i++)
    {
	printf("\tparaffins[%2d] : %d\n", i, paraffins_counts[i]);
    }
}


paraffins_until (n)
     int n;
{
  struct tms start_time, finish_time;
  float t1, t2;
  int i;
  TUPLE *t;

  /* main body */
#ifdef DBG
  printf("Begin RADICALS computed\n");
#endif
  radical_generator(n/2);
#ifdef DBG
  printf("RADICALS computed\n");

  printf("PARAFFINS being computed ...\n");

  printf("\tBCP being computed ...\n");
#endif
  for (i = 1; i <= n; ++i) {
    BCP_generator(i);
#ifdef DBG
    printf("\tBCP computed\n");
    printf("\tCCP being computed ...\n");
#endif
    CCP_generator_with_no_copying(i);
#ifdef DBG
    printf("\tCCP computed\n");
    printf("PARAFFINS computed ...\n");
#endif
    t = U_NEW_STRUCT(TUPLE);
    t->bcp = BCP_array[i];
    t->ccp = CCP_array[i];    
    paraffins_array[i] = t;
    paraffins_counts[i] = BCP_counts[i] + CCP_counts[i];
  }
}


radical_generator (n)
     int n;
{
  RADICAL *h;
  int i;

  /* initialize "radicals" */
  h = U_NEW_STRUCT(RADICAL);
  h->kind = H;
  radicals[0] = h;
  for (i = 1; i <= n; i++) {
      rads_of_size_n_with_no_copying(i);
#ifdef DBG
      printf("\tradicals[%d] computed\n", i);
#endif
  }
}

paraffins_generator (n)
     int n;
{
    TUPLE *t;

    t = U_NEW_STRUCT(TUPLE);
    t->bcp = BCP_array[n];
    t->ccp = CCP_array[n];    
    paraffins_array[n] = t;
}


BCP_generator (n)
     int n;
{
  PARAFFIN *p, *root;
  RADICAL  *r1, *r2;
  int half_n;
  int num_BCPs;

  num_BCPs = 0;
  root = (PARAFFIN *) NULL;

  if (n != (half_n = (n/2)) * 2)
    {
#ifdef DBG
	printf("\tNumber of BCPs with size %d : %d\n", n, num_BCPs);  */
#endif
	BCP_array[n] = root;
	BCP_counts[n] = num_BCPs;
	return;
    }
  
#ifdef DBG
  printf("\t\t(%d(%d),%d(%d))\n", half_n, num_rads[half_n], half_n, num_rads[ha
lf_n]);
#endif
  for (r1 = radicals[half_n]; r1 != (RADICAL *)NULL; r1 = r1->next)
      for (r2 = r1; r2 != (RADICAL *)NULL; r2 = r2->next)
	  {
	      p = U_NEW_STRUCT(PARAFFIN);
	      p->kind = BCP;
	      p->radicals[0] = r1;
	      p->radicals[1] = r2;
	      p->next = root;
	      root = p;
	      num_BCPs++;
	  }
  BCP_array[n] = root;
  BCP_counts[n] = num_BCPs;
  /*   printf("\tNumber of BCPs with size %d : %d\n", n, num_BCPs);  */
}


/*--------------------------------------------------------
 * This function is used only on VERSION 3 and 4.
 *--------------------------------------------------------
 */
CCP_generator_with_no_copying (n)
     int n;
{
  PARAFFIN *p, *root;
  RADICAL  *r1, *r2, *r3, *r4;
  int m, half_m, nc1, nc2, nc3, nc4, k, num_CCPs;

  root = (PARAFFIN *) NULL;
  num_CCPs = 0;
  m = n - 1;
  for (nc1 = 0; nc1 <= m/4; nc1++)
    for (nc2 = nc1; nc2 <= (m-nc1)/3; nc2++)
      {
	half_m = m/2;
	if (m == half_m*2)
	  k = half_m-nc1-nc2;
	else 
	  k = half_m+1-nc1-nc2;
	if (k < nc2)
	  k = nc2;
	for (nc3 = k; nc3 <= (m-nc1-nc2)/2; nc3++)
	  {
	    nc4 = m - nc1 - nc2 - nc3;
	    for (r1 = radicals[nc1]; r1 != (RADICAL *)NULL; r1 = r1->next)
	      for (r2 = (nc1 == nc2) ? r1 : radicals[nc2]; r2 != (RADICAL *)NULL; r2 = r2->next)
		for (r3 = (nc2 == nc3) ? r2 : radicals[nc3]; r3 != (RADICAL *)NULL; r3 = r3->next)
		  for (r4 = (nc3 == nc4) ? r3 : radicals[nc4]; r4 != (RADICAL *)NULL; r4 = r4->next)
		    {
		      p = U_NEW_STRUCT(PARAFFIN);
		      p->kind = CCP;
		      p->radicals[0] = r1;
		      p->radicals[1] = r2;
		      p->radicals[2] = r3;
		      p->radicals[3] = r4;
		      p->next = root;
		      root = p;
		      num_CCPs++;
		    }
	  }
      }
  CCP_array[n] = root;
  CCP_counts[n] = num_CCPs;
  /* free_four_partitions(parts); */
}

/*--------------------------------------------------------
 * This function is used only on VERSION 3 and 4.
 *--------------------------------------------------------
 */
rads_of_size_n_with_no_copying (m)
     int m;
{
  RADICAL *rad, *r1, *r2, *r3;
  int nc1, nc2, nc3, n;

  n = m-1;
  for (nc1 = 0; nc1 <= n/3; nc1++)
    for (nc2 = nc1; nc2 <= (n-nc1)/2; nc2++)
      {
	nc3 = n - (nc1 + nc2);
	for (r1 = radicals[nc1]; r1 != (RADICAL *)NULL; r1 = r1->next)
	  for (r2 = (nc1 == nc2) ? r1 : radicals[nc2]; r2 != (RADICAL *)NULL; r2 = r2->next)
	    for (r3 = (nc2 == nc3) ? r2 : radicals[nc3]; r3 != (RADICAL *)NULL; r3 = r3->next)
	      {
		rad = U_NEW_STRUCT(RADICAL);
		rad->kind = C;
		rad->sub_radicals[0] = r1;
		rad->sub_radicals[1] = r2;
		rad->sub_radicals[2] = r3;
		rad->next = radicals[m];
		radicals[m] = rad;
	      }
      }
  /* free_three_partitions(parts); */
}


/*----------------------------------------------------------------
INIT_HEAP should be called at start of MAIN
*/

init_heap()
{
    int p;

    p = sbrk(HEAPSIZE);
    if (p == -1) {
        printf("Error (init_heap) cannot alloc %d bytes.\n", HEAPSIZE);
        exit(0);
    }
    free = (char *) p;
    limit = free + HEAPSIZE - 1;
}

/*---------------------------------------------------------------
HALLOC
  Takes N: int
  Returns: pointer to heap object of size N bytes

*/

halloc(n)
     int n;
{
    char *p;

    p=free;
    free += n;

    if (free > limit) {
      printf("Error (halloc) No space for %d bytes.\n", n);
      exit(0);
    }

    return (int)p;
  }


/**********************************************************************
*                                                                     *
*                                                                     *
*      Identifier Processing                                          *
*                                                                     *
*                                                                     *
**********************************************************************/

#include <stdio.h>

#include "hspincl.h"
#include "constants.h"
#include "id.h"
#include "utils.h"

/* partain: special version for strings that may have NULs (etc) in them
   (used in UgenUtil.lhs)
*/
long
get_hstring_len(hs)
  hstring hs;
{
    return(hs->len);
}

char *
get_hstring_bytes(hs)
  hstring hs;
{
  return(hs->bytes);
}

hstring
installHstring(length, s)
  int  length;
  char *s;
{
  char *p;
  hstring str;
  int i;

/* fprintf(stderr, "installHstring: %d, %s\n",length, s); */

  if (length > 999999) { /* too long */
      fprintf(stderr,"String length more than six digits\n");
      exit(1);
  } else if (length < 0) { /* too short */
      fprintf(stderr,"String length < 0 !!\n");
      abort();
  }

  /* alloc the struct and store the length */
  str = (hstring) xmalloc(sizeof(Hstring));
  str->len = length;

  if (length == 0) {
     str->bytes = NULL;

  } else {
     p = xmalloc(length);

     /* now store the string */
     for (i = 0; i < length; i++) {
       p[i] = s[i];
     }
     str->bytes = p;
  }
  return str;
}


/**********************************************************************
*                                                                     *
*                                                                     *
*      Hashed Identifiers                                             *
*                                                                     *
*                                                                     *
**********************************************************************/


extern BOOLEAN hashIds;				/* Whether to use hashed ids. */

unsigned hash_table_size = HASH_TABLE_SIZE;

static char **hashtab = NULL;

static unsigned  max_hash_table_entries = 0;

void
hash_init()
{
  if(!hashIds) {
    /*NOTHING*/;

  } else {

  /* Create an initialised hash table */
  hashtab = (char **) calloc( hash_table_size, sizeof(char *) );
  if(hashtab == NULL)
    {
      fprintf(stderr,"Cannot allocate a hash table with %d entries -- insufficient memory\n",hash_table_size);
      exit(1);
    }
#ifdef HSP_DEBUG
  fprintf(stderr,"hashtab = %x\n",hashtab);
#endif

  /* Allow no more than 90% occupancy -- Divide first to avoid overflows with BIG tables! */
  max_hash_table_entries = (hash_table_size / 10) * 9;
  }
}

void
print_hash_table()
{
  if(hashIds)
    {
      unsigned i;

      printf("%u ",hash_table_size);

      for(i=0; i < hash_table_size; ++i)
	if(hashtab[i] != NULL)
	  printf("(%u,%s) ",i,hashtab[i]);
    }
}


long int
hash_index(ident)
  id ident;
{
  return((char **) /* YURGH */ ident - hashtab);
}


/*
  The hash function.  Returns 0 for Null strings.
*/

static unsigned hash_fn(char *ident)
{
  unsigned len = (unsigned) strlen(ident);
  unsigned res;

  if(*ident == '\0')
    return( 0 );

  /* does not work well for hash tables with more than 35K elements */
  res = (((unsigned)ident[0]*631)+((unsigned)ident[len/2-1]*217)+((unsigned)ident[len-1]*43)+len)
	  % hash_table_size;

#ifdef HSP_DEBUG
  fprintf(stderr,"\"%s\" hashes to %d\n",ident,res);
#endif
  return(res);
}


/*
  Install a literal identifier, such as "+" in hsparser.
  If we are not using hashing, just return the string.
*/

id
install_literal(s)
  char *s;
{
  return( hashIds? installid(s): s);
}


char *
id_to_string(sp)
  id sp;
{
  return( hashIds? *(char **)sp: (char *)sp );
}

id
installid(s)
  char *s;
{
  unsigned hash, count;

  if(!hashIds)
    return(xstrdup(s));

  for(hash = hash_fn(s),count=0; count<max_hash_table_entries; ++hash,++count)
    {
      if (hash >= hash_table_size) hash = 0;

      if(hashtab[hash] == NULL)
	{
	  hashtab[hash] = xstrdup(s);
#ifdef HSP_DEBUG
	  fprintf(stderr,"New Hash Entry %x\n",(char *)&hashtab[hash]);
#endif
	  if ( count >= 100 ) {
	    fprintf(stderr, "installid: %d collisions for %s\n", count, s);
	  }

	  return((char *)&hashtab[hash]);
	}

      if(strcmp(hashtab[hash],s) == 0)
	{
#ifdef HSP_DEBUG
	  fprintf(stderr,"Old Hash Entry %x (%s)\n",(char *)&hashtab[hash],hashtab[hash]);
#endif
	  if ( count >= 100 ) {
	    fprintf(stderr, "installid: %d collisions for %s\n", count, s);
	  }

	  return((char *)&hashtab[hash]);
	}
    }
  fprintf(stderr,"Hash Table Contains more than %d entries -- make larger?\n",max_hash_table_entries);
  exit(1);
}


/**********************************************************************
*                                                                     *
*                                                                     *
*     Memory Allocation                                               *
*                                                                     *
*                                                                     *
**********************************************************************/

/* Malloc with error checking */

char *
xmalloc(length)
unsigned length;
{
    char *stuff = malloc(length);

    if (stuff == NULL) {
	fprintf(stderr, "xmalloc failed on a request for %d bytes\n", length);
	exit(1);
    }
    return (stuff);
}

char *
xrealloc(ptr, length)
char *ptr;
unsigned length;
{
    char *stuff = realloc(ptr, length);

    if (stuff == NULL) {
	fprintf(stderr, "xrealloc failed on a request for %d bytes\n", length);
	exit(1);
    }
    return (stuff);
}

/* Strdup with error checking */

char *
xstrdup(s)
char *s;
{
    unsigned len = strlen(s);
    return xstrndup(s, len);
}

/*
 * Strdup for possibly unterminated strings (e.g. substrings of longer strings)
 * with error checking.  Handles NULs as well.
 */

char *
xstrndup(s, len)
char *s;
unsigned len;
{
    char *p = xmalloc(len + 1);

    bcopy(s, p, len);
    p[len] = '\0';

    return (p);
}

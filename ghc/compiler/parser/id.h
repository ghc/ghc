#ifndef ID_H
#define ID_H

typedef char *id;
typedef id unkId;	/* synonym */
typedef id stringId;	/* synonym */
typedef id numId;	/* synonym, for now */

typedef struct { long len; char *bytes; } Hstring;
typedef Hstring *hstring;

long  get_hstring_len  PROTO((hstring));
char *get_hstring_bytes PROTO((hstring));

id installid PROTO((char *));		     /* Create a new identifier */
hstring installHstring PROTO((int, char *)); /* Create a new literal string */

/* defines for special-syntax ids, see comment next
   to creategid()
*/
#define ARROWGID   (-2)
#define NILGID     (-1)
#define UNITGID    (0)


#endif

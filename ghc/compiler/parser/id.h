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

#endif

# include "id.h"

#define bool int
#define true 1
#define false 0

char id_area[10000];
char *id_top = &id_area[0];



/*
**	Equalid returns true if the two identifiers are the same,
**	otherwise false.
*/
bool equalid(i1, i2)
    id i1, i2;
{
	return(i1 == i2);
}

/*
**	Installid installs an identifier into the id_area.
*/
id installid(string)
    char *string;
{
	char	*startofid, *search, *s;

	for(search = id_area; search < id_top;) {
		startofid = search;
		s = string;
		while(*search++ == *s++) {
			if(*search == 0 && *s == 0) {
				return(startofid);
			}
		}
		while(*search != 0)
			search++;
		search++;
	}

	startofid = id_top;
	for(s = string; *s != 0;) {
		*id_top++ = *s++;
	}
	*id_top++ = 0;
	return(startofid);
}

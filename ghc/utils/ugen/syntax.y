%{
#define YYSTYPE long
# include "id.h"
# include "tree.h"
extern tree root;
%}
%token	ID TYPE SEMICOL COLON END STDEF ENDDEF
%%

typdef	: 
	TYPE ID SEMICOL deflist END SEMICOL =
	{
		root = mktypdef($2, $4);
	};

deflist	:
	def =
	{
		$$ = $1;
	} |
	deflist def =
	{
		$$ = (long) mkdeflist($1, $2);
	};

def	:
	ID COLON STDEF itemlist ENDDEF SEMICOL =
	{
		$$ = (long) mkdef($1, $4);
	} |
	ID COLON STDEF ENDDEF SEMICOL =
	{
		$$ = (long) mkdef($1, mkemitemlist());
	};

itemlist:
	item =
	{
		$$ = $1;
	} |
	itemlist item =
	{
		$$ = (long) mkitemlist($1, $2);
	};

item	:
	ID COLON ID SEMICOL =
	{
		$$ = (long) mkitem($1, $3);
	};

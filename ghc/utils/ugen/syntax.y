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
		root = mktypdef((id)$2,(tree)$4);
	};

deflist	:
	def =
	{
		$$ = $1;
	} |
	deflist def =
	{
		$$ = (long)mkdeflist((tree)$1,(tree)$2);
	};

def	:
	ID COLON STDEF itemlist ENDDEF SEMICOL =
	{
		$$ = (long)mkdef((id)$1,(tree)$4);
	} |
	ID COLON STDEF ENDDEF SEMICOL =
	{
		$$ = (long)mkdef((id)$1,mkemitemlist());
	};

itemlist:
	item =
	{
		$$ = $1;
	} |
	itemlist item =
	{
		$$ = (long)mkitemlist((tree)$1,(tree)$2);
	};

item	:
	ID COLON ID SEMICOL =
	{
		$$ = (long)mkitem((id)$1,(id)$3);
	};

/*
	msub - Read file(s) and perform substitutions using values of
		variables defined in makefile.

	Syntax: msub [-f makefile] [file ...]

	Multiple -f options may be given.

	27 Mar 1990	Paul DuBois	dubois@primate.wisc.edu

	27 Mar 1990 V1.0.  Created.
*/

# include	<stdio.h>
# include	<ctype.h>
# include	<string.h>

# define	true	(1)
# define	false	(0)


extern char	*calloc ();


char	*NewString();


typedef struct Def	Def;

struct Def
{
	char	*var;		/* variable name */
	char	*val;		/* variable value */
	int	simple;		/* whether value has been expanded */
	Def	*nextDef;	/* next definition in list */
};


char	*usage = "Usage: msub [ -f makefile ] file";
char	*makefile = NULL;
Def	*defList = NULL;	/* list of definitions */
int	nDefs = 0;		/* number of definitions */
int	changes;

main (argc, argv)
int	argc;
char	**argv;
{
FILE	*f;
Def	*dp;
int	i, pass;

	--argc;
	++argv;

	while (argc > 0 && argv[0][0] == '-')
	{
		if (strcmp (argv[0], "-f") == 0)
		{
			if (argc < 2)
				panic (usage);
			if ((f = fopen (makefile = argv[1], "r")) == NULL)
				panic ("Can't open makefile");
			ReadMake (f);
			fclose (f);
			argc -= 2;
			argv += 2;
		}
		else
			panic (usage);	/* bad flag */
	}

	if (makefile == NULL)	/* no -f options given */
	{
		if ((f = fopen ("makefile", "r")) == NULL)
		{
			if ((f = fopen ("Makefile", "r")) == NULL)
				panic ("Can't open makefile");
		}
		ReadMake (f);
		fclose (f);
	}

	EncodeEscapes ();

	/* determine which values need expanding */
	for (dp = defList; dp != NULL; dp = dp->nextDef)
		dp->simple = !FindVarRef (dp->val, NULL, NULL, false);

	/* expand values to eliminate embedded var references */
	for (pass = 0; pass < nDefs; pass++)
	{
		changes = 0;
		for (dp = defList; dp != NULL; dp = dp->nextDef)
		{
			if (!dp->simple)
				Expand (dp);
		}
		if (changes == 0)	/* loop while values expand */
			break;
	}

	/* sanity check: shouldn't have to make more than nDefs passes */
	if (pass >= nDefs)
		panic ("Too many expansion passes.  Something's wrong!");

	DecodeEscapes ();

	/* partain addition:
	    args of form "foo=bar" are taken to be defs to add in
	*/
	while (argc > 0 && strchr(argv[0], '=') && isalpha(argv[0][0]))
	{
	 char name[BUFSIZ], *np, *p;
	 int len;
	 p = argv[0];
		/* copied from below */
			np = name;
			while (isalnum (*p) || *p == '_')
				*np++ = *p++;
			*np = '\0';
			while (isspace (*p))	/* skip whitespace */
				++p;
			if (*p++ == '=')	/* it's a definition */
			{
				/* skip leading/trailing whitespace */
				while (isspace (*p))
					++p;
				len = strlen (p);
				while (len > 0 && isspace (p[len-1]))
					p[--len] = '\0';
				AddDef (name, p, 1);
				/* find var refs in value -- NUKED */
				/* FindAllVarRefs (p); */
				/* also NUKED: continue; */
			}
		/* end of: copied from below */
		--argc;
		++argv;
	}
	/* end of partain addition */

	/* read source file(s) and perform substitutions */
	if (argc == 0)
		DoSub (stdin);
	else while (argc > 0)
	{
		if ((f = fopen (*argv, "r")) == NULL)
		{
			fprintf (stderr, "Can't open \"%s\".  ", *argv);
			panic ("Quitting.");
		}
		DoSub (f);
		fclose (f);
		--argc;
		++argv;
	}

	exit (0);
}


/*
	Read a makefile.
*/

ReadMake (f)
FILE	*f;
{
char	input[BUFSIZ * 4], name[BUFSIZ], *p, *np;
int	i, len;

	while (GetLine (f, input))	/* get line, check whether def'n */
	{
		for (p = input; isspace (*p); p++) { /* nop */ }
		if (*p == '#' || *p == '\0')	/* comment or blank line */
			continue;
		if (isalpha (*p))	/* look for var name */
		{
			np = name;
			while (isalnum (*p) || *p == '_')
				*np++ = *p++;
			*np = '\0';
			while (isspace (*p))	/* skip whitespace */
				++p;
			if (*p++ == '=')	/* it's a definition */
			{
				/* skip leading/trailing whitespace */
				while (isspace (*p))
					++p;
				len = strlen (p);
				while (len > 0 && isspace (p[len-1]))
					p[--len] = '\0';
				AddDef (name, p, 1);
				/* find var refs in value */
				FindAllVarRefs (p);
				continue;
			}
		}
		/* not a definition; find var refs anywhere in line */
		FindAllVarRefs (input);
	}
}


/*
	Find definition by variable name.
*/

Def *FindDefByVar (s)
char	*s;
{
Def	*dp;

	for (dp = defList; dp != NULL; dp = dp->nextDef)
	{
		if (strcmp (dp->var, s) == 0)
			return (dp);
	}
	return (NULL);
}


/*
	Add a definition.  If the name hasn't been seen yet, create a new
	definition on the list.  If the name has been seen, and replace is
	non-zero, replace the current value with the new one.  (replace will
	be zero if we're just adding a variable which is known by its being
	referenced somewhere.)
*/

AddDef (name, value, replace)
char	*name, *value;
int	replace;
{
Def	*dp;

	if ((dp = FindDefByVar (name)) == NULL)
	{
		if ((dp = (Def *) calloc (1, sizeof (Def))) == NULL)
			panic ("AddDef: out of memory");
		dp->var = NewString (name);
		dp->val = NewString (value);
		dp->simple = 0;		/* assume not */
		dp->nextDef = defList;
		defList = dp;
		++nDefs;
	}
	else if (replace)
	{
		free (dp->val);
		dp->val = NewString (value);
	}
}


/*
	Replace instances of '$$' with a single ^A.
*/

EncodeEscapes ()
{
Def	*dp;
char	*p, *q;

	for (dp = defList; dp != NULL; dp = dp->nextDef)
	{
		for (p = q = dp->val; *p != '\0'; p++, q++)
		{
			*q = *p;
			if (*p == '$' && *(p+1) == '$')
			{
				*q = '\01';
				p++;
			}
		}
		*q = '\0';
	}
}


/*
	Replace instances of ^A with a '$'.
*/

DecodeEscapes ()
{
Def	*dp;
char	*p;

	for (dp = defList; dp != NULL; dp = dp->nextDef)
	{
		for (p = dp->val; *p != '\0'; p++)
		{
			if (*p == '\01')
				*p = '$';
		}
	}
}


/*
	Find variable reference in variable value.  begin is set to
	the index of the '$' and end is set to the index of the closing
	delimiter.  (If either is NULL, it is not set.)

	recogEsc is non-zero (true) if '$$' is recognized as an escaped '$'
	and skipped.  It will be true during initial searching for var refs
	and while substituting in source files, false while expanding variable
	values.

	$v, ${}, ${1}, ${v), etc. are not accepted as valid and are ignored.
*/

FindVarRef (s, beg, end, recogEsc)
char	*s;
int	*beg, *end, recogEsc;
{
int	i;
char	c, delim;

	i = 0;
	for (;;)
	{
		while ((c = s[i]) != '\0')
		{
			if (c == '$')
			{
				if ((c = s[i+1]) == '{' || c == '(')
					break;
				if (recogEsc && c == '$')	/* escaped $ */
					++i;
			}
			++i;
		}
		if (c == '\0')
			return (0);	/* no reference */
		if (beg != (int *) NULL)
			*beg = i;
		if (s[++i] == '{')
			delim = '}';
		else
			delim = ')';
		++i;
		if (!isalpha (s[i]))	/* must have at least one char, must */
			continue;	/* begin with letter */
		while ((c = s[++i]) != '\0')
		{
			if (c == delim)	/* find matching delim */
			{
				if (end != (int *) NULL)
					*end = i;
				return (1);
			}
			if (!isalnum (c) && c != '_')
				break;
		}
	}
}


FindAllVarRefs (s)
char	*s;
{
char	name[BUFSIZ];
int	begin, end;

	while (FindVarRef (s, &begin, &end, true))
	{
		/* add with empty value if unknown, */
		/* but don't replace value if known */
		strncpy (name, s + begin + 2, end - begin - 2);
		name[end-begin-2] = '\0';
		AddDef (name, "", 0);
		s += end + 1;
	}
}


/*
	Pull out a variable reference, skipping leading '$(' or '${' and
	trailing ')' or '}'.
*/

YankVarRef (dst, src, begin, end)
char	*dst, *src;
int	begin, end;
{
	strncpy (dst, src + begin + 2, end - begin - 2);
	dst[end - begin - 2] = '\0';
}


/*
	Look for variable references in a variable value and expand them
	when possible.  If a variable is referenced but never defined, it
	disappears.  If a variable is referenced, but its value has not itself
	been fully expanded, defer expansion of value until another pass, at
	which time the referenced variable might then be expanded.  This
	prevents infinite expansions on circular references.
*/

Expand (dp)
Def	*dp;
{
Def	*dp2;
/* partain: made 4 * BUFSIZ
char	buf[BUFSIZ];
*/
char	buf[4 * BUFSIZ];
int	begin, end, vn;

	while (FindVarRef (dp->val, &begin, &end, false))
	{
		YankVarRef (buf, dp->val, begin, end);
		if ((dp2 = FindDefByVar (buf)) == NULL)
		{
			fprintf (stderr, "Expand error: can't find %s, ", buf);
			panic ("quitting");
		}
		if (!dp2->simple)	/* can't expand, give up for now */
			break;
		dp->val[begin] = '\0';
		sprintf (buf, "%s%s%s", dp->val, dp2->val, &(dp->val)[end+1]);
		free (dp->val);
		dp->val = NewString (buf);
		++changes;
	}
	dp->simple = !FindVarRef (dp->val, NULL, NULL, false);
}


/*
	Read through file, performing substitutions for any variables
	known from Makefile.  If a variable reference is found that is
	for an unknown variable, leave it alone, as it may be a reference
	to a real variable in a shell script.
*/

DoSub (f)
FILE	*f;
{
Def	*dp;
char	input[BUFSIZ], var[BUFSIZ], *p;
int	begin, end;

	while (fgets (input, sizeof (input), f) != NULL)
	{
		p = input;
		while (FindVarRef (p, &begin, &end, true))
		{
			write (1, p, begin);	/* write prefix */
			YankVarRef (var, p, begin, end);
			/* if var is known from makefile, write */
			/* value, else just echo the reference */
			if ((dp = FindDefByVar (var)) != NULL)
				write (1, dp->val, strlen (dp->val));
			else
				write (1, p + begin, end - begin + 1);
			p += end + 1;
		}
		write (1, p, strlen (p));
	}
}


/*
	Get next line from Makefile (combines continuation lines into one).
	No overflow checking, oops.
*/

GetLine (f, lbuf)
FILE	*f;
char	*lbuf;
{
char	buf[BUFSIZ];
int	loop = 1, haveLine = 0, len;

	*lbuf = '\0';
	while (loop && fgets (buf, sizeof (buf), f) != NULL)
	{
		loop = 0;
		haveLine = 1;
		len = strlen (buf);
		if (len > 0 && buf[len-1] == '\n')	/* trim newline */
			buf[--len] = '\0';
		if (len > 0 && buf[len-1] == '\\')	/* need continuation */
		{
			loop = 1;
			buf[--len] = ' ';
		}
		strcat (lbuf, buf);
	}
	return (haveLine);
}


/*
	Allocate space for a string, copy the string into it, and return
	a pointer to the copy.
*/


char	*NewString (s)
char	*s;
{
char	*p;

	if ((p = calloc (1, strlen (s) + 1)) == NULL)
		panic ("NewString: out of space");
	return (strcpy (p, s));
}


panic (s)
char	*s;
{
	fprintf (stderr, "%s\n", s);
	exit (1);
}

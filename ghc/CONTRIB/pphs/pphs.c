				/* pphs - a pretty printer for Haskell code */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAXLINELENGTH 256

enum face {KW, ID, IS, SU, ST, CO, NU, MA, SP, LC, RC, CR, BF, FQ, EQ, DQ, QD, EE, DC, DP, CP, LE, GE, LA, RA, RR, TI, BE};
				/* Possible values of typeface */

int widecolons = 0;		/* User may want space between double colons */
int subscripts = 0;		/* User may want subscripts after '_' in identifiers */
int tablength = 8;		/* User's input file tablength */

typedef struct ElementType_Tag { /* Basic storage unit */
  char chars[MAXLINELENGTH];	/* Characters */
  enum face typeface[MAXLINELENGTH]; /* Typefaces */
  int indentation, length, col;	/* Indentation level, non-empty length, column level */
} ElementType;

typedef struct StackNodeType_Tag *Link; /* Stack-related types */
typedef struct StackNodeType_Tag {
  ElementType Element;		/* Stack item */
  Link Next;			/* Link to next node */
} StackNodeType;
typedef StackNodeType *StackNodePtr;
typedef StackNodePtr StackType;

typedef int QueueSizeType;	/* Queue-related types */
typedef struct QueueNodeType_Tag *Connection;
typedef struct QueueNodeType_Tag {
  ElementType Element;		/* Queue item */
  Connection Next;		/* Link to next node */
} QueueNodeType;
typedef QueueNodeType *QueueNodePtr;
typedef struct QueueType_Tag {
  QueueNodePtr Front, Rear;
  QueueSizeType Length;
} QueueType;

FILE *ifptr;			/* input file pointer */

				/* * * STACK FUNCTIONS * * */
StackType
  CreateStack()			/* Returns an empty stack */
{
  return(NULL);
}

int
  IsEmptyStack(s)		/* Returns 1 if s is empty, 0 otherwise */
StackType s;
{
  return(s == NULL);
}

StackType
  Push(s, x)			/* Returns stack with x pushed onto s */
StackType s;
ElementType x;
{
  StackType p;

  p = (StackNodeType *) malloc(sizeof(StackNodeType));
  if (p == NULL) {
    fprintf(stderr, "pphs: Stack is too big\n");
    exit(3);
  }
  else {
    (*p).Element = x;
    (*p).Next = s;
    return(p);
  }
}

ElementType
  Top(s)			/* Returns value of top element in s */
StackType s;
{
  return((*s).Element);
}

StackType
  Pop(s)		        /* Returns stack with top element of s popped off */
StackType s;
{
  StackType t;

  t = (*s).Next;
  free(s);
  return(t);
}

StackType
  PopSym(s)	      /* Returns stack with top element of s popped off without freeing */
StackType s;
{
  StackType t;

  t = (*s).Next;
/* free(s); As PopSym is called within a function, free would free space needed later */
  return(t);
}
				/* * * QUEUE FUNCTIONS * * */
QueueType
  CreateQueue()			/* Returns an empty queue */
{
  QueueType q;

  q.Front = NULL;
  q.Rear = NULL;
  q.Length = 0;
  return(q);
}

int
  IsEmptyQueue(q)		/* Returns 1 if q is empty, 0 otherwise */
QueueType q;
{
  return(q.Front == NULL);
}

int
  LengthOfQueue(q)		/* Returns length of q */
QueueType q;
{
  return(q.Length);
}

QueueNodePtr
  FrontOfQueue(q)		/* Returns pointer to front of q */
QueueType q;
{
  return(q.Front);
}

QueueNodePtr
  RearOfQueue(q)		/* Returns pointer to rear of q */
QueueType q;
{
  return(q.Rear);
}

QueueType
  AddToQueue(q, x)		/* Adds item x to rear of queue q */
QueueType q;
ElementType x;
{
  QueueNodePtr p;

  p = (QueueNodeType *) malloc(sizeof(QueueNodeType));
  if (p == NULL) {
    fprintf(stderr, "pphs: Queue is too big\n");
    exit(4);
  }
  else {
    (*p).Element = x;
    (*p).Next = NULL;
    if (q.Front == NULL)
      q.Front = p;
    else
      (*(q.Rear)).Next = p;
    q.Rear = p;
    q.Length++;
    return(q);
  }
}

QueueType
  TakeFromQueue(q)		/* Removes front item from queue */
QueueType q;
{
  QueueNodePtr p;

  if (q.Front == NULL) {
    fprintf(stderr, "pphs: Stack underflow\n");
    exit(5);
  }
  else {
    p = q.Front;
    q.Front = (*(q.Front)).Next;
    if (q.Front == NULL)
      q.Rear = NULL;
    q.Length--;
    free(p);
    return(q);
  }
}
				/* * * TYPEFACE FUNCTIONS * * */
int
  IsMathsChar(c)		/* Returns 1 if c is a character to be in maths */
char c;
{
  return((c == '[') || (c == ']') || (c == '/') || (c == ',') || (c == '!')
	 || (c == ':') || (c == ';') || (c == '(') || (c == ')') || (c == '&')
	 || (c == '#') || (c == '+') || (c == '-') || (c == '<') || (c == '>')
	 || (c == '{') || (c == '}') || (c == '=') || (c == '|') || (c == '\'')
	 || (c == '^'));	 
}

ElementType
  ChangeTypeface(store, length, finish, tf) /* Changes the typeface to tf in store
					       for length until finish */
ElementType store;
int length, finish;
enum face tf;
{
  int counter;

  for (counter = (finish - length); counter < finish; counter++)
    store.typeface[counter] = tf;
  return(store);
}

ElementType
  CheckForDoubleChar(store, position) /* Checks for double character
					 in store.chars[position - 2..position - 1],
					 if found alters typeface */
ElementType store;
int position;
{
  if ((position >= 2) && (store.typeface[position - 2] != DC)) {
    if ((store.chars[position - 2] == '-') && (store.chars[position - 1] == '-')) {
      store.typeface[position - 2] = LC; /* Haskell "--" line comment */
      store.typeface[position - 1] = LC;
    }
    else if ((store.chars[position - 2] == '{') && (store.chars[position - 1] == '-')) {
      store.typeface[position - 2] = RC; /* Haskell "{-" regional comment begin */
      store.typeface[position - 1] = DC;
    }
    else if ((store.chars[position - 2] == '-') && (store.chars[position - 1] == '}')) {
      store.typeface[position - 2] = CR; /* Haskell "-}" regional comment end */
      store.typeface[position - 1] = DC;
    }
    else if ((store.chars[position - 2] == '+') && (store.chars[position - 1] == '+')) {
      store.typeface[position - 2] = DP; /* Double plus */
      store.typeface[position - 1] = DC;
    }
    else if ((store.chars[position - 2] == ':') && (store.chars[position - 1] == '+')) {
      store.typeface[position - 2] = CP; /* Colon plus */
      store.typeface[position - 1] = DC;
    }
    else if ((store.chars[position - 2] == '<') && (store.chars[position - 1] == '=')) {
      store.typeface[position - 2] = LE; /* Less than or equal to */
      store.typeface[position - 1] = DC;
    }
    else if ((store.chars[position - 2] == '>') && (store.chars[position - 1] == '=')) {
      store.typeface[position - 2] = GE; /* Greater than or equal to */
      store.typeface[position - 1] = DC;
    }
    else if ((store.chars[position - 2] == '<') && (store.chars[position - 1] == '-')) {
      store.typeface[position - 2] = LA; /* Leftarrow */
      store.typeface[position - 1] = DC;
    }
    else if ((store.chars[position - 2] == '-') && (store.chars[position - 1] == '>')) {
      store.typeface[position - 2] = RA; /* Rightarrow */
      store.typeface[position - 1] = DC;
    }
    else if ((store.chars[position - 2] == '=') && (store.chars[position - 1] == '>')) {
      store.typeface[position - 2] = RR; /* Double rightarrow */
      store.typeface[position - 1] = DC;
    }
    else if (((store.chars[position - 2] == '*') && (store.chars[position - 1] == '*'))
	     || ((store.chars[position - 2] == '^') && (store.chars[position - 1] == '^'))) {
      store.typeface[position - 2] = MA; /* Exponent, ie not Times */
      store.typeface[position - 1] = MA;
    }
  }
  return(store);
}

int
  IsHaskellPunc(c)     /* Returns 1 if c is a punctuation mark not part of identifier */
char c;
{
  return((c == ' ') || (c == ',') || (c == '@') || (c == '#') || (c == '$')
	 || (c == '%') || (c == '&') || (c == '*') || (c == '(') || (c == ')')
	 || (c == '-') || (c == '+') || (c == '=') || (c == '\\') || (c == '|')
	 || (c == '[') || (c == ']') || (c == '{') || (c == '}') || (c == ':')
	 || (c == ';') || (c == '"') || (c == '~') || (c == '?') || (c == '/')
	 || (c == '<') || (c == '>') || (c == '^'));
}

int
  IsKeyWord(str)		/* Returns 1 if str is a keyword to be in keyword font */
char str[MAXLINELENGTH];
{
  return((!(strcmp(str, "case"))) || (!(strcmp(str, "class")))
	 || (!(strcmp(str, "data"))) || (!(strcmp(str, "default")))
	 || (!(strcmp(str, "deriving"))) || (!(strcmp(str, "else")))
	 || (!(strcmp(str, "hiding"))) || (!(strcmp(str, "if")))
	 || (!(strcmp(str, "import"))) || (!(strcmp(str, "in")))
	 || (!(strcmp(str, "infix"))) || (!(strcmp(str, "infixl")))
	 || (!(strcmp(str, "infixr"))) || (!(strcmp(str, "instance")))
	 || (!(strcmp(str, "interface"))) || (!(strcmp(str, "let")))
	 || (!(strcmp(str, "module"))) || (!(strcmp(str, "of")))
	 || (!(strcmp(str, "renaming"))) || (!(strcmp(str, "then")))
	 || (!(strcmp(str, "to"))) || (!(strcmp(str, "type")))
	 || (!(strcmp(str, "where"))));
}

int
  KeyWord(c, store, position) /* Returns length of keyword if a keyword ends
				       at store.chars[position - 1] */
char c;
ElementType store;
int position;
{
  int counter, start, end = position - 1, keywordlen = 0;
  char str[MAXLINELENGTH];
  
  if ((!isalpha(c)) && (c != '_') && (c != '\'') && (position)) {
    for (counter = end; (counter >= 0) && ((isalpha(store.chars[counter]))
					   || (c == '_') || (c == '\''))
	                               && (counter >= store.indentation); counter--) {
      ;	 /* Just count letters */
    }
    start = ++counter;
    for (counter = 0; counter + start <= end; counter++) {
      str[counter] = store.chars[counter + start]; /* Copy letters into str */
    }
    str[counter] = '\0'; /* Add null character to end */
    if (IsKeyWord(str))		/* Checks word in str is keyword */
      keywordlen = strlen(str);	/* and measures it */
  }
  return(keywordlen);
}

ElementType
  CheckForKeyword(c, store, position) /* Returns store with any possible keyword
					 ending at store.chars[position - 1]
					 identified as such in store.typeface */
char c;
ElementType store;
int position;
{
  if (KeyWord(c, store, position))
    store = ChangeTypeface(store, KeyWord(c, store, position), position, KW);
  return(store);
}

int
  IsNumber(c, store, position, statesok) /* Returns 1 if c forms part of a number */
char c;
ElementType store;
int position, statesok;
{
  int counter, foundident = 0, foundpunc = 0;

  if (((isdigit(c)) || (c == 'e') || (c == 'E') || (c == '|') || (c == '.'))
      && (statesok)) {
    counter = position - 1;
    while ((isdigit(store.chars[counter])) && (counter >= 0))
      counter--;
    if (((store.chars[counter] == '+') || (store.chars[counter] == '-'))
	&& ((store.chars[counter - 1] == 'e') || (store.chars[counter - 1] == 'E'))
	&& (counter > 2))
      counter -= 2;
    else if (((store.chars[counter] == 'e') || (store.chars[counter] == 'E'))
	     && (counter > 1))
      counter--;
    while ((isdigit(store.chars[counter])) && (counter >= 0))
      counter--;
    if ((store.chars[counter] == '.') && (counter > 1))
      counter--;
    while ((isdigit(store.chars[counter])) && (counter >= 0))
      counter--;
    if ((isalpha(store.chars[counter])) && (counter >= 0))
      foundident = 1;		/* ie not number */
    else if ((IsHaskellPunc(store.chars[counter])) || (counter < 0))
      foundpunc = 1; /* ie is number */
  }
  return(foundpunc);
}
				/* * * LINE SELECTION FUNCTIONS * * */
ElementType
  SelectSkipLine(s, store, linecounter)	/* Returns store containing line for skipover */
StackType s;
ElementType store;
int linecounter;
{
  ElementType temp;
  int counter;
  
  if (!(IsEmptyStack(s))) {
    while (((Top(s)).length <= linecounter) || ((Top(s)).indentation >= linecounter)) {
      temp = Top(s);
      s = PopSym(s);
      if (IsEmptyStack(s)) {
	counter = temp.length;
	while (counter < linecounter) {
	  temp.chars[counter] = ' ';
	  temp.typeface[counter++] = SP;
	}
	temp.chars[counter] = '\0'; /* Add null character to end */
	s = Push(s, temp);
	break;
      }
    }
    store = Top(s);
  }
  else {			/* Stack is empty */
    counter = store.length;
    while (counter < linecounter) {
      store.chars[counter] = ' ';
      store.typeface[counter++] = SP;
    }
    store.chars[counter] = '\0'; /* Add null character to end */
  }
  return(store);
}
				/* * * STORING FUNCTIONS * * */
ElementType
  CreateStore()			/* Returns an empty store */
{
  ElementType store;

  strcpy(store.chars, "");
  store.length = 0;
  store.indentation = 0;
  store.col = 0;
  return(store);
}

ElementType
  StoreSpace(store, position) /* Stores a space in the store at current position */
ElementType store;
int position;
{
  store.chars[position] = ' ';
  store.typeface[position] = SP;
  return(store);
}
				/* * * WRITING FUNCTIONS * * */
void
  WriteStartFace(tf)		/* Writes LaTeX typeface commands for start of section */
enum face tf;
{
  if (tf == KW)			/* Keywords */
    printf("{\\keyword ");
  else if ((tf == ID) || (tf == IS)) /* Identifiers */
    printf("{\\iden ");
  else if (tf == ST)		/* Strings */
    printf("{\\stri ");
  else if (tf == CO)		/* Comments */
    printf("{\\com ");
  else if (tf == NU)		/* Numbers */
    printf("{\\numb ");
  else if ((tf == MA) || (tf == TI)) /* Various maths */
    printf("$");
}

void
  WriteFinishFace(tf)		/* Writes LaTeX typeface commands for end of section */
enum face tf;
{
  if ((tf == KW) || (tf == ID) || (tf == ST) || (tf == CO)
      || (tf == NU)) /* Keywords, identifiers, strings, comments or numbers */
    printf("\\/}");
  else if ((tf == MA) || (tf == TI)) /* Various maths */
    printf("$");
  else if (tf == IS) /* Subscripts in identifiers */
    printf("\\/}$");
}

int
  WriteSpaces(store, counter, finish) /* Writes consecutive spaces,
					 returning new counter value */
ElementType store;
int counter, finish;
{
  int spaces = 0;		/* The number of spaces found */
  
  for (; (store.typeface[counter] == SP) && (counter < finish); counter++)
    spaces++;
  printf("\\xspa{%d}", spaces);
  return(--counter);
}

int
  WriteChar(store, counter, finish)	/* Writes charater, returning new counter value */
ElementType store;
int counter, finish;
{
  if (store.typeface[counter] == SP) /* Space */
    printf("\\xspa1");	/* Redundant */
  else if (store.typeface[counter] == BE) /* Bar under equals sign */
    printf("\\bareq");
  else if (store.typeface[counter] == DP) { /* Double plus */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("\\plusplus");
      counter++;
    }
  }
  else if (store.typeface[counter] == CP) { /* Colon plus */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("{:}{+}");
      counter++;
    }
  }
  else if (store.typeface[counter] == LE) { /* Less than or equal to */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("$\\leq$");
      counter++;
    }
  }
  else if (store.typeface[counter] == GE) { /* Greater than or equal to */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("$\\geq$");
      counter++;
    }
  }
  else if (store.typeface[counter] == LA) { /* Leftarrow */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("$\\leftarrow$");
      counter++;
    }
  }
  else if (store.typeface[counter] == RA) { /* Rightarrow */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("$\\rightarrow$");
      counter++;
    }
  }
  else if (store.typeface[counter] == RR) { /* Double rightarrow */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("$\\Rightarrow$");
      counter++;
    }
  }
  else if (store.typeface[counter] == RC) { /* Regional comment begin */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("{\\com \\{-\\/}");
      counter++;
    }
    else
      printf("{\\com \\{\\/}");
  }
  else if (store.typeface[counter] == CR) { /* Regional comment end */
    if ((counter < finish - 1) && (store.typeface[counter + 1] == DC)) {
      printf("{\\com -\\}\\/}");
      counter++;
    }
    else
      printf("{\\com -\\/}");
  }
  else if ((store.typeface[counter] == LC) && (store.chars[counter] == '-'))
    printf("{\\rm -}");	/* Comment - problem: "--" becomes "-" in LaTeX so fix done */
  else if (store.chars[counter] == '\\')
    printf("\\hbox{$\\setminus$}"); /* Backslash */
  else if (store.chars[counter] == '*') {
    if (store.typeface[counter] == TI)
      printf("\\times ");	/* Multiplication */
    else
      printf("*");		/* Other star symbols, eg Exponent */
  }
  else if ((store.chars[counter] == '_') && (store.typeface[counter] == SU)) {
      if ((counter < finish - 1) && (store.typeface[counter + 1] == IS))
	printf("$_");		/* Subscript character */
  }
  else if (store.chars[counter] == '^')
    printf("\\char'136 ");	/* Up-arrow */
  else if (store.chars[counter] == '~')
    printf("\\char'176 ");	/* Tilda */
  else if ((store.chars[counter] == ':') && (store.chars[counter - 1] == ':')
	   && (widecolons))
    printf("\\,:");	/* Double colon */
  else if (store.chars[counter] == '"') {
    if ((counter) && ((store.chars[counter - 1] == '"')
		      || (store.chars[counter - 1] == '\'')))
      printf("\\,");	/* If previous character was a quote, leave a little space */
    if (store.typeface[counter] == DQ)
      printf("{\\rm ``}");	/* Open doublequote */
    else if (store.typeface[counter] == QD)
      printf("{\\rm \"}");	/* Close doublequote */
    else
      printf("{\\rm \\char'175}"); /* Escape doublequote in string */
  }
  else if (store.chars[counter] == '\'') {
    if ((counter) && ((store.chars[counter - 1] == '"')
		      || ((store.chars[counter - 1] == '\'')
			  && ((store.typeface[counter - 1] != MA)
			      || (store.typeface[counter] != MA)))))
      printf("\\,");    /* If previous character was a quote, leave a little space
			   except when it's a double prime */
    if (store.typeface[counter] == FQ)
      printf("\\forquo ");	/* Forward single quote */
    else if (store.typeface[counter] == EQ)
      printf("\\escquo ");	/* Escape single quote */
    else if (store.typeface[counter] == BF) {
      if ((counter + 1 < store.length) && (store.typeface[counter + 1] == BF)
	  && (counter + 1 != store.indentation)) {
	printf("{\\com \'\'\\/}"); /* Closing LaTeX style quote */
	counter++;
      }
      else
	printf("{\\com \'\\/}"); /* Single quote following backquote in comment */
    }
    else
      printf("\'");		/* Prime */
  }
  else if (store.chars[counter] == '{')
    printf("\\hbox{$\\cal \\char'146$}"); /* Open curly bracket */
  else if (store.chars[counter] == '}')
    printf("\\hbox{$\\cal \\char'147$}"); /* Close curly bracket */
  else if ((counter) && (store.chars[counter - 1] == '[') && (store.chars[counter] == ']'))
    printf("\\,]");		/* Leave small gap between adjacent square brackets */
  else if ((store.chars[counter] == '$') || (store.chars[counter] == '%')
	   || (store.chars[counter] == '_') || (store.chars[counter] == '#')
	   || (store.chars[counter] == '&')) /* Various characters needing '\' for LaTeX */
    printf("\\%c", store.chars[counter]);
  else				/* Other characters */
    printf("%c", store.chars[counter]);
  return(counter);
}

void
  WriteSkipover(store) /* Writes the skipover portion of line in store */
ElementType store;
{
  int counter = 0;

  printf("\\skipover{");	/* Write opening LaTeX skipover command */
  WriteStartFace(store.typeface[counter]); /* Write opening LaTeX typeface command */
  if (store.typeface[counter] == SP)
    counter = WriteSpaces(store, counter, store.indentation); /* Write spaces */
  else
    counter = WriteChar(store, counter, store.indentation); /* Write character */
  for (counter++; counter < store.indentation; counter++){ /* until end of skipover */
    if (store.typeface[counter - 1] != store.typeface[counter]) { /* If typeface change */
      WriteFinishFace(store.typeface[counter - 1]); /* write closing typeface command */
      WriteStartFace(store.typeface[counter]); /* write opening LaTeX typeface command */
    }	    
    if (store.typeface[counter] == SP)
      counter = WriteSpaces(store, counter, store.indentation); /* Write spaces */
    else
      counter = WriteChar(store, counter, store.indentation); /* Write character */
  }
  if (store.typeface[counter - 1] == SU)
    ;		/* If indentation is under subscript don't open math section */
  else
    WriteFinishFace(store.typeface[counter - 1]); /* Write closing LaTeX typeface command */
  printf("}");			/* Write closing LaTeX skipover command */
}

void
  WriteWords(store)		/* Writes rest of line, starting at indentation level */
ElementType store;
{
  int counter = store.indentation;
  int intabular = 0;	       /* Boolean: is in tabular section for internal alignment */

  WriteStartFace(store.typeface[counter]); /* Write opening LaTeX typeface command */
  if (store.typeface[counter] == SP)
    counter = WriteSpaces(store, counter, store.length); /* Write spaces */
  else
    counter = WriteChar(store, counter, store.length); /* Write character */
  for (counter++; counter < store.length; counter++){ /* until end of word */
    if ((store.col) && (store.col == counter)) {
      printf(" & ");
      if (store.chars[counter - 1] == ':')
	printf("$:");
      intabular = 1;
    }
    if (store.typeface[counter - 1] != store.typeface[counter]) /* If typeface change */
      WriteFinishFace(store.typeface[counter - 1]); /* Write closing typeface command */
    if ((store.typeface[counter] == SP) && (intabular)) {
      printf(" & ");
      intabular = 0;
    }
    if ((store.typeface[counter - 1] != store.typeface[counter]) /* If typeface change */
	&& ((store.chars[counter] != ':') || (store.col != counter + 1)))
      WriteStartFace(store.typeface[counter]); /* Write opening LaTeX typeface command */
    if (store.typeface[counter] == SP)
      counter = WriteSpaces(store, counter, store.length); /* Write spaces */
    else if ((store.chars[counter] != ':') || (!store.col) || (store.col != counter + 1))
      counter = WriteChar(store, counter, store.length); /* Write character */
  }
  WriteFinishFace(store.typeface[counter - 1]); /* Write closing LaTeX typeface command */
}

void
  WriteLine(store, needed)	/* Writes the line in store,
				   only writing LaTeX newline if needed */
ElementType store;
int needed;
{
  if (store.indentation)
    WriteSkipover(store);
  if (store.indentation < store.length)
    WriteWords(store);
  if (needed)
    printf("\\\\");		/* LaTeX newline character */
  printf("\n");
}

QueueType
  WriteQueue(q)			/* Writes lines, removing them from queue,
				   leaves last line in queue if not in tabular section */
QueueType q;
{
  int intabular = 0;

  if ((!(IsEmptyQueue(q))) && ((*(FrontOfQueue(q))).Element.col)) {
    printf("\\begin{tabular}{@{}l@{\\xspa1}c@{}l}\n");
    intabular = 1;
  }
  while (LengthOfQueue(q) > !intabular) {
    WriteLine((*(FrontOfQueue(q))).Element, 1);	/* LaTeX newline character is needed */
    q = TakeFromQueue(q);
  }
  if (intabular)
    printf("\\end{tabular}\\\\\n");
  return(q);
}

QueueType
  WriteRestOfQueue(q)		/* Writes all lines, removing them from queue,
				   doesn't have LaTeX newline after last line */
QueueType q;
{
  int intabular = 0;

  if ((!(IsEmptyQueue(q))) && ((*(FrontOfQueue(q))).Element.col)) {
    printf("\\begin{tabular}{@{}l@{\\xspa1}c@{}l}\n");
    intabular = 1;
  }
  while (!(IsEmptyQueue(q))) {
    WriteLine((*(FrontOfQueue(q))).Element, (LengthOfQueue(q) > 1)); /* Last line doesn't
							   need LaTeX newline character */
    q = TakeFromQueue(q);
  }
  if (intabular) {
    printf("\\end{tabular}");
    if (!IsEmptyQueue(q))	/* Last line doesn't need LaTeX newline character */
      printf("\\\\");
    printf("\n");
  }
  return(q);
}

int
main (argc, argv)		/* * * MAIN PROGRAM * * */
     int argc;
     char *argv[];
{
  int tripped = 1, instring = 0, instringincomment = 0, inlinecomment = 0;
  int incharquote = 0, incharquoteincomment = 0, inbackquoteincomment = 0;
  int insub = 0;
  /* Booleans - just taken new line, in string, in string inside comment, in line comment,
     in character quote, in character quote inside comment, in backquote inside comment,
     in subscript */
  int linecounter = 0, indentcounter = 0, inregcomment = 0, pos;
  /* Counters: current position on line, indentation of current line,
     nesting level of regional comments, position marker */
  char c;			/* Character */
  StackType s;			/* Stack of previous longest lines */
  QueueType q;			/* Queue of lines waiting to be printed */
  ElementType store;		/* Store of letters, typefaces and non-empty length */

  if ((argc == 3) && (argv[1][0] == '-')) { /* If options specified with call */
    if (strstr(argv[1], "s"))	/* if -s option, subscripts in identifiers wanted */
      subscripts = 1;
    if (strstr(argv[1], "t")) {	/* if -tX option, tab characters are X spaces */
      for (pos = 1; (argv[1][pos] != 't'); pos++) /* find 't' */
	;
      for (pos++, tablength = 0; isdigit(argv[1][pos]); pos++) /* read number */
	tablength = (tablength * 10) + (argv[1][pos] - '0');
    }
    if (strstr(argv[1], "w"))	/* if -w option called, wide double colons wanted */
      widecolons = 1;
  }
  else if (argc == 2)		/* If no options */
    ;
  else {			/* If not called with pphs and a filename */
    fprintf(stderr, "pphs: Call with one file name\n");
    exit(1);
  }
  
  if ((strcspn(argv[argc - 1], ".") == strlen(argv[argc - 1])) /* If filename has no extention */
      && ((ifptr = fopen(argv[argc - 1], "r")) == NULL)) /* and no plain file of that name */
    strcat(argv[argc - 1], ".hs"); /* add a ".hs" extention */
  if ((ifptr = fopen(argv[argc - 1], "r")) == NULL) { /* Open input file */
    fprintf(stderr, "pphs: File could not be opened\n"); /* eg isn't there */
    exit(2);
  }
  else {
    
    printf("\\begin{tabbing}\n"); /* Start of Haskell program */
    
    store = CreateStore();	/* an empty one */
    s = CreateStack();		/* an empty one */
    q = CreateQueue();		/* an empty one */
    
    fscanf(ifptr, "%c", &c);	/* Read character */
    while (!feof(ifptr)) {	/* While not at end of input file */
      while ((isspace(c)) && (!(feof(ifptr)))) { /* Read blank characters */
	if (c == ' ') {
	  if (tripped)
	    linecounter++;	/* Count leading spaces */
	  else {		/* or */
	    store = StoreSpace(store, linecounter++); /* Store intermediate
							 or trailing space */
	    if (store.length < linecounter)
	      store.chars[linecounter] = '\0'; /* Add null character to end */
	  }
	  fscanf(ifptr, "%c", &c); /* Read next character */
	}
	else if (c == '\t') {
	  if (tripped)
	    linecounter += (tablength - (linecounter % tablength));
	  else {
	    store = StoreSpace(store, linecounter++);
	    for (; linecounter % tablength; linecounter++)
	      store = StoreSpace(store, linecounter);
	    if (store.length < linecounter)
	      store.chars[linecounter] = '\0'; /* Add null character to end */
	  }
	  fscanf(ifptr, "%c", &c); /* Read next character */
	}
	else if (c == '\n') {
	  tripped = 1;		/* Just taken a new line */
	  inlinecomment = 0;
	  if (!(IsEmptyStack(s)))
	    while (((Top(s)).length <= store.length)
		   && ((Top(s)).indentation >= store.length)) {
	      s = Pop(s);
	      if (IsEmptyStack(s))
		break;
	    }
	  if (store.length > 0) { /* Push non-empty line onto indentation stack */
	    store.indentation = indentcounter;
	    s = Push(s, store);
	  }
	  if (!(IsEmptyQueue(q))) {
	    if ((store.col != (*(FrontOfQueue(q))).Element.col)
		|| (!(*(FrontOfQueue(q))).Element.col))
	      q = WriteQueue(q); /* If internal alignment changes or there is none
				    write out lines */
	  }
	  q = AddToQueue(q, store); /* Add to writing queue */
	  linecounter = 0;	/* Get ready to count leading spaces */
	  store.length = linecounter;
	  fscanf(ifptr, "%c", &c); /* Read next character */
	}
	else break;
      }
      if (tripped) {
	indentcounter = linecounter;
	store.indentation = linecounter;
	store.col = 0;
      }
      if ((tripped) && (linecounter)) { /* Skipover necessary for indentation */
	store = SelectSkipLine(s, store, linecounter);
	store.indentation = linecounter;
	store.col = 0;
      }
      if (!feof(ifptr))
	tripped = 0;		/* No longer just taken new line */
      while ((!(isspace(c))) && (!(feof(ifptr)))) { /* Read word */
	if ((linecounter > 1) && (!IsEmptyQueue(q))
	    && ((*(RearOfQueue(q))).Element.length >= linecounter)
	    && (linecounter > store.indentation)
	    && (linecounter > (*(RearOfQueue(q))).Element.indentation)
	    && (store.chars[linecounter - 1] == ' ')
	    && ((((*(RearOfQueue(q))).Element.chars[linecounter - 1] == ' ')
		 && ((c == (*(RearOfQueue(q))).Element.chars[linecounter])
		     || ((c == '=')
			 && ((*(RearOfQueue(q))).Element.chars[linecounter] == ':')
			 && ((*(RearOfQueue(q))).Element.chars[linecounter + 1] == ':'))))
		|| (((*(RearOfQueue(q))).Element.chars[linecounter - 1] == ':')
		    && ((*(RearOfQueue(q))).Element.chars[linecounter] == ':')
		    && (c == '=')))
	    && ((store.chars[linecounter - 2] == ' ')
		|| ((*(RearOfQueue(q))).Element.chars[linecounter - 2] == ' '))
	    && (((*(RearOfQueue(q))).Element.col == 0)
		|| ((*(RearOfQueue(q))).Element.col == linecounter))) {
	  store.col = linecounter; /* Identify any internal alignment */
	  (*(RearOfQueue(q))).Element.col = linecounter;
	}
	if ((c == '"') && (!incharquote) /* String outside comments */
	    && (!inregcomment) && (!inlinecomment)) {
	  if (((linecounter) && (store.chars[linecounter - 1] != '\\'))
	      || (!linecounter))
	    instring = !instring;
	}
	else if ((c == '"') && (!incharquoteincomment) /* String inside comment */
		 && (!inbackquoteincomment)
		 && ((inregcomment) || (inlinecomment))) {
	  if (((linecounter) && (store.chars[linecounter - 1] != '\\'))
	      || (!linecounter))
	    instringincomment = !instringincomment;
	}
	else if ((c == '`') && ((inlinecomment) || (inregcomment))) {
	  if ((linecounter) && (store.chars[linecounter - 1] == '`'))
	    inbackquoteincomment = 2; /* Opening LaTeX style quote in comment */
	  else
	    inbackquoteincomment = !inbackquoteincomment; /* Backquote in comment */
	}
	else if ((linecounter) && (!inlinecomment) && (!instring)) {
	  if ((store.chars[linecounter - 1] == '{') && (c == '-'))
	    inregcomment++;	/* Haskell "{-" regional comment begin */
	  else if ((store.chars[linecounter - 1] == '-') && (c == '}')) {
	    inregcomment--;	/* Haskell "-}" regional comment end */
	    instringincomment = 0;
	    incharquoteincomment = 0;
	    inbackquoteincomment = 0;
	  }
	}
	if (c == '|') {
	  if ((!IsEmptyQueue(q))
	      && ((((*(RearOfQueue(q))).Element.chars[linecounter] == '=')
		   && (linecounter == store.indentation))
		  || ((*(RearOfQueue(q))).Element.typeface[linecounter] == BE)))
	    store.typeface[linecounter] = BE;
	  else
	    store.typeface[linecounter] = MA;
	}
	else if ((c == '\'') && (linecounter) && (store.chars[linecounter - 1] == '\\'))
	  store.typeface[linecounter] = EQ; /* Escape character quote */
	else if ((c == '\'') && (!instring) && (!inregcomment) && (!inlinecomment)) {
	  if (((linecounter) && (store.chars[linecounter - 1] != '\\')
	       && ((IsHaskellPunc(store.chars[linecounter - 1])) || (incharquote)))
	      || (!linecounter)) {
	    incharquote = !incharquote;
	    store.typeface[linecounter] = FQ; /* Character quote */
	  }
	  else
	    store.typeface[linecounter] = MA; /* Prime */
	}
	else if ((c == '\'') && (!instringincomment)
		 && ((inregcomment) || (inlinecomment))) {
	  if (((linecounter) && (store.chars[linecounter - 1] != '\\')
	       && ((IsHaskellPunc(store.chars[linecounter - 1]))
		   || (incharquoteincomment)))
	      || (!linecounter)) {
	    incharquoteincomment = !incharquoteincomment;
	    store.typeface[linecounter] = FQ; /* Character quote in comment */
	  }
	  else if (inbackquoteincomment) {
	    inbackquoteincomment--;
	    store.typeface[linecounter] = BF; /* `x' character quote in comment */
	  }
	  else
	    store.typeface[linecounter] = MA; /* Prime */
	}
	else if (c == '"') {
	  if ((!incharquote) && (!incharquoteincomment) && (!inbackquoteincomment)
	      && ((instring) || (instringincomment))) {
	    if (((linecounter) && (store.chars[linecounter - 1] != '\\'))
		|| (!linecounter))
	      store.typeface[linecounter] = DQ; /* Open doublequote */
	    else if (store.chars[linecounter - 1] == '\\')
	      store.typeface[linecounter] = EE; /* Escape doublequote */
	  }
	  else if ((!incharquote) && (!incharquoteincomment) && (!inbackquoteincomment)) {
	    if (((linecounter) && (store.chars[linecounter - 1] != '\\'))
		|| (!linecounter))
	      store.typeface[linecounter] = QD; /* Close doublequote */
	    else if (store.chars[linecounter - 1] == '\\')
	      store.typeface[linecounter] = EE; /* Escape doublequote */
	  }
	  else
	    store.typeface[linecounter] = EE; /* Character quote of doublequote */
	}
	else if (c == '`') {
	  if ((inlinecomment) || (inregcomment))
	    store.typeface[linecounter] = CO;
	  else
	    store.typeface[linecounter] = MA;
	}
	else if ((linecounter) && (subscripts) && (c == '_')
		 && (store.typeface[linecounter - 1] == ID))
	  store.typeface[linecounter] = SU; /* Subscript in identifier */
	else if (c == '*')
	  store.typeface[linecounter] = TI; /* Times - may be changed by double char */
	else if (IsMathsChar(c))
	  store.typeface[linecounter] = MA; /* Maths characters */
	else if (IsNumber(c, store, linecounter,
			  ((!inregcomment) && (!instring) && (!inlinecomment))))
	  store.typeface[linecounter] = NU; /* Numbers */
	else if ((instring) || (incharquote))
	  store.typeface[linecounter] = ST; /* Characters in strings */
	else if ((inlinecomment) || (inregcomment))
	  store.typeface[linecounter] = CO; /* Characters in comments */
	else {
	  if (insub)
	    store.typeface[linecounter] = IS; /* Subscript identifiers */
	  else
	    store.typeface[linecounter] = ID; /* Others */
	}
	if (linecounter)
	  if ((store.typeface[linecounter - 1] == IS)
	      && (store.typeface[linecounter] != IS))
	    insub = 0;		/* End of subscript identifier */
	store.chars[linecounter++] = c; /* Place character in store */
	if (linecounter > store.indentation + 1)
	  store = CheckForDoubleChar(store, linecounter);
	if ((store.typeface[linecounter - 1] == LC) && (!inregcomment)
	    && (!instring) && (!incharquote)) {
	  instringincomment = 0;
	  incharquoteincomment = 0;
	  inbackquoteincomment = 0;
	  inlinecomment = 1;
	}
	else if ((store.typeface[linecounter - 1] == SU)
		 && (linecounter != store.indentation))
	  insub = 1;
	fscanf(ifptr, "%c", &c); /* Read next character */
	if (feof(ifptr))
	  c = ' ';
	if ((!inregcomment) && (!inlinecomment) && (!instring))
	  store = CheckForKeyword(c, store, linecounter); /* Keywords not in comments or
							     strings to be in keyword typeface */
      }
      insub = 0;
      store.chars[linecounter] = '\0'; /* String terminating null character */
      store.length = linecounter;
    }
    if ((!tripped) && (!store.col)) /* If last line not in internal alignment */
      q = WriteQueue(q);	      /*   write previous lines which might */
    if (!tripped)		/* Put final line in queue if non-empty */
      q = AddToQueue(q, store);
    if (feof(ifptr))		/* Write remaining lines */
      q = WriteRestOfQueue(q);
    
    printf("\\end{tabbing}\n"); /* End of Haskell program */
    
    exit(0);
  }
}

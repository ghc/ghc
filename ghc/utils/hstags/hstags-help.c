#include <stdio.h>
#include <string.h> /* for strlen */

/* typedef enum { False, True } Boolean; */

#define SKIP	/* Algol-68 lives */

main(argc,argv)
int argc;
char **argv;
{
  unsigned line;
  FILE *srcf;
  int thisline = 0, lastline = 0, linestart = 0;
  char linebuff[1024];

  if(argc < 2)
    {
      fprintf(stderr,"usage: %s sourcefile",argv[0]);
      exit(1);
    }

  if((srcf=fopen(argv[1],"r")) == NULL)
    {
      fprintf(stderr,"can't read %s\n",argv[1]);
      exit(2);
    }

  *linebuff = '\0';

  while(scanf("%u",&line)!=EOF)
    {
      if(line != lastline)
	{
	  while(thisline < line && !feof(srcf))
	    {
	      linestart+=strlen(linebuff);
	      fgets(linebuff,1023,srcf);
	      thisline++;
	    }

	  if(thisline >= line)
	    {
	      char *chpos;
	      for(chpos = linebuff; *chpos != '=' && *chpos != '\n' && *chpos != '\0'; ++chpos)
		putchar(*chpos);

	      if(*chpos == '=')
		putchar('=');

	      printf("%c%d,%d\n",0177,line,linestart);
	    }
	  lastline = line;
	}
    }

  fclose(srcf);
  exit(0);
}

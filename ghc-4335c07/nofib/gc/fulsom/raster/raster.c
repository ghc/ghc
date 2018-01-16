/**
*** Converts csg coded output into grey-scale raster
*** ready for input to rawtopgm
***
*** Copyright 1990,1992 Duncan Sinclair
***
*** Now generates "rawbits" ("P5") pgm output.  31/7/90
***
*** Updated for better compatibility with Haskell version 27/7/92
***
**/

/*

Description of new protocol:

Each value is a "Word", which is a two byte value, MSB, then LSB.

Flags, Dimensions, (Location, Value)*

Flags      :   F        (defined below.)
Dimensions :   XX,YY    (If not square.)
           :   S        (If square.)
Location   :   X,Y,H,W  (If not square.)
           :   X,Y,D    (If square.)
Value      :   V        (If mono.)
           :   R,G,B    (If colour.)

Flags: 0x0001 = square
       0x0002 = colour

Background is defined as values unassigned at end.

*/

#include <unistd.h>
#include <stdio.h>
#include <ctype.h>

char *malloc();

int getword();     /* get a word on stdin */

#define FF_SQUARE 0x0001
#define FF_COLOUR 0x0002

struct pixm {
  int r,g,b;
  };

struct pix {
  int info;
  union {
    int v;
    struct pixm rgb;
    } pixun;
  };

main (argc,argv)
int argc;
char *argv[];
  {
  struct pix *raster;    /* Stores raster */
  int F,square,colour;   /* flags stuff */
  int X,Y,xy,x,y,w,h;  /* Abs X,Y,X*Y ; Cur x,y ; Cur w,h */
  int v;                 /* Current shade */
  int r,g,b;             /* Current colour */
  int i,j;               /* Loop counters */
  int n;                 /* Array offset */
  int inc;               /* Incomplete picture */
  int eff=0,occ=0,tot=0; /* Statistics accumulators */
  int back = 0;          /* Background Shade */
  int ac = argc;         /* Arg. counter */
  char **av = argv;      /* Arg. pointer */
  int scale = 0;         /* scaling */
  int z,a;               /* scale counters */

  while (--ac)     /* Do flags backwards! */
    {
    if (access(argv[ac],R_OK) == -1)   /* Is this a file we can open? */
      if (isdigit(argv[ac][0]))          /* is it a value? */
        if (back == 0)                     /* set the background first */
          back = atoi(argv[ac]);           /* back has default value 0 */
        else
          scale = atoi(argv[ac]);
      else
        {
        fputs ("usage : code [scale] [background] [file]\n",stderr);
        exit(99);
        }
    else
      if (freopen(argv[ac],"r",stdin) == NULL)  /* we've got a file */
        {
        fputs ("code : panic can open file/can't open file?",stderr);
	exit(66);
	}
    }
  if (isatty(0))    /* Still a tty?  That's not right! */
    {
    fputs ("raster: can't find input data.\n",stderr);
    exit(2);
    }

  F = getword();
  colour = F & FF_COLOUR;
  square = F & FF_SQUARE;

  X = getword();
  Y = square ? X : getword();
#ifdef DEBUG
  fprintf (stderr,"%d %d \n", X , Y);
#endif DEBUG
  xy = X*Y;
  raster = (struct pix *)malloc(sizeof(struct pix) * xy);
  while ((x = getword()) != EOF)
    {
    inc = 1;     /* incomplete flag ON */
    if ((y = getword()) == EOF) break;
    if ((w = getword()) == EOF) break;
    if ((h = (square ? w : getword()) ) == EOF) break;
    if (colour) {
      if ((r = getword()) == EOF) break;
      if ((g = getword()) == EOF) break;
      if ((b = getword()) == EOF) break;
      }
    else
      if ((v = getword()) == EOF) break;
    if ((x+w>X) || (y+h>Y)) break;
    inc = 0;     /* incomplete flag OFF */
    eff++;
    for (i=0;i<w;i++)
      {
      for (j=0;j<h;j++)
        {
        n = i+x+((Y-(1+(j+y)))*X);     /* Is this right? */
        raster[n].info++;
        if (colour) {
          raster[n].pixun.rgb.r = r;
          raster[n].pixun.rgb.g = g;
          raster[n].pixun.rgb.b = b;
          }
        else
          raster[n].pixun.v = v;
        }
      }
    }
  if (inc) fputs("Picture incomplete?\n",stderr);
  for (n=0;n<(xy);n++)
    {
    tot += raster[n].info;
    if (raster[n].info != 0)
      occ++;
    else
      {
      if (colour) {
        raster[n].pixun.rgb.r = back;
        raster[n].pixun.rgb.g = back;
        raster[n].pixun.rgb.b = back;
        }
      else
        raster[n].pixun.v = back;
      }
    }
  fprintf (stderr,"redundency = %f\toccupancy  = %f\tefficiency = %f\n",
          (float)tot/xy, (float)occ/xy, (float)tot/eff);

  if (!(isatty(1)))
    {
    if (scale == 0)
      {
      if (colour)
        printf("P6\n%d %d 255\n",X,Y);
      else
        printf("P5\n%d %d 255\n",X,Y);
      for (i=0;i<(xy);i++)
        if (colour) {
          putchar( (char) raster[i].pixun.rgb.r);
          putchar( (char) raster[i].pixun.rgb.g);
          putchar( (char) raster[i].pixun.rgb.b);
          }
        else
          putchar( (char) raster[i].pixun.v);
      }
    else
      {
      if (colour)
        printf("P6\n%d %d 255\n",X*scale,Y*scale);
      else
        printf("P5\n%d %d 255\n",X*scale,Y*scale);
      for (i=0;i<X;i++)
        for (a=0;a<scale;a++)
          for (j=0;j<Y;j++)
            {
            n = j+i*X;
            for (z=0;z<scale;z++)
              if (colour) {
                putchar( (char) raster[i].pixun.rgb.r);
                putchar( (char) raster[i].pixun.rgb.g);
                putchar( (char) raster[i].pixun.rgb.b);
                }
              else
                putchar( (char) raster[i].pixun.v);
            }
      }
    }
  exit(0);
  }


int getword()
{
  int a,b;

  if ((a = getchar()) == EOF) return -1;
  if ((b = getchar()) == EOF) return -1;

  return a*256+b;
}

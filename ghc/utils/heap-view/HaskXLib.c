/*----------------------------------------------------------------------*
 *  X from Haskell (PicoX)
 *
 * (c) 1993 Andy Gill
 *
 *----------------------------------------------------------------------*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <stdio.h>
#include <strings.h>

/*----------------------------------------------------------------------*/

/* First the X Globals */

Display *MyDisplay;
int	 MyScreen;
Window   MyWindow;
XEvent   MyWinEvent;
GC       DrawGC;
GC       UnDrawGC;

/* and the Haskell globals */

typedef struct {
  int HaskButtons[5];
  int HaskPointerX,HaskPointerY;
  int PointMoved;
} HaskGlobType;

HaskGlobType HaskGlob;

/*----------------------------------------------------------------------*/

/*
 * Now the access functions into the haskell globals
 */

int haskGetButtons(int n)
{
  return(HaskGlob.HaskButtons[n]);
}

int haskGetPointerX(void)
{
  return(HaskGlob.HaskPointerX);
}

int haskGetPointerY(void)
{
  return(HaskGlob.HaskPointerY);
}

/*----------------------------------------------------------------------*/

/*
 *The (rather messy) initiualisation
 */

haskXBegin(int x,int y,int sty)
{
 /*
  *  later include these via interface hacks
  */

 /* (int argc, char **argv) */
  int argc = 0;
  char **argv = 0;

  XSizeHints XHints;
  int MyWinFG, MyWinBG,tmp;
 
  if ((MyDisplay = XOpenDisplay("")) == NULL) {
      fprintf(stderr, "Cannot connect to X server '%s'\n", XDisplayName(""));
      exit(1);
  }

  MyScreen = DefaultScreen(MyDisplay);

  MyWinBG = WhitePixel(MyDisplay, MyScreen);
  MyWinFG = BlackPixel(MyDisplay, MyScreen);
 
  XHints.x      = x;
  XHints.y      = y;
  XHints.width  = x;
  XHints.height = y;
  XHints.flags  = PPosition | PSize;
 
  MyWindow =
      XCreateSimpleWindow(
			  MyDisplay,
			  DefaultRootWindow(MyDisplay),
			  x,y, x, y,
			  5,
			  MyWinFG,
			  MyWinBG
			  );
 
  XSetStandardProperties(
			 MyDisplay,
			 MyWindow,
			 "XLib for Glasgow Haskell",
			 "XLib for Glasgow Haskell",
			 None,
			 argv,
			 argc,
			 &XHints
			 );
 
  /* Create drawing and erasing GC */
 
  DrawGC = XCreateGC(MyDisplay,MyWindow,0, 0);
  XSetBackground(MyDisplay,DrawGC,MyWinBG);
  XSetForeground(MyDisplay,DrawGC,MyWinFG);

  UnDrawGC = XCreateGC(MyDisplay,MyWindow,0, 0);
  XSetBackground(MyDisplay,UnDrawGC,MyWinFG);
  XSetForeground(MyDisplay,UnDrawGC,MyWinBG);

  XSetGraphicsExposures(MyDisplay,DrawGC,False);
  XSetGraphicsExposures(MyDisplay,UnDrawGC,False);
  XMapRaised(MyDisplay,MyWindow);
 
  /* the user should be able to choose which are tested for
   */

  XSelectInput(
	       MyDisplay,
	       MyWindow,
	           ButtonPressMask | ButtonReleaseMask | PointerMotionMask 
	       );

  /*  later have more drawing styles
   */

  switch (sty)
    {
    case 0:   
      /* Andy, this used to be GXor not much use for Undrawing so I
         changed it. (Not much use for colour either - see next
         comment */
      XSetFunction(MyDisplay,DrawGC,GXcopy);
      XSetFunction(MyDisplay,UnDrawGC,GXcopy);
      break;
    case 1:   
      /* Andy, this can have totally bogus results on a colour screen */
      XSetFunction(MyDisplay,DrawGC,GXxor);
      XSetFunction(MyDisplay,UnDrawGC,GXxor);
      break;
    default:
      /* Andy, is this really a good error message? */
      printf(stderr,"Wrong Argument to XSet function\n");
    }
 /*
  *  reset the (Haskell) globals
  */

 for(tmp=0;tmp<5;tmp++)
   {
     HaskGlob.HaskButtons[tmp] = 0;
   }
  HaskGlob.HaskPointerX = 0;
  HaskGlob.HaskPointerY = 0;
  HaskGlob.PointMoved = 0;

  XFlush(MyDisplay);

} 

/*----------------------------------------------------------------------*/

/* Boring X ``Do Something'' functions
 */

haskXClose(void)
{
  XFreeGC( MyDisplay, DrawGC);
  XFreeGC( MyDisplay, UnDrawGC);
  XDestroyWindow( MyDisplay, MyWindow);
  XCloseDisplay( MyDisplay);
  return(0);
}

haskXDraw(x,y,x1,y1)
int x,y,x1,y1;
{
  XDrawLine(MyDisplay,
	    MyWindow,
	    DrawGC,
	    x,y,x1,y1);
  return(0);
}


haskXPlot(c,x,y)
int c;
int x,y;
{
  XDrawPoint(MyDisplay,
	    MyWindow,
	    (c?DrawGC:UnDrawGC), 
	    x,y);
  return(0);
}

haskXFill(c,x,y,w,h)
int c;
int x, y;
int w, h;
{
  XFillRectangle(MyDisplay,
	    MyWindow,
	    (c?DrawGC:UnDrawGC),
	    x, y, w, h);
  return(0);
}

/*----------------------------------------------------------------------*/
 
 /* This has to be called every time round the loop,
  * it flushed the buffer and handles input from the user
  */

haskHandleEvent()
{
  XFlush( MyDisplay);
  while (XEventsQueued( MyDisplay, QueuedAfterReading) != 0) {
    XNextEvent( MyDisplay, &MyWinEvent);
    switch (MyWinEvent.type) {
    case ButtonPress:
      switch (MyWinEvent.xbutton.button) 
	{
	case Button1: HaskGlob.HaskButtons[0] = 1; break;
	case Button2: HaskGlob.HaskButtons[1] = 1; break;
	case Button3: HaskGlob.HaskButtons[2] = 1; break;
	case Button4: HaskGlob.HaskButtons[3] = 1; break;
	case Button5: HaskGlob.HaskButtons[4] = 1; break;
	}
      break;
    case ButtonRelease:
      switch (MyWinEvent.xbutton.button) 
	{
	case Button1: HaskGlob.HaskButtons[0] = 0; break;
	case Button2: HaskGlob.HaskButtons[1] = 0; break;
	case Button3: HaskGlob.HaskButtons[2] = 0; break;
	case Button4: HaskGlob.HaskButtons[3] = 0; break;
	case Button5: HaskGlob.HaskButtons[4] = 0; break;
	}
      break;
    case MotionNotify: 
        HaskGlob.HaskPointerX = MyWinEvent.xmotion.x;
        HaskGlob.HaskPointerY = MyWinEvent.xmotion.y;
        HaskGlob.PointMoved = 1;
      break;
    default:
    printf("UNKNOWN INTERUPT ???? (%d) \n",MyWinEvent.type); 
      break;
    } /*switch*/
  } /*if*/
  return(0);
} 


/*----------------------------------------------------------------------*/

 /* A function to clear the screen 
  */

haskXCls(void)
{
  XClearWindow(MyDisplay,MyWindow);
}

/*----------------------------------------------------------------------*/

 /* A function to write a string
  */

haskXDrawString(int x,int y,char *str)
{
  return(0);
/*  printf("GOT HERE %s %d %d",str,x,y); 
  XDrawString(MyDisplay,MyWindow,DrawGC,x,y,str,strlen(str));
*/
}

/*----------------------------------------------------------------------*/

extern int prog_argc;
extern char **prog_argv;

haskArgs()
{
  return(prog_argc > 1 ? atoi(prog_argv[1]) : 0);
}

/* ------------------------------------------------------------------------
 * $Id: main.c,v 1.4 2005/12/02 12:45:16 simonmar Exp $
 *									
 *	Copyright (C) 1995-2000 University of Oxford
 *									
 * Permission to use, copy, modify, and distribute this software,
 * and to incorporate it, in whole or in part, into other software,
 * is hereby granted without fee, provided that
 *   (1) the above copyright notice and this permission notice appear in
 *	 all copies of the source code, and the above copyright notice
 *	 appear in clearly visible form on all supporting documentation
 *	 and distribution media;
 *   (2) modified versions of this software be accompanied by a complete
 *	 change history describing author, date, and modifications made;
 *	 and
 *   (3) any redistribution of the software, in original or modified
 *	 form, be without fee and subject to these same conditions.
 * --------------------------------------------------------------------- */

#include "ghcconfig.h"

#include <stdio.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif

#include "symbol.h"
#include "cgprof.h"
#include "matrix.h"
#include "daVinci.h"

#if HAVE_WINDOWS_H
#include <windows.h>
#define sleep(x) Sleep((x)*1000)
#endif


#define NoDeletes 80

int     CriticalPath=CRITICAL_SYNCS;
int     CriticalType=CRITTYPE_ABSOLUTE;
int     Verbose=1;
int     NodeviewTime=1;
int     NodeviewCompress=1;
int     PrintLogo=1;
int     Colour=1;
int     DeltaNormalise=1;
int     PieView=TCL_COMP;
int     PieCombine=0;
char   *Pgm;
char   *ProfileData;
int     NoNodes,root;
char    usage[]="usage:  cgprof profile-data [See man 1 cgprof]";
char    helpUrl[]="http://www.dcs.warwick.ac.uk/people/academic/Stephen.Jarvis/profiler/";
Matrix  graph; /* NoNodes x NoNodes matrix of integers */
Matrix  costs; /* NoNodes x 1       matrix of costs    */

double   TotalComp, TotalComm, TotalCompIdle;
int      TotalSyncs;
long int TotalH;

char    *dateProfiled, *machineName;
double minNodeSize = 0.01;   /* i.e, don't show nodes with _combined_
				    comp and comm % less than this */
double bsp_s = 74.0;
double bsp_l = 1902;
double bsp_g = 9.3;
int    bsp_p;

FILE *logFile;


extern void printDaVinci(int);

int 
main(int argc, char *argv[]) {
  char davinci_stdin[MAX_PROFILE_LINE_LENGTH];
  FILE   *fptr;
  int i,j,k,going=1,*select_nodes, select_nodes_next,MaxNoNodes;
  davinciCmd cmd;
  int *undo_stack, undo_stack_next;
  float temp_f;
  char *ptr;
  int mode = 0;
  char *tempstring = malloc (80);
  char *tempstring2 = malloc (80);
  

  /* printf("Starting main routine of browser script\n"); */
  /* fflush(stderr); */

  if (argc!=14) {
    fprintf(stderr,"The perl script bspsgprof is buggered\n");
    exit(1);
  }

  /* Most (if not all) of these BSP specific arguments can be removed */

  Pgm         = argv[0];
  ProfileData = argv[1];
  bsp_p       = atoi(argv[2]);
  machineName = argv[3];
  dateProfiled= argv[4];
  sscanf(argv[5],"%f",&temp_f);
  bsp_s = temp_f;
  sscanf(argv[6],"%f",&temp_f);
  bsp_l = temp_f;
  sscanf(argv[7],"%f",&temp_f);
  bsp_g = temp_f;
  sscanf(argv[8],"%f",&temp_f);
  minNodeSize=temp_f;
  Verbose = atoi(argv[9]);
  PrintLogo=atoi(argv[10]);
  Colour=atoi(argv[11]);
  DeltaNormalise=atoi(argv[12]);
  MaxNoNodes=atoi(argv[13]);

  /* printf("Initialisation done\n"); */

  if (Verbose) sleep(10);  
  if (!(fptr=fopen(ProfileData,"r"))) {
    fprintf(stderr,"%s: unable to open profile data in \"%s\".\n%s\n",
            Pgm,ProfileData,usage);
    exit(1);
  }
  if (!(logFile=fopen("ghcprof.log","w"))) {
    fprintf(stderr,"%s: unable to open log file for writing\n",Pgm);
    exit(1);
  }

  /* printf("Files opened OK\n"); */

  if (!fgets(davinci_stdin, MAX_PROFILE_LINE_LENGTH, stdin) || 
       strcmp(davinci_stdin,"ok\n")) {
    fprintf(stderr,"%s{%s}: failed to receive ok from daVinci.\n",
	    davinci_stdin,Pgm);
    exit(1);
  }

  /* printf("Initialising daVinci\n"); */

  initDaVinci();
  
  /* printf("Ending initialisation of daVinci\n"); */
 
  if (Verbose) fprintf(logFile,"%s: opened profile file \"%s\".\n",Pgm,ProfileData);
  readRawProfile(fptr,&NoNodes,MaxNoNodes);
  fclose(fptr);
  if (Verbose) fprintf(logFile,"%s: %d nodes in profile.\n",Pgm,NoNodes);

  if (NoNodes<=0) {
    fprintf(logFile,"%s: no call-graph profile data in \"%s\".\n"
            "Re-run your program using the appropriate profiling flags\n",
            Pgm,ProfileData);
    exit(1);
  }
  if (Verbose) printRawProfile();

  /* Do we want INHERITANCE to begin with or not? Set to yes. */
  createConnectivityMatrix(NoNodes,&graph,&costs,&root,1);

  TotalComp     = Mat(object_cost,costs,root,0).comp_max;
  TotalComm     = Mat(object_cost,costs,root,0).comm_max;
  TotalCompIdle = Mat(object_cost,costs,root,0).comp_idle_max;
  TotalH        = Mat(object_cost,costs,root,0).hrel_max;
  TotalSyncs    = Mat(object_cost,costs,root,0).syncs;
  if (Verbose) printConnectivityMatrix(graph,costs,root);
  fflush(logFile);
  graphToDaVinci(root,&graph,&costs,0);
  fflush(stdout);
  undo_stack   = calloc(NoDeletes,sizeof(int));
  select_nodes = calloc(NoNodes,sizeof(int));
  if (undo_stack==NULL || select_nodes==NULL) {
    fprintf(stderr,"Unable to allocate storage for undo stack\n");
    exit(1);
  }
  undo_stack_next=0;
  select_nodes_next=0;
  // Pie chart stuff not wanted for GHC
  // tclPieInit();
  // tclPieUpdate(&Mat(object_cost,costs,root,0),root,PieView);
  select_nodes_next=1;
  select_nodes[0]=root;
  while (fgets(davinci_stdin, MAX_PROFILE_LINE_LENGTH, stdin) && going) {
    cmd = parseDaVinciCmd(davinci_stdin);
    if (Verbose) fprintf(logFile,"From davinci=\"%s\"\n",davinci_stdin);
    switch (cmd.type) {
    case DAVINCI_OK:
      continue;

    case DAVINCI_QUIT:
      going=0;
      break;

    case DAVINCI_NODE:
      select_nodes_next=cmd.size;
      for(i=0;((i<cmd.size) && (i<NoNodes));i++)
        select_nodes[i]=atoi(cmd.list[i]);
      if (select_nodes_next>0)
        //Pie chart stuff not wanted for GHC
        //tclPieUpdate(&Mat(object_cost,costs,select_nodes[0],0),
        //		     select_nodes[0],
        //		     PieView);
      if (mode==3) 
      {
         mode = atoi(cmd.list[0]);
         getNameFromSymbolTable(mode,tempstring);
         for(ptr=tempstring;*ptr!='\0';ptr++)
            if (*ptr=='&') *ptr=' ';
         mode = 3;
         strcpy(tempstring2,"window(show_status(\"");
         strcat(tempstring2,tempstring);
         strcat(tempstring2,"\"))");
         cmdDaVinci(tempstring2);
         strcpy(tempstring,"");
         strcpy(tempstring2,"");
      }
      break;

    case DAVINCI_MENU:
      if (cmd.size>0) {
        if (strcmp(cmd.list[0], "jump")==0)  {
	  if ((select_nodes_next>=0)      && 
              (select_nodes[0]>0)         &&
	      (select_nodes[0] < NoNodes) &&
	      (Mat_dense(graph,select_nodes[0],select_nodes[0]))) {
	    cmdDaVinci("special(focus_node(\"%d\"))\n",select_nodes[0]);
	  }
        }
      }
      break;

    case DAVINCI_ICON:
      if (cmd.size>0) {
        if (strcmp(cmd.list[0], "sync")==0) {
	  CriticalPath=CRITICAL_SYNCS;
	  activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Graph view\"))");
          updateColours(root,&graph,&costs);

	} else if (strcmp(cmd.list[0], "comp")==0) {
	  CriticalPath=CRITICAL_COMP;
	  activateDaVinciMenu(cmd.list[0]);
          cmdDaVinci("window(show_status(\"SCCs critical path\"))");
          updateColours(root,&graph,&costs);
          
        } else if (strcmp(cmd.list[0], "comm")==0) {
	  CriticalPath=CRITICAL_COMM;
	  activateDaVinciMenu(cmd.list[0]);
          cmdDaVinci("window(show_status(\"Computation time critical path\"))");
          updateColours(root,&graph,&costs);
          
        } else if (strcmp(cmd.list[0], "wait")==0) {
	  CriticalPath=CRITICAL_WAIT;
	  activateDaVinciMenu(cmd.list[0]);
          cmdDaVinci("window(show_status(\"Heap usage critical path\"))"); 
          updateColours(root,&graph,&costs);
          
        } else if (strcmp(cmd.list[0], "hrel")==0) {

          if (mode != 3)
          {
            cmdDaVinci("window(show_status(\"Node spy on\"))");
            mode = 3;
          }
          else 
          {
            mode = 0;
            cmdDaVinci("window(show_status(\"Node spy off\"))");
          }

        } else if (strcmp(cmd.list[0], "absolute")==0) {
          /* Now deals with inheritance profile */
	  CriticalType=CRITTYPE_ABSOLUTE;
          activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Inheritance profile\"))");
          freeMat(&graph); 
	  freeMat(&costs); 
          createConnectivityMatrix(NoNodes,&graph,&costs,&root,1);
          graphToDaVinci(root,&graph,&costs,0);
	  cmdDaVinci("window(show_status(\"Inheritance profile\"))");
          updateColours(root,&graph,&costs);

        } else if (strcmp(cmd.list[0], "absdelta")==0) {
          /* Now deals with flat profile */
	  CriticalType=CRITTYPE_ABSDELTA;
          activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Flat profile\"))");
          freeMat(&graph); 
	  freeMat(&costs); 
          createConnectivityMatrix(NoNodes,&graph,&costs,&root,0);
          graphToDaVinci(root,&graph,&costs,0);
	  cmdDaVinci("window(show_status(\"Flat profile\"))");
          updateColours(root,&graph,&costs);

        } else if (strcmp(cmd.list[0], "reldelta")==0) {
	  CriticalType=CRITTYPE_ABSOLUTE;
          activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Trimmed zero-cost sub-trees\"))");
          strcpy(cmd.list[0], "absolute");
          activateDaVinciMenu(cmd.list[0]);
          graphToDaVinci(root,&graph,&costs,2);
          updateColours(root,&graph,&costs);
	  
        } else if (strcmp(cmd.list[0], "weightdelta")==0) {
	  CriticalType=CRITTYPE_ABSOLUTE;
          activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Marked zero-cost nodes ready for deletion\"))");
          strcpy(cmd.list[0], "absolute");
          activateDaVinciMenu(cmd.list[0]);
          graphToDaVinci(root,&graph,&costs,1);
          updateColours(root,&graph,&costs);
	  
        } else if (strcmp(cmd.list[0],"help")==0) {
          cmdDaVinci("special(show_url(\"%s\"))",helpUrl);

	} else if (strcmp(cmd.list[0],"time")==0) {
	  NodeviewTime=1;
	  activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Cost metric view\"))");
          graphToDaVinci(root,&graph,&costs,0);

	} else if (strcmp(cmd.list[0],"percent")==0) {
	  NodeviewTime=0;
	  activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Percentage view\"))");
          graphToDaVinci(root,&graph,&costs,0);

	} else if (strcmp(cmd.list[0],"compress")==0) {
	  NodeviewCompress=1;
	  activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Compressed node view\"))");
	  cmdDaVinci("menu(layout(compact_all))");
          graphToDaVinci(root,&graph,&costs,0);

	} else if (strcmp(cmd.list[0],"uncompress")==0) {
	  NodeviewCompress=0;
	  activateDaVinciMenu(cmd.list[0]);
	  cmdDaVinci("window(show_status(\"Uncompressed node view\"))");
          graphToDaVinci(root,&graph,&costs,0);

	} else if ((strcmp(cmd.list[0],"delete")==0) ||
		   (strcmp(cmd.list[0],"undo")==0)) {
          if (strcmp(cmd.list[0],"delete")==0) {
	    if (undo_stack_next==0) 
	      activateDaVinciMenu("undo");
            for(i=0;(i<select_nodes_next) && (undo_stack_next<NoNodes);i++) 
	      undo_stack[undo_stack_next++] = select_nodes[i];
	    if (undo_stack_next==NoDeletes) 
	      activateDaVinciMenu("delete");
	    cmdDaVinci("window(show_status(\"Deleted node (s)\"))");
            select_nodes_next=0;
	  } else {
	    if (undo_stack_next==NoDeletes) 
	      activateDaVinciMenu("delete");
	    undo_stack_next--;
	    if (undo_stack_next==0) 
	      activateDaVinciMenu("undo");
	    cmdDaVinci("window(show_status(\"Undone deletion\"))");
	    select_nodes_next=1;
	    select_nodes[0]=undo_stack[undo_stack_next];
            
	    for(i=0;i<raw_profile_next;i++)
	      raw_profile[i].active=1;
	  }
	  activateDaVinciMenu("default");
	  for(i=0;i<undo_stack_next;i++) {
	    for(j=0;j<raw_profile_next;j++) {
              for(k=0;k<raw_profile[j].stack_size;k++) {
                if (raw_profile[j].stack[k]==undo_stack[i])
		  raw_profile[j].active=0;
	      }
            }
          }
          cmdDaVinci("window(show_message(\"Deleting node...\"))");
          freeMat(&graph);
	  freeMat(&costs);
          createConnectivityMatrix(NoNodes,&graph,&costs,&root,1);
          graphToDaVinci(root,&graph,&costs,0);
          if (strcmp(cmd.list[0],"undo")==0) {
            if ((select_nodes[0]>0)         &&
	        (select_nodes[0] < NoNodes) &&
	        (Mat_dense(graph,select_nodes[0],select_nodes[0]))) {
	    cmdDaVinci("special(focus_node(\"%d\"))\n",select_nodes[0]);
	    cmdDaVinci("special(select_nodes([\"%d\"]))",select_nodes[0]);
            //Pie chart stuff not wanted for GHC
	    //tclPieUpdate(&Mat(object_cost,costs,select_nodes[0],0),
	    //	       select_nodes[0],
	    //	       PieView);
            }
          }
	}    
      }
      break;
    case DAVINCI_TCL: 
    // This stuff can go as it is related to the input for the Pie chart tool
      if (cmd.size>0) {
        if        (strcmp(cmd.list[0], "comm")==0)  {
	  PieView=TCL_COMM;
	} else if (strcmp(cmd.list[0], "comp")==0)  {
	  PieView=TCL_COMP;
	} else if (strcmp(cmd.list[0], "hrel")==0)  {
	  PieView=TCL_HREL;
	} else if (strcmp(cmd.list[0], "wait")==0)  {
	  PieView=TCL_WAIT;
	} else if (strcmp(cmd.list[0], "combine")==0)  {
	  PieCombine=!PieCombine;
        } else if (strlen(cmd.list[0])==0) {
	  break;
	}
	if (select_nodes_next>0) break;
          //Added a break for compiliation above since it does not compile if 
          //we just remove the Pie chart code 
	  //tclPieUpdate(&Mat(object_cost,costs,select_nodes[0],0),
	  //	       select_nodes[0],
	  //	       PieView);
      }
      break;
    case DAVINCI_ERROR:  
    default:
      fprintf(stderr,"CGPROF error:\n"
                     "\tCommand = %s\n"
	             "\tError   = %s\n",lastDavinciCmd,davinci_stdin);
      exit(1); 
      break;
    }
    fflush(stdout);
    fflush(logFile);
  }  

  return 0;
}

/* ------------------------------------------------------------------------
 * $Id: cgprof.c,v 1.6 2004/08/13 13:11:22 simonmar Exp $
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
#if HAVE_STRING_H
#include <string.h>
#endif

#include "daVinci.h"
#include "symbol.h"
#include "cgprof.h"
#include "matrix.h"

/* -----------------------------------------------------------------------------
 * Data structures
 * -------------------------------------------------------------------------- */

int                 raw_profile_next=0;
int                 raw_profile_size=0;
parsed_cost_object *raw_profile=NULL;

/* -----------------------------------------------------------------------------
 * Create/grow data sequence of raw profile data
 * -------------------------------------------------------------------------- */

void enlargeRawProfile() {

  if (raw_profile_size==0) {
    raw_profile_next = 0;
    raw_profile_size = RAW_PROFILE_INIT_SIZE;
    raw_profile      = calloc(raw_profile_size,sizeof(parsed_cost_object));
  } else {
    raw_profile_size += RAW_PROFILE_INIT_SIZE;
    raw_profile       = realloc(raw_profile,
				 raw_profile_size*sizeof(parsed_cost_object));
  }
  if (raw_profile==NULL) {
    fprintf(stderr,"{enlargeRawProfile} unable to allocate %d elements",
            raw_profile_size);
    exit(1);
  }
}

/* -----------------------------------------------------------------------------
 * Function that adds two cost centers together
 *
 * This will be used to generate the inheretance profile.
 * -------------------------------------------------------------------------- */

void add_costs(object_cost *left, object_cost right) {

  left->syncs         += right.syncs;
  left->comp_max      += right.comp_max;
  left->comp_avg      += right.comp_avg;
  left->comp_min      += right.comp_min;
  left->comm_max      += right.comm_max;
  left->comm_avg      += right.comm_avg;
  left->comm_min      += right.comm_min;
  left->comp_idle_max += right.comp_idle_max;
  left->comp_idle_avg += right.comp_idle_avg;
  left->comp_idle_min += right.comp_idle_min;
  left->hrel_max      += right.hrel_max;
  left->hrel_avg      += right.hrel_avg;
  left->hrel_min      += right.hrel_min;
  if ((left->proc==NULL) || (right.proc==NULL)) {
    fprintf(stderr,"Cost is null");
    exit(0);
  }
}


int ignore_function(char *fname) {
  return 0;
}

/* -----------------------------------------------------------------------------
 * GHC specific data structures
 * -------------------------------------------------------------------------- */

/* Globals */
/* You will need to update these when you increase the number of */
/*   cost centres, cost centre stacks, heap objects              */

   #define MAX_IDENTIFIERS 2000 /* maximum number of identifiers */
                                /* or size of matrix structure   */

  /* make this dynamic */

   #define MAX_TIME    100      /* Maximum serial time for heap profile */
   #define MAX_SAMPLES 50       /* Maximum heap samples */

                                /* To do: modify this to be dynamic */

   #define MAX_STRING_SIZE 70
   #define MAX_LINE_LENGTH 80
   #define EOF (-1)

/* Cost centre data structure */

   struct cost_centre { char *name;
                        char *module;
                        char *group;
   } _cc_;

   typedef struct cost_centre cc_matrix[MAX_IDENTIFIERS];

   //typedef struct cost_centre *cc_matrix;

   typedef cc_matrix* p_cc_matrix;
   typedef char* MY_STRING;

/* Heap sample structure */

   struct heap_sample { 
                        int count; /* heap_sample */
   };

   typedef struct heap_sample heap_sample_matrix[MAX_IDENTIFIERS];
   typedef heap_sample_matrix* p_heap_sample_matrix;

/* Cost centre stack data structure */

   struct cost_centre_stack { 
                      int cc;
                      int ccs;
                      int scc;   /* scc_sample  */
                      int ticks; /* scc_sample  */
                      int bytes; /* scc_sample  */
                      p_heap_sample_matrix hsm; /* heap_sample */
   };

   typedef struct cost_centre_stack ccs_matrix[MAX_IDENTIFIERS];
   typedef ccs_matrix* p_ccs_matrix;

/* Heap object data structure */

   struct heap_object { int   type;            /* type of heap object */
                        char* descriptor; 
                        int   type_constr_ref; /* if present */
                      };

   typedef struct heap_object heap_object_matrix[MAX_IDENTIFIERS];
   typedef heap_object_matrix* p_heap_object_matrix;

/* Type constructor structure */

   struct type_constr { char* module;
                        char* name;
                      };

   typedef struct type_constr type_constr_matrix[MAX_IDENTIFIERS];
   typedef type_constr_matrix* p_type_constr_matrix;

/* Heap update structure */

   struct heap_update_sample { int ccs;   /* associated cost centre stack */
                               int ho;    /* associated heap object */
                               int count; 
                              };

   typedef struct heap_update_sample heap_update_list[MAX_SAMPLES];
   typedef heap_update_list* p_heap_update_list;

   struct heap_update_record { int no_samples; /* Number of samples */
                               p_heap_update_list acc_samples;
                             };

   typedef struct heap_update_record TheHeap[MAX_TIME];
   typedef TheHeap* p_TheHeap;


/* -----------------------------------------------------------------------------
 * GHC specific functions
 * -------------------------------------------------------------------------- */

// Initialisation routines

void initialise_heap_update_list(heap_update_list *m)
{
  int i;
  for (i=0; i<MAX_SAMPLES;i++)
  {
    (*m)[i].ccs   = -1;
    (*m)[i].ho    = -1;
    (*m)[i].count    = -1;
  }
}

void add_to_heap_update_list(heap_update_list *m, int ccs, int ho, int count, int pos)
{
  (*m)[pos].ccs    = ccs;
  (*m)[pos].ho     = ho;
  (*m)[pos].count    = count;
}

void initialise_TheHeap(TheHeap *h)
{
  int i;
  for (i=0; i<MAX_TIME;i++)
  {
    heap_update_list *h_u_l;
    h_u_l = (p_heap_update_list) malloc (sizeof(heap_update_list));
    initialise_heap_update_list(h_u_l);
    (*h)[i].acc_samples = h_u_l;
    (*h)[i].no_samples   = 0;
  }
}

void add_to_TheHeap(TheHeap *h, int time, int ccs, int ho, int count)
{
  add_to_heap_update_list((*h)[time].acc_samples,ccs,ho,count,(*h)[time].no_samples);
  (*h)[time].no_samples++;
}

void initialise_cc_matrix(cc_matrix *m)
{ 
  int i;
  char *blank="blank"; /* To do: Modify this terminator string */
  for (i=0; i<MAX_IDENTIFIERS; i++)
    { 
      (*m)[i].name =  (MY_STRING) malloc ((MAX_STRING_SIZE));
      (*m)[i].module = (MY_STRING) malloc ((MAX_STRING_SIZE));
      (*m)[i].group = (MY_STRING) malloc ((MAX_STRING_SIZE));

      strcpy((*m)[i].name,blank); 
      strcpy((*m)[i].module,blank);
      strcpy((*m)[i].group,blank);  
    }
}

void free_cc_matrix(cc_matrix *m)
{
  int i;
  for (i=0; i<MAX_IDENTIFIERS; i++)
    {
      free((*m)[i].name);
      free((*m)[i].module);
      free((*m)[i].group);
    }
    free(m);
}

void initialise_heap_object_matrix(heap_object_matrix *m)
{
  int i;
  char *blank="blank"; /* To do: ditto */
  for (i=0; i<MAX_IDENTIFIERS; i++)
  {
    (*m)[i].type = -1;
    (*m)[i].descriptor = (MY_STRING) malloc ((MAX_STRING_SIZE));
    strcpy((*m)[i].descriptor,blank);
    (*m)[i].type_constr_ref = -1; 
  }
}

void initialise_type_constr_matrix(type_constr_matrix *m)
{
  int i;
  char *blank="blank";
  for (i=0; i<MAX_IDENTIFIERS; i++)
  {
    (*m)[i].module = (MY_STRING) malloc ((MAX_STRING_SIZE));
    (*m)[i].name   = (MY_STRING) malloc ((MAX_STRING_SIZE));
    strcpy((*m)[i].module,blank);
    strcpy((*m)[i].name,blank);
  }
}

void initialise_heap_sample_matrix(heap_sample_matrix *m)
{
  int i;
  for (i=0; i<MAX_IDENTIFIERS; i++)
  { (*m)[i].count = -1; }
}

void initialise_ccs_matrix(ccs_matrix *m)
{ 
  int i;
  for (i=0; i<MAX_IDENTIFIERS; i++)
    { 
      /* Stack heap samples */
      heap_sample_matrix *hs_m;
      hs_m = (p_heap_sample_matrix) malloc (sizeof(heap_sample_matrix));
      initialise_heap_sample_matrix(hs_m);
      (*m)[i].hsm = hs_m;
      /* Stack scc samples */
      (*m)[i].cc    = 0; 
      (*m)[i].ccs   = 0;
      (*m)[i].scc   = 0;
      (*m)[i].ticks = 0; 
      (*m)[i].bytes = 0; 
    }
}


// Filling matrix routines

char* StripDoubleQuotes(char* s) /* For fussy daVinci! */
{
  char *p = s;
  char *tempchar;
  char *empty="";
  char *tempstring = (MY_STRING) malloc ((MAX_STRING_SIZE));
  strcpy(tempstring,empty);
  while (*p)
  { if (*p!='"')
    { tempchar = p; strncat(tempstring,p,1);
     }
    p++; 
  }
  return tempstring;
}

void fill_cc_matrix(cc_matrix *m,char* name,char* module,char* group,int i)
{ 
  if (i>MAX_IDENTIFIERS) 
  {  fprintf(logFile,"Cost centre MAX_IDENTIFIERS exceeded: %i \n",i); exit(1); }
  name = StripDoubleQuotes(name);
  strcpy((*m)[i].name,name); 
  module = StripDoubleQuotes(module);
  strcpy((*m)[i].module,module);
  group = StripDoubleQuotes(group);
  strcpy((*m)[i].group,group);
}

void fill_ccs_matrix(ccs_matrix *m,int cc, int ccs, int scc, int ticks, int bytes, int h_o, int count, int i)
{
  heap_sample_matrix *hsm;

  if ((*m)[i].cc == 0)  /* added for type 2 stack semantics, but should not */
                        /* change behaviour of type 1 (apart from CAF:REP.  */
  {
    if (i>MAX_IDENTIFIERS) 
    {  fprintf(logFile,"Cost centre stack MAX_IDENTIFIERS exceeded: %i \n",i); exit(1); }
    hsm = (*m)[i].hsm;
    (*m)[i].cc = cc; (*m)[i].ccs = ccs; 
    (*m)[i].ticks = ticks; (*m)[i].bytes = bytes; (*m)[i].scc = scc;
    (*hsm)[h_o].count = count;
  }
  else fprintf(logFile,"Ignoring redeclaration of stack %i\n",i);
}

void add_ccs_costs(ccs_matrix *m, int b,int c,int d,int x,int y,int h_o, int co)
{
  (*m)[c].scc    = (*m)[c].scc + d;
  (*m)[c].ticks  = (*m)[c].ticks + x;
  (*m)[c].bytes  = (*m)[c].bytes + y;
}

void add_heap_sample_costs(ccs_matrix *m, int b,int c,int d,int x,int y,int h_o, int co)
{ 
  heap_sample_matrix *hsm = (*m)[c].hsm;
  if (((*hsm)[h_o].count)==-1)
     (*hsm)[h_o].count = (*hsm)[h_o].count + co + 1; /* as init is -1 */
  else 
     (*hsm)[h_o].count = (*hsm)[h_o].count + co;
}

void add_heap_object(heap_object_matrix *m, int pos, int t, char* des, int tr)
{
  if (pos>MAX_IDENTIFIERS) 
  {  fprintf(logFile,"Heap object MAX_IDENTIFIERS exceeded: %i \n",pos); exit(1); }
  (*m)[pos].type = t;
  strcpy((*m)[pos].descriptor,des);
  (*m)[pos].type_constr_ref = tr;
} 

void add_type_constr_object(type_constr_matrix *m, int pos, char* mod, char* n)
{
  if (pos>MAX_IDENTIFIERS) 
  {  fprintf(logFile,"Type constructor MAX_IDENTIFIERS exceeded: %i \n",pos); exit(1); }
  strcpy((*m)[pos].module,mod);
  strcpy((*m)[pos].name,n);
}


// Printing routines

void print_heap_update_list(heap_update_list *m, int number)
{
  int i;
  fprintf(logFile,"[");
  for (i=0; i<number;i++)
  {
    fprintf(logFile," (%i,%i,%i) ",(*m)[i].ccs,(*m)[i].ho,(*m)[i].count);
  }
  fprintf(logFile,"]\n");
}

void print_TheHeap(TheHeap *h)
{
  int i;
  fprintf(logFile,"The Heap\n========\n");
  for (i=0; i<MAX_TIME;i++)
  {
    if ((*h)[i].no_samples>0)
    {
      fprintf(logFile,"Sample time %i, number of samples %i actual samples "
                 ,i,(*h)[i].no_samples);
      print_heap_update_list((*h)[i].acc_samples,(*h)[i].no_samples);
    }
  }
}

void PrintXaxis(FILE *HEAP_PROFILE, TheHeap *h)
{
  int i;
  fprintf(HEAP_PROFILE," ");
  for (i=0; i<MAX_TIME;i++)
  {
    if ((*h)[i].no_samples>0)
       fprintf(HEAP_PROFILE,"%i ",i);
  }
}

int FindSample(heap_update_list *m, int number, int element)
{
  int i;
  for (i=0; i<number;i++)
  {
    if ((*m)[i].ho==element)
        return ((*m)[i].count);
  }
  return 0;
}

void PrintSampleCosts(FILE *hfp, TheHeap *h, int element)
{
  int i;
  int total = 0;
  for (i=0; i<MAX_TIME;i++)
  {
    if ((*h)[i].no_samples>0)
    {
      total = total + FindSample((*h)[i].acc_samples,(*h)[i].no_samples,element);
      fprintf(hfp," %i ",total);
    }
  }
}

void print_cc_matrix(cc_matrix *m)
{ 
  int i;
  char *blank="blank";
  fprintf(logFile,"Cost centre matrix\n");
  fprintf(logFile,"==================\n");
  for (i=0; i<MAX_IDENTIFIERS; i++)
    { if (strcmp((*m)[i].name,blank)!=0) 
         fprintf(logFile,"%s %s %s\n",(*m)[i].name,(*m)[i].module,(*m)[i].group); }
  fprintf(logFile,"\n");
}

void print_heap_object_matrix(FILE* hfp, TheHeap *h, heap_object_matrix *m)
{
  int i;
  for (i=0; i<MAX_IDENTIFIERS; i++)
  { 
    if (((*m)[i].type)!=-1)
    {
      fprintf(hfp,"Y%i set {",i);
      /* if ((*m)[i].type==1) fprintf(hfp,"data_contr ");
      if ((*m)[i].type==2) fprintf(hfp,"PAP ");
      if ((*m)[i].type==3) fprintf(hfp,"thunk ");
      if ((*m)[i].type==4) fprintf(hfp,"function ");
      if ((*m)[i].type==5) fprintf(hfp,"dictionary ");
      if ((*m)[i].type==1) 
         fprintf(hfp,"%s %i ",(*m)[i].descriptor,(*m)[i].type_constr_ref);
      else
         fprintf(hfp,"%s ",(*m)[i].descriptor); */
      PrintSampleCosts(hfp,h,i);
      fprintf(hfp,"}\n");
    }
  }         
}

int number_of_heap_objects(heap_object_matrix *m)
{
  int i;
  int count = 0;
  for (i=0; i<MAX_IDENTIFIERS; i++)
  {
    if (((*m)[i].type)!=-1) count++;
  }
  return count;
}

void names_of_heap_objects(FILE *hfp, heap_object_matrix *m)
{
  int i;
  for (i=0; i<MAX_IDENTIFIERS; i++)
  {
    if (((*m)[i].type)!=-1) 
      fprintf(hfp,"Y%i ",i);
  }
  fprintf(hfp,"\n");
}

void names_and_colour_assignment(FILE *hfp, heap_object_matrix *m)
{
  int i;
  int colour=0;
  for (i=0; i<MAX_IDENTIFIERS; i++)
  {
    if (((*m)[i].type)!=-1) 
    {
      switch(colour)
      {
        case 0 : fprintf(hfp,"%s \t Y%i \t red \t fdiagonal1\n",(*m)[i].descriptor,i); 
                 colour++; break;
        case 1 : fprintf(hfp,"%s \t Y%i \t blue \t fdiagonal1\n",(*m)[i].descriptor,i);
                 colour++; break;
        case 2 : fprintf(hfp,"%s \t Y%i \t green \t fdiagonal1\n",(*m)[i].descriptor,i);
                 colour++; break;
        case 3 : fprintf(hfp,"%s \t Y%i \t yellow \t fdiagonal1\n",(*m)[i].descriptor,i);
                 colour++; break;
        case 4 : fprintf(hfp,"%s \t Y%i \t pink \t fdiagonal1\n",(*m)[i].descriptor,i);
                 colour++; break;
        case 5 : fprintf(hfp,"%s \t Y%i \t goldenrod \t fdiagonal1\n",(*m)[i].descriptor,i);
                 colour++; break;
        case 6 : fprintf(hfp,"%s \t Y%i \t orange \t fdiagonal1\n",(*m)[i].descriptor,i);
                 colour++; break;
        default: fprintf(hfp,"%s \t Y%i \t purple \t fdiagonal1\n",(*m)[i].descriptor,i);
                 colour=0; break;
      }
    }
  }
}

void print_type_constr_matrix(type_constr_matrix *m)
{
  int i;
  char *blank="blank";
  fprintf(logFile,"Type constructor matrix\n");
  fprintf(logFile,"=======================\n");
  for (i=0; i<MAX_IDENTIFIERS; i++)
  {
    if (strcmp((*m)[i].name,blank)!=0)
         fprintf(logFile,"%i %s %s\n",i,(*m)[i].module,(*m)[i].name);
  }
}

void print_heap_sample_matrix(heap_sample_matrix *m)
{
  int i;
  fprintf(logFile,"HeapSamples[");
  for (i=0; i<MAX_IDENTIFIERS; i++)
  {
    if ((*m)[i].count!=-1) fprintf(logFile,"(%i,%i),",i,(*m)[i].count);
  }
  fprintf(logFile,"]\n");
}

void print_ccs_matrix(ccs_matrix *m)
{ 
  int i;
  fprintf(logFile,"Cost centre stack matrix\n");
  fprintf(logFile,"========================\n");
  for (i=0; i<MAX_IDENTIFIERS; i++)
  {  if ((*m)[i].cc!=0)
     {
       fprintf(logFile,"%i %i %i %i %i \n",(*m)[i].cc,(*m)[i].ccs,(*m)[i].scc,
                               (*m)[i].ticks,(*m)[i].bytes); 
     } 
  }
  fprintf(logFile,"\n");
}


/* No longer used */

void FormStack(ccs_matrix *m, cc_matrix *n, int i, char s[])
{
  int j = i;
  if ((*m)[j].cc != 0)
  {
    strcat(s,(*n)[(*m)[j].cc].name);
    strcat(s," ");
    while ((*m)[j].ccs != (-1))
    {
      strcat(s,(*n)[(*m)[(*m)[j].ccs].cc].name);
      strcat(s,",");
      j = (*m)[j].ccs;
    }
  }
  else fprintf(logFile,"ERROR: Form Stack %i\n",i);
}

/* This version, which is used, adds the module and group name to the cost centre name*/
/* This means the cost centre name remains unique when it is textualised and fed into */
/* daVinci. It also allows the module and group name to be extracted at the display   */
/* level */

void FormStack2(ccs_matrix *m, cc_matrix *n, int i, char s[])
{
  int j = i;
  if ((*m)[j].cc != 0)
  {
    strcat(s,(*n)[(*m)[j].cc].name);
    strcat(s,"&");
    strcat(s,(*n)[(*m)[j].cc].module);
    strcat(s,"&");
    strcat(s,(*n)[(*m)[j].cc].group);
    strcat(s," ");
    while ((*m)[j].ccs != (-1))
    {
      strcat(s,(*n)[(*m)[(*m)[j].ccs].cc].name);
      strcat(s,"&");
      strcat(s,(*n)[(*m)[(*m)[j].ccs].cc].module);
      strcat(s,"&");
      strcat(s,(*n)[(*m)[(*m)[j].ccs].cc].group);
      strcat(s,",");
      j = (*m)[j].ccs;
    }
  }
  else fprintf(logFile,"ERROR: Form Stack %i\n",i);
}

void PrintStack(ccs_matrix *m, cc_matrix *n, int i)
{
  int j = i;
  if ((*m)[j].cc != 0)
  {
    fprintf(logFile,"<"); 
    fprintf(logFile,"%s,",(*n)[(*m)[j].cc].name);
    while ((*m)[j].ccs != (-1))
    {
      fprintf(logFile,"%s,",(*n)[(*m)[(*m)[j].ccs].cc].name);
      j = (*m)[j].ccs;
    }
    fprintf(logFile,"> ");
    fprintf(logFile,"%i scc %i ticks %i bytes  ",
            (*m)[i].scc,(*m)[i].ticks,(*m)[i].bytes);
    print_heap_sample_matrix((*m)[i].hsm);
  }
  else
  { /* fprintf(logFile,"empty stack\n"); */ }
}

int CountStacks(ccs_matrix *m)
{
  int j;
  int count = 0;
  for (j=0; j<MAX_IDENTIFIERS;j++) if ((*m)[j].cc != 0) count++;
  return count;
}

void PrintAllStacks(ccs_matrix *m, cc_matrix *n)
{
  int i;
  fprintf(logFile,"Stacks\n======\n");
  for (i=0;i<MAX_IDENTIFIERS;i++) { PrintStack(m,n,i); }
}


/* -----------------------------------------------------------------------------
 * TCL Heap profile generator
 * -------------------------------------------------------------------------- */

void produce_HEAP_PROFILE(FILE *HEAP_PROFILE, TheHeap *th, heap_object_matrix *ho_m)
{
  // First the header information 
  fprintf(HEAP_PROFILE,"#!/home/sj/blt2.4o/src/bltwish\n");
  fprintf(HEAP_PROFILE,"package require BLT\n");
  fprintf(HEAP_PROFILE,"if { $tcl_version >= 8.0 } {\n");
  fprintf(HEAP_PROFILE,"\t \t namespace import blt::*\n");
  fprintf(HEAP_PROFILE,"namespace import -force blt::tile::*\n");
  fprintf(HEAP_PROFILE,"}\n");
  fprintf(HEAP_PROFILE,"source scripts/demo.tcl\n");
  fprintf(HEAP_PROFILE,"proc FormatXTicks { w value } {\n");
  fprintf(HEAP_PROFILE,"\t \t set index [expr round($value)]\n");
  fprintf(HEAP_PROFILE,"\t \t if { $index != $value } {\n");
  fprintf(HEAP_PROFILE,"\t \t \t return $value\n");
  fprintf(HEAP_PROFILE,"\t \t}\n");
  fprintf(HEAP_PROFILE,"incr index -1\n");

  // Now the code to generate the units in the X axis

  fprintf(HEAP_PROFILE,"set name [lindex { ");
  PrintXaxis(HEAP_PROFILE,th);
  fprintf(HEAP_PROFILE," } $index]\n");

  fprintf(HEAP_PROFILE,"return $name\n");
  fprintf(HEAP_PROFILE,"}\n");
  
  // more general graph stuff 

  fprintf(HEAP_PROFILE,"source scripts/stipples.tcl\n");
  fprintf(HEAP_PROFILE,"image create photo bgTexture -file ./images/chalk.gif\n");
  fprintf(HEAP_PROFILE,"option add *Button.padX			5\n");
  fprintf(HEAP_PROFILE,"option add *tile			bgTexture\n");
  fprintf(HEAP_PROFILE,"option add *Radiobutton.font		-*-courier*-medium-r-*-*-14-*-*\n");
  fprintf(HEAP_PROFILE,"option add *Radiobutton.relief		flat\n");
  fprintf(HEAP_PROFILE,"option add *Radiobutton.borderWidth     2\n");
  fprintf(HEAP_PROFILE,"option add *Radiobutton.highlightThickness 0\n");
  fprintf(HEAP_PROFILE,"option add *Htext.font			-*-times*-bold-r-*-*-14-*-*\n");
  fprintf(HEAP_PROFILE,"option add *Htext.tileOffset		no\n");
  fprintf(HEAP_PROFILE,"option add *header.font			-*-times*-medium-r-*-*-14-*-*\n");
  fprintf(HEAP_PROFILE,"option add *Barchart.font		 -*-helvetica-bold-r-*-*-14-*-*\n");

  fprintf(HEAP_PROFILE,"option add *Barchart.title		\"Heap profile of program ");
  // TO DO: Add program name in here
  fprintf(HEAP_PROFILE,"\"\n");

  fprintf(HEAP_PROFILE,"option add *Axis.tickFont		-*-helvetica-medium-r-*-*-12-*-*\n");
  fprintf(HEAP_PROFILE,"option add *Axis.titleFont		-*-helvetica-bold-r-*-*-12-*-*\n");
  fprintf(HEAP_PROFILE,"option add *x.Command			FormatXTicks\n");
  fprintf(HEAP_PROFILE,"option add *x.Title			\"Time (seconds)\"\n");
  fprintf(HEAP_PROFILE,"option add *y.Title			\"Heap usage (000 bytes)\"\n");
  fprintf(HEAP_PROFILE,"option add *activeBar.Foreground	pink\noption add *activeBar.stipple		dot3\noption add *Element.Background		red\noption add *Element.Relief		raised\n");
  fprintf(HEAP_PROFILE,"option add *Grid.dashes			{ 2 4 }\noption add *Grid.hide			no\noption add *Grid.mapX			\"\"\n");
  fprintf(HEAP_PROFILE,"option add *Legend.Font			\"-*-helvetica*-bold-r-*-*-12-*-*\"\noption add *Legend.activeBorderWidth	2\noption add *Legend.activeRelief		raised \noption add *Legend.anchor		ne \noption add *Legend.borderWidth		0\noption add *Legend.position		right\n");
  fprintf(HEAP_PROFILE,"option add *TextMarker.Font		*Helvetica-Bold-R*14*\n");
  fprintf(HEAP_PROFILE,"set visual [winfo screenvisual .] \nif { $visual != \"staticgray\" && $visual != \"grayscale\" } {\n    option add *print.background	yellow\n    option add *quit.background		red\n    option add *quit.activeBackground	red2\n}\n");
  fprintf(HEAP_PROFILE,"htext .title -text {\n    Heap profile\n}\n");
  fprintf(HEAP_PROFILE,"htext .header -text {\n    %%%% \n");
  fprintf(HEAP_PROFILE,"      radiobutton .header.stacked -text stacked -variable barMode \\\n            -anchor w -value \"stacked\" -selectcolor red -command {\n            .graph configure -barmode $barMode\n        } \n        .header append .header.stacked -width 1.5i -anchor w\n");
  fprintf(HEAP_PROFILE,"    %%%%      Heap usage stacked: overall height is the sum of the heap used. \n    %%%% \n");
  fprintf(HEAP_PROFILE,"        radiobutton .header.aligned -text aligned -variable barMode \\\n          -anchor w -value \"aligned\" -selectcolor yellow -command {\n            .graph configure -barmode $barMode        }\n        .header append .header.aligned -width 1.5i -fill x\n");
  fprintf(HEAP_PROFILE,"    %%%%      Heap usage components displayed side-by-side.\n    %%%%\n");
  fprintf(HEAP_PROFILE,"        radiobutton .header.overlap -text \"overlap\" -variable barMode \\\n            -anchor w -value \"overlap\" -selectcolor green -command {\n            .graph configure -barmode $barMode\n        }\n         .header append .header.overlap -width 1.5i -fill x\n");
  fprintf(HEAP_PROFILE,"    %%%%      Heap  usage shown as an overlapped histogram.\n    %%%%\n");
  fprintf(HEAP_PROFILE,"        radiobutton .header.normal -text \"normal\" -variable barMode \\\n            -anchor w -value \"normal\" -selectcolor blue -command {\n            .graph configure -barmode $barMode\n        }\n         .header append .header.normal -width 1.5i -fill x\n");
  fprintf(HEAP_PROFILE,"    %%%%      Heap components overlayed one on top of the next. \n}\n");
  fprintf(HEAP_PROFILE,"htext .footer -text { To create a postscript file \"heap_profile.ps\", press the %%%%\n  button $htext(widget).print -text print -command {\n        puts stderr [time {.graph postscript output heap_profile.ps}]\n  }\n  $htext(widget) append $htext(widget).print\n%%%% button.}\n");
  fprintf(HEAP_PROFILE,"barchart .graph -tile bgTexture\n");

  // This is where the actual data comes in

  fprintf(HEAP_PROFILE,"vector X ");
  names_of_heap_objects(HEAP_PROFILE,ho_m);
  fprintf(HEAP_PROFILE,"\nX set { ");
  PrintXaxis(HEAP_PROFILE,th);
  fprintf(HEAP_PROFILE," }\n");

  print_heap_object_matrix(HEAP_PROFILE,th, ho_m);

  // NAMES FOR THE ATTRIBUTES 
  fprintf(HEAP_PROFILE,"set attributes {\n");
  names_and_colour_assignment(HEAP_PROFILE,ho_m);
  fprintf(HEAP_PROFILE,"}\n");
  
  fprintf(HEAP_PROFILE,"foreach {label yData color stipple} $attributes {\n    .graph element create $yData -label $label -bd 1 \\\n	-ydata $yData -xdata X -fg ${color}3 -bg ${color}1 -stipple $stipple\n}\n");
  fprintf(HEAP_PROFILE,".header.stacked invoke\n");
  fprintf(HEAP_PROFILE,"scrollbar .xbar -command { .graph axis view x } -orient horizontal\nscrollbar .ybar -command { .graph axis view y } -orient vertical\n.graph axis configure x -scrollcommand { .xbar set } -logscale no -loose no\n.graph axis configure y -scrollcommand { .ybar set } -logscale no -loose no\n");
  fprintf(HEAP_PROFILE,"table . \\\n    0,0 .title -fill x \\\n    1,0 .header -fill x  \\\n    2,0 .graph -fill both \\\n    3,0 .xbar -fill x \\\n    5,0 .footer -fill x\n");
  fprintf(HEAP_PROFILE,"table configure . r0 r1 r3 r4 r5 -resize none\n");
  fprintf(HEAP_PROFILE,"Blt_ZoomStack .graph\nBlt_Crosshairs .graph\nBlt_ActiveLegend .graph\nBlt_ClosestPoint .graph\n");
  fprintf(HEAP_PROFILE,".graph marker bind all <B2-Motion> {\n    set coords [%%W invtransform %%x %%y]\n    catch { %%W marker configure [%%W marker get current] -coords $coords }\n}\n.graph marker bind all <Enter> {\n    set marker [%%W marker get current]\n    catch { %%W marker configure $marker -bg green}\n}\n.graph marker bind all <Leave> {\n    set marker [%%W marker get current]\n    catch { %%W marker configure $marker -bg \"\"}\n}\n");

}


/* -----------------------------------------------------------------------------
 * Read and create the raw profile data structure
 * -------------------------------------------------------------------------- */

/* void readRawProfile(FILE *fptr,int *nonodes) { */

void readRawProfile(FILE *fp,int *nonodes, int MaxNoNodes) {
  char    stack[MAX_PROFILE_LINE_LENGTH];
  int     i,nolines,sstepline,syncs;
  char   *ptr,*drag;

  float   comp_max,      comp_avg,      comp_min,          /* SYNCS    */
          comm_max,      comm_avg,      comm_min,          /* COMP     */
          comp_idle_max, comp_idle_avg, comp_idle_min;     /* COMM     */

  /* Cost  relationships are comp=scc, comm=ticks, comp_idle=bytes */

  long int hmax,havg,hmin;                                 /* COMPIDLE */

  /* set to zero for now. Might use these later for heap costs. */

  /* GHC specific variables */

  int a,b,c,d,x,z,count, next;
  int newloop;
  char e[MAX_STRING_SIZE];
  char f[MAX_STRING_SIZE];
  char lline[MAX_PROFILE_LINE_LENGTH];

  /* identifiers generated by the XML handler */
  char *ccentre=">>cost_centre";
  char *ccstack=">>cost_centre_stack";
  char *sccsample=">>scc_sample";
  char *heapsample=">>heap_sample";
  char *heapupdate=">>heap_update";
  char *heapobject=">>heap_object";
  char *typeconstr=">>type_constr";
  char *ending=">>";

  /* FILE *fp; */

  cc_matrix *cc_m;
  ccs_matrix *ccs_m;
  heap_object_matrix *ho_m;
  type_constr_matrix *tc_m;
  TheHeap *th;

  FILE *HEAP_PROFILE;

  HEAP_PROFILE = fopen("GHCbarchart.tcl", "w");
  if (HEAP_PROFILE == NULL){
   fprintf(stderr,"tcl script generator: ERROR- GHCbarchart.tcl cannot be created\a\n");
    exit(1);
  }

  th = (p_TheHeap) malloc (sizeof(TheHeap));
  cc_m = (p_cc_matrix) malloc (sizeof(cc_matrix));
  //cc_m = (p_cc_matrix) calloc(MAX_IDENTIFIERS,sizeof(_cc_));
  ccs_m = (p_ccs_matrix) malloc (sizeof(ccs_matrix));
  ho_m  = (p_heap_object_matrix) malloc (sizeof(heap_object_matrix));
  tc_m  = (p_type_constr_matrix) malloc (sizeof(type_constr_matrix));

  /* End of GHC specific variables */

  //fprintf(logFile,"Number 1 %i \n",MAX_IDENTIFIERS*sizeof(_cc_));
  //fprintf(logFile,"Number 2 %i \n",sizeof(cc_matrix));

  nolines=0; /* Number of lines read in from profile log file */

  /* GHC specific */
  count = 0;
  next = 0;

  initialise_cc_matrix(cc_m);
  initialise_ccs_matrix(ccs_m);
  initialise_heap_object_matrix(ho_m);
  initialise_type_constr_matrix(tc_m);
  initialise_TheHeap(th);

  fprintf(logFile,"MAX_IDENTIFIERS = %i \n",MAX_IDENTIFIERS);
  
  /* end GHC specific */

  /* CAF fixing */
  fill_cc_matrix(cc_m,"CAF:REPOSITORY","PROFILER","PROFILER",MAX_IDENTIFIERS-1);
  fill_ccs_matrix(ccs_m,MAX_IDENTIFIERS-1,1,0.0,0.0,0.0,0,-1,MAX_IDENTIFIERS-1);

  /* 

  This builds a node in the graph called CAF:REPOSITORY, which can be 
  found off the root node. All CAFs are subsequently hung from this node
  which means the node node can be hidden using the abstraction 
  mechanisms provided by daVinci.

  */


  /* This is the GHC file handler which reads the lines from the profile log file and */
  /* puts the stack and cost information in the raw profile data structure */

   while (fscanf(fp,"%s",lline))
   { 
    /* Kill the end of the logfile with the ">>" string */
    if (strcmp(lline,ending)==0) break;

    /* Deal with the cost centres */
    if (strcmp(ccentre,lline)==0)
    {
      next = fgetc(fp);
      //while (fscanf(fp," %i %[^ ] %[^ ] %s", &z, e, f, g)!=0)
      while (fscanf(fp," %i %[^ ] %s", &z, e, f)!=0)
      {
        fprintf(logFile,"Declaring cost centre `%i %s %s %s' \n",z,e,f,f);
        fflush(logFile);
        fill_cc_matrix(cc_m,e,f,f,z);
        next = fgetc(fp);
      }
    }
    else 
    {

      /* Deal with the cost centre stacks */
      if (strcmp(ccstack,lline)==0)
      { 
        next = fgetc(fp);
        while (fscanf(fp,"%i %i %i",&a,&d,&b)!=0)
        {
          if (d==1) /* of size one */
          {  
            fprintf(logFile,"Declaring cost centre stack `%i %i %i'\n",a,d,b);
            fill_ccs_matrix(ccs_m,b,-1,(*ccs_m)[a].scc,(*ccs_m)[a].ticks,(*ccs_m)[a].bytes,0,-1,a);
          }
          if (d==2) /* of size > 1 */
          { 
            fscanf(fp," %i",&c);

            /* CAF fixing */
            fprintf(logFile,"Declaring cost centre stack `%i %i %i %i'\n",a,d,b,c);
            if ((c==1)&&!(strncmp((*cc_m)[b].name,"CAF",2)))
               // fill_ccs_matrix(ccs_m,b,MAX_IDENTIFIERS-1,(*ccs_m)[a].scc,(*ccs_m)[a].ticks,(*ccs_m)[a].bytes,0,-1,a);
               /* The line above hangs all CAFs off the CAF:REPOSITORY node
                  in the daVinci graph. For programs which have a small 
                  number of CAFs this works nicely. However, when the 
                  number of CAFs become very large (eg +200) then the 
                  daVinci graph begins to look horid and, after (say)
                  +500 CAF nodes, becomes very slow to load. So to 
                  fix this we replace the code with the line below.
               */
                 if (!(strncmp((*cc_m)[b].name,"CAF:main",7)))
                    /* Treat CAF:main as a normal node */ 
                    fill_ccs_matrix(ccs_m,b,c,(*ccs_m)[a].scc,(*ccs_m)[a].ticks,(*ccs_m)[a].bytes,0,-1,a); 
                    /* merge the rest */
                 else
                    //add_ccs_costs(ccs_m,0,MAX_IDENTIFIERS-1,(*ccs_m)[a].scc,(*ccs_m)[a].ticks,(*ccs_m)[a].bytes,0,0);
                    fill_ccs_matrix(ccs_m,MAX_IDENTIFIERS-1,1,(*ccs_m)[a].scc,(*ccs_m)[a].ticks,(*ccs_m)[a].bytes,0,-1,a);
                /* This does not even bother registering the new CAFs
                   as daVinci nodes, but instead just merges the CAF
                   with the CAF:REPOSITORY node. This greatly reduces
                   the number of CAFs daVinci has to deal with, though
                   may make the graph look a little different!

                   Also note that now Simon has changed the semantics,
                   you will want to treat adding CAF nodes in a 
                   different way to adding normal program nodes
                 */
            else 
               /* Normal mode */
               fill_ccs_matrix(ccs_m,b,c,(*ccs_m)[a].scc,(*ccs_m)[a].ticks,(*ccs_m)[a].bytes,0,-1,a);
          }
          next = fgetc(fp);
        }
      } 
      else
      {

        /* Deal with the scc_samples */
        if (strcmp(sccsample,lline)==0)
        {
          next = fgetc(fp);
          while (fscanf(fp,"%i %i %i %i",&a,&d,&b,&c))
          {
            fprintf(logFile,"Loading scc_samples `%i %i %i %i'\n",a,d,b,c);
            add_ccs_costs(ccs_m,0,a,d,b,c,0,0);
            next = fgetc(fp);
          }
        } /* end sccsample if */
        else 
        {
        
          /* Deal with the heap samples */
          if (strcmp(heapsample,lline)==0)
          {
            next = fgetc(fp);
            while (fscanf(fp,"%i %i %i",&a,&d,&b))
            {
              fprintf(logFile,"Loading heap_samples `%i %i %i'\n",a,d,b);
              add_heap_sample_costs(ccs_m,0,a,0,0,0,d,b);
              next = fgetc(fp);
            }
          } /* end heapsample if */
          else 
          {

            /* Deal with the heap objects */
            if (strcmp(heapobject,lline)==0)
            {
              next = fgetc(fp);
              while (fscanf(fp,"%i %i",&a,&d)) 
              {
                if (d==1) 
                {
                  fscanf(fp," %s %i",e,&b);
                  add_heap_object(ho_m,a,d,e,b);
                }
                else 
                {
                  fscanf(fp," %s",e);
                  add_heap_object(ho_m,a,d,e,-1);
                }
                next = fgetc(fp);
              }
            } /* end heapobject if */
            else
            {

              /* Deal with the type constructors */ 
              if (strcmp(typeconstr,lline)==0)
              {
                next = fgetc(fp);
                while (fscanf(fp,"%i %s %s",&a,e,f))
                {
                  add_type_constr_object(tc_m,a,e,f);
                  next = fgetc(fp);
                }
              } /* end type constructor if */
              else
              {
                
                /* Deal with the heap_updates */ 
                if (strcmp(heapupdate,lline)==0)
                {
                  next = fgetc(fp);
                  while (fscanf(fp,"%i %i %i %i %i %i",&a,&d,&b,&c,&z,&x))
                  {
                    add_to_TheHeap(th,a,b,c,z);
                    fprintf(logFile,"Adding heap sample %i %i %i %i\n",a,b,c,z);
                    while (x) /* more than one sample */
                    {
                      fscanf(fp," %i %i %i %i",&b,&c,&z,&x);
                      add_to_TheHeap(th,a,b,c,z);
                      fprintf(logFile,"Adding heap sample %i %i %i %i\n",a,b,c,z);
                    }  
                    next = fgetc(fp);
                  }

                } /* end heap update if */

              }  /* end type constructor else */

             } /* end heapobject else */

           } /* end heapsample else */
         } /* end sccsample else */
       } /* end ccstack else */
     } /* end ccstack if */
   } /* end while */

   print_cc_matrix(cc_m);
   print_ccs_matrix(ccs_m);
   fprintf(logFile,"There are %i stacks\n",CountStacks(ccs_m));
   print_type_constr_matrix(tc_m);

   /* Functions for heap profile */
   print_TheHeap(th);
   fprintf(logFile,"The units for the x axis are \n");
   PrintXaxis(logFile,th);
   fprintf(logFile,"\n");
   fprintf(logFile,"There are %i distinct heap objects\n",number_of_heap_objects(ho_m));
   names_of_heap_objects(logFile,ho_m);
   names_and_colour_assignment(logFile,ho_m);
   print_heap_object_matrix(logFile,th,ho_m);

   PrintAllStacks(ccs_m,cc_m);
   /* comment out line below to remove the heap profile generator */
   produce_HEAP_PROFILE(HEAP_PROFILE,th,ho_m);
   fclose(HEAP_PROFILE);

   /* End of GHC file handler */
  

  /* Now process the stack matrix */ 

  for (newloop=0;newloop<MAX_IDENTIFIERS;newloop++) 
  { if ((*ccs_m)[newloop].cc != 0)
    {
         
    sstepline = 0;
    FormStack2(ccs_m,cc_m,newloop,stack);

    syncs = 0;     
    comp_max = (float)(*ccs_m)[newloop].scc; 
    comp_avg = (float)(*ccs_m)[newloop].scc; 
    comp_min = (float)(*ccs_m)[newloop].scc; 
    comm_max = (float)(*ccs_m)[newloop].ticks; 
    comm_avg = (float)(*ccs_m)[newloop].ticks; 
    comm_min = (float)(*ccs_m)[newloop].ticks; 
    comp_idle_max = (float)(*ccs_m)[newloop].bytes; 
    comp_idle_avg = (float)(*ccs_m)[newloop].bytes; 
    comp_idle_min = (float)(*ccs_m)[newloop].bytes; 
    hmax = 0.0; havg = 0.0; hmin = 0.0; 

      /* Dynamic memory allocation for raw_profile data structure */ 

      if (raw_profile_next==raw_profile_size) enlargeRawProfile();

      /* Assign data from single logfile entry to raw_profile data structure */
      /* this deals with the cost metrics */

      raw_profile[raw_profile_next].active            = 1;
      raw_profile[raw_profile_next].cost.syncs        = syncs;
      raw_profile[raw_profile_next].cost.comp_max     = comp_max;
      raw_profile[raw_profile_next].cost.comp_avg     = comp_avg;
      raw_profile[raw_profile_next].cost.comp_min     = comp_min;
      raw_profile[raw_profile_next].cost.comm_max     = comm_max;
      raw_profile[raw_profile_next].cost.comm_avg     = comm_avg;
      raw_profile[raw_profile_next].cost.comm_min     = comm_min;
      raw_profile[raw_profile_next].cost.comp_idle_max= comp_idle_max;
      raw_profile[raw_profile_next].cost.comp_idle_avg= comp_idle_avg;
      raw_profile[raw_profile_next].cost.comp_idle_min= comp_idle_min;
      raw_profile[raw_profile_next].cost.hrel_max     = hmax;
      raw_profile[raw_profile_next].cost.hrel_avg     = havg;
      raw_profile[raw_profile_next].cost.hrel_min     = hmin;

      /* this deals with the stack itself */

      raw_profile[raw_profile_next].stack=calloc(MAX_STACK_DEPTH,
						 sizeof(int));
      if (raw_profile[raw_profile_next].stack==NULL) {
        fprintf(stderr,"{readRawProfile} unable to allocate stack entry");
        exit(1);
      }

      fprintf(logFile,"STACK=\"%s\"\n",stack);
      raw_profile[raw_profile_next].stack_size=1;
      /* move the stack read frame to the first space (or comma) in the stack string */ 
      for(ptr=stack; ((*ptr)!=' ') && (*ptr!=',');ptr++) {}
      fprintf(logFile,"TOS=%d at line %d\n",*ptr,sstepline);
     
      /* to distinguish the head of the stack from the rest */
      /* if read frame points to space you are at the head of the stack */
      if (*ptr==' ') 
        /* raw_profile[raw_profile_next].stack[0]
          =lookupSymbolTable(CG_SSTEP,sstepline,(*ptr='\0',stack)); */
        /* This line has changed as GHC treats its cost-centres in a different     */
        /* way to BSP. There is no distinction between 'a cost centre at line x'   */
        /* and a normal cost centre. The fix is easy, just treat all cost centres, */
        /* even those at the head of the stack in the same way.                    */
           raw_profile[raw_profile_next].stack[0]
          =lookupSymbolTable(CG_STACK,sstepline,(*ptr='\0',stack));
      else
      /* otherwise you are looking at just another stack element */
        raw_profile[raw_profile_next].stack[0]
          =lookupSymbolTable(CG_STACK,sstepline,(*ptr='\0',stack));

      ptr++; /* move the read frame on one */
      drag=ptr;
      for(;*ptr;ptr++) { /* find the next element in the stack */
        if (*ptr==',') {
	  *ptr='\0';
          if (Verbose) fprintf(logFile,"NAME=\"%s\"\n",drag); /* name of the next element */
          if (!ignore_function(drag)) {
            raw_profile[raw_profile_next].stack[
              raw_profile[raw_profile_next].stack_size++]
              = lookupSymbolTable(CG_STACK,0,drag); /* add element to the raw_profile */
	  }
          drag = ptr+1;
        }
      }

      /* create cost object */

      raw_profile[raw_profile_next].cost.proc
	=calloc(bsp_p,sizeof(object_cost_proc));
      if (raw_profile[raw_profile_next].cost.proc==NULL) {
	fprintf(stderr,"Unable to allocate storage");
	exit(0);
      }
  
      /* process the HREL information - one set for every BSP process */

      for(i=0;i<bsp_p;i++) {

         raw_profile[raw_profile_next].cost.proc[i].proc_comp     = 0.0; 
         raw_profile[raw_profile_next].cost.proc[i].proc_comm     = 0.0; 
         raw_profile[raw_profile_next].cost.proc[i].proc_comp_idle= 0.0; 
         raw_profile[raw_profile_next].cost.proc[i].proc_hrel_in  = 0; 
         raw_profile[raw_profile_next].cost.proc[i].proc_hrel_out = 0; 

      }

      raw_profile_next++;    /* Increase the raw profile data structure counter */
      nolines++;             /* Increase the number of lines read               */

       strcpy(stack,""); /* reset the stack */
    } /* end of new if statement */
  } /* end of new for loop */

  *nonodes = symbol_table_next;
  fprintf(logFile,"%s: read %d lines from profile.Graph contains %i nodes.\n",
          Pgm,nolines,symbol_table_next);

  free_cc_matrix(cc_m); /* be nice and clean up the cost centre matrix */
}

/* -----------------------------------------------------------------------------
 * Pretty print the raw profile data
 * -------------------------------------------------------------------------- */

void printRawProfile() {
  int i,j;
  object_cost *cost;
  int         *stack;
  
  fprintf(logFile,"\n\nRAW DATA:\n");
  for(i=0;i<raw_profile_next;i++) {
    cost  = &raw_profile[i].cost;
    stack = raw_profile[i].stack;
    fprintf(logFile,"Stack=[");
    for(j=0;j<raw_profile[i].stack_size;j++) 
      printSymbolTable_entry(stack[j]);
    fprintf(logFile,"] %d Syncs %f Comp %f Comm %f Wait\n\n",
	    cost->syncs,cost->comp_max,cost->comm_max,cost->comp_idle_max);
  }
}

/* -----------------------------------------------------------------------------
 * Create connectivity matrix
 * -------------------------------------------------------------------------- */

void createConnectivityMatrix(int NoNodes,Matrix *graph,
			      Matrix *costs,int *root, int inherit) {
  object_cost zero_cost,*update;
  int i,j,this,next;


  zero_cost.comp_max     =0.0;
  zero_cost.comp_avg     =0.0;
  zero_cost.comp_min     =0.0;
  zero_cost.comm_max     =0.0;
  zero_cost.comm_avg     =0.0;
  zero_cost.comm_min     =0.0;
  zero_cost.comp_idle_max=0.0;
  zero_cost.comp_idle_avg=0.0;
  zero_cost.comp_idle_min=0.0;
  zero_cost.hrel_max     =0;
  zero_cost.hrel_avg     =0;
  zero_cost.hrel_min     =0;
  zero_cost.syncs=0;
  zero_cost.proc = NULL;
  *graph = newMat(NoNodes,NoNodes,sizeof(int),(i=0,&i));
  *costs = newMat(NoNodes,1,sizeof(object_cost),&zero_cost);
  for(i=0;i<NoNodes;i++) {
    update=&Mat(object_cost,*costs,i,0);
    update->proc=calloc(bsp_p,sizeof(object_cost_proc));
    if (update->proc==NULL){
      fprintf(stderr,"Unable to allocate storage");
      exit(0);
    }
    for(j=0;j<bsp_p;j++) {
      update->proc[j].proc_comp      =0.0;
      update->proc[j].proc_comm      =0.0;
      update->proc[j].proc_comp_idle =0.0;
      update->proc[j].proc_hrel_in   =0;
      update->proc[j].proc_hrel_out  =0;
    }
  }
      
  for(i=0;i<raw_profile_next;i++) {
    if (raw_profile[i].active) {
      this = raw_profile[i].stack[0];
      next = this;
      Mat(int,*graph,this,next) = 1;
      update = &Mat(object_cost,*costs,next,0);
      add_costs(update,raw_profile[i].cost);
      for(j=1;j<raw_profile[i].stack_size;j++) {
        this = next;
        next = raw_profile[i].stack[j];
        Mat(int,*graph,next,this)=1;
	update = &Mat(object_cost,*costs,next,0);
        /* include this line for INHERITANCE; remove it for not! */
        if (inherit) add_costs(update,raw_profile[i].cost);
      }
    }
  }
  *root =  raw_profile[0].stack[raw_profile[0].stack_size-1];

  /* Check graph isn't empty */
  if (!Mat_dense(*costs,*root,0)) *root=-1;
}

void printConnectivityMatrix(Matrix graph,Matrix costs,int root) { 
  int i,j;
  object_cost cost;

  fprintf(logFile,"Root node is %d\n",root);
  for(i=0;i<graph.rows;i++) {
    fprintf(logFile,"%4d)",i);
    printSymbolTable_entry(i);
    cost = Mat(object_cost,costs,i,0);
    fprintf(logFile,"%d %f %f %f\n\tBranch=[",
	    cost.syncs,cost.comp_max,cost.comm_max,cost.comp_idle_max);
    for(j=0;j<graph.cols;j++) 
      if (Mat_dense(graph,i,j)) fprintf(logFile,"%d ",j);
    fprintf(logFile,"]\n\n");
  }
}

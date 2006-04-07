/* ------------------------------------------------------------------------
 * $Id: daVinci.c,v 1.5 2006/01/09 14:38:01 simonmar Exp $
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

#include "daVinci.h"
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

static char* extra_space(int);
static void recur_graphToDaVinci(int,Matrix *, Matrix *,char*,int);
static char *parse_word(char**);
static char *parse_quoted(char**);
static char *dup_str(char*);
double this_total_time,
       this_total_comp_max, this_total_comp_avg,
       this_total_comm_max, this_total_comm_avg, 
       this_total_comp_idle_max, this_total_comp_idle_avg;
long int this_hrel_max, this_hrel_avg;
int  this_syncs;

char *lastDavinciCmd;

/* -----------------------------------------------------------------------------
 * Send a command with ok return value daVinci
 * -------------------------------------------------------------------------- */

void cmdDaVinci(char* format,...) {
  va_list args;

  va_start(args, format);
  vfprintf(stdout, format, args);
  fprintf(stdout, "\n");
  va_end(args);
  fflush(stdout); 
  lastDavinciCmd = format;
}

/* -----------------------------------------------------------------------------
 * Initialise daVinci
 * -------------------------------------------------------------------------- */

void initDaVinci() {
  cmdDaVinci("window(title(\"GHC profiler: cost-centre-stack view\"))\n");
  cmdDaVinci("set(font_size(8))");  
  cmdDaVinci("set(animation_speed(0))");
  cmdDaVinci("set(scrolling_on_selection(false))");
  /* SAJ */
  /* cmdDaVinci("set(no_cache(true)))"); */
  cmdDaVinci("app_menu(create_icons(["
                  "icon_entry(\"delete\","
                             "\"delete.xbm\","
                             "\"Delete node and its children\"),"
                  "icon_entry(\"undo\","
                             "\"undo.xbm\","
                             "\"Undo delete\"),"
                  "blank,"
                  "icon_entry(\"time\","
                             "\"time.xbm\","
                             "\"Cost metric view\"),"
                  "icon_entry(\"percent\","
                             "\"percent.xbm\","
                             "\"Percentage view\"),"
                  "blank,"
                  "icon_entry(\"compress\","
                             "\"compress.xbm\","
                             "\"Compressed node view\"),"
                  "icon_entry(\"uncompress\","
                             "\"uncompress.xbm\","
                             "\"Uncompressed node view\"),"
                  "blank,"
                  "icon_entry(\"absolute\","
                             "\"absolute.xbm\","
                             "\"Display inherited profile results\"),"
                  "icon_entry(\"absdelta\","
                             "\"absdelta.xbm\","
                             "\"Display flat profile results\"),"
                  "icon_entry(\"reldelta\","
                             "\"reldelta.xbm\","
                             "\"Trim zero-cost sub-trees\"),"
                  "icon_entry(\"weightdelta\","
                             "\"weightdelta.xbm\","
                             "\"Trim zero-cost nodes\"),"
                  "blank,"
	          "icon_entry(\"sync\","
                             "\"sync.xbm\","
                             "\"Graph view\"),"
                  "icon_entry(\"comp\","
                             "\"comp.xbm\","
                             "\"SCCs critical path\"),"
                  "icon_entry(\"comm\","
                             "\"comm.xbm\","
                             "\"Computation time critical path\"),"
                  "icon_entry(\"wait\","
                             "\"wait.xbm\","
                             "\"Heap usage critical path\"),"
                  "icon_entry(\"hrel\","
                             "\"hrel.xbm\","
                             "\"Node spy\"),"
                  "blank,"
	          "icon_entry(\"help\","
                             "\"help.xbm\","
                             "\"Help\"),"
              "]))");

  activateDaVinciMenu("default");     
  cmdDaVinci("app_menu(create_menus([menu_entry_mne(\"jump\",\"Goto a node\",\"G\",control,\"G\")]))\n");
  /* SAJ */
  // cmdDaVinci("app_menu(activate_menus([\"jump\"]))"); 
}

/* -----------------------------------------------------------------------------
 * Menu FSM
 * -------------------------------------------------------------------------- */

void activateDaVinciMenu(char *pressed) {
  static int compress=1,time=1,critical_type=0,critical=0,undo=1,delete=0;

  if (strcmp(pressed,"absolute")==0)    critical_type=0;
  if (strcmp(pressed,"absdelta")==0)    critical_type=1;
  if (strcmp(pressed,"reldelta")==0)    critical_type=2;
  if (strcmp(pressed,"weightdelta")==0) critical_type=3;

  if (strcmp(pressed,"sync")==0)  critical=0;
  if (strcmp(pressed,"comp")==0)  critical=1;
  if (strcmp(pressed,"comm")==0)  critical=2;
  if (strcmp(pressed,"wait")==0)  critical=3;
  if (strcmp(pressed,"hrel")==0)  critical=4;

  if (strcmp(pressed,"compress")==0 || strcmp(pressed,"uncompress")==0) 
    compress=!compress;

  if (strcmp(pressed,"time")==0 || strcmp(pressed,"percent")==0)
    time=!time;

  if (strcmp(pressed,"undo")==0)   {undo=!undo;}
  if (strcmp(pressed,"delete")==0) {delete=!delete;}

  printf("app_menu(activate_icons([");
  if (critical_type!=0) printf("\"absolute\",");
  if (critical_type!=1) printf("\"absdelta\",");
  if (critical_type!=2) printf("\"reldelta\",");
  if (critical_type!=3) printf("\"weightdelta\",");

  if (critical!=0) printf("\"sync\",");
  if (critical!=1) printf("\"comp\",");
  if (critical!=2) printf("\"comm\",");
  if (critical!=3) printf("\"wait\",");
  if (critical!=4) printf("\"hrel\",");

  if (!compress)   printf("\"compress\",");
  if (compress)    printf("\"uncompress\",");
  if (!time)       printf("\"time\",");
  if (time)        printf("\"percent\",");
  if (!delete)     printf("\"delete\",");
  if (!undo)       printf("\"undo\",");
  
  cmdDaVinci("\"help\"]))");  
}

/* -----------------------------------------------------------------------------
 * Graph to daVinci
 * -------------------------------------------------------------------------- */

void graphToDaVinci(int root,Matrix *graph, Matrix *costs, int removezerocosts) {
  int i,j;
  object_cost *ptr;
  char zeronodes[MAX_PROFILE_LINE_LENGTH*2];     // is this a sen. MAX
  char TEMPzeronodes[MAX_PROFILE_LINE_LENGTH*2];
  char* p_zeronodes = zeronodes;
  char* TEMPp_zeronodes = TEMPzeronodes;
 
  printf("graph(new([");
  if (PrintLogo) {
    /* I have implemented some name changes here. They are purely for output and */
    /* following the relation (comp = scc, comm = ticks, wait = bytes            */
    printf("l(\"info\",n(\"\",["
	   "a(\"COLOR\",\"gold\"),"
	   "a(\"FONTFAMILY\",\"courier\"),"
	   //"a(\"_GO\",\"icon\"),"
	   //"a(\"ICONFILE\",\"oxpara.xbm\"),"
	   "a(\"OBJECT\",\""
           "Program statistics\\n\\n"
	   "Time elapsed     =  %6.2f ticks\\n"
	   "Heap usage       =  %6.2f bytes\\n"
	   "Total scc count  =  %6.2f (scc)\\n"
	   "\")],[])),",
           TotalComm,TotalCompIdle,
	   TotalComp
	   );
  } 

  if (root==-1) {
    printf("]))\n");
  } else {
    ptr = &Mat(object_cost,*costs,root,0);
    this_total_comp_max     = ptr->comp_max;
    this_total_comp_avg     = ptr->comp_avg;
    this_total_comm_max     = ptr->comm_max;
    this_total_comm_avg     = ptr->comm_avg;
    this_total_comp_idle_max= ptr->comp_idle_max;
    this_total_comp_idle_avg= ptr->comp_idle_avg;
    this_total_time         = 0.00001 + 
                              this_total_comp_max+ this_total_comm_max;
    this_hrel_max       = ptr->hrel_max;
    this_hrel_avg       = ptr->hrel_avg;
    this_syncs          = ptr->syncs;
    recur_graphToDaVinci(root,graph,costs,p_zeronodes,removezerocosts);

    printf("]))\n");
    fflush(stdout);
    cmdDaVinci("special(focus_node(\"%d\"))\n",root);

    /* graph will have been altered so that visted elements are marked
       by a negative value. These are reset */
    for(i=0;i<graph->rows;i++) {
      for(j=0;j<graph->cols;j++) {
        if (Mat_dense(*graph,i,j))
          if (Mat(int,*graph,i,j)<0) Mat(int,*graph,i,j)=1;
      }
    }

    if (removezerocosts==1)
    {
      if (strlen(p_zeronodes)>0) 
         { strncpy(TEMPp_zeronodes,p_zeronodes,strlen(p_zeronodes)-1);
           printf("select_nodes_labels([%s])\n",TEMPp_zeronodes);
         }
      strcpy(TEMPp_zeronodes,"");
      strcpy(p_zeronodes,"");
    }
  }
}

static char *printCompressNode(int node, object_cost *ptr) {
  char name[MAX_FUNNAME+20];
  char comp[MAX_FUNNAME+20];
  char comm[MAX_FUNNAME+20];
  static char res[(MAX_FUNNAME+20)*4];
  char tempstring[MAX_FUNNAME+20];
  char *padding;
  int x;
  char delimiter[] = "&";

  if (symbol_table[node].type==CG_SSTEP) 
    sprintf(name,"%d %s",
	    symbol_table[node].lineno,symbol_table[node].filename);
  else
  { 
    strcpy(tempstring,symbol_table[node].filename);
    sprintf(name,"%s",strtok(tempstring,delimiter));
  }  

  if (NodeviewTime) {
    /* changed this for GHC stats */
    sprintf(comp,"\\nTime  %6.2fticks\\n",ptr->comm_max);
    sprintf(comm,"Bytes %6.2funits",ptr->comp_idle_max);
  } else {
    sprintf(comp,"\\nTime  %6.2f%%\\n",(ptr->comm_max/TotalComm)*100.0);
    sprintf(comm,"Bytes %6.2f%%",(ptr->comp_idle_max/TotalCompIdle)*100.0);
  }
  /* Slightly arbitrary choice for max display length of CC string */
  /* If it is larger than this the display nodes look bad */
  if (strlen(name)>20) name[20]='\0';
  x=strlen(name);
  if (((20-(strlen(name)+3))/2)>19)
     padding = extra_space(0);
  else
     padding = extra_space((20-(strlen(name)+3))/2); /* includes \\n */
  strcpy(res,padding);
  strcat(res,name);
  strcat(res,comp);
  strcat(res,comm);
  return res;
}

static char *printUncompressNode(int node, object_cost *ptr) {
  char name   [MAX_FUNNAME+40];
  char module [MAX_FUNNAME+40];
  char group  [MAX_FUNNAME+40];
  char head [MAX_FUNNAME+40];
  char comp [MAX_FUNNAME+40];
  char comm [MAX_FUNNAME+40];
  char wait [MAX_FUNNAME+40];
  char hrel [MAX_FUNNAME+40];
  char tempstring[MAX_FUNNAME+20];
  char tempstring2[MAX_FUNNAME+20];
  char *tempstring3;
  char *tempstring5;
  char tempstring4[MAX_FUNNAME+20];
  char delimiter[] = "&";


  static char res[(MAX_FUNNAME+40)*7];
  char *padding;
  int width=0,x;

  if (symbol_table[node].type==CG_SSTEP) 
    sprintf(name,"%s line %d\\n",
	    symbol_table[node].filename,symbol_table[node].lineno);
  else
  {
    strcpy(tempstring,symbol_table[node].filename);
    strcpy(tempstring2,symbol_table[node].filename);
    sprintf(name,"%s",strtok(tempstring,delimiter));
    strcpy(tempstring4,tempstring2);
    tempstring5 = strpbrk(tempstring4,delimiter);
    sprintf(module,"%s",strtok(tempstring5+1,delimiter));
    tempstring3 = strrchr(tempstring2,'&');
    sprintf(group,"%s",tempstring3+1);
  }

  if (NodeviewTime) {

    sprintf(head, "Metric   Total  \\n");
    sprintf(comp, " Time    %6.2ft \\n",ptr->comm_max);
    sprintf(comm, " Bytes   %6.2fu \\n",ptr->comp_idle_max);
    sprintf(wait, " SCC     %6.2fc \\n",ptr->comp_max);


  } else {

    sprintf(head, "Metric   Total  \\n");
    sprintf(comp, " Time    %5.1f%% \\n",100.0*SAFEDIV(ptr->comm_max,TotalComm));
    sprintf(comm, " Bytes   %5.1f%% \\n",100.0*SAFEDIV(ptr->comp_idle_max,TotalCompIdle));
    sprintf(wait, " SCC     %5.1f%% \\n",100.0*SAFEDIV(ptr->comp_max,TotalComp));

  }
	  
  if ((x=strlen(name))>width)  width=x;
  if ((x=strlen(hrel))>width)  width=x;
  padding = extra_space((width-strlen(name)+3)/2); /* includes \\n */
  /* strcpy(res,padding); */
  strcpy(res,"Cost centre: ");
  strcat(res,name);
  strcat(res,"\\n");
  strcat(res,"Module     : ");
  strcat(res,module);
  strcat(res,"\\n");
  strcat(res,"Group      : ");
  strcat(res,group);
  strcat(res,"\\n\\n");
 
  strcat(res,head);
  strcat(res,comp);
  strcat(res,comm);
  strcat(res,wait);
  /* strcat(res,hrel); */
  return res;
}


double nodeColour(object_cost *cost) {

  switch (CriticalPath + CriticalType) {
  case CRITTYPE_ABSOLUTE+CRITICAL_SYNCS:      
  case CRITTYPE_ABSDELTA+CRITICAL_SYNCS:      
  case CRITTYPE_RELDELTA+CRITICAL_SYNCS:      
  case CRITTYPE_WEIGHTDELTA+CRITICAL_SYNCS:
    return SAFEDIV(((double)cost->syncs),((double)this_syncs));

  case CRITTYPE_ABSOLUTE+CRITICAL_COMP:       
    return SAFEDIV(cost->comp_max,this_total_comp_max);

  case CRITTYPE_ABSOLUTE+CRITICAL_COMM:       
    return SAFEDIV(cost->comm_max,this_total_comm_max);

  case CRITTYPE_ABSOLUTE+CRITICAL_WAIT:       
    return SAFEDIV(cost->comp_idle_max,this_total_comp_idle_max);

  case CRITTYPE_ABSOLUTE+CRITICAL_HREL:       
    return SAFEDIV(((double) cost->hrel_max),((double)this_hrel_max));

  case CRITTYPE_ABSDELTA+CRITICAL_COMP:
    return SAFEDIV(cost->comp_max,TotalComp);

  case CRITTYPE_ABSDELTA+CRITICAL_COMM:
    return SAFEDIV(cost->comm_max,TotalComm);

  case CRITTYPE_ABSDELTA+CRITICAL_WAIT:
    return SAFEDIV(cost->comp_idle_max,TotalCompIdle);

  case CRITTYPE_ABSDELTA+CRITICAL_HREL:
    return SAFEDIV(((double) (cost->hrel_max - cost->hrel_avg)),
	           ((double) (this_hrel_max-this_hrel_avg)));

  case CRITTYPE_RELDELTA+CRITICAL_COMP:
   return SAFEDIV((cost->comp_max-cost->comp_avg),
                  (cost->comp_avg*DeltaNormalise));

  case CRITTYPE_RELDELTA+CRITICAL_COMM:
   return SAFEDIV((cost->comm_max-cost->comm_avg),
           (cost->comm_avg*DeltaNormalise));

  case CRITTYPE_RELDELTA+CRITICAL_WAIT:
   return SAFEDIV((cost->comp_idle_max-cost->comp_idle_avg),
                  (cost->comp_idle_avg*DeltaNormalise));

  case CRITTYPE_RELDELTA+CRITICAL_HREL:
    return SAFEDIV(((double) (cost->hrel_max - cost->hrel_avg)),
	           ((double) (cost->hrel_avg*DeltaNormalise)));

  case CRITTYPE_WEIGHTDELTA+CRITICAL_COMP:
   return (SAFEDIV((cost->comp_max-cost->comp_avg),
                   (cost->comp_avg*DeltaNormalise))*
	   SAFEDIV(cost->comp_max,this_total_comp_max));

  case CRITTYPE_WEIGHTDELTA+CRITICAL_COMM:
   return (SAFEDIV((cost->comm_max-cost->comm_avg),
                   (cost->comm_avg*DeltaNormalise))*
           SAFEDIV(cost->comm_max,this_total_comm_max));

  case CRITTYPE_WEIGHTDELTA+CRITICAL_WAIT:
   return (SAFEDIV((cost->comp_idle_max-cost->comp_idle_avg),
                   (cost->comp_idle_avg*DeltaNormalise))*
	   SAFEDIV(cost->comp_idle_max,this_total_comp_idle_max));

  case CRITTYPE_WEIGHTDELTA+CRITICAL_HREL:
    return (SAFEDIV(((double) (cost->hrel_max - cost->hrel_avg)),
	            ((double) (cost->hrel_avg*DeltaNormalise)))*
	    SAFEDIV(((double) cost->hrel_max),((double)this_hrel_max)));

  }
  return 0.0;
}

int percentToColour(double colour) {
  int range=255,base=0;

  if (!Colour) {
    base =100;
    range=155;
  }
  if      (colour>1.0) return (base+range);
  else if (colour<0.0) return base;
  else return (((int) (((double)range)*colour))+base);
}

/* -----------------------------------------------------------------------------
 * Recursively draw the graph
 * -------------------------------------------------------------------------- */

static void recur_graphToDaVinci(int node,Matrix *graph,Matrix *costs,char* p_zeronodes, int mode){
  object_cost *ptr;
  int i,j,no_children=0,*children=NULL,colour;
  char *node_str;
  char tempnode[MAX_FUNNAME];
  if (Mat(int,*graph,node,node)<0) {
    printf("r(\"%d\") ",node);
  } else {
    for(i=0;i<graph->cols;i++) 
      if (node!=i && Mat_dense(*graph,node,i)) no_children++;
  
    if (no_children>0) {
      children = calloc(no_children,sizeof(int));
      if (children==NULL) {
        fprintf(stderr,"{printDaVinci} unable to allocate %d ",no_children);
        exit(1);
      }
      for((i=0,j=0);i<graph->cols;i++)
        if (node!=i && Mat_dense(*graph,node,i)) children[j++]=i;

      qsort(children,no_children,sizeof(int),
	    (int (*)(const void *,const void *)) cmp_symbol_entry);
    }
    ptr = &Mat(object_cost,*costs,node,0);
    node_str=(NodeviewCompress)?
	        printCompressNode(node,ptr):
	        printUncompressNode(node,ptr);
    printf("l(\"%d\",n(\"\",[a(\"OBJECT\",\"%s\"),",node,node_str);
    printf("a(\"FONTFAMILY\",\"courier\"),");
      

      // hide the CAF:REPOSITORY as default
      if (!strncmp(node_str,"Cost centre: CAF:REPOSITORY",26))
         printf("a(\"HIDDEN\",\"true\"),"); // when uncompressed
      if (!strncmp(node_str," CAF:REPOSITORY",12)) 
         printf("a(\"HIDDEN\",\"true\"),"); // when compressed


      if (mode==2)
      {
        if ((ptr->comm_max+ptr->comp_idle_max+ptr->comp_max) <= 0.0)
            printf("a(\"HIDDEN\",\"true\"),");
      }  
      //for pruning all zero-cost nodes
      if (mode==1)
      {
      if ((ptr->comm_max+ptr->comp_idle_max+ptr->comp_max) <= 0.0)
          { fprintf(logFile,"Node %d %s is a candidate for deletion\n",node, node_str);
            sprintf(tempnode,"\"%d\",",node);
            strcat(p_zeronodes,tempnode);
          }
      } 

    colour=percentToColour(1.0-nodeColour(ptr));
       printf("a(\"COLOR\",\"#ff%.2x%.2x\")",colour,colour);
    printf("],[");
    Mat(int,*graph,node,node)=-1;
    for(i=0;i<no_children;i++) {

      printf("e(\"%d->%d\",[],",node,children[i]);
 
      recur_graphToDaVinci(children[i],graph,costs,p_zeronodes,mode);
      printf(")");
      if (i<(no_children-1)) {printf(",");}
    } 
    printf("]))");
  } 
}



static void recur_graphToDaVinci_old(int node,Matrix *graph, Matrix *costs) {
  object_cost *ptr;
  int i,j,no_children=0,*children=NULL,colour;
  char *node_str;
  if (Mat(int,*graph,node,node)<0) {
    fprintf(logFile,"r(\"%d\") ",node);
    printf("r(\"%d\") ",node);
  } else {
    for(i=0;i<graph->cols;i++) 
      if (node!=i && Mat_dense(*graph,node,i)) no_children++;
  
    if (no_children>0) {
      children = calloc(no_children,sizeof(int));
      if (children==NULL) {
        fprintf(stderr,"{printDaVinci} unable to allocate %d ",no_children);
        exit(1);
      }
      for((i=0,j=0);i<graph->cols;i++)
        if (node!=i && Mat_dense(*graph,node,i)) children[j++]=i;

      qsort(children,no_children,sizeof(int),
	    (int (*)(const void *,const void *)) cmp_symbol_entry);
    }
    ptr = &Mat(object_cost,*costs,node,0);
    node_str=(NodeviewCompress)?
	        printCompressNode(node,ptr):
	        printUncompressNode(node,ptr);
    fprintf(logFile,"l(\"%d\",n(\"\",[a(\"OBJECT\",\"%s\"),",node,node_str);
    printf("l(\"%d\",n(\"\",[a(\"OBJECT\",\"%s\"),",node,node_str);
    fprintf(logFile,"a(\"FONTFAMILY\",\"courier\"),");
    printf("a(\"FONTFAMILY\",\"courier\"),");
    if (symbol_table[node].type==CG_SSTEP)
      printf("a(\"BORDER\",\"double\"),");
    else 
      //if (prune subgraphs of zero cost node)
                                                            // minNodeSize hardwired
      if ((ptr->comm_max+ptr->comp_idle_max+ptr->comp_max) < minNodeSize)
          printf("a(\"HIDDEN\",\"true\"),");
        
      //if ((ptr->comm_max+ptr->comp_idle_max+ptr->comp_max) < 0.01) 
      //    small=1; 
      //else small=0;
 

    colour=percentToColour(1.0-nodeColour(ptr));
    //if (!small) 
       fprintf(logFile,"a(\"COLOR\",\"#ff%.2x%.2x\")",colour,colour);
       printf("a(\"COLOR\",\"#ff%.2x%.2x\")",colour,colour);
    //else 
    //   printf("a(\"COLOR\",\"yellow\"),"); 
    fprintf(logFile,"],[");
    printf("],[");
    Mat(int,*graph,node,node)=-1;
    for(i=0;i<no_children;i++) {

      //if (!small) 
           fprintf(logFile,"e(\"%d->%d\",[],",node,children[i]);
           printf("e(\"%d->%d\",[],",node,children[i]);
      //else 
      //     printf("e(\"%d->%d\",[a(\"EDGECOLOR\",\"yellow\")],",node,children[i]);
 
      recur_graphToDaVinci_old(children[i],graph,costs);
      fprintf(logFile,")");
      printf(")");
      if (i<(no_children-1)) {fprintf(logFile,","); printf(",");}
    } 
    fprintf(logFile,"]))");
    printf("]))");
  } 
}


/* -----------------------------------------------------------------------------
 * Update colour
 * -------------------------------------------------------------------------- */

void updateColours(int root, Matrix *graph, Matrix *costs) {
  int i,colour,last;

  printf("graph(change_attr([");
  for(last=costs->rows-1;last>=0;last--)
    if (Mat_dense(*graph,last,last)) break;

  for(i=0;i<costs->rows;i++) {
    if (Mat_dense(*graph,i,i)) {
      colour = percentToColour(1.0-nodeColour(&Mat(object_cost,*costs,i,0)));
      printf("node(\"%d\",[a(\"COLOR\",\"#ff%.2x%.2x\")])",
	     i,colour,colour);
      if (i<last) printf(",");    
    }
  }
  printf("]))\n");
}

/* -----------------------------------------------------------------------------
 * Parse answer from daVinci
 * -------------------------------------------------------------------------- */

davinciCmd parseDaVinciCmd(char *input) {
  davinciCmd result;
  char *crp;
  char *word;
  int i;
  
  result.size=1;
  result.list=NULL;
  for(crp=input;*crp;crp++)
    if (*crp==',') result.size++;

  crp=input;
  word = parse_word(&crp);
  if (Verbose) fprintf(logFile,"{parseDaVinciCmd}=%s size=%d\n",word,result.size);
  if        (strcmp(word,"node_selections_labels")==0) {
    result.type=DAVINCI_NODE;
    result.list =calloc(result.size,sizeof(char*));
    if (result.list==NULL) {
      fprintf(stderr,"{parseDaVinciCmd} failed to allocate storage");
      exit(1);
    }
    crp+=2;
    i=0;
    word = parse_quoted(&crp);
    result.list[i++] = dup_str(word);
    while (*crp++==',') {
      word = parse_quoted(&crp);
      result.list[i++] = dup_str(word);
    }
  } else if (strcmp(word,"icon_selection")==0) {
    result.type=DAVINCI_ICON;
    result.list =calloc(result.size,sizeof(char*));
    if (result.list==NULL) {
      fprintf(stderr,"{parseDaVinciCmd} failed to allocate storage");
      exit(1);
    }
    crp++;
    i=0;
    word = parse_quoted(&crp);
    result.list[i++] = dup_str(word);
  } else if (strcmp(word,"tcl_answer")==0) {
    result.type=DAVINCI_TCL;
    result.list =calloc(result.size,sizeof(char*));
    if (result.list==NULL) {
      fprintf(stderr,"{parseDaVinciCmd} failed to allocate storage");
      exit(1);
    }
    crp++;
    i=0;
    word = parse_quoted(&crp);
    result.list[i++] = dup_str(word);
  } else if (strcmp(word,"menu_selection")==0) {
    result.type=DAVINCI_MENU;
    result.list =calloc(result.size,sizeof(char*));
    if (result.list==NULL) {
      fprintf(stderr,"{parseDaVinciCmd} failed to allocate storage");
      exit(1);
    }
    crp++;
    i=0;
    word = parse_quoted(&crp);
    result.list[i++] = dup_str(word);
  }else if (strcmp(word,"node_double_click")==0) {
    result.type=DAVINCI_OK;
  } else if (strcmp(word,"edge_selection_labels")==0)  {
    result.type=DAVINCI_OK;
  } else if (strcmp(word,"ok")==0)  {
    result.type=DAVINCI_OK;
  } else if (strcmp(word,"quit")==0)  {
    result.type=DAVINCI_QUIT;
  } else {
    result.type=DAVINCI_ERROR;
  }
  return result;  
}

/* -----------------------------------------------------------------------------
 * Misc.
 * -------------------------------------------------------------------------- */


/* Function that returns a string containing \texttt{x} spaces. */
static char* extra_space(int x) {
  static char space[MAX_FUNNAME+1];
  int i;

  if (Verbose) fprintf(logFile,"Padding is %d\n",x);
  for(i=0;(i<x)&&(i<MAX_FUNNAME);i++) space[i]=' ';
  space[i]='\0';
  return space;
}


static char *parse_word(char **crp) {
  static char result[MAX_FUNNAME];
  int i=0;

  while(islower(**crp) || **crp=='_') {
    result[i++]=**crp;
    (*crp)++;
  }
  result[i]='\0';
  return result;
}

static char *parse_quoted(char **crp) {
  static char result[MAX_FUNNAME];
  int i=0;
  if (**crp=='\"') {
    (*crp)++;
    while (**crp != '\"') {
      result[i++]=**crp;
      (*crp)++;
    }
    (*crp)++;
  }
  result[i]='\0';
  return result;
}

static char *dup_str(char *xs) {
  char *result;

  if (xs==NULL) return NULL;
  else {
    result = malloc(strlen(xs)+1);
    if (result==NULL) {
      fprintf(stderr,"{dup_str}: unable to allocate bytes");
      exit(1);
    }
    strcpy(result,xs);
    return result;
  }
}

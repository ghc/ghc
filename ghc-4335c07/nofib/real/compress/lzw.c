#include <stdio.h>
#include <stdlib.h>

struct prefix_tree {
    char                    pt_char;
    int                     pt_int;
    struct prefix_tree *    pt_tree;
    struct prefix_tree *    pt_left;
    struct prefix_tree *    pt_right;
};

#define MAX_ENTRIES 4096

struct prefix_tree *next_element_p;
int next_element;

struct prefix_tree *prefix_table;

void error (char *s)
{
    printf ("%s\n",s);
    exit (1);
}

struct prefix_tree *create_code_table (int first_code,int n_codes)
{
    switch (n_codes){
        case 0:
            return NULL;
        case 1:
        {
            struct prefix_tree *element;
            
            element=next_element_p++;
            
            element->pt_char=first_code;
            element->pt_int=first_code;
            element->pt_tree=NULL;
            element->pt_left=NULL;
            element->pt_right=NULL;
            
            return element;
        }
        default:
        {
            struct prefix_tree *element;
            int m_code;
            
            element=next_element_p++;
            
            m_code=(first_code + (first_code+n_codes-1)) >> 1;
            
            element->pt_char=m_code;
            element->pt_int=m_code;
            element->pt_tree=NULL;
            element->pt_left=create_code_table (first_code,m_code-first_code);
            element->pt_right=create_code_table (m_code+1,first_code+n_codes-(m_code+1));
            
            return element;     
        }
    }
}

FILE *input_file,*output_file;

void code_file (struct prefix_tree **tree_p)
{
    int old_code,c,k,previous_code;
    struct prefix_tree *element;
    struct prefix_tree **element_p;

    previous_code=-1;

    old_code=0;
    element_p=tree_p;

    c=getc (input_file);
    if (c==EOF)
        return;

    for (;;){           
        if (*element_p==NULL){
            if (next_element>=MAX_ENTRIES){
                element_p=tree_p;
            } else {
                element=next_element_p++;
                
                element->pt_char=c;
                element->pt_int=next_element++;
                element->pt_tree=NULL;
                element->pt_left=NULL;
                element->pt_right=NULL;
                
                *element_p=element;
                
                element_p=tree_p;
            }
            if (previous_code<0){
                putc (old_code>>4,output_file);
                previous_code=old_code;
            } else {
                putc ((previous_code<<4) | ((old_code>>8) & 0xf),output_file);
                putc (old_code,output_file);
                previous_code=-1;
            }
            old_code=0;
        } else {
            k=(*element_p)->pt_char;
            if (c<k){
                element_p=&(*element_p)->pt_left;
            } else if (c>k){
                element_p=&(*element_p)->pt_right;          
            } else if (c==k){
                old_code=(*element_p)->pt_int;
                element_p=&(*element_p)->pt_tree;
                
                c=getc (input_file);
                if (c==EOF){
                    if (previous_code<0){
                        putc (old_code>>4,output_file);
                        putc (old_code<<4,output_file);
                    } else {
                        putc ((previous_code<<4) | ((old_code>>8) & 0xf),output_file);
                        putc (old_code,output_file);
                    }
                    return;
                }
            }
        }
    }
}

int main (void)
{
    struct prefix_tree *code_table;
    
    prefix_table=malloc (sizeof (struct prefix_tree) * MAX_ENTRIES);
    
    if (prefix_table==NULL)
        error ("Out of memory");
    
    next_element_p=prefix_table;    
    
    input_file=stdin;
    output_file=stdout;
/* partain: change:
    input_file=fopen ("csh.man","rb");
    if (input_file==NULL)
        error ("Can't open the input file");
    
    output_file=fopen ("csh.man.z","wb");
    if (output_file==NULL)
        error ("Can't create the output file");
*/

    setvbuf (input_file,NULL,_IOFBF,4096);
    setvbuf (output_file,NULL,_IOFBF,4096);

    code_table=create_code_table (0,256);

    next_element=256;
    
    code_file (&code_table);
    
    return 0;
}

/*
 * prefix_trees.c: Prefix tree manipulation routines
 */

#include <stdio.h>

struct ptree {
  unsigned char prefix;
  int code;
  struct ptree *left;
  struct ptree *right;
  struct ptree *extensions;
};

struct ptree *code_table;

struct ptree *add_code ();

struct ptree *encode_one ();

void initialise_table ();
void output ();
void output_codes ();

int next_code = 256;
int MAX_CODE = 1000;
unsigned char current_char;
int output_buffer = -1;

main ()
{
  code_table = NULL;

  initialise_table ();
  current_char = (unsigned char) getchar ();

  do {
    encode_one (code_table, code_table, current_char, 0);
    next_code = (next_code == MAX_CODE) ? next_code : next_code+1;
  }
  while ((char) current_char != EOF);
 return(0);
}
  int initial_order [256] = {
    128,64,32,16,8,4,2,1,0,3,6,5,7,12,10,9,11,14,13,15,24,20,18,17,19,22,
    21,23,28,26,25,27,30,29,31,48,40,36,34,33,35,38,37,39,44,42,41,43,46,
    45,47,56,52,50,49,51,54,53,55,60,58,57,59,62,61,63,96,80,72,68,66,65,
    67,70,69,71,76,74,73,75,78,77,79,88,84,82,81,83,86,85,87,92,90,89,91,
    94,93,95,112,104,100,98,97,99,102,101,103,108,106,105,107,110,109,111,
    120,116,114,113,115,118,117,119,124,122,121,123,126,125,127,192,160,
    144,136,132,130,129,131,134,133,135,140,138,137,139,142,141,143,152,
    148,146,145,147,150,149,151,156,154,153,155,158,157,159,176,168,164,
    162,161,163,166,165,167,172,170,169,171,174,173,175,184,180,178,177,
    179,182,181,183,188,186,185,187,190,189,191,224,208,200,196,194,193,
    195,198,197,199,204,202,201,203,206,205,207,216,212,210,209,211,214,
    213,215,220,218,217,219,222,221,223,240,232,228,226,225,227,230,229,
    231,236,234,233,235,238,237,239,248,244,242,241,243,246,245,247,252,
    250,249,251,254,253,255
    };

void initialise_table ()
{
  register int i;

  for (i = 0; i < 256; i++)
    if (!code_table)
      code_table = add_code (code_table, code_table, initial_order [i]);
  else add_code (code_table, code_table, initial_order [i]);
}

struct ptree *add_code (root,r,n) 
     struct ptree *root;
     struct ptree *r;
     register int n;
{
  if (!r) {
    r = (struct ptree *) malloc (sizeof (struct ptree));
    if (!r) {
      printf ("Out of memory\n");
      exit (0);
    }

    r->prefix = n;
    r->code = n;
    r->left = r->right = r->extensions = NULL;
    if (!root) return r;
    if (n < root->code) root->left = r;
    else root->right = r;
    return r;
  }

  if (n < r->code) add_code (r, r->left, n);
  else
    add_code (r, r->right, n);
}

struct ptree *encode_one (root,r,c,n)
     struct ptree *root;
     struct ptree *r;
     register unsigned char c;
     register int n;
{
  if (!r) {
    /* output (n); */
    current_char = c;

    if ((next_code < MAX_CODE) && ((char) c != EOF)) {
      r = (struct ptree *) malloc (sizeof (struct ptree));
      if (!r) {
        printf ("Out of memory\n");
        exit (0);
      }

      r->prefix = c;
      r->code = next_code;
      r->left = r->right = r->extensions = NULL;

      return r;
    }
    else return root;
  }

  if (c == r->prefix) {
    if (!r->extensions)
      r->extensions = encode_one (r, r->extensions, 
				  (unsigned char) getchar(), r->code);
    else
      encode_one (r, r->extensions, (unsigned char) getchar(), r->code);
  }
  else if (c < r->prefix) encode_one (r, r->left, c, n);
  else encode_one (r, r->right, c, n);
}

void output (ch)
     register int ch;
{
  if (output_buffer == -1)
    output_buffer = ch;
  else
    {
      output_codes (output_buffer, ch);
      output_buffer = -1;
    }
}

void output_codes (hi, lo)
     register int hi, lo;
{
  register short int o1, o2, o3;

  o1 = hi >> 4;
  o2 = ((hi & 15) << 4) + (lo >> 8);
  o3 = lo & 255;

  putchar (o1); putchar (o2); putchar (o3);
}

print_table (root)
     struct ptree *root;
{
  if (!root) return 0;
  printf ("Reached character %c, value %d\n", root->prefix, root->prefix);
  print_table (root->left);
  print_table (root->right);
  return 0;
}

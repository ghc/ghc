#ifndef tree_defined
#define tree_defined
typedef enum {
	typdef,
	deflist,
	def,
	itemlist,
	emitemlist,
	item
} Ttree;

typedef struct { Ttree tag; } *tree;

/* Compatibility defines */
extern Ttree ttree();

#endif
extern tree mktypdef();
extern id *Rgtid();
#define gtid(xyzxyz) (*Rgtid(xyzxyz))
extern tree *Rgtdeflist();
#define gtdeflist(xyzxyz) (*Rgtdeflist(xyzxyz))

extern tree mkdeflist();
extern tree *Rgdeflist();
#define gdeflist(xyzxyz) (*Rgdeflist(xyzxyz))
extern tree *Rgdef();
#define gdef(xyzxyz) (*Rgdef(xyzxyz))

extern tree mkdef();
extern id *Rgdid();
#define gdid(xyzxyz) (*Rgdid(xyzxyz))
extern tree *Rgditemlist();
#define gditemlist(xyzxyz) (*Rgditemlist(xyzxyz))

extern tree mkitemlist();
extern tree *Rgitemlist();
#define gitemlist(xyzxyz) (*Rgitemlist(xyzxyz))
extern tree *Rgitem();
#define gitem(xyzxyz) (*Rgitem(xyzxyz))

extern tree mkemitemlist();

extern tree mkitem();
extern id *Rgitemfunid();
#define gitemfunid(xyzxyz) (*Rgitemfunid(xyzxyz))
extern id *Rgitemtypid();
#define gitemtypid(xyzxyz) (*Rgitemtypid(xyzxyz))


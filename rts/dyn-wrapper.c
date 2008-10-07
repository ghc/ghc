/* This is the wrapper for dynamically linked executables
 *
 * Have mercy with this creature born in cross-platform wasteland.
 */

#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ghcplatform.h>
#include <shell-tools.c>

/* All defining behavior string */
char behaviour[]=BEHAVIOUR;

#define REAL_EXT ".dyn"
#define REAL_EXT_S (sizeof(REAL_EXT)-1)

void *smalloc(size_t size);

#if defined(mingw32_HOST_OS)
#include <wtypes.h>
#include <winbase.h>

#define ENV_NAME "PATH"
#define ENV_SEP ';'
#define EXEEXT ".exe"

#define SET_ENV(n,v) SetEnvironmentVariable(n,v)
#define GET_ENV(n) getEnvWrapper(n)
#define FREE_GET_ENV(x) free(x)

#define DIR_SEP '\\'

char *getEnvWrapper(const char *name) {
    int len=GetEnvironmentVariableA(name,NULL,0);
    char *value;
    if(!len) return NULL;

    value=smalloc(len);
    GetEnvironmentVariableA(name,value,len);
    return value;
}

#define CONVERT_PATH(x) replace(x,'/','\\')


#elif defined(linux_HOST_OS)
#define ENV_NAME "LD_LIBRARY_PATH"
#define ENV_SEP ':'

#define EXEEXT ""
#define SET_ENV(n,v) setenv(n,v,1)
#define GET_ENV(n) getenv(n)

#define FREE_GET_ENV(x)
#define CONVERT_PATH(x)
#define DIR_SEP '/'

#elif defined(darwin_HOST_OS)
#define ENV_NAME "DYLD_LIBRARY_PATH"
#define ENV_SEP ':'

#define EXEEXT ""
#define SET_ENV(n,v) setenv(n,v,1)
#define GET_ENV(n) getenv(n)
#define FREE_GET_ENV(x)

#define CONVERT_PATH(x)
#define DIR_SEP '/'
#else
#error no OS interface defined
#endif

#define EXEEXT_S (sizeof(EXEEXT)-1)

/* Utility functions */

/* Like strtok_r but omitting the first arg and allowing only one delimiter */
char *stringTokenizer (char **this, const char delim)
{
    char *oldthis=*this;
    char *matched;
    if(!this || !(*this)) return NULL;

    matched=strchr(*this, delim);
    if(matched) {
	*matched=0;
	*this=matched+1;
	return oldthis;
    } else {
	*this=NULL;
	return oldthis;
    }
}

/* Replaces all occourances of character 'from' with 'to' in 'x' */
void replace(char *x, char from, char to) {
    while(*x) {
	if(*x == from)
	    *x=to;
	x++;
    }
}

/* Non-failing malloc -- will die on failure */
void *smalloc(size_t size)
{
    void *ret=malloc(size);
    if(!ret) {
	fprintf(stderr,"Can not allocate %d bytes",size);
	perror("");
	exit(-1);
    }
    return ret;
}

/* String Cons (scons) -- basically a linked list */
struct scons {
    char *value;
    struct scons *next;
};

/* Free up a linked list */
void freescons(struct scons *root) {
    while(root) {
	struct scons *c=root;
	root=root->next;
	free(c->value);
	free(c);
    }
}

/* Removes duplicates among the string cons */
struct scons *unique(struct scons *in) {
    struct scons *ret=NULL;
    struct scons *ci;
    for(ci=in; ci!=NULL; ci=ci->next) {
	struct scons *cj;
	struct scons *nextscons;
	for(cj = ret; cj != NULL; cj=cj->next) {
	    if(!strcmp(ci->value,cj->value))
		break;
	}
	if(cj!=NULL) continue;

	nextscons=smalloc(sizeof(struct scons));
	nextscons->next=ret;
	nextscons->value=strdup(ci->value);
	ret=nextscons;
    }
    return ret;
}

/* Tries to get a single line from the input stream really _inefficently_ */
char *afgets(FILE *input) {
	int bufsize=0;
	char *buf=(char *)malloc(bufsize);
	do {
	    bufsize+=1;
	    buf=realloc(buf,bufsize);
	} while(fread(buf+bufsize-1,1,1,input)==1 && buf[bufsize-1]!='\n');
	buf[bufsize-1]=0;
	return buf;
}

/* Computes the real binaries' name from argv0 */
char *real_binary_name(char *argv0) {
    int arg0len=strlen(argv0);
    char *alterego;

    alterego=strdup(argv0);
    if(!strcmp(alterego+arg0len-EXEEXT_S,EXEEXT)) {
	alterego[arg0len-EXEEXT_S]=0;
	arg0len-=EXEEXT_S;
    }
    alterego=realloc(alterego,arg0len+REAL_EXT_S+EXEEXT_S+1);
    sprintf(alterego+arg0len,"%s%s",REAL_EXT,EXEEXT);
    return alterego;
}

/* Gets a field for a GHC package
 * This method can't deal with multiline fields
 */
#warning FIXME - getGhcPkgField can not deal with multline fields

char *getGhcPkgField(char *ghcpkg, char *package, char *field) {
 	char *command;
	char *line;
	FILE *input;
	int fieldLn=strlen(field);

	/* Format ghc-pkg command */
	command=smalloc(strlen(ghcpkg)+strlen(package)+fieldLn+9);
	sprintf(command,"%s field %s %s",ghcpkg,package,field);

	/* Run */
	input=popen(command,"r");

	if(!input) {
	    fprintf(stderr,"Failed to invoke %s", command);
	    perror("");
	    free(command);
	    exit(-1);
	}

	line=afgets(input);

	pclose(input);

	free(command);

	/* Check for validity */
	if(strncmp(line,field,fieldLn)) {
	    /* Failed */
	    free(line);
	    return NULL;
	}

	/* Cut off "<field>: " in the output and return */
	strcpy(line,line+fieldLn+2);
	return line;
}

/* Prepends a prefix to an environment variable. 
   If it is set already, it puts a separator in between */

void prependenv(char *name, char *prefix, char sep)
{
    char *orig=GET_ENV(name);
    if(orig) {
	char *new;
	int prefixlength=strlen(prefix);

	new=(char *)smalloc(strlen(orig)+prefixlength+2);

	strcpy(new,prefix);
	new[prefixlength]=sep;
	strcpy(new+prefixlength+1,orig);

	SET_ENV(name,new);
	free(new);
    } else {
	SET_ENV(name,prefix);
    }
    FREE_GET_ENV(orig);
}

/* This function probes the library-dirs of all package dependencies,
   removes duplicates and adds it to the environment ENV_NAME */
void withghcpkg(char *ghcpkg, char *packages)
{
    struct scons *rootlist=NULL;
    struct scons *uniqueRootlist=NULL;
    struct scons *c;

    /* Save pointers for strtok */
    char *packageParse;
    char *libParse;

    char *curpack;

    while(curpack=stringTokenizer(&packages,';')) {
#warning We should query for a dynamic-library field not library-dirs.
	char *line=getGhcPkgField(ghcpkg, curpack,"library-dirs");
	char *line_p=line;  /* need to retain original line for freeing */
	char *curlib;

	if(!line) {
	    fprintf(stderr,"Can not query ghc-pkg for fields of packages %s",curpack);
	    perror("");
	    exit(-1);
	}

	while(curlib=stringTokenizer(&line_p,' ')) {
	    c=smalloc(sizeof(struct scons));
	    c->next=rootlist;
	    c->value=strdup(curlib);
	    rootlist=c;
	}
	free(line);
    }
    uniqueRootlist=unique(rootlist);
    for(c = uniqueRootlist; c != NULL; c=c->next) {
	CONVERT_PATH(c->value);
	prependenv(ENV_NAME,c->value,ENV_SEP);
    }
    freescons(rootlist);
    freescons(uniqueRootlist);
}

void add_to(char **base, int *base_size, char **target, const char *src, int src_size) {
    if((*target)-(*base)+src_size > *base_size) {
	*base=realloc(*base,*base_size+src_size);
	*base_size=*base_size+src_size;
    }
    memcpy(*target,src,src_size);
    *target=*target+src_size;
}

/* Scans str and constructs */
char *expandFlexiblePath(char *str)
{
    int buffersize=strlen(str)+1;
    char *base=smalloc(buffersize);
    char *current=base;

    while(*str && *str!=';') {
	if(*str=='$' && *(str+1)=='{') {
	    char *start;
	    char *envcont;
	    str+=2;
	    start=str;

	    while(*str && *str != '}') {
		str++;
	    }

	    if(!str) {
		fprintf(stderr,"End of string while scanning environment variable. Wrapper broken\n");
		exit(-1);
	    }
	    *str='\0';
	    str++;
	    envcont=GET_ENV(start);
	    if(!envcont) {
		fprintf(stderr,"Referenced environment variable %s not set.",start);
		exit(-1);
	    }

	    add_to(&base,&buffersize,&current,envcont,strlen(envcont));
	    FREE_GET_ENV(envcont);
	} else {
	    add_to(&base,&buffersize,&current,str,1);
	    str++;
	}
    }
    return base;
}

char *getBasename(const char *path) {
    int i;
    char *ret;
    for(i=strlen(path); i>=0; i--) {
	if(path[i]==DIR_SEP) break;
    }
    ret=smalloc(i+1);
    strncpy(ret,path,i);
}

char *agetcwd() {
    char *cwd;
    int size=100;
    cwd=malloc(size);
    while(!getcwd(cwd,size)) {
	size+=100;
	cwd=realloc(cwd,size);
    }
    return cwd;
}

int main(int argc, char **argv) {
    char *alterego;
    int arg0len=strlen(argv[0]);
    switch(behaviour[0]) {
    case 'H': /* hard paths */
	replace(behaviour+1,';',ENV_SEP);
	CONVERT_PATH(behaviour+1);
	prependenv(ENV_NAME,behaviour+1,ENV_SEP);
	break;
    case 'F':
	{ /* flexible paths based on ghc-pkg in $GHC_PKG */
	    char *expanded;
	    char *arg0base=getBasename(argv[0]);
	    char *ghc_pkg=behaviour+1;
	    char *packages;
	    char *oldwd=agetcwd();

	    packages=strchr(behaviour+1,';');
	    *packages=0;
	    packages++;
	    expanded=expandFlexiblePath(ghc_pkg);

#warning Will this also change drive on windows? WINDOWS IS SO BROKEN.
	    chdir(arg0base);

	    withghcpkg(expanded,packages);
	    chdir(oldwd);
	    free(oldwd);
	    free(expanded);
	}
	break;
    default:
	printf("unset wrapper called\n");
	exit(-1);
    }
    alterego=real_binary_name(argv[0]);
    return run(argv[0],alterego,argc,argv);
}

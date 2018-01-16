#include <stdio.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/ioctl.h>

#define PtyProto  "/dev/ptyXY"
#define TtyProto  "/dev/ttyXY"

extern errno;
extern sys_nerr;
extern char *sys_errlist[];
extern char *rindex(), *malloc(), *getenv(), *ttyname();

static char PtyName[32], TtyName[32];
static OpenPTY();

main(argc,argv)
int argc;
char *argv[];
{
	int fdIn, fdOut;
	char c;

	if (argc < 3 ) {
		printf("Usage: %s /Utils/bin/gs hiddenline [object]\n",argv[0]);
		exit(1);
	}

	if ((fdIn = OpenPTY()) == -1 ) {
		printf("no more PTYS\n");
		exit(1);
	}

	if (fork() == 0)
	{
		close(fdIn);
		if ((fdIn = open("/dev/tty",O_RDWR)) != -1)
		{
			ioctl(fdIn,TIOCNOTTY,(char *) 0);
			close(fdIn);
		}

		if ((fdIn = open(TtyName,O_RDWR)) == -1)
		{
			perror("open ");
			exit(1);
		}
		dup2(fdIn,1);
		close(fdIn);
		execv(argv[2],argv+2);
		perror("exec 2 failed");
		exit(2);
	}

	if ((fdOut = OpenPTY()) == -1 ) {
		printf("no more PTYS\n");
		exit(1);
	}

	if (fork() == 0)
	{
		close(fdOut);
		if ((fdOut = open("/dev/tty",O_RDWR)) != -1)
		{
			ioctl(fdOut,TIOCNOTTY,(char *) 0);
			close(fdOut);
		}

		if ((fdOut = open(TtyName,O_RDWR)) == -1)
		{
			perror("open ");
			exit(1);
		}
		dup2(fdOut,0);
		if ((fdOut = open("/dev/null",O_WRONLY)) == -1)
		{
			perror("void ");
			exit(1);
		}
		dup2(fdOut,1);
		execl(argv[1],0);
		perror("exec 1 failed");
		exit(2);
	}

	while (read(fdIn,&c,1) == 1)
	{
		if (write(fdOut,&c,1) != 1)
		{
			perror("write failed ");
			exit(0);
		}
	}
	c = 0x4;
	write(fdOut,&c,1);
}


static OpenPTY () {
    register char *p, *l, *d;
    register i, f, tf;

    strcpy (PtyName, PtyProto);
    strcpy (TtyName, TtyProto);
    for (p = PtyName, i = 0; *p != 'X'; ++p, ++i) ;
    for (l = "pqr"; *p = *l; ++l) {
	for (d = "0123456789abcdef"; p[1] = *d; ++d) {
	    if ((f = open (PtyName, O_RDWR)) != -1) {
		TtyName[i] = p[0];
		TtyName[i+1] = p[1];
		if ((tf = open (TtyName, O_RDWR)) != -1) {
		    close (tf);
		    return f;
		}
		close (f);
	    }
	}
    }
    return -1;
}

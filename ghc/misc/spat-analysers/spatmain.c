#include <stdio.h>
#include <varargs.h>
#include <sys/time.h>
#include <sys/resource.h>

#define TVTIME(tv)	((tv).tv_sec + (tv).tv_usec / 1e6)


extern char	*anal_usage, *anal_version,
		*shade_bench_path, *shade_ego, *shade_version,
		*shade_argtrange();

extern char	*ctime();
extern int	analyze();
extern long	time();
extern void	exit(), initialize(), terminate();


FILE	*statsfp;	/* output stats file */
double	nina;		/* # non-annulled instructions executed */


static double	usr, sys, real;
static int	t_flag,
		main_stats_analyze();
static void	main_stats_start(),
		main_stats_stop();


int
shade_main (argc, argv, envp)
	int	argc;
	char	**argv, **envp;
{
	int	aargc, ec, i, j, pid = getpid();
	char	**aargv, *cmd = 0, *x;

	argc = shade_splitargs (argv, &aargv, &aargc);

	for (i = j = 1; i < argc; i++)
		if (argv[i][0] == '-' ||
		    argv[i][0] == '+' && argv[i][1] == 't')
			switch (argv[i][1]) {
			case 'c':
				if (cmd)
					usage ("too many -c options");
				if (aargc > 0)
					usage ("-c not allowed with --");
				if (argv[i][2] || ++i >= argc)
					usage
					    ("-c: missing/misplaced command");
				cmd = argv[i];
				break;
			case 'o':
				if (statsfp)
					shade_fatal ("too many -o's");
				if (argv[i][2] || ++i >= argc)
					usage
					    ("-o: missing/misplaced file name");
				statsfp = fopen (argv[i], "w");
				if (!statsfp)
					usage ("%s: can't open", argv[i]);
				break;
			case 't':
				if (!t_flag++)
					(void) shade_argtrange (argv[i][0] ==
					    '-' ? "+t," : "-t,");
				if (x = shade_argtrange (argv[i]))
					usage ("%s: %s", argv[i], x);
				/* should print tranges */
				break;
			case 'U':
				usage ("");
				return (0);
			case 'V':
				fprintf (stderr, "%s: version: %s\n",
				    argv[0], anal_version);
				fprintf (stderr, "shade version: %s\n",
				    shade_version);
				return (0);
			default:
				argv[j++] = argv[i];
				break;
			}
		else	argv[j++] = argv[i];

	if (!statsfp)
		statsfp = stdout;

	argv[argc = j] = 0;
	initialize (argc, argv, envp);

	main_stats_start();

	if (cmd)
		ec = shade_sshell (cmd, main_stats_analyze);
	else if (aargc <= 0)
		ec = shade_shell (main_stats_analyze);
	else	if (0 > shade_loadp (*aargv, aargv, envp))
			ec = 1;
		else	ec = main_stats_analyze (aargc, aargv, envp, (char **) 0);

	if (pid == getpid()) {
		main_stats_stop();
		terminate();
	}
	return (ec);
}


usage (va_alist)
	va_dcl
{
	char	*fmt;
	va_list	ap;

	va_start (ap);
	fmt = va_arg (ap, char *);
	if (fmt && *fmt) {
		fprintf (stderr, "%s: ", shade_ego);
		vfprintf (stderr, fmt, ap);
		fprintf (stderr, "\n\n");
	}
	va_end (ap);

	fprintf (stderr, "usage: %s [-U] [-V] [-o outfile] [+/-t[from],[to]] ",
	    shade_ego);
	if (anal_usage && *anal_usage)
		fprintf (stderr, "\\\n\t%s ", anal_usage);
	fprintf (stderr, "\\\n\t[-c \"command\" | -- bench benchargs]\n");

	exit (1);
}


static void
getcputime (usr, sys)
	double	*usr, *sys;
{
	struct rusage	ru;

	if (-1 == getrusage (RUSAGE_SELF, &ru))
		*usr = *sys = 0.0;
	else {
		*usr = TVTIME (ru.ru_utime) - *usr;
		*sys = TVTIME (ru.ru_stime) - *sys;
	}
}


static void
getrealtime (real)
	double	*real;
{
	struct timeval	tv;
	struct timezone	tz;

	tz.tz_dsttime = DST_NONE;
	tz.tz_minuteswest = 0;

	(void) gettimeofday (&tv, &tz);

	*real = TVTIME (tv) - *real;
}


static void
main_stats_start()
{
	long	start;

	if (statsfp == 0)
		return;

	fprintf (statsfp, "Analyzer: %s\n", shade_ego);
	fprintf (statsfp, "Version: %s (shade version: %s)\n",
	    anal_version, shade_version);

 {
	char	host[64];

	if (-1 != gethostname (host, sizeof host))
		fprintf (statsfp, "Hostname: %s\n", host);
 }

	(void) time (&start);
	getrealtime (&real);
	getcputime (&usr, &sys);

	fprintf (statsfp, "Start: %s", ctime (&start));
	fflush (statsfp);
}


static int
main_stats_analyze (argc, argv, envp, iov)
	int	argc;
	char	**argv, **envp, **iov;
{
	int	i;

	/* BUG: if t_flag, shouldn't change application program */

	if (statsfp) {
		fprintf (statsfp, "Application: %s", shade_bench_path);
		for (i = 1; i < argc; i++)
			fprintf (statsfp, " %s", argv[i]);
		if (iov)
			for (i = 0; iov[i]; i += 2) {
				fprintf (statsfp, " %s", iov[i]);
				if (iov[i+1])
					fprintf (statsfp, " %s", iov[i+1]);
			}
		fprintf (statsfp, "\n");
		fflush (statsfp);
	}

	return (analyze());
}


static void
main_stats_stop()
{
	long	stop;

	if (statsfp == 0)
		return;

	(void) time (&stop);
	getcputime (&usr, &sys);
	getrealtime (&real);

	fprintf (statsfp, "Stop: %s", ctime (&stop));
	if (nina > 0)
		fprintf (statsfp, "Instructions: %.0f\n", nina);
	fprintf (statsfp, "Time: %.3f usr  %.3f sys  %.3f real  %.3f%%\n",
	    usr, sys, real,
	    real > 0 ? 100. * (usr + sys) / real : 100.);
	if (usr + sys > 0 && nina > 0)
		fprintf (statsfp, "Speed: %.3f KIPS\n", 
		    nina / (usr + sys) / 1000.);
}

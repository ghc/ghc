
void die(const char *fmt, ...);
char *mkString(const char *fmt, ...);
typedef void (*runCallback)(void);
__attribute__((noreturn)) int run(char *exePath, int numArgs1, char **args1,
                                  int numArgs2, char **args2,
                                  runCallback callback);

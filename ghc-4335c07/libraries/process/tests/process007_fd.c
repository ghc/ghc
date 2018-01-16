
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define SIZE 1024

int main(int argc, char **argv) {
    int fd;
    char buf[SIZE];
    int nRead, nWrite;

    if (argc != 2) {
        printf("Bad arguments\n");
        exit(1);
    }

    fd = atoi(argv[1]);

    while ((nRead = read(fd, buf, SIZE)) != 0) {
        if (nRead > 0) {
            ssize_t nWritten = 0;
            while (nWritten < nRead) {
                nWrite = write(STDOUT_FILENO, buf + nWritten, nRead - nWritten);
                if (nWrite < 0) {
                    perror("printf failed");
                    exit(1);
                }
                nWritten += nWrite;
            }
        }
        else if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) {
            perror("read failed");
            exit(1);
        }
    }

    return 0;
}


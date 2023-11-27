#include <unistd.h>
#include <signal.h>

int main() {
        kill(getpid(), SIGINT);
        sleep(1);
        return 0;
}


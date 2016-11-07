#define _GNU_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <signal.h>
#include <sys/types.h>
#include <glob.h>

int num_tasks = 0;
void handle_sigchld(int __attribute__((unused)) sig) {
    num_tasks--;
}

#define MAX_TASKS 4
#define TESTBROWSER "WebKitBuild/Release/bin/QtTestBrowser"

sigset_t sigset_chld;
void init_sigset_chld() {
    sigemptyset(&sigset_chld);
    sigaddset(&sigset_chld, SIGCHLD);
}

void fork_task(const char *filename, const char *logfile) {
    pid_t pid = fork();
    if (pid == -1) {
        perror("Fork failed");
        exit(1);
    } else if (pid == 0) {
        execl(TESTBROWSER, TESTBROWSER,
                "-r", filename,
                "-log", logfile,
                "-robot-timeout=60",
                "-robot-extra-time=5");
        perror("Exec failed");
        exit(1);
    }
}

void start_task(const char *filename) {
    char *logfile;
    sigset_t sigset_before;
    while (num_tasks > MAX_TASKS) wait(NULL);
    if (asprintf(&logfile, "%s.log", filename) == -1) {
        perror("asprintf for log filename");
        exit(1);
    }
    fork_task(filename, logfile);
    free(logfile);
    sigprocmask(SIG_BLOCK, &sigset_chld, &sigset_before);
    num_tasks++;
    sigprocmask(SIG_SETMASK, &sigset_before, NULL);
}

void shutdown() {
    while (num_tasks > 0) wait(NULL);
}

void process() {
    glob_t globbuf;
    size_t i;

    globbuf.gl_offs = 0;
    glob("jobs??", 0, NULL, &globbuf);
    for (i = 0; i < globbuf.gl_pathc; i++) {
        start_task(globbuf.gl_pathv[i]);
    }
    globfree(&globbuf);
}

int main(int __attribute__((unused)) argc, const char __attribute__((unused)) ** argv) {
    struct sigaction chldhandler;
    init_sigset_chld();
    chldhandler.sa_handler = handle_sigchld;
    chldhandler.sa_mask = sigset_chld;
    chldhandler.sa_flags = SA_NOCLDSTOP | SA_RESTART;
    sigaction(SIGCHLD, &chldhandler, NULL);
    process();
    shutdown();
    return 0;
}

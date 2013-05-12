
/*
Need to concatenate this file with something that defines:
LPTSTR path_dirs[];
LPTSTR progDll;
LPTSTR rtsDll;
*/

#include <stdarg.h>
#include <stdio.h>
#include <Windows.h>
#include <Shlwapi.h>

#include "Rts.h"

void die(char *fmt, ...) {
    va_list argp;

    fprintf(stderr, "error: ");
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    fprintf(stderr, "\n");

    exit(1);
}

LPTSTR getModuleFileName(void) {
    HMODULE hExe;
    LPTSTR exePath;
    DWORD exePathSize;
    DWORD res;

    hExe = GetModuleHandle(NULL);
    if (hExe == NULL) {
        die("GetModuleHandle failed");
    }

    // 300 chars ought to be enough, but there are various cases where
    // it might not be (e.g. unicode paths, or \\server\foo\... paths.
    // So we start off with 300 and grow if necessary.
    exePathSize = 300;
    exePath = malloc(exePathSize);
    if (exePath == NULL) {
        die("Mallocing %d for GetModuleFileName failed", exePathSize);
    }

    while ((res = GetModuleFileName(hExe, exePath, exePathSize)) &&
           (GetLastError() == ERROR_INSUFFICIENT_BUFFER)) {
        exePathSize *= 2;
        exePath = realloc(exePath, exePathSize);
        if (exePath == NULL) {
            die("Reallocing %d for GetModuleFileName failed", exePathSize);
        }
    }

    if (!res) {
        die("GetModuleFileName failed");
    }
    return exePath;
}

void setPath(void) {
    LPTSTR *dir;
    LPTSTR path;
    int n;
    int len = 0;
    LPTSTR exePath, s;

    exePath = getModuleFileName();
    for(s = exePath; *s != '\0'; s++) {
        if (*s == '\\') {
            *s = '/';
        }
    }
    s = StrRChr(exePath, NULL, '/');
    if (s == NULL) {
        die("No directory separator in executable path: %s", exePath);
    }
    s[0] = '\0';
    n = s - exePath;

    for (dir = path_dirs; *dir != NULL; dir++) {
        len += n + 7/* /../../ */ + lstrlen(*dir) + 1/* semicolon */;
    }
    len++; // NUL

    path = malloc(len);
    if (path == NULL) {
        die("Mallocing %d for PATH failed", len);
    }
    s = path;
    for (dir = path_dirs; *dir != NULL; dir++) {
        StrCpy(s, exePath);
        s += n;
        StrCpy(s, "/../../");
        s += 7;
        StrCpy(s, *dir);
        s += lstrlen(*dir);
        s[0] = ';';
        s++;
    }
    s[0] = '\0';
    free(exePath);

    if (! SetEnvironmentVariable(TEXT("PATH"), path)) {
        printf("SetEnvironmentVariable failed (%d)\n", GetLastError());
    }
    free(path);
}

HINSTANCE loadDll(LPTSTR dll) {
    HINSTANCE h;
    DWORD dw;
    LPVOID lpMsgBuf;

    h = LoadLibrary(dll);

    if (h == NULL) {
        dw = GetLastError();
        FormatMessage(
            FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_IGNORE_INSERTS,
            NULL,
            dw,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            (LPTSTR) &lpMsgBuf,
            0, NULL );
        die("loadDll %s failed: %d: %s\n", dll, dw, lpMsgBuf);
    }

    return h;
}

void *GetNonNullProcAddress(HINSTANCE h, char *sym) {
    void *p;

    p = GetProcAddress(h, sym);
    if (p == NULL) {
        die("Failed to find address for %s", sym);
    }
    return p;
}

HINSTANCE GetNonNullModuleHandle(LPTSTR dll) {
    HINSTANCE h;

    h = GetModuleHandle(dll);
    if (h == NULL) {
        die("Failed to get module handle for %s", dll);
    }
    return h;
}

typedef int (*hs_main_t)(int , char **, StgClosure *, RtsConfig);

int main(int argc, char *argv[]) {
    void *p;
    HINSTANCE hRtsDll, hProgDll;
    LPTSTR oldPath;

    StgClosure *main_p;
    RtsConfig *rts_config_p;
    hs_main_t hs_main_p;

    // MSDN says: An environment variable has a maximum size limit of
    // 32,767 characters, including the null-terminating character.
    oldPath = malloc(32767);
    if (oldPath == NULL) {
        die("Mallocing 32767 for oldPath failed");
    }

    if (!GetEnvironmentVariable(TEXT("PATH"), oldPath, 32767)) {
        if (GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
            oldPath[0] = '\0';
        }
        else {
            die("Looking up PATH env var failed");
        }
    }
    setPath();
    hProgDll = loadDll(progDll);
    if (! SetEnvironmentVariable(TEXT("PATH"), oldPath)) {
        printf("SetEnvironmentVariable failed (%d)\n", GetLastError());
    }
    free(oldPath);

    hRtsDll = GetNonNullModuleHandle(rtsDll);

    hs_main_p    = GetNonNullProcAddress(hRtsDll,  "hs_main");
    rts_config_p = GetNonNullProcAddress(hRtsDll,  "defaultRtsConfig");
    main_p       = GetNonNullProcAddress(hProgDll, "ZCMain_main_closure");

    return hs_main_p(argc, argv, main_p, *rts_config_p);
}


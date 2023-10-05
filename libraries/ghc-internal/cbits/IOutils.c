/*
 * (c) The GHC Team 2017-2018.
 *
 * I/O Utility functions for Windows.
 */

#include <stdbool.h>
#include <stdint.h>
#include <winsock2.h>
#include <windows.h>
#include <io.h>
#include <math.h>

/* Import some functions defined in base.  */
extern void maperrno(void);

/* Enum of Handle type.  */
typedef
enum HandleType
  {
    TYPE_CHAR,   // 0
    TYPE_DISK,   // 1
    TYPE_PIPE,   // 2
    TYPE_SOCKET, // 3
    TYPE_REMOTE, // 4
    TYPE_RAW,    // 5
    TYPE_UNKNOWN // 6
  } HANDLE_TYPE;

/*
 * handleReady(hwnd) checks to see whether input is available on the file
 * handle 'hwnd'.  Input meaning 'can I safely read at least a
 * *character* from this file object without blocking?'
 */
int
__handle_ready(HANDLE hFile, bool write, int msecs)
{
    DWORD handleType = GetFileType (hFile);

    DWORD rc;
        DWORD avail;

    switch (handleType)
      {
        case FILE_TYPE_CHAR:
        {
            INPUT_RECORD buf[1];
            DWORD count;

            /* A Console Handle will appear to be ready
             (WaitForSingleObject() returned WAIT_OBJECT_0) when
             it has events in its input buffer, but these events might
             not be keyboard events, so when we read from the Handle the
             read() will block.  So here we try to discard non-keyboard
             events from a console handle's input buffer and then try
             the WaitForSingleObject() again.
             Phyx: I'm worried that we're discarding events someone else may need.  */
            while (true) // keep trying until we find a real key event
            {
                rc = WaitForSingleObject( hFile, msecs );
                switch (rc)
                  {
                    case WAIT_TIMEOUT:
                        return false;
                    case WAIT_OBJECT_0:
                        break;
                    default:
                        /* WAIT_FAILED */
                        maperrno();
                        return -1;
                  }

                while (true) // discard non-key events
                {
                    /* I wonder if we can do better by grabbing a list of
                       input records at a time by using PeekConsoleInput.  */
                    rc = PeekConsoleInput(hFile, buf, 1, &count);
                    if (rc == 0) {
                        rc = GetLastError();
                        if (rc == ERROR_INVALID_HANDLE || rc == ERROR_INVALID_FUNCTION)
                            return true;
                        else {
                            maperrno();
                            return -1;
                        }
                    }

                    if (count == 0)
                        break; /* no more events => wait again.  */

                    /* discard console events that are not "key down", because
                       these will also be discarded by ReadFile().  */
                    if (buf[0].EventType == KEY_EVENT &&
                        buf[0].Event.KeyEvent.bKeyDown &&
                        buf[0].Event.KeyEvent.uChar.AsciiChar != '\0')
                          return true; /* it's a proper keypress.  */
                    else
                    {
                        /* it's a non-key event, a key up event, or a
                           non-character key (e.g. shift).  discard it.  */
                        rc = ReadConsoleInput(hFile, buf, 1, &count);
                        if (rc == 0) {
                            rc = GetLastError();
                            if (rc == ERROR_INVALID_HANDLE || rc == ERROR_INVALID_FUNCTION)
                                return true;
                            else {
                                maperrno();
                                return -1;
                            }
                        }
                    }
                }
            }
        }
        case FILE_TYPE_DISK:
            /* assume that disk files are always ready.  */
            return true;

        case FILE_TYPE_PIPE:
        {
            // Try to see if this is a socket
            //-------------------------
            // Create new event
            WSAEVENT newEvent = WSACreateEvent();

            //-------------------------
            // Associate event types FD_WRITE or FD_READ
            // with the listening socket and NewEvent
            rc = WSAEventSelect((SOCKET)hFile, newEvent, write ? FD_WRITE : FD_READ);

            if (rc == WSAENOTSOCK)
            {
                CloseHandle (newEvent);

                // WaitForMultipleObjects() doesn't work for pipes (it
                // always returns WAIT_OBJECT_0 even when no data is
                // available).  If the HANDLE is a pipe, therefore, we try
                // PeekNamedPipe:
                //
                rc = PeekNamedPipe( hFile, NULL, 0, NULL, &avail, NULL );
                if (rc != 0)
                    return avail != 0;
                else {
                    rc = GetLastError();
                    if (rc == ERROR_BROKEN_PIPE)
                        return true; // this is probably what we want

                    if (rc != ERROR_INVALID_HANDLE && rc != ERROR_INVALID_FUNCTION) {
                        maperrno();
                        return -1;
                    }
                }
                /* PeekNamedPipe didn't work - fall through to the general case */
            }
            else if (rc != 0)
            {
                CloseHandle (newEvent);
                // It seems to be a socket but can't determine the state.
                // Maybe not initialized. Either way, we know enough.
                return false;
            }

            // Wait for the socket event to trigger.
            rc = WaitForSingleObject( newEvent, msecs );
            CloseHandle (newEvent);

            /* 1 => Input ready, 0 => not ready, -1 => error */
            switch (rc)
              {
                case WAIT_TIMEOUT:
                    return false;
                case WAIT_OBJECT_0:
                    return true;
                default:
                {
                    /* WAIT_FAILED */
                    maperrno();
                    return -1;
                }
              }
        }
        default:
            rc = WaitForSingleObject( hFile, msecs );

            /* 1 => Input ready, 0 => not ready, -1 => error */
            switch (rc)
              {
                case WAIT_TIMEOUT:
                    return false;
                case WAIT_OBJECT_0:
                    return true;
                default:
                {
                    /* WAIT_FAILED */
                    maperrno();
                    return -1;
                }
              }
      }
}

bool
__is_console(HANDLE hFile)
{
    /* Broken handle can't be terminal */
    if (hFile == INVALID_HANDLE_VALUE)
        return false;

    DWORD handleType = GetFileType (hFile);

    /* TTY must be a character device */
    if (handleType != FILE_TYPE_CHAR)
        return false;

    DWORD st;
    /* GetConsoleMode appears to fail when it's not a TTY.  In
       particular, it's what most of our terminal functions
       assume works, so if it doesn't work for all intents
       and purposes we're not dealing with a terminal. */
    if (!GetConsoleMode(hFile, &st)) {
        /* Clear the error buffer before returning.  */
        SetLastError (ERROR_SUCCESS);
        return false;
    }

    return true;
}

#if !defined(ENABLE_VIRTUAL_TERMINAL_INPUT)
#define ENABLE_VIRTUAL_TERMINAL_INPUT 0x0200
#endif

bool
__set_console_buffering(HANDLE hFile, bool cooked)
{
    if (hFile == INVALID_HANDLE_VALUE) {
        return false;
    }

    DWORD  st;
    if (!GetConsoleMode(hFile, &st)) {
        return false;
    }

    /* According to GetConsoleMode() docs, it is not possible to
       leave ECHO_INPUT enabled without also having LINE_INPUT,
       so we have to turn both off here.
       We toggle ENABLE_VIRTUAL_TERMINAL_INPUT to enable us to receive
       virtual keyboard keys in ReadConsole.  */
    DWORD flgs = ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT;
    DWORD enabled = (st & ~flgs) | ENABLE_VIRTUAL_TERMINAL_INPUT;
    DWORD disabled = (st | ENABLE_LINE_INPUT) & ~ENABLE_VIRTUAL_TERMINAL_INPUT;


        return SetConsoleMode(hFile, cooked ? enabled : disabled);
}

bool
__set_console_echo(HANDLE hFile, bool on)
{
    DWORD  st;
    DWORD flgs = ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT;

    if (hFile == INVALID_HANDLE_VALUE) {
        return false;
    }

        return GetConsoleMode(hFile, &st) &&
               SetConsoleMode(hFile, ( on ? (st | flgs) : (st & ~flgs)));
}

bool
__get_console_echo(HANDLE hFile)
{
    DWORD  st;

    if (hFile == INVALID_HANDLE_VALUE) {
        return false;
    }

        return GetConsoleMode(hFile, &st) &&
               (st & ENABLE_ECHO_INPUT) == ENABLE_ECHO_INPUT;
}

bool
__flush_input_console(HANDLE hFile)
{
    if ( hFile == INVALID_HANDLE_VALUE )
      return false;

        /* If the 'handle' isn't connected to a console; treat the flush
         * operation as a NOP.
         */
        DWORD unused;
        if ( !GetConsoleMode(hFile, &unused) &&
             GetLastError() == ERROR_INVALID_HANDLE ) {
            return false;
        }

        if ( FlushConsoleInputBuffer(hFile) )
            return true;

    maperrno();
    return false;
}

HANDLE_TYPE
__handle_type (HANDLE hFile)
{
    DWORD handleType = GetFileType (hFile);
    switch (handleType)
      {
        case FILE_TYPE_PIPE:
          {
            WSAEVENT newEvent = WSACreateEvent();
            DWORD rc = WSAEventSelect((SOCKET)hFile, newEvent, FD_CLOSE);
            CloseHandle (newEvent);
            if (rc == WSAENOTSOCK)
              return TYPE_SOCKET;
            else
              return TYPE_PIPE;
          }
        case FILE_TYPE_CHAR:
          return TYPE_CHAR;
        case FILE_TYPE_DISK:
          return TYPE_DISK;
        case FILE_TYPE_REMOTE:
          return TYPE_REMOTE;
        case FILE_TYPE_UNKNOWN:
        default:
          return TYPE_UNKNOWN;
      }
}

bool
__close_handle (HANDLE hFile)
{
    switch (__handle_type (hFile))
      {
        case TYPE_SOCKET:
            return closesocket ((SOCKET)hFile) == 0;
        default:
            return CloseHandle (hFile);
      }
}

bool __set_file_pointer (HANDLE hFile, int64_t pos, DWORD moveMethod,
                         int64_t* outPos)
{
    LARGE_INTEGER ret;
    LARGE_INTEGER li;
    li.QuadPart = pos;
    bool success = SetFilePointerEx (hFile, li, &ret, moveMethod)
                    != INVALID_SET_FILE_POINTER;
    *outPos = ret.QuadPart;
    return success;
}

int64_t __get_file_pointer (HANDLE hFile)
{
    LARGE_INTEGER ret;
    LARGE_INTEGER pos;
    pos.QuadPart = 0;
    if (SetFilePointerEx(hFile, pos, &ret, FILE_CURRENT)
        == INVALID_SET_FILE_POINTER)
      return -1;

    return ret.QuadPart;
}

int64_t __get_file_size (HANDLE hFile)
{
    /* Broken handle can't do stat.  */
    if (hFile == INVALID_HANDLE_VALUE)
        return false;

    switch (GetFileType (hFile))
    {
      case FILE_TYPE_CHAR:
      case FILE_TYPE_DISK:
        break;
      default:
        return -1;
    }

    LARGE_INTEGER ret;
    if (!GetFileSizeEx(hFile, &ret))
      return -1;

    return ret.QuadPart;
}

bool __set_file_size (HANDLE hFile, int64_t size)
{
    LARGE_INTEGER li;
    li.QuadPart = size;
    if(!SetFilePointerEx (hFile, li, NULL, FILE_BEGIN))
        return false;

    return SetEndOfFile (hFile);
}

bool __duplicate_handle (HANDLE hFile, HANDLE* hFileDup)
{
    switch (__handle_type (hFile))
    {
        case TYPE_SOCKET:
            // should use WSADuplicateSocket
            return false;
        default:
            return DuplicateHandle(GetCurrentProcess(),
                                hFile,
                                GetCurrentProcess(),
                                hFileDup,
                                0,
                                FALSE,
                                DUPLICATE_SAME_ACCESS);
    }
}

bool __set_console_pointer (HANDLE hFile, int64_t pos, DWORD moveMethod,
                            int64_t* outPos)
{
    CONSOLE_SCREEN_BUFFER_INFO info;
    if(!GetConsoleScreenBufferInfo (hFile, &info))
        return false;

    COORD point;
    switch (moveMethod)
    {
        case FILE_END:
          {
              int64_t end = info.dwSize.X * info.dwSize.Y;
              pos = end + pos;
              point = (COORD) { pos % info.dwSize.X, pos / info.dwSize.X };
              break;
          }
        case FILE_CURRENT:
          {
              int64_t current = (info.dwCursorPosition.Y * info.dwSize.X)
                              + info.dwCursorPosition.X;
              pos = current + pos;
              point = (COORD) { pos % info.dwSize.X, pos / info.dwSize.X };
              break;
          }
        case FILE_BEGIN:
        default:
          point = (COORD) { pos % info.dwSize.X, pos / info.dwSize.X };
          break;
    }

    *outPos = pos;
    return SetConsoleCursorPosition (hFile, point);
}

int64_t __get_console_pointer (HANDLE hFile)
{
    CONSOLE_SCREEN_BUFFER_INFO info;
    if(!GetConsoleScreenBufferInfo (hFile, &info))
        return -1;

    return (info.dwCursorPosition.Y * info.dwSize.X) + info.dwCursorPosition.X;
}

int64_t __get_console_buffer_size (HANDLE hFile)
{
    CONSOLE_SCREEN_BUFFER_INFO ret;
    if (!GetConsoleScreenBufferInfo(hFile, &ret))
      return -1;

    return ret.dwSize.X * ret.dwSize.Y;
}

bool __set_console_buffer_size (HANDLE hFile, int64_t size)
{
    CONSOLE_SCREEN_BUFFER_INFO ret;
    if (!GetConsoleScreenBufferInfo(hFile, &ret))
      return false;

    COORD sz = {ret.dwSize.X, (int)ceil(size / ret.dwSize.X)};
    return SetConsoleScreenBufferSize (hFile, sz);
}



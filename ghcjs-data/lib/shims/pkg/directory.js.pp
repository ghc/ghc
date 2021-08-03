#include "HsBaseConfig.h"

#ifdef GHCJS_TRACE_DIRECTORY
function h$logDirectory() { h$log.apply(h$log,arguments); }
#define TRACE_DIRECTORY(args...) h$logDirectory(args)
#else
#define TRACE_DIRECTORY(args...)
#endif

// get/set permissions for file
// set errno and return -1 on error
// masks: 1 - read
//        2 - write
//        4 - exe
//        8 - search
function h$directory_getPermissions(file, c) {
    TRACE_DIRECTORY("getPermissions: " + file);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(file, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var m = fs.mode;
                var r = (m&4) || (m&32) || (m&256);
                var w = (m&2) || (m&16) || (m&128);
                var x = (m&1) || (m&8)  || (m&64);
                var exe    = x; // fixme?
                var search = x; // fixme?
                if(process.platform == 'win32') exe = true;
                c((r?1:0)|(w?2:0)|(exe?4:0)|(search?8:0));
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_setPermissions(file, perms, c) {
    TRACE_DIRECTORY("setPermissions: " + file + " " + perms);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(file, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var r = perms & 1;
                var w = perms & 2;
                var x = perms & 4;
                var search = perms & 8;
                var m  = fs.mode;
                m = r ? (m | 292) : (m & ~292);
                m = w ? (m | 146) : (m & ~146);
                m = (x || search) ? (m | 73) : (m & ~73);
                h$fs.chmod(file, function(err) {
                    h$handleErrnoC(err, -1, 0, c);
                });
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_copyPermissions(file1, file2, c) {
    TRACE_DIRECTORY("copyPermissions: " + file1 + " " + file2);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(file1, function(err1, fs) {
            if(err1) {
                h$handleErrnoC(err1, -1, 0, c);
            } else {
                h$fs.chmod(file2, fs.mode, function(err2) {
                    h$handleErrnoC(err2, -1, 0, c);
                });
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}


function h$directory_createDirectory(dir, c) {
    TRACE_DIRECTORY("createDirectory: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.mkdir(dir, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_removeDirectory(dir, c) {
    TRACE_DIRECTORY("removeDirectory: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.rmdir(dir, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_removeFile(file, c) {
    TRACE_DIRECTORY("removeFile: " + file);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.unlink(file, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_renamePath(file1, file2, c) {
    TRACE_DIRECTORY("renamePath: " + file1 + " " + file2);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.rename(file1, file2, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_canonicalizePath(path) {
    TRACE_DIRECTORY("canonicalizePath: " + path);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
      try {
        return h$path.resolve(path);
      } catch(e0) { }
      if(h$path.isAbsolute(path)) {
        try {
          return h$path.normalize(path);
        } catch(e1) { }
      } else {
        /* relative path:
             if getting the working directory fails (it may not exist anymore),
             fall back to a cached directory from setCurrentDirectory
         */
        var cwd;
        try {
          cwd = process.cwd();
        } catch(e2) {
          cwd = h$directory_cachedCurrentDirectory;
        }
        if(cwd) {
          try {
            return h$path.join(cwd, path);
          } catch(e3) { }
        }
      }
      // our attempts have failed
      return path;
    } else
#endif
        return path;
}
/*
function h$directory_findExecutables(name, c) {
    TRACE_DIRECTORY("findExecutables: " + name);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var result = [];
        var pathSep = process.platform === 'win32'?';':':';
        var parts   = process.env['PATH'].split(pathSep);
        var exts    = []; // process.platform === 'win32'?process.env['PATHEXT'].split(pathSep):[];
        exts.push(null);
        var files = [];
        result = [];
        for(var i=0;i<parts.length;i++) {
            for(var j=0;j<exts.length;j++) {
                files.push(parts[i] + h$path.sep + name + (exts[j]?(exts[j]):""));
            }
        }
        var tryFile = function(n) {
            if(n >= files.length) {
                c(result);
            } else {
                TRACE_DIRECTORY("trying: " + files[n]);
                h$fs.stat(files[n], function(err, fs) {
                    if(!err && ((fs.mode & 73) || process.platform === 'win32')) result.push(files[n]);
                    tryFile(n+1);
                });
            }
        }
        tryFile(0);
    } else
#endif
        c([]);
}
*/

function h$directory_getDirectoryContents(dir,c) {
    TRACE_DIRECTORY("getDirectoryContents: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.readdir(dir, function(err, d) {
            h$handleErrnoC(err, null, d, c);
        });
    } else
#endif
        h$unsupported(null, c);
}

function h$directory_getCurrentDirectory() {
    TRACE_DIRECTORY("getCurrentDirectory");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return h$handleErrno(null, function() {
            return process.cwd();
        });
    } else
#endif
        return "/";
}
var h$directory_cachedCurrentDirectory = null;
function h$directory_setCurrentDirectory(dir) {
    TRACE_DIRECTORY("setCurrentDirectory: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return h$handleErrnoS(-1, 0, function() {
          process.chdir(dir);
          h$directory_cachedCurrentDirectory = process.cwd();
        });
    } else
#endif
        return h$unsupported(-1);
}

function h$directory_getPath() {
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return process.env['PATH'] || null;
    } else
#endif
        return null
}

function h$directory_getHomeDirectory(dir) {
    TRACE_DIRECTORY("getHomeDirectory: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return process.env['HOME'] ||
            process.env['HOMEPATH'] ||
            process.env['USERPROFILE'] ||
            null;
    } else
#endif
        return "/"
}

function h$directory_getAppUserDataDirectory(appName) {
    TRACE_DIRECTORY("getAppUserDataDirectory: " + appName);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        if(process.env['APPDATA'])
            return process.env['APPDATA'] + h$path.sep + appName;
        if(process.env['HOME'])
            return process.env['HOME'] + h$path.sep + "." + appName;
        TRACE_DIRECTORY("getAppUserDataDirectory fallback");
        return "/";
    } else
#endif
        return "/";
}

function h$directory_getUserDocumentsDirectory(appName) {
    TRACE_DIRECTORY("getUserDocumentsDirectory: " + appName);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        if(process.env['HOME'])
            return process.env['HOME'];
        // fixme handle Windows
        TRACE_DIRECTORY("getUserDocumentsDirectory fallback");
        return "/";
    } else
#endif
        return "/";
}

function h$directory_getTemporaryDirectory() {
    TRACE_DIRECTORY("getTemporaryDirectory");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return h$handleErrno(null, function() {
            return h$os.tmpdir();
        });
    } else
#endif
        return "/";
}

function h$directory_exeExtension() {
    TRACE_DIRECTORY("exeExtension");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return (h$os.platform() === 'windows') ? 'exe' : '';
    } else
#endif
        return '';
}

function h$directory_getFileStatus(file, c) {
    TRACE_DIRECTORY("getFileStatus: " + file);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(file, function(err, s) {
            h$handleErrnoC(err, null, s, c);
        });
    } else
#endif
        h$unsupported(null, c);
}

function h$directory_getFileOrSymlinkStatus(file, c) {
    TRACE_DIRECTORY("getFileOrSymlinkStatus: " + file);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.lstat(file, function(err, s) {
            h$handleErrnoC(err, null, s, c);
        });
    } else
#endif
        h$unsupported(null, c);
}

function h$directory_getFileStatusAccessTime(fs) {
  TRACE_DIRECTORY("getFileStatusAccessTime: " + fs.atime.getTime());
  return fs.atime.getTime();
}

function h$directory_getFileStatusModificationTime(fs) {
  TRACE_DIRECTORY("getFileStatusModificationTime: " + fs.mtime.getTime());
  return fs.mtime.getTime();
}

function h$directory_getFileStatusIsDirectory(fs) {
  TRACE_DIRECTORY("getFileStatusIsDirectory: " + fs + " " + fs.isDirectory());
  return fs.isDirectory();
}

function h$directory_getFileStatusIsSymbolicLink(fs) {
  TRACE_DIRECTORY("getFileStatusIsSymbolicLink: " + fs + " " + fs.isSymbolicLink());
  return fs.isSymbolicLink();
}

function h$directory_getFileStatusFileSize(fs) {
  TRACE_DIRECTORY("getFileStatusFileSize: " + fs + " " + fs.size);
  return fs.size;
}

function h$directory_getFileStatusFileMode(fs) {
  TRACE_DIRECTORY("getFileStatusFileMode: " + fs + " " + fs.mode);
  return fs.mode;
}

function h$directory_getFileStatusGroup(fs) {
  TRACE_DIRECTORY("getFileStatusGroup: " + fs + " " + fs.group);
  return fs.group;
}

function h$directory_getFileStatusOwner(fs) {
  TRACE_DIRECTORY("getFileStatusOwner: " + fs + " " + fs.owner);
  return fs.owner;
}

function h$directory_getFileAccess(path, r, w, x, cont) {
  TRACE_DIRECTORY("getFileAccess: " + path);
  h$fs.access(path
             , (r ? h$fs.constants.R_OK : 0) |
               (w ? h$fs.constants.W_OK : 0) |
               (x ? h$fs.constants.X_OK : 0) |
               h$fs.constants.F_OK
             , function(err) {
               cont(err ? false : true);
             }
           );
}

function h$directory_setFileMode(file, mode, c) {
    TRACE_DIRECTORY("setFileMode: " + file + " " + mode);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.chmod(file, mode, function(err) {
          h$handleErrnoC(err, -1, 0, c)
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_setOwnerAndGroup(file, owner, group, c) {
    TRACE_DIRECTORY("setOwnerAndGroup: " + file + " " + mode);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.chown(file, owner, group, function(err) {
          h$handleErrnoC(err, -1, 0, c)
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_readSymbolicLink(path, c) {
  TRACE_DIRECTORY("readSymbolicLink: " + path);
#ifndef GHCJS_BROWSER
  if(h$isNode) {
      h$fs.readlink(path, function(err, linkString) {
        h$handleErrnoC(err, null, linkString, c)
      });
  } else
#endif
      h$unsupported(null, c);
}

function h$directory_setFileTimes(path, atime, set_atime, mtime, set_mtime, c) {
  TRACE_DIRECTORY("setFileTimes: " + path);
#ifndef GHCJS_BROWSER
  if(h$isNode) {
      if(set_atime && set_mtime) {
        h$fs.utimes(path, atime, mtime, function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
      } else {
        h$fs.stat(path, function(err, fs) {
          if(err) {
            h$handleErrnoC(err, -1, 0, c);
          } else {
            if(!set_atime) {
              atime = fs.atimeMs ? fs.atimeMs : fs.atime.getTime();
            }
            if(!set_mtime) {
              mtime = fs.mtimeMs ? fs.mtimeMs : fs.mtime.getTime();
            }
            h$fs.utimes(path, atime, mtime, function(err) {
              h$handleErrnoC(err, -1, 0, c);
            })
          }
        });
      }
  } else
#endif
      h$unsupported(-1, c);
}

function h$directory_createSymbolicLink(target, link, c) {
  TRACE_DIRECTORY("createSymbolicLink: " + target + " <- " + link);
#ifndef GHCJS_BROWSER
  if(h$isNode) {
    h$fs.symlink(target, link, function(err) {
      h$handleErrnoC(err, -1, 0, c);
    });
  } else
#endif
    h$unsupported(-1, c);
}

function h$directory_copyFileContents(src, dst, c) {
  TRACE_DIRECTORY("copyFileContents: " + src + " -> " + dst);
#ifndef GHCJS_BROWSER
  if(h$isNode) {
    h$fs.copyFile(src, dst, function(err) {
      h$handleErrnoC(err, -1, 0, c);
    });
  } else
#endif
    h$unsupported(-1, c);
}


// fixme this doesn't really belong here
function h$chmod(path_d, path_o, m) {
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var path = h$decodeUtf8z(path_d, path_o);
        TRACE_DIRECTORY("chmod: " + path + " mode: " + m);
        h$fs.chmodSync(path, m);
        return 0;
    } else
#endif
        return h$unsupported(-1);
}

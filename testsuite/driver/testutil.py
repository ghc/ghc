import errno
import os
import subprocess
import shutil

def strip_quotes(s):
    # Don't wrap commands to subprocess.call/Popen in quotes.
    return s.strip('\'"')

def getStdout(cmd_and_args):
    # Can't use subprocess.check_output as it's not available in Python 2.6;
    # It's also not quite the same as check_output, since we also verify that
    # no stderr was produced
    p = subprocess.Popen([strip_quotes(cmd_and_args[0])] + cmd_and_args[1:],
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE)
    (stdout, stderr) = p.communicate()
    r = p.wait()
    if r != 0:
        raise Exception("Command failed: " + str(cmd_and_args))
    if stderr:
        raise Exception("stderr from command: " + str(cmd_and_args))
    return stdout

def mkdirp(path):
    try:
        os.makedirs(path)
    except OSError as e:
        if e.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else:
            raise

def lndir(srcdir, dstdir):
    # Create symlinks for all files in src directory.
    # Not all developers might have lndir installed.
    # os.system('lndir -silent {0} {1}'.format(srcdir, dstdir))
    for filename in os.listdir(srcdir):
        src = os.path.join(srcdir, filename)
        dst = os.path.join(dstdir, filename)
        if os.path.isfile(src):
            link_or_copy_file(src, dst)
        else:
            os.mkdir(dst)
            lndir(src, dst)

# On Windows, os.symlink is not defined. Except when using msys2, as ghc
# does. Then it copies the source file, instead of creating a symbolic
# link to it. We define the following function to make this magic more
# explicit/discoverable. You are enouraged to use it instead of
# os.symlink.
link_or_copy_file = getattr(os, "symlink", shutil.copyfile)

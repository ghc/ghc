#! /usr/bin/env python
# Script to create and restore a git fingerprint of the ghc repositories.

from   datetime   import datetime
from   optparse   import OptionParser
import os
import os.path
import re
import subprocess
from   subprocess import PIPE, Popen
import sys

def main():
  opts, args = parseopts(sys.argv[1:])
  opts.action(opts)

def create_action(opts):
  """Action called for the create commmand"""
  if opts.fpfile:
    fp = FingerPrint.read(opts.source)
  else:
    fp = fingerprint(opts.source)
  if len(fp) == 0:
    error("Got empty fingerprint from source: "+str(opts.source))
  if opts.output_file:
    print "Writing fingerprint to: ", opts.output_file
  fp.write(opts.output)

def restore_action(opts):
  """Action called for the restore commmand"""
  def branch_name(filename):
    return "fingerprint_" + os.path.basename(filename).replace(".", "_")
  if opts.fpfile:
    try:
      fp = FingerPrint.read(opts.source)
      bn = branch_name(opts.fpfile)
    except MalformedFingerPrintError:
      error("Error parsing fingerprint file: "+opts.fpfile)
    if len(fp) == 0:
      error("No fingerprint found in fingerprint file: "+opts.fpfile)
  elif opts.logfile:
    fp = fingerprint(opts.source)
    bn = branch_name(opts.logfile)
    if len(fp) == 0:
      error("No fingerprint found in build log file: "+opts.logfile)
  else:
    error("Must restore from fingerprint or log file")
  restore(fp, branch_name=bn if opts.branch else None)

def fingerprint(source=None):
  """Create a new fingerprint of current repositories.

  The source argument is parsed to look for the expected output
  from a `sync-all` command. If the source is `None` then the
  `sync-all` command will be run to get the current fingerprint.
  """
  if source is None:
    if sys.platform == 'win32':
      # Can't rely on perl being located at `/usr/bin/perl`.
      sync_all = ["perl", "./sync-all", "log", "-1", "--pretty=oneline"]
    else:
      sync_all = ["./sync-all", "log", "-1", "--pretty=oneline"]

    source = Popen(sync_all, stdout=PIPE).stdout

  lib = ""
  commits = {}
  for line in source.readlines():
    if line.startswith("=="):
      lib = line.split()[1].rstrip(":")
      lib = "." if lib == "running" else lib # hack for top ghc repo
    elif re.match("[abcdef0-9]{40}", line):
      commit = line[:40]
      commits[lib] = commit
  return FingerPrint(commits)

def restore(fp, branch_name=None):
  """Restore the ghc repos to the commits in the fingerprint

  This function performs a checkout of each commit specifed in
  the fingerprint. If `branch_name` is not None then a new branch
  will be created for the top ghc repository. We also add an entry
  to the git config that sets the remote for the new branch as `origin`
  so that the `sync-all` command can be used from the branch.
  """
  checkout = ["git", "checkout"]

  # run checkout in all subdirs
  for (subdir, commit) in fp:
    if subdir != ".":
      cmd = checkout + [commit]
      print "==", subdir, " ".join(cmd)
      if os.path.exists(subdir):
        rc = subprocess.call(cmd, cwd=subdir)
        if rc != 0:
          error("Too many errors, aborting")
      else:
        sys.stderr.write("WARNING: "+
          subdir+" is in fingerprint but missing in working directory\n")

  # special handling for top ghc repo
  # if we are creating a new branch then also add an entry to the
  # git config so the sync-all command is happy
  branch_args = ["-b", branch_name] if branch_name else []
  rc = subprocess.call(checkout + branch_args + [fp["."]])
  if (rc == 0) and branch_name:
    branch_config = "branch."+branch_name+".remote"
    subprocess.call(["git", "config", "--add", branch_config, "origin"])

actions = {"create" : create_action, "restore" : restore_action}
def parseopts(argv):
  """Parse and check the validity of the command line arguments"""
  usage = "fingerprint ("+"|".join(sorted(actions.keys()))+") [options]"
  parser = OptionParser(usage=usage)

  parser.add_option("-d", "--dir", dest="dir",
    help="write output to directory DIR", metavar="DIR")

  parser.add_option("-o", "--output", dest="output",
    help="write output to file FILE", metavar="FILE")

  parser.add_option("-l", "--from-log", dest="logfile",
    help="reconstruct fingerprint from build log", metavar="FILE")

  parser.add_option("-f", "--from-fp", dest="fpfile",
    help="reconstruct fingerprint from fingerprint file", metavar="FILE")

  parser.add_option("-n", "--no-branch",
    action="store_false", dest="branch", default=True,
    help="do not create a new branch when restoring fingerprint")

  parser.add_option("-g", "--ghc-dir", dest="ghcdir",
    help="perform actions in GHC dir", metavar="DIR")

  opts,args = parser.parse_args(argv)
  return (validate(opts, args, parser), args)

def validate(opts, args, parser):
  """ Validate and prepare the command line options.

  It performs the following actions:
    * Check that we have a valid action to perform
    * Check that we have a valid output destination
    * Opens the output file if needed
    * Opens the input  file if needed
  """
  # Determine the action
  try:
    opts.action = actions[args[0]]
  except (IndexError, KeyError):
    error("Must specify a valid action", parser)

  # Inputs
  if opts.logfile and opts.fpfile:
    error("Must specify only one of -l and -f")

  opts.source = None
  if opts.logfile:
    opts.source = file(opts.logfile, "r")
  elif opts.fpfile:
    opts.source = file(opts.fpfile, "r")

  # Outputs
  if opts.dir:
    fname = opts.output
    if fname is None:
      fname = datetime.today().strftime("%Y-%m-%d_%H-%M-%S") + ".fp"
    path = os.path.join(opts.dir, fname)
    opts.output_file = path
    opts.output = file(path, "w")
  elif opts.output:
    opts.output_file = opts.output
    opts.output = file(opts.output_file, "w")
  else:
    opts.output_file = None
    opts.output = sys.stdout

  # GHC Directory
  # As a last step change the directory to the GHC directory specified
  if opts.ghcdir:
    os.chdir(opts.ghcdir)

  return opts

def error(msg="fatal error", parser=None, exit=1):
  """Function that prints error message and exits"""
  print "ERROR:", msg
  if parser:
    parser.print_help()
  sys.exit(exit)

class MalformedFingerPrintError(Exception):
  """Exception raised when parsing a bad fingerprint file"""
  pass

class FingerPrint:
  """Class representing a fingerprint of all ghc git repos.

  A finger print is represented by a dictionary that maps a
  directory to a commit. The directory "." is used for the top
  level ghc repository.
  """
  def __init__(self, subcommits = {}):
    self.commits = subcommits

  def __eq__(self, other):
    if other.__class__ != self.__class__:
      raise TypeError
    return self.commits == other.commits

  def __neq__(self, other):
    not(self == other)

  def __hash__(self):
    return hash(str(self))

  def __len__(self):
    return len(self.commits)

  def __repr__(self):
    return "FingerPrint(" + repr(self.commits) + ")"

  def __str__(self):
    s = ""
    for lib in sorted(self.commits.keys()):
      commit = self.commits[lib]
      s += "{0}|{1}\n".format(lib, commit)
    return s

  def __getitem__(self, item):
    return self.commits[item]

  def __iter__(self):
    return self.commits.iteritems()

  def write(self, outh):
      outh.write(str(self))
      outh.flush()

  @staticmethod
  def read(inh):
    """Read a fingerprint from a fingerprint file"""
    commits = {}
    for line in inh.readlines():
      splits = line.strip().split("|", 1)
      if len(splits) != 2:
        raise MalformedFingerPrintError(line)
      lib, commit = splits
      commits[lib] = commit
    return FingerPrint(commits)

if __name__ == "__main__":
  main()

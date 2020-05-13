#!/usr/bin/python3
# 
# Run doctests on a python file
# Pedro Vasconcelos, 2013
#
import os
import sys
import doctest


if len(sys.argv) != 4:
    sys.stderr.write("usage: "+sys.argv[0]+" <scripts-dir> <doctest-file> <python-file>\n")
    sys.exit (-1)


scripts = sys.argv[1]
docfile = sys.argv[2]
inpfile = sys.argv[3]

(pydir, pyfile) = os.path.split(inpfile)
(pymod, pyext) = os.path.splitext(pyfile)

# setup module search path
# add submissions directory to module search path
sys.path.insert(0, scripts)
sys.path.insert(0, pydir)

# lower recursion depth for shorter stack traces on unbounded recursions
# sys.setrecursionlimit(100)    

# avoid cluttering submission directory with bytecode files
sys.dont_write_bytecode = True

tstmod = __import__(pymod, globals(), locals(), [], 0)

flags = doctest.IGNORE_EXCEPTION_DETAIL|doctest.ELLIPSIS|doctest.REPORT_ONLY_FIRST_FAILURE

(failed,tested) = doctest.testfile(docfile, 
                                   module_relative=False, 
                                   extraglobs=vars(tstmod), 
                                   report=False,
                                   optionflags=flags)
print()
print("%d tests, %d failed." % (tested, failed))

if failed > 0:
    sys.exit(1)
else:
    sys.exit(0)

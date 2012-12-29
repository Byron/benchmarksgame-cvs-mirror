# The Computer Language Benchmarks Game
# $Id: bencher.py,v 1.1 2012/12/29 19:19:30 igouy-guest Exp $

"""
Description: bencher does repeated measurements of program
cpu time, elapsed time, resident memory usage, cpu load while
a program is running.

copyright Isaac Gouy 2010-2011
"""



from __future__ import with_statement

__author__ =  'Isaac Gouy'



# =============================
# imports
# =============================



import bz2, copy, fnmatch, logging, os, re, sys

# need to use ConfigParser not SafeConfigParser
from ConfigParser import ConfigParser, NoSectionError, NoOptionError

from contextlib import nested
from cStringIO import StringIO
from domain import FileNameParts, LinkNameParts, Record
from errno import ENOENT, EEXIST
from filecmp import cmp
from getopt import getopt, GetoptError
from gzip import GzipFile
from logging.handlers import RotatingFileHandler
from os.path import expanduser, expandvars, normpath, isdir, join, realpath, \
   getmtime, isfile, getsize, split, splitext
from shutil import copyfile, move, rmtree, copy2
from subprocess import call, STDOUT
from time import strftime, localtime, gmtime, time



# =============================
# platform specifics
# =============================



if sys.platform == 'win32':
   from usewin32 import nullName, planDesc, linkToSource, linkToIncludeDir, measure
else:
   from uselinux import nullName, planDesc, linkToSource, linkToIncludeDir, measure



# =============================
# global variables
# =============================



logfilemax = 0
logger = None

logdirName = 'log'
datdirName = 'dat'
tmpdirName = 'tmp'
codedirName = 'code'

_OUT = '_out'

codedir = None
datdir = None
logdir = None
testdir = None
tmpdir = None
srcdir = None

dirs = {
    'make' : None
   ,'data' : None
   }

filters = {
    'onlydirs' : frozenset()
   ,'allow' : []
   ,'only' : []
   ,'ignore' : []
   }

alias = {}
commandlines = {}
make = frozenset()
makefile = ''
ndiff_outputcheck = {}
outputcheck = {}
testname = None
testdata = {}
testenv = {}
testrange = {}
testvalues = ['0']

outputmax = 10240
affinitymask = None

exes = set()
makeExeName = 'make'
ndiffExeName = 'ndiff'
cmpExeName = 'cmp'
diffExeName = 'diff'
highlightExeName = 'highlight'



# =============================
# initialize & configure
# =============================


def setDefaultDirectories():
   global dirs
   script = realpath(sys.argv[0])

   bencher = split( split(script)[0] )[0]
   dirs['bencher'] = bencher if isdir(bencher) else None

   d = join(bencher,'tmp')
   dirs['tmp'] = d if isdir(d) else None

   d = join(bencher,'programs')
   dirs['src'] = d if isdir(d) else None

   d = join(bencher,'summary')
   dirs['dat_sweep'] = d if isdir(d) else None

   d = join(bencher,'run_markup')
   dirs['code_sweep'] = d if isdir(d) else None

   d = join(bencher,'run_logs')
   dirs['log_sweep'] = d if isdir(d) else None



def defaultIni():
   return defaultMakefiles('ini')


def defaultMakefile():
   return defaultMakefiles('Makefile')


def defaultMakefiles(ext):
   f = 'my.win32.' if sys.platform == 'win32' else 'my.linux.'
   # check default location
   script = realpath(sys.argv[0])
   d = split( split(script)[0] )[0]
   default = join(d,'makefiles',f+ext)
   return default if isfile(default) else None



def configure(ini):
   global runs, repeatevery, cutoff, delay, maxtime, logfilemax, outputmax, \
      make, makefile, affinitymask

   try:
      parser = ConfigParser()
      parser.read(ini)

      # override default directory locations
      for k,v in parser.items('dirs'):
         dirs[k] = normpath( expandvars( expanduser( v )))

      for k,v in parser.items('filters'):
         filters[k] = v.split()
      filters['onlydirs'] = frozenset( filters['onlydirs'])

      for k,v in parser.items('alias'):
         alias[k] = v.split() 

      make = frozenset( parser.get('build', 'make').split() )

      f = dirs['makefile'] if 'makefile' in dirs else defaultMakefile()
      makefile = normpath( expandvars( expanduser( f ))) if f else None

      # compiler interpreter runtime location shell vars
      for k,v in parser.items('tools'):
         os.environ[k.upper()] = v

      commandlines.update( parser.items('commandlines') )

      for k,v in parser.items('testrange'):
         testrange[k] = v.split()

      for k,v in parser.items('testdata'):
         testdata[k] = v

      for k,v in parser.items('outputcheck'):
         outputcheck[k] = frozenset( v.split() )

      for k,v in parser.items('ndiff_outputcheck'):
         ndiff_outputcheck[k] = v

      # test specific shell vars
      default = {}
      for each in filters['onlydirs']:
         if parser.has_section(each):
            d = {}
            for k,v in parser.items(each):
               d[k.upper()] = v
               default[k.upper()] = ''
            testenv[each] = d

      testenv['default'] = default

      s = 'measure'
      if parser.has_section(s):
         for o in parser.options(s):
            if o in ('runs'):
               runs = parser.getint(s,o)
            elif o in ('repeatevery'):
               repeatevery = parser.getboolean(s,o)
            elif o in ('cutoff'):
               cutoff = parser.getint(s,o)
            elif o in ('delay'):
               delay = parser.getfloat(s,o)
            elif o in ('maxtime'):
               maxtime = parser.getint(s,o)
            elif o in ('logfilemax'):
               logfilemax = parser.getint(s,o)  
            elif o in ('outputmax'):
               outputmax = parser.getint(s,o)
            elif o in ('affinitymask'):
               affinitymask = parser.getint(s,o) 

   except (NoSectionError,NoOptionError), e:
      if logger: logger.debug(e)
      print e, 'in', realpath(ini)
      sys.exit(2)



def checkDirectories():
   os.environ['BENCHER_BIN'] = join( dirs['bencher'], 'bin')

   if dirs['dat_sweep'] and not isdir(dirs['dat_sweep']):
      if logger: logger.warn('no directory %s', dirs['dat_sweep'])

   if dirs['log_sweep'] and not isdir(dirs['log_sweep']): 
      if logger: logger.warn('no directory %s', dirs['log_sweep'])

   if dirs['code_sweep'] and not isdir(dirs['code_sweep']): 
      if logger: logger.warn('no directory %s', dirs['code_sweep'])




def makeSubDirectoriesFor(dirName):
   global datdir

   def ifNoneMkdir(path,name):
      d = join(path,name)
      if not isdir(d): 
         os.mkdir(d)
         if logger: logger.debug('mkdir %s' % d)
      return d

   workingdir = ifNoneMkdir(dirs['tmp'],dirName)
   ifNoneMkdir(workingdir,logdirName)
   datdir = ifNoneMkdir(workingdir,datdirName)
   ifNoneMkdir(workingdir,codedirName)



def configureLogger(logfilemax,loggerDir=None):
   global logger
   # if a logfilemax size was set, log line-by-line to a rotating log file
   # (as well as logging program-run by program-run to stdout)

   if logfilemax:
      f = join(loggerDir,'bencher.log') if loggerDir else join('bencher.log')
      h = RotatingFileHandler(filename=f,maxBytes=logfilemax,backupCount=1)

      fmt = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
      h.setFormatter(fmt)

      logger = logging.getLogger("bencher")
      logger.addHandler(h)
      logger.setLevel(logging.DEBUG)


      
def checkExes():

   def check(cmd):
      try:
         with open( nullName, 'w') as df:
            call(cmd,stdout=df,stderr=STDOUT)
            exes.add(cmd[0])
      except OSError, (e,err):
         if e == ENOENT: # No such file or directory
            if logger: logger.debug('%s program not found', cmd[0])

   check([makeExeName])
   check([ndiffExeName])
   check([cmpExeName])
   check([diffExeName])
   check([highlightExeName,' -h'])



def isexe(exename):
   return exename in exes



# =============================
# clean & filter files
# =============================



def clean(d,srcSet): 

   def cleanLocal(name,localDirName,ext,sweepDir=None,isSymlink=False):
      names = frozenset( [n.__getattribute__(name) for n in srcSet] )

      localdir = join(dirs['tmp'],d,localDirName)
      filelist = os.listdir(localdir) if isdir(localdir) else []
      localNames = frozenset([f for f in os.listdir(localdir) if f.endswith(ext)])

      obsolete = localNames - names
      if obsolete and logger: logger.debug('cleaning %s', join(d,localDirName))
      for f in obsolete:
         os.remove( join(localdir,f) )

      if sweepDir:
         sweepNames = frozenset(
            [f for f in os.listdir( sweepDir) if f.endswith(ext)])
         obsolete = sweepNames - names
         if obsolete and logger: logger.debug('cleaning %s directory', sweepDir)
         # rm those obsolete files
         # probably want this to be a CVS remove?


   cleanLocal('datName',datdirName,'dat')
   #cleanLocal('logName',logdirName,'log',sweepDir=dirs['log_sweep'])
   #cleanLocal('codeName',codedirName,'_code',isSymlink=True)
   #cleanLocal('highlightName',codedirName,'.code', dirs['code_sweep'])




def targets(originalFiles,aliasedFiles):
   # ALLOW these helper file extensions to be available unchanged 
   # from the working directory - they will never be measured
   a = frozenset( filters['allow'] )
   allowed = []; _files = originalFiles; files = []

   for f in _files:
      if f.imp in a:
         allowed.append(f)
      else:
         files.append(f)
      # giving a list of ALLOWed filenames and a list without ALLOWed filenames

   # reverse alias dictionary so we can look up the target for each alias
   revAlias = {}
   for k,vs in alias.iteritems():
      for v in vs:
         revAlias[v] = k

   links = [] 
 
   # ONLY measure files with these extensions 
   o = filters['only']
   if o:
      _files = files; files = []
      
      for imp in o:
         reva = revAlias.get(imp,None)   
         for f in _files:
            if reva:
               if f.imp == reva:
                  links.append(LinkNameParts(f.filename,imp)) 
            else:
               if f.imp == imp:
                  files.append(f)
      # giving a list of ONLY links and a list of ONLY files

   # measure files with ANY extension, except ...
   else: 
      # create alias filenames
      links = aliasedFiles

      # IGNORE files or links with these extensions
      ignore = frozenset( filters['ignore'] )
      if ignore:
         links = [f for f in links if not f.imp in ignore]
         files = [f for f in files if not f.imp in ignore]

   # assume dat file is only written once, when all data is available

   def notUpToDate(s,d):
      try:
         return getmtime( join(srcdir,s) ) > getmtime( join(datdir,d) )
      except OSError, (e,_): 
         return e == ENOENT # No such file or directory

   links = [f for f in links if notUpToDate(f.filename,f.datName)]
   files = [f for f in files if notUpToDate(f.filename,f.datName)]

   return allowed,files,links



def worklist():
   global srcdir

   w = []
   subdirs = [each for each in os.listdir( dirs['src'] ) 
      if each in filters['onlydirs']]

   subdirs.sort()
   for d in subdirs:  
      makeSubDirectoriesFor(d) 
 
      # take all likely names in src directory
      # create simpleNames for each name and each possible alias
      srcdir = join(dirs['src'],d)

      # include undeleted filenames that might have a file extension
      files = fnmatch.filter( os.listdir(srcdir), '*.*[!~]' )

      # exclude bogus files created by touch
      files = [FileNameParts(f) for f in files if not f.startswith('*')]

      original = [fp for fp in files]

      src = set( original )
      aliased = []
      for o in original:
         for imp in alias.get(o.imp,[]): 
            aliased.append( LinkNameParts(o.filename,imp))
      src.update(aliased)

      clean(d,src)

      allowed,sources,links = targets(original,aliased)

      programs = set(sources)
      programs.update(links)

      # symlink these programs from codedir so they are available 
      # for conversion to highlight-ed xml files
      codedir = join(dirs['tmp'],d,'code')
      for f in programs:
         linkToSource(directory=srcdir, srcFilename=f.filename,
            dstDir=codedir, filename=f.programName)

         linkToSource(directory=srcdir, srcFilename=f.filename,
            dstDir=codedir, filename=f.codeName)

      w.append( (d,programs,allowed) )

   return w



# =============================
# make
# =============================


def makeName():
   return join('_make')



def hasMake(ext):
   return ext in make



def callMake(p):
   with open( makeName(), 'w') as mf: 
      mf.write('\nMAKE:\n')

   cmd = makeExeName + ' -f ' + makefile + ' ' + p.runName
   with open( makeName(), 'a') as mf:
      if isexe(makeExeName):
         startmake = time()
         call(cmd.split(),stdout=mf,stderr=STDOUT)
         endmake = time()
         print >>mf, '%0.2fs to complete and log all make actions' % (endmake - startmake)
      else:
         print >>mf, '%s program not found' % (makeExeName)
         if logger: logger.debug('%s %s - %s program not found', \
            makeExeName, p.runName, makeExeName)


def cmdTemplate(p):
   specials = {}
   specials['%X'] = p.runName if hasMake(p.imp) else p.programName
   specials['%T'] = p.name
   specials['%B'] = p.baseName
   specials['%I'] = '' if p.id == '1' else p.id

   s = commandlines.get(p.imp,None)
   if s:
      for m in re.finditer('\$[\w]+' ,s):
         k = m.group(0)
         v = os.environ.get( k.lstrip('$'), '' )
         s = re.sub('\\' + k + '(?P<c>[\W])', v + '\g<c>', s) # ate [\W] !

      for m in re.finditer('\%[XTBI]' ,s):
         value = specials.get( m.group(0), '' )
         s = re.sub('\\'+ m.group(0), value, s) 

   else:
      s = join('.',p.runName) + ' %A'

   return s



def cmdWithArg(s,arg):
   _a = '0' if testdata.get(testname,None) else arg
   for m in re.finditer('\%A' ,s):
      s = re.sub('\\'+ m.group(0), _a, s) 
   return s


def qsplit(cmd):
#  for Smalltalk commandline
   if cmd.count('"') == 2:
      s0,_,s = cmd.partition('"')
      s1,_,s2 = s.partition('"')
      cmdline = s0.split()
      cmdline.append(s1)
      cmdline += s2.split()
   else:
      cmdline =  cmd.split()

   return cmdline


# =============================
# measure
# =============================


def diffName():
   return join('_diff')



def errName():
   return join('_err')



def logName(p):
   return join(logdir, p.logName)



def outName(index):
   return join(_OUT) if index == 0 else nullName



def dataName(f,arg):
   # assume there is a data file
   stem,ext = splitext(f)
   return normpath( expandvars( expanduser( stem + arg + ext )))



def setVariables(name):
   global testname,testvalues,testdir,codedir,datdir,logdir,srcdir
   testname = name

   # export test specific environment variables for tools
   os.environ['TEST'] = testname
   
   for k,v in testenv.get('default',{}).iteritems():
      os.environ[k] = v

   for k,v in testenv.get(testname,{}).iteritems():
      os.environ[k] = v

   testvalues = testrange.get(testname,['0'])
   testdir = join(dirs['tmp'],testname)
   codedir = join(testdir,codedirName)
   datdir = join(testdir,datdirName)
   logdir = join(testdir,logdirName)
   srcdir = join(dirs['src'],testname)



def cleanTmpdirFor(p,allowed):
   # every time, provide each program with a clean empty tmpdir
   os.chdir(testdir)
   tmpdir = join(testdir,tmpdirName)

   if isdir(tmpdir):
      try:
         rmtree(tmpdir)
      except OSError, (e,err):
         if logger: logger.error('%s %s',err,tmpdir)
         print "Please exit all other programs using " + tmpdir
         sys.exit(0)

   # make sure all build files and crash files are created in tmpdir
   os.mkdir(tmpdir)
   os.chdir(tmpdir)

   # symlink from tmpdir to Include directory
   linkToIncludeDir(dirs['src'],'Include')

   # symlink from tmpdir to these allowed helper files
   for f in allowed:
      linkToSource(srcdir,f.filename)

   # symlink from tmpdir to expected output files
   for f in [f for f in os.listdir(testdir) if f.endswith(_OUT)]:
      linkToSource(testdir,f)

   # symlink from tmpdir to the program source code file
   linkToSource(codedir,p.programName)



def callHighlightSourceCodeMarkup(p):
   hidir = join( dirs['bencher'], 'highlight')

   cmd = [highlightExeName,'--fragment'
         ,'--add-config-dir=' + hidir + '/'
         ,'--style=edit-eclipse'
         ,'--force'
         ,'-i'
         ,join(codedir,p.codeName)
         ,'-o'
         ,join(codedir,p.highlightName)
         ]

   try:
      call(cmd)
   except OSError, (e,err): 
      if logger: logger.debug('%s %s',e,err)



def sizeCompressedSourceCode(p):
   s = ''
   try:
      with open( join(codedir,p.highlightName), 'r') as sf:
         s = sf.read()
         s = re.sub('<span class="com">.*<\/span>', '', s)
         s = re.sub('<span class="slc">.*<\/span>', '', s)
         s = re.sub('<span class="[a-z]{3}">', '', s)

         s = re.sub('<span class="hl com">.*<\/span>', '', s)
         s = re.sub('<span class="hl slc">.*<\/span>', '', s)
         s = re.sub('<span class="hl [a-z]{3}">', '', s)

         s = re.sub('<\/span>', '', s)

         s = re.sub('\s+', ' ', s)
         s = re.sub('&quot;', '"', s)
         s = re.sub('&lt;', '<', s)
         s = re.sub('&gt;', '>', s)
         s = re.sub('&#64;', '@', s)
   except (OSError,IOError), err:
      if logger: logger.error(err)

   sz = 0
   if s:
      path = join('_gz')
      try:
         gz = GzipFile(path,'wb',1)
         gz.write(s)
      except (OSError,IOError), err:
         if logger: logger.error(err)
      finally:
         gz.close()

      sz = os.stat(path).st_size

   return sz



def isNotChecked(testname):
   return testname in outputcheck['notchecked']



def isCheckNDiff(testname):
   return ndiff_outputcheck.has_key(testname)



def isCheckCmp(testname):
   return testname in outputcheck['binarycmp']



def checkAndLog(m,outFile,logf):

   def cmpCheck(f1,f2,df):
      if not cmp(f1,f2):
         print >>df, '\n %s %s differ\n' % (f1,f2)


   if m.isOkay():
      # assume outFile has been closed
      argFile = m.argString + _OUT

      # diff against expected output file
      if isfile(argFile):
         with open( diffName(), 'w') as df:
            try:
            
               if isNotChecked(testname):
                  pass

               # compare _OUT not outFile so short name will be shown in logf

               elif isCheckNDiff(testname):
                  if isexe(ndiffExeName):
                     optionkv = ndiff_outputcheck[testname].split()
                     call([ndiffExeName,'-quiet',optionkv[0],optionkv[1],_OUT,argFile], \
                        stdout=df,stderr=STDOUT)
                  elif isexe(diffExeName):
                     call([diffExeName,_OUT,argFile],stdout=df,stderr=STDOUT)
                  else:
                     cmpCheck(_OUT,argFile,df)

               elif isCheckCmp(testname):
                  if isexe(cmpExeName):
                     call([cmpExeName,_OUT,argFile],stdout=df,stderr=STDOUT)
                  else:
                     cmpCheck(_OUT,argFile,df)

               else:
                  if isexe(diffExeName):
                     call([diffExeName,_OUT,argFile],stdout=df,stderr=STDOUT)
                  else:
                     cmpCheck(_OUT,argFile,df)

            except OSError, (e,err):
               if e == ENOENT: # No such file or directory
                  m.setBadOutput()
                  print >>logf, '\nFAIL:', err, '\n'
                  print 'FAIL ', err,

            if df.tell():
               m.setBadOutput()

      else: # create the initial expected output file
         copyfile(outFile, join('..', m.argString + _OUT))
                  
   if not m.isOkay():
      extra = '' if not m.hasTimedout() else '%s %d%s' % ('after',maxtime,'s')
      print >>logf, '\n%s%s\n' % (m.statusStr(),extra)



def measureOnly(a,t,ms):
   for i in range(1,runs):
      cmd = cmdWithArg(t,a)
      with open( outName(i), 'w') as of:

         # pass the test data file to program stdin
         if testdata.get(testname,None):
            dfName = dataName(testdata[testname],a)
            with open(dfName,'r') as df:
               m = measure(a,qsplit(cmd),delay,maxtime,of,STDOUT,df,
                  logger=logger,affinitymask=affinitymask)

         # pass the test value as a command line argument in cmd
         else:
               m = measure(a,qsplit(cmd),delay,maxtime,of,STDOUT,
                  logger=logger,affinitymask=affinitymask)

         # add to the measurements
         ms.append(m)

      sys.stdout.write('.'); sys.stdout.flush()
      loggerLine.write('.')



# =============================
# Code Duplication BEGIN
# =============================
# There a 2 different versions of measureCheckAndLog
# There a 2 different versions of measurePrograms
#
# The differences are slight and one-day may be factored
# out - but not today.
#
# 1)
# measureCheckAndLog does repeat measurements at every
# value in [testrange]
# measurePrograms does repeat measurements at every
# value in [testrange]
#
# 2)
# measureCheckAndLogRepeatLargest only does repeat measurements
# at the largest value in [testrange]
# measureProgramsRepeatLargest only does repeat measurements
# at the largest value in [testrange]
#
# =============================



def measureCheckAndLog(p,t,ms):
   repeatTestValues = []

   for a in testvalues:
      cmd = cmdWithArg(t,a)
      ofName = outName(0) 

      # a logged Empty record will reveal a bencher failure
      m = Record() 

      try:
         with open(logName(p),'w') as logf:
            with open(ofName,'w') as of:
               # append timestamp to log
               print >>logf, '\n', strftime("%a, %d %b %Y %H:%M:%S GMT", gmtime())

               # append Make output to log
               if hasMake(p.imp):
                  with open( makeName(), 'r') as mf:
                     logf.write( mf.read() ) 

               with open(errName(),'w+') as ef:
                  # append command line showing redirected test data file to log
                  if testdata.get(testname,None):
                     dfName = dataName(testdata[testname],a)
                     print >>logf, '\nCOMMAND LINE:\n', cmd, '<', split(dfName)[1]
                     with open(dfName,'r') as df:
                        m = measure(a,qsplit(cmd),delay,maxtime,of,ef,df,
                           logger=logger,affinitymask=affinitymask)

                  # append command line showing test value argument to log
                  else:
                     print >>logf, '\nCOMMAND LINE:\n', cmd
                     m = measure(a,qsplit(cmd),delay,maxtime,of,ef,
                        logger=logger,affinitymask=affinitymask)

            # check the program output was as expected
            checkAndLog(m,ofName,logf)

            # add to the measurements
            ms.append(m)

            # if there was a problem finding the program just ignore the program output
            if not m.isMissing():
               with open(ofName,'r+') as of:
                  # append diff or ndiff or cmp output to log
                  if m.hasBadOutput():
                     with open(diffName(), 'r') as df:
                        logf.write( df.read() )

                  # append program output to log
                  if isNotChecked(testname):
                     logf.write( '\nPROGRAM OUTPUT NOT CHECKED:\n' )
                     logf.write( of.read() )
                  elif isCheckCmp(testname):
                     logf.write( '\n(BINARY) PROGRAM OUTPUT NOT SHOWN\n' )
                  elif getsize(ofName) > outputmax:
                     of.truncate(outputmax)
                     logf.write( '\n(TRUNCATED) PROGRAM OUTPUT:\n' )
                     logf.write( of.read() )
                  else:
                     logf.write( '\nPROGRAM OUTPUT:\n' )
                     logf.write( of.read() )

                  # append program stderr to log
                  if getsize(errName()):
                     with open(errName(), 'r') as ef:
                        logf.write( '\n' )
                        logf.write( ef.read() )               

         if m.isOkay():
            if not m.hasExceeded(cutoff):
               repeatTestValues.append(a)
         else:
            break


      except IOError, err:
         if logger: logger.error(err)

      finally:
         sys.stdout.write('.'); sys.stdout.flush()
         loggerLine.write('.')

   sys.stdout.write( m.statusStr() ); sys.stdout.flush()
   loggerLine.write( m.statusStr() )

   return repeatTestValues



def measurePrograms(name,programs,allowed,total):
   global loggerLine

   def fillInMissingRecords(ms): 
      for a in testvalues[len(ms):]:
         c = copy.deepcopy(ms[-1:])
         if c:
            c[0].argString = a
            ms.append(c[0])

   setVariables(name)

   for p in programs:
      cleanTmpdirFor(p,allowed)

      sys.stdout.write('%s ' % strftime('%a %H:%M:%S', localtime()))
      sys.stdout.flush()

      if isexe(highlightExeName):
         callHighlightSourceCodeMarkup(p)
      else:
         copyfile( join(codedir,p.codeName), join(codedir,p.highlightName) )

      srcSize = sizeCompressedSourceCode(p)

      if hasMake(p.imp):
         callMake(p)   

      loggerLine = StringIO()
      t = cmdTemplate(p)
      ms = []
      repeatTestValues = measureCheckAndLog(p,t,ms)
      fillInMissingRecords(ms)
      for testValue in repeatTestValues:
         measureOnly(testValue,t,ms)

      # Write compressed dat file once, when all data is available
      try:
         datf = bz2.BZ2File( join(datdir,p.datName) ,'w')
         for m in ms: 
            m.gz = srcSize
            print >>datf, m
      except IOError, err:
         if logger: logger.error(err)
      finally:
         datf.close()

      loggerLine.write('%s [%d]' % (p.programName,total))
      sys.stdout.write('%s [%d]\n' % (p.programName,total)); sys.stdout.flush()
      total -= 1

      if logger: logger.info( loggerLine.getvalue() )
      loggerLine.close()

   return total



# =============================
# Duplicates
# =============================



def measureCheckAndLogRepeatLargest(p,t,ms):
   repeatTestValue = None

   for a in testvalues:
      cmd = cmdWithArg(t,a)
      ofName = outName(0)

      # a logged Empty record will reveal a bencher failure
      m = Record() 

      try:
         with open(logName(p),'w') as logf:
            with open(ofName,'w') as of:
               # append timestamp to log
               print >>logf, '\n', strftime("%a, %d %b %Y %H:%M:%S GMT", gmtime())

               # append Make output to log
               if hasMake(p.imp):
                  with open( makeName(), 'r') as mf:
                     logf.write( mf.read() ) 

               with open(errName(),'w+') as ef:
                  # append command line showing redirected test data file to log
                  if testdata.get(testname,None):
                     dfName = dataName(testdata[testname],a)
                     print >>logf, '\nCOMMAND LINE:\n', cmd, '<', split(dfName)[1]
                     with open(dfName,'r') as df:
                        m = measure(a,qsplit(cmd),delay,maxtime,of,ef,df,
                           logger=logger,affinitymask=affinitymask)

                  # append command line showing test value argument to log
                  else:
                     print >>logf, '\nCOMMAND LINE:\n', cmd
                     m = measure(a,qsplit(cmd),delay,maxtime,of,ef,
                        logger=logger,affinitymask=affinitymask)

            # check the program output was as expected
            checkAndLog(m,ofName,logf)

            # add to the measurements
            ms.append(m)

            # if there was a problem finding the program just ignore the program output
            if not m.isMissing():
               with open(ofName,'r+') as of:
                  # append diff or ndiff or cmp output to log
                  if m.hasBadOutput():
                     with open(diffName(), 'r') as df:
                        logf.write( df.read() )

                  # append program output to log
                  if isNotChecked(testname):
                     logf.write( '\nPROGRAM OUTPUT NOT CHECKED:\n' )
                     logf.write( of.read() )
                  elif isCheckCmp(testname):
                     logf.write( '\n(BINARY) PROGRAM OUTPUT NOT SHOWN\n' )
                  elif getsize(ofName) > outputmax:
                     of.truncate(outputmax)
                     logf.write( '\n(TRUNCATED) PROGRAM OUTPUT:\n' )
                     logf.write( of.read() )
                  else:
                     logf.write( '\nPROGRAM OUTPUT:\n' )
                     logf.write( of.read() )

                  # append program stderr to log
                  if getsize(errName()):
                     with open(errName(), 'r') as ef:
                        logf.write( '\n' )
                        logf.write( ef.read() )               

         if m.isOkay():
            if m.hasExceeded(cutoff):
               repeatTestValue = None
            else:
               repeatTestValue = a
         else:
            break


      except IOError, err:
         if logger: logger.error(err)

      finally:
         sys.stdout.write('.'); sys.stdout.flush()
         loggerLine.write('.')

   sys.stdout.write( m.statusStr() ); sys.stdout.flush()
   loggerLine.write( m.statusStr() )

   return repeatTestValue



def measureProgramsRepeatLargest(name,programs,allowed,total):
   global loggerLine

   def fillInMissingRecords(ms):
      for a in testvalues[len(ms):]:
         c = copy.deepcopy(ms[-1:])
         if c:
            c[0].argString = a
            ms.append(c[0])

   setVariables(name)

   for p in programs:
      cleanTmpdirFor(p,allowed)

      sys.stdout.write('%s ' % strftime('%a %H:%M:%S', localtime()))
      sys.stdout.flush()

      if isexe(highlightExeName):
         callHighlightSourceCodeMarkup(p)
      else:
         copyfile( join(codedir,p.codeName), join(codedir,p.highlightName) )

      srcSize = sizeCompressedSourceCode(p)

      if hasMake(p.imp):
         callMake(p)

      loggerLine = StringIO()
      t = cmdTemplate(p)
      ms = []
      repeatTestValue = measureCheckAndLogRepeatLargest(p,t,ms)
      fillInMissingRecords(ms)
      if repeatTestValue:
         measureOnly(repeatTestValue,t,ms)

      # Write compressed dat file once, when all data is available
      try:
         datf = bz2.BZ2File( join(datdir,p.datName) ,'w')
         for m in ms:
            m.gz = srcSize
            print >>datf, m
      except IOError, err:
         if logger: logger.error(err)
      finally:
         datf.close()

      loggerLine.write('%s [%d]' % (p.programName,total))
      sys.stdout.write('%s [%d]\n' % (p.programName,total)); sys.stdout.flush()
      total -= 1

      if logger: logger.info( loggerLine.getvalue() )
      loggerLine.close()

   return total
   


# =============================
# Code Duplication END
# =============================



# =============================
# summary files
# =============================



def appendNames(fp,f):
   # append benchmark name, language implementation and program id
   f.write('%s,%s,%s,' % (fp.name,fp.imp,fp.id))



def appendToBothDataCsv(ms,fp,df,ndf):
   # for each test value find the measurements with lowest time
   d = {}
   mem = {}
   for each in ms: 
      # find lowest time
      v = d.get(each.arg,None)
      #if not v or (each.isOkay() and each.userSysTime < v.userSysTime):
      if not v or (each.isOkay() and each.elapsed < v.elapsed):
         d[each.arg] = each

      # find highest maxMem
      v = mem.get(each.arg,None)
      if not v or (each.isOkay() and each.maxMem > v):
         mem[each.arg] = each.maxMem

      # if there's something wrong then these measurements are bogus
      if not each.isOkay():
         each.userSysTime = 0.0
         each.maxMem = 0


   # set maxMem to the highest recorded memory for that test value
   for k,v in d.iteritems():
      v.maxMem = mem[k]

   # sort and append measurements to ndatacsv
   ms = d.values()
   ms.sort()
   for each in ms:
      appendNames(fp,ndf)
      ndf.write(str(each)); ndf.write('\n')

   # there should be a measurement for each test value
   # the measurements are sorted ascending by test value
   # the last measurement should be the largest test value
   # append that last measurement to datacsv
   try:
      appendNames(fp,df)
      df.write(str(ms[-1:][0])); df.write('\n')
   except IndexError, err:
      if logger: logger.error(err)



def appendToBulkdataCsv(ms,fp,bdf):
   # append every measurement to bulkdatacsv
   for each in ms:
      if each.isOkay():
         appendNames(fp,bdf)
         bdf.write(str(each)); bdf.write('\n')



def appendToCsv(d,path,filename,df,ndf,bdf):
   try:
      f = bz2.BZ2File( join(path,filename),'r')
      ms = [Record().fromString( s.rstrip('\n')) for s in f.readlines()]
      fp = FileNameParts(filename)
      appendToBulkdataCsv(ms,fp,bdf)
      appendToBothDataCsv(ms,fp,df,ndf)

   except IOError, err:
      if logger: logger.error(err)
   finally:
      f.close()



def walkDatFiles(df,ndf,bdf):
   subdirs = [each for each in os.listdir( dirs['tmp'] ) 
      if each in filters['onlydirs']]
   subdirs.sort()
   for d in subdirs: 
      try:
         path = join(d,datdirName)
         datfiles = os.listdir(path)
      except OSError, err: 
         if err[0] == ENOENT: 
            if logger: logger.debug(err)
            continue # No such file or directory
      else:
         if logger: logger.info('mkcsv building csv files from %s',path)
         for f in datfiles: appendToCsv(d,path,f,df,ndf,bdf)



def writeHeader(f):
   f.write('name,lang,id,n,size(B),cpu(s),mem(KB),status,load,elapsed(s)\n')



def makeSummaryFiles(iniName):
   os.chdir(dirs['tmp'])

   datacsv = 'filtered_measurements.csv'
   ndatacsv = 'fastest_measurements.csv'
   bulkdatacsv = 'all_measurements.csv'

   with nested( open( join(datacsv), 'w'),
                open( join(ndatacsv), 'w')) as (df,ndf):

      try:
         #bdf = bz2.BZ2File( join(bulkdatacsv), 'w')
         bdf = open( join(bulkdatacsv), 'w')

         writeHeader(df); writeHeader(ndf); writeHeader(bdf)
         walkDatFiles(df,ndf,bdf)

      except (OSError,IOError), err:
         if logger: logger.error(err)
      finally:
         bdf.close()

   try:
      if dirs['dat_sweep']:
         copy2( join(datacsv), dirs['dat_sweep'])
         copy2( join(ndatacsv), dirs['dat_sweep'])
         copy2( join(bulkdatacsv), dirs['dat_sweep'])
         if logger: logger.info('copy %s files to %s','*.csv',dirs['dat_sweep'])

   except (OSError,IOError), err:
      if logger: logger.error(err)



# =============================
# sweep .code & .log
# =============================


def sweepLogAndCodeFiles():

   def sweep(sweepName, pattern, subdirs, dirName):
      if dirs[sweepName]:
         if logger: logger.info('copy %s files to %s',pattern,dirs[sweepName])
         for d in subdirs: 
            try:
               path = join(d,dirName)
               fs = fnmatch.filter( os.listdir(path), pattern)
            except OSError, err: 
               if err[0] == ENOENT: 
                  if logger: logger.debug(err)
                  continue # No such file or directory
            else:
               for f in fs: 
                  copy2( join(path,f), dirs[sweepName])

   subdirs = [each for each in os.listdir( dirs['tmp'] ) 
      if each in filters['onlydirs']]

   sweep('code_sweep', '*.code', subdirs, codedirName)
   sweep('log_sweep', '*.log', subdirs, logdirName)



# =============================
# main
# =============================



def benchmarksGame(ini):
   checkDirectories()
   w = worklist()
   total = sum([len(s) for k,s,a in w])

   if total == 0:
      print 'nothing to be done - measurements are up-to-date'
      if logger: logger.info('nothing to be done - measurements are up-to-date')

   else:
      # do repeated measurements at every [testrange] value
      if repeatevery:
         for d,s,a in w:
            total = measurePrograms(d,s,a,total)

      # do repeated measurements at largest [testrange] value
      else:
         for d,s,a in w:
            total = measureProgramsRepeatLargest(d,s,a,total)

      _,iniName= split(ini)
      iniName,_ = splitext(iniName)
      makeSummaryFiles(iniName)
      sweepLogAndCodeFiles()



def force(items):
   items = set(items)
   onlydirs = set(filters['onlydirs'])
   dirsToForce = items.intersection(onlydirs)
   impsToForce = items.difference(onlydirs)

   for each in dirsToForce:
      d = join(dirs['tmp'],each,datdirName)
      if not isdir(d):
         if logger: logger.info('no %s directory - no *.dat to remove',d)
      else:
         if logger: logger.info('remove %s *.dat',d)
         fs = fnmatch.filter( os.listdir(d), '*.*' )
         for f in fs:
            try:
               os.remove( join(d,f) )
            except OSError, (e,err):
               if logger: logger.error('%s %s %s %s',e,err,d,f)

   for each in onlydirs:
      d = join(dirs['tmp'],each,datdirName)
      if not isdir(d):
         if logger: logger.info('no %s directory - no *.dat to remove',d)
      else:
         for i in impsToForce:
            fs = fnmatch.filter( os.listdir(d), '*.' + i + '_dat' )
            for f in fs:
               try:
                  os.remove( join(d,f) )
                  if logger: logger.info('remove %s %s',d,f)
               except OSError, (e,err):
                  if logger: logger.error('%s %s %s %s',e,err,d,f)



def main():
   print 'bencher release 0.9'

   try:
      # check for default directory locations
      setDefaultDirectories()

      # check for ini file in command line args
      options,remeasure = getopt(sys.argv[1:],'',['conf='])
      ini = None
      for o, v in options:
         if o in ('--conf'):
            ini = v

      # check for ini file in default location
      if not ini:
         ini = defaultIni()

      if ini:
         configure(ini)
         if dirs['tmp'] and isdir(dirs['tmp']):
            if not logger: configureLogger(logfilemax,dirs['tmp'])
         else:
            print 'No bencher/tmp'
            sys.exit(0)

         checkExes()

         if remeasure:
            force(remeasure)

         print planDesc
         benchmarksGame(ini)

      else:   
         print 'No .ini file'
         sys.exit(0)    

   except KeyboardInterrupt:
      if logger: logger.debug('Keyboard Interrupt')
      sys.exit(1)

   except GetoptError, err:
      if logger: logger.debug(err)
      print err
      sys.exit(2)

           
if __name__ == "__main__":
   main()

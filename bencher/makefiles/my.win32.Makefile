# The Computer Language Benchmarks Game
# $Id: my.win32.Makefile,v 1.2 2016/11/29 22:09:41 igouy-guest Exp $

# ASSUME each program will build in a clean empty tmpdir
# ASSUME there's a symlink to the program source in tmpdir
# ASSUME there's a symlink to the Include directory in tmpdir
# ASSUME there are symlinks to helper files in tmpdir
# ASSUME no responsibility for removing temporary files from tmpdir

# TYPICAL actions include an initial mv to give the expected extension 

# ASSUME environment variables for compilers and interpreters are set in the header


COPTS := -O3 -fomit-frame-pointer



############################################################
# ACTIONS for specific language implementations
############################################################


########################################
# java
########################################

%.java_run: %.java 
	-copy $< $(TEST).java
	-$(JDKC) -d . $(TEST).java


%.javaxint_run: %.javaxint
	-copy $< $(TEST).java
	-$(JDKC) -d . $(TEST).java


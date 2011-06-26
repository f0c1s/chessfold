# Make file for the Erlang Chess Move Generator
# Author: Francois Cardinaux
# Date: 2011.06.24
# 
# To compile for tests
#     make fortest
#     
# To run tests
#     make clean
#     make fortest
#     make testall > test_result.txt

# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W -o beams $<

.yrl.erl:
	erlc -W -o beams $<

ERL = erl -boot start_clean 

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character 
# to the end of the line and continue on the next line

# use vpath to tell make where to search for %.erl files (source http://stackoverflow.com/questions/1894363/how-to-make-two-different-source-directories-in-a-makefile-output-to-one-bin-dire)
vpath %.erl erl

# Edit the lines below
MODS = chessfold

# The first target in any makefile is the default target.
# If you just type "make" then "make all" is assumed (because
#   "all" is the first target in this makefile)

all: compile

compile: ${MODS:%=%.beam}

# Compile for tests

fortest:
	cd test; make
	
# the subdirs target compiles any code in 
# sub-directories

# remove all the code

clean:	
	rm -rf *.beam erl_crash.dump
	cd test; make clean  
	cd beams; make clean
	
# run unit tests

testall: 
	cd beams; make


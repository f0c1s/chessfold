# Make file for the Erlang Chess Move Generator
# Author: Francois Cardinaux
# Date: 2011.06.24
# 
# Use the Makefile in the parent directory instead.

# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character 
# to the end of the line and continue on the next line

# The first target in any makefile is the default target.
# If you just type "make" then "make all" is assumed (because
#   "all" is the first target in this makefile)

all: testall

# run unit tests

testall: 
	date 
	erl -run chessfold_test test -run init stop -noshell
	date

clean:	
	rm -rf *.beam erl_crash.dump test_result.txt


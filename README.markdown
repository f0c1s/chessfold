Erlang Chess Readme File
========================

Author: François Cardinaux

Overview of the current folder: 
* The current folder is dedicated to the erlang chess project
* It contains all files that are necessary to this project
* There is no symlink of any sort to external resources

The subfolders (updated on 2011.06.26): 
* beams:          the beam files
* erl:            Erlang part of the project
* test:           Erlang unit tests
* test_data:      any kind of test data
  
Credits
-------

For the algorithm:
 * http://stackoverflow.com/questions/494721/what-are-some-good-resources-for-writing-a-chess-engine
 * Jonatan Pettersson's blog: 
  * http://mediocrechess.blogspot.com/
  * http://mediocrechess.blogspot.com/2006/12/guide-move-generation.html
  * http://mediocrechess.blogspot.com/2006/12/0x88-representation.html
  * http://mediocrechess.blogspot.com/2006/12/guide-attacked-squares.html

For debugging and tests: 
 * Roce: http://www.rocechess.ch/rocee.html
 * Perft suite: http://hem.passagen.se/maragor/perftsuite.epd, which can be found inside Roce
 * http://mediocrechess.blogspot.com/2007/01/guide-perft-scores.html
 * http://www.albert.nu/programs/sharper/perft.asp
 * https://chessprogramming.wikispaces.com/Perft+Results
  
License
-------

This software is released under the GNU General Public License (GPL) version 3.

Conventions
-----------

Forsyth–Edwards Notation (FEN) is used to describe a particular board position: 
* http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation

How to run unit tests
---------------------

```
$ cd ~/path/to/chessfold
$ make fortest
$ make testall
```

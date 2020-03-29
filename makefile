.PHONY: all clean

all:
	dune build bin/main.bc.js

clean:
	dune clean

run:
	firefox html/index.html


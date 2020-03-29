.PHONY: all clean

all:
	dune build bin/main.bc.js

release:
	dune build bin/main.bc.js --profile release

clean:
	dune clean

run:
	firefox html/index.html


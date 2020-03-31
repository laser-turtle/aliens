.PHONY: all clean

all:
	dune build bin/main.bc.js
	cp _build/default/bin/main.bc.js html/.

release:
	dune build bin/main.bc.js --profile release
	cp _build/default/bin/main.bc.js html/.

clean:
	dune clean

run:
	firefox html/index.html


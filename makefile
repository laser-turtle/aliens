.PHONY: all clean

all:
	dune build bin/main.bc.js editor/editor.bc.js
	cp _build/default/bin/main.bc.js html/.
	cp _build/default/editor/editor.bc.js html/.

release:
	dune build bin/main.bc.js editor/editor.bc.js --profile release
	cp _build/default/bin/main.bc.js html/.
	cp _build/default/editor/editor.bc.js html/.

clean:
	dune clean

run:
	firefox html/index.html


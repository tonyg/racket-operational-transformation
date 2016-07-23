PACKAGENAMES=\
	operational-transformation-lib \
	operational-transformation-demo \
	operational-transformation

all: setup test

clean:
	find . -name compiled -type d | xargs rm -rf

setup:
	raco setup --pkgs $(PACKAGENAMES)

test:
	raco test --package $(PACKAGENAMES)

link:
	(for d in $(PACKAGENAMES); do (cd $$d; raco pkg install); done)

unlink:
	raco pkg remove $(PACKAGENAMES)


all: build

build:
	chicken-csc interpreter.scm

clean:
	rm interpreter

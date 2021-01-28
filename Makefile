
EXE=caskell

all: exe

exe:
	cabal build

run: exe
	cabal exec $(EXE)

runi:
	cabal repl

clean:
	cabal clean


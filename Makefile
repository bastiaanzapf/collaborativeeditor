
development: stub.o main.js server

server: main.hs
	ghc main.hs -o server stub.o

main.js: main.hs Editor.hs ConsoleLog.hs JSHash.hs
	~/.cabal/bin/hastec --with-js=externals.js main.hs --debug

stub.o:
	gcc -c stub.c


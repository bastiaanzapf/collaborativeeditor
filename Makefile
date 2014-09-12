
development: stub.o main.js server

server: main.hs
	ghc main.hs -o server 

main.js: main.hs Editor.hs ConsoleLog.hs JSHash.hs JSEscape.hs
	~/.cabal/bin/hastec --with-js=externals.js main.hs --debug


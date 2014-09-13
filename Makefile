
development: stub.o main.js server

server: main.hs
	ghc main.hs -o server stub.o

main.js: main.hs Editor.hs ConsoleLog.hs JSHash.hs JSEscape.hs Profile.hs
	~/.cabal/bin/hastec main.hs --with-js=externals.js


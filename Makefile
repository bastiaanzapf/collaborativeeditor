
development: stub.o main.js server

server: main.hs JSHash.hs Server.hs Operations.hs WCharacter.hs
	ghc main.hs -o server 

main.js: main.hs Editor.hs ConsoleLog.hs JSHash.hs JSEscape.hs Profile.hs Operations.hs WCharacter.hs Client.hs Caret.hs externals.js 
	~/.cabal/bin/hastec main.hs --with-js=externals.js

main-production.js: main.js
	yui-compressor main.js > main-production.js


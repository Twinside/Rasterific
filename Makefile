
build:
	cabal build

doc:
	cabal haddock

depinstall:
	cabal install -j4 --only-dependencies

run:
	dist/build/test/test
	
test:
	dist/build/test/test

lint:
	hlint lint src


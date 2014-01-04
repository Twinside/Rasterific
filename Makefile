
build:
	cabal build

depinstall:
	cabal install -j4 --only-dependencies

test:
	dist/build/test/test

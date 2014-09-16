
build:
	cabal build

doc:
	cabal haddock

depinstall:
	cabal install -j4 --only-dependencies

test:
	dist/build/test/test

formula:
	eq eval -f forward_diff.eq -o rez.txt


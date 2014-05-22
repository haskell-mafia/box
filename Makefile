MFLAGS = -s
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox

.PHONY: test build deps repl

default: repl

${SANDBOX}:
	cabal sandbox init

build: ${SANDBOX}
	cabal configure  && cabal build

test: ${SANDBOX}
	cabal configure --enable-tests  && cabal build

deps: ${SANDBOX}
	cabal install --only-dependencies

repl: ${SANDBOX}
	cabal configure --enable-tests && cabal repl

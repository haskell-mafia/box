MFLAGS = -s
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox

.PHONY: test build deps repl repl-test

default: repl

${SANDBOX}:
	cabal sandbox init

build: ${SANDBOX}
	cabal configure  && cabal build

test: ${SANDBOX}
	cabal test

deps: ${SANDBOX}
	cabal install --only-dependencies --enable-tests

repl: ${SANDBOX}
	cabal repl

repl-test: ${SANDBOX}
	cabal configure --enable-tests && cabal repl test

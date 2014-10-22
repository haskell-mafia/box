MFLAGS = -s
MAKEFLAGS = $(MFLAGS)
SANDBOX = .cabal-sandbox
DEPS = .cabal-sandbox/.cairn

.PHONY: test build deps repl repl-test

default: repl

${SANDBOX}:
	cabal sandbox init

%.cabal:

${DEPS}: ${SANDBOX} $(wildcard *.cabal)
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests ${CABAL_FLAGS}
	touch $@

build: ${DEPS}
	cabal configure  && cabal build

test: ${DEPS}
	cabal test

repl: ${DEPS}
	cabal repl

repl-test: ${DEPS}
	cabal repl test

quick: ${DEPS}
	ghci -package-db=$(wildcard ${SANDBOX}/*-packages.conf.d) -isrc -itest test/test.hs

spec: ${DEPS}
	runhaskell -package-db=$(wildcard ${SANDBOX}/*-packages.conf.d) -isrc -itest test/Spec.hs

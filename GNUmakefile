################################################################################
SITE = dist/build/site/site

################################################################################
.PHONEY: all clean test

################################################################################
all:
	cabal configure && cabal build
	(cd example && \
	  cabal configure && cabal build && $(SITE) build)

################################################################################
clean:
	cabal clean
	(cd example && test -r $(SITE) && $(SITE) clean || echo)
	(cd example && cabal clean)

################################################################################
test:
	cabal configure --enable-tests && \
	  cabal build && cabal test

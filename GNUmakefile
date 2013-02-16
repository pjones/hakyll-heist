################################################################################
.PHONEY: all clean test

################################################################################
all:
	cabal configure && cabal build
	(cd example && \
	  cabal configure && cabal build)

################################################################################
clean:
	cabal clean
	(cd example && cabal clean)

################################################################################
test:
	cabal configure --enable-tests && \
	  cabal build && cabal test

CONF_FLAGS=--enable-tests --enable-library-coverage

all:
		cabal configure $(CONF_FLAGS)
		cabal build
		-cabal test

server: all
		sudo dist/build/TextToMath/TextToMath

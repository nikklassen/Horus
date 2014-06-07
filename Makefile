CONF_FLAGS=--enable-tests

all:
		cabal configure $(CONF_FLAGS)
		cabal build
		-cabal test

coverage: CONF_FLAGS += --enable-library-coverage
coverage: all

server: all
		sudo dist/build/TextToMath/TextToMath

clean:
		rm -f TextToMath.tix
		cabal clean

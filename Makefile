all: build doc

.PHONY: init build doc test ghc-7.6.1 unittyped


init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build

test: build
	cabal test

install: init build test
	cabal install

doc:
	cabal haddock --hyperlink-source


ghc-7.6.1:
	wget --quiet -O ghc.tar.bz2 http://www.haskell.org/ghc/dist/7.6.1/ghc-7.6.1-x86_64-unknown-linux.tar.bz2
	tar xf ghc.tar.bz2
	sudo apt-get install libgmp3c2 libgmp3-dev  libghc-zlib-dev freeglut3-dev -y
	cd ghc-7.6.1/; ./configure;	sudo make install
	cabal install cabal-install
	cabal update
	ghc --version
	cabal --version

unittyped:
	hg clone https://bitbucket.org/xnyhps/haskell-unittyped
	cd haskell-unittyped; cabal install

embeddock:
	git clone git://github.com/nushio3/embeddock.git
	cd embeddock; cabal install

STACK := $(shell which stack 2> /dev/null)

all: stack-setup-build move-bins

ifdef STACK
	@echo "stack was found"
else
	@echo "stack was not installed; installing now"
	curl -sSL https://get.haskellstack.org/ | sh
endif

stack-setup-build:
	stack setup
	stack build

move-bins:
	mv $(shell stack exec which mapper) haskell-mapper-solution-binary
	mv $(shell stack exec which reducer) haskell-reducer-solution-binary

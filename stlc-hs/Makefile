.PHONY: build
build: syntax
	bash ./build.sh

.PHONY: syntax
syntax: src/stlc-syntax/Language/STLC/Syntax/STLC.cf
	bnfc --haskell --functor -p Language.STLC.Syntax.Generated -o src/stlc-syntax/ src/stlc-syntax/Language/STLC/Syntax/STLC.cf

.PHONY: clean
clean:/home/lllssskkk/Downloads/work-main/fourmolu.yaml
	cabal clean
	rm -rf src/stlc-syntax/Language/STLC/Syntax/Generated/*

EXTENSION := -o -XImportQualifiedPost

# Run fourmolu formatter
.PHONY: format
format:
	fourmolu --mode inplace $(EXTENSION) $(shell fd -ehs)

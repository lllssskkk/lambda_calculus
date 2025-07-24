.PHONY: build
build: syntax
	bash ./build.sh

.PHONY: syntax
syntax: src/stlc-interpreter/Language/STLC/Syntax/STLC.cf
	bnfc --haskell --functor -p Language.STLC.Syntax.Generated -o src/stlc-interpreter/ src/stlc-interpreter/Language/STLC/Syntax/STLC.cf

.PHONY: clean
clean:/home/lllssskkk/Downloads/work-main/fourmolu.yaml
	cabal clean
	rm -rf src/stlc-interpreter/Language/STLC/Syntax/Generated/*

EXTENSION := -o -XImportQualifiedPost

# Run fourmolu formatter
.PHONY: format
format:
	fourmolu --mode inplace $(EXTENSION) $(shell fd -ehs)

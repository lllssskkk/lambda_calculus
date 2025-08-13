.PHONY: build
build: syntax
	bash ./build.sh

.PHONY: syntax
syntax: src/letpoly-interpreter/Language/LetPoly/Syntax/LetPoly.cf
	bnfc --haskell --functor -p Language.LetPoly.Syntax.Generated -o src/letpoly-interpreter/ src/letpoly-interpreter/Language/LetPoly/Syntax/LetPoly.cf

.PHONY: clean
clean:/home/lllssskkk/Downloads/work-main/fourmolu.yaml
	cabal clean
	rm -rf src/letpoly-interpreter/Language/LetPoly/Syntax/Generated/*

EXTENSION := -o -XImportQualifiedPost

# Run fourmolu formatter
.PHONY: format
format:
	fourmolu --mode inplace $(EXTENSION) $(shell fd -ehs)

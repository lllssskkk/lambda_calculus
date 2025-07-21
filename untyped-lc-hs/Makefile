.PHONY: build
build: syntax
	bash ./build.sh

.PHONY: syntax
syntax: src/untypedLC-interpreter/Language/UntypedLC/Syntax/UntypedLC.cf
	bnfc --haskell --functor -p Language.UntypedLC.Syntax.Generated -o src/untypedLC-interpreter/ src/untypedLC-interpreter/Language/UntypedLC/Syntax/UntypedLC.cf

.PHONY: clean
clean:/home/lllssskkk/Downloads/work-main/fourmolu.yaml
	cabal clean
	rm -rf src/untypedLC-interpreter/Language/UntypedLC/Syntax/Generated/*

EXTENSION := -o -XImportQualifiedPost

# Run fourmolu formatter
.PHONY: format
format:
	fourmolu --mode inplace $(EXTENSION) $(shell fd -ehs)

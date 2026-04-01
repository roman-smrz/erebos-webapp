all: gen/Version/Git.hs
	wasm32-wasi-cabal build
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $(shell wasm32-wasi-cabal list-bin erebos-webapp) -o ghc_wasm_jsffi.js

gen/Version/Git.hs: FORCE
	@mkdir -p gen/Version
	@if [ -d .git ]; then echo "module Version.Git where gitVersion :: Maybe String; gitVersion = Just \"$$(git describe --always --dirty=' (dirty)')\"" > $@; fi

FORCE: ;

DEFAULT_GOAL: help

#====================== Dev =====================

.PHONY: provision
## Install dev tools
provision:
	@cabal install fswatcher
	@cabal install cabal-hoogle
	@cabal install hpack

.PHONY: hoogle-generate
## Generate hoogle
hoogle-generate:
	@cabal-hoogle generate

.PHONY: hoogle-server
## Run hoogle server
hoogle-server:
	@cabal-hoogle run -- server --local --port 9000

.PHONY: dev
## Run cabal build on any file change
dev:
	@fswatcher \
		--path . \
		--throttle 1000 \
		--include ".+\.(?:hs|cabal|yaml|x)" \
		--exclude "dist.*|\.stack-work/.*" \
		make build-quick

#==================== Install ===================

.PHONY: build-quick
## Build without optimization
build-quick:
	@hpack
	@cabal build --disable-optimization

.PHONY: install
## Install the binaries
install:
	@hpack
	@cabal install \
		--install-method=copy \
		--overwrite-policy=always


#===================== Help =====================

.PHONY: help
# Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

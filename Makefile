SHELL         := /bin/bash
BUILD_DIR     := build
CABAL_VERSION := 3.14.2.0
GHC_VERSION   := 9.12.2
EMSDK_VERSION := 3.1.74

.PHONY: build debug clean update init-tools

build:
	mkdir -p $(BUILD_DIR)

	cp -v -r static/* $(BUILD_DIR)

	cabal v2-build --project-file cabal-js.project --allow-newer

	@INPUT_JS=$(shell cabal list-bin ItemAmountCalculator-exe --project-file cabal-js.project --allow-newer --verbose=0).jsexe/all.js && \
	OUTPUT_JS="$(BUILD_DIR)/index.js" && \
	if [ "$(PRODUCTION)" = "1" ]; then \
		node-minify --compressor uglify-js --input "$$INPUT_JS" --output "$$OUTPUT_JS"; \
	else \
		cp -v "$$INPUT_JS" "$$OUTPUT_JS"; \
	fi

	@echo ""
	@echo "BUILD SUCCESSFUL."
	@echo ""

debug: build
ifeq (, $(shell which http-server))
	sudo npm install -g http-server
endif

	http-server $(BUILD_DIR)

clean:
	cabal v2-clean
	rm -rf $(BUILD_DIR)

	@echo ""
	@echo "CLEAN SUCCESSFUL."
	@echo ""

update:
	cabal v2-update

	@echo ""
	@echo "UPDATE SUCCESSFUL."
	@echo ""

init-tools:
ifeq (, $(shell which npm))
	$(error Please install npm)
endif
ifeq (, $(shell which curl))
	$(error Please install curl)
endif
	
	make -f <(curl -fsSL https://raw.githubusercontent.com/t-sasaki915/haskell-makefiles/refs/tags/0.1.0.0/ghc/javascript-unknown-ghcjs-ghc/Makefile)
	make -f <(curl -fsSL https://raw.githubusercontent.com/t-sasaki915/haskell-makefiles/refs/tags/0.1.0.0/utils/haskell-tools/Makefile) ONLY_CABAL=1

	sudo npm install -g @node-minify/cli @node-minify/uglify-js

	@echo ""
	@echo "INIT-ENV SUCCESSFUL."
	@echo ""
